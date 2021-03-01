;;; docstr-writers.el --- Writers for document string  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Shen, Jen-Chieh <jcs090218@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Writers for document string.
;;

;;; Code:

(require 'cl-lib)
(require 's)

(require 'docstr-util)

(declare-function docstr-form-param "ext:docstr.el")
(declare-function docstr-form-return "ext:docstr.el")

(defvar docstr-default-typename)
(defvar docstr-desc-param)
(defvar docstr-desc-return)
(defvar docstr-show-return)

;;
;; (@* "Analyzer" )
;;

(defun docstr-writers--param-empty-p (param-lst)
  "Check if the full PARAM-LST empty."
  (let ((param-lst-len (length param-lst))
        (index 0) (is-empty t))
    (while (and (< index param-lst-len) is-empty)
      (unless (string= "" (string-trim (nth index param-lst)))
        (setq is-empty nil))
      (setq index (1+ index)))
    is-empty))

(defun docstr-writers--function-name (search-string)
  "Analyze SEARCH-STRING to get function name."
  (let ((pos (docstr-util-last-regex-in-string "(" search-string)) fn-str)
    (when pos
      (setq fn-str (substring search-string 0 pos)
            fn-str (split-string fn-str " " t)
            fn-str (nth (1- (length fn-str)) fn-str)))
    (if (stringp fn-str) (string-trim fn-str) nil)))

(defun docstr-writers--return-type (search-string)
  "Analyze SEARCH-STRING to get return type.
This is for c-like programming languages."
  (let ((pos (docstr-util-last-regex-in-string "(" search-string))
        return-type-str)
    (when pos
      (setq return-type-str (substring search-string 0 pos)
            return-type-str (split-string return-type-str " " t)
            return-type-str (nth (- (length return-type-str) 2) return-type-str)))
    (if (stringp return-type-str) (string-trim return-type-str) nil)))

(defun docstr-writers--return-type-behind (search-string &optional spi-sym)
  "Analyze SEARCH-STRING to get return type.

This is for colon type programming languages.  For example, `actionscript',
`typescript', etc.

An optional argument SPI-SYM is the split symbol for return type."
  (let ((pos (docstr-util-last-regex-in-string ")" search-string))
        return-type-str)
    (when pos
      (setq return-type-str (substring search-string (1+ pos) (length search-string)))
      (when spi-sym
        (setq return-type-str (nth 1 (split-string return-type-str spi-sym)))))
    (if (and (stringp return-type-str)
             (not (string-empty-p return-type-str)))
        (string-trim return-type-str)
      nil)))

(defun docstr-writers--analyze-param-string (search-string)
  "Get rid of the open and close parentheses, only get the center part.
SEARCH-STRING : string that use to analyze."
  (let (pos param-string)
    (setq param-string (substring search-string
                                  (1+ (docstr-util-last-regex-in-string "(" search-string))
                                  (length search-string))
          pos (docstr-util-last-regex-in-string ")" param-string)
          param-string (substring param-string 0 pos))
    param-string))

(defun docstr-writers--paren-param-list (search-string)
  "Return parentheses type parameter list using SEARCH-STRING.

This will works with programming language that define function like this

  `(type-name var-name, type-name var-name)`

or with default value

  `(type-name var-name = default-val, type-name var-name = default-val)`."
  (let ((param-string "") param-lst
        param-type-str-lst param-var-str-lst
        param-types param-vars result-datas)
    (setq param-string
          (ignore-errors (docstr-writers--analyze-param-string search-string)))

    (when (stringp param-string)
      (setq param-lst (split-string param-string ",")))
    (when (docstr-writers--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '())
            (param-split-str-lst-len -1) (param-split-str-lst-len-1 -1)
            (param-var-str "") (param-type-str ""))
        (setq param-sec-string (nth 0 (split-string param-sec-string "=")))
        (setq param-split-str-lst (docstr-util-chop param-sec-string " "))

        (delete-dups param-split-str-lst)
        (setq param-split-str-lst (remove " " param-split-str-lst))

        (setq param-split-str-lst-len (length param-split-str-lst))
        (setq param-split-str-lst-len-1 (1- param-split-str-lst-len))

        ;; Variable name should always be that last element in the list.
        (setq param-var-str (string-trim (nth param-split-str-lst-len-1 param-split-str-lst)))

        ;; Data type name should be the rest except the last element.
        (let ((index 0) (sep ""))
          (while (< index param-split-str-lst-len-1)
            (setq sep (if (string= param-type-str "") "" " "))
            (setq param-type-str (concat param-type-str sep (string-trim (nth index param-split-str-lst))))
            (setq index (1+ index))))

        (unless (string= "" param-var-str)
          (push param-var-str param-var-str-lst))
        (unless (string= "" param-type-str)
          (push param-type-str param-type-str-lst))))

    (setq param-types (reverse param-type-str-lst))
    (setq param-vars (reverse param-var-str-lst))

    (push param-types result-datas)
    (push param-vars result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

(defun docstr-writers--paren-param-list-behind (search-string &optional spi-sym last-word)
  "Like `docstr-writers--paren-param-list' but handle programming languages \
that use colon to separate the type.

Support format like

  `(var-name : type-name, var-name : type-name)`

or with default value

  `(var-name : type-name = default-val, var-name : type-name = default-val)`.

See `docstr-writers--paren-param-list' function for argument description
SEARCH-STRING.

Optional argument SPI-SYM is the split symbol for return type.  In most
cases, this symbol often will be a 'colon'.

If optional argument LAST-WORD is non-nil; then limit the variable name to
the last word only."
  (let ((param-string "") param-lst
        param-type-str-lst param-var-str-lst
        param-types param-vars result-datas)
    (setq param-string
          (ignore-errors (docstr-writers--analyze-param-string search-string)))

    (when (stringp param-string)
      (setq param-lst (split-string param-string ",")))
    (when (docstr-writers--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '()) (param-var-str "") (param-type-str ""))
        ;; First remove the possible default value.
        (setq param-sec-string (nth 0 (split-string param-sec-string "=[^>]"))
              param-split-str-lst (split-string param-sec-string spi-sym)
              param-var-str (string-trim (nth 0 param-split-str-lst)))
        (if (= (length param-split-str-lst) 1)
            ;; Set default type name string here.
            (setq param-type-str docstr-default-typename)
          (setq param-type-str (string-trim (nth 1 param-split-str-lst))))

        (when last-word
          (setq param-var-str (split-string param-var-str " " t)
                param-var-str (nth (1- (length param-var-str)) param-var-str)))

        (push param-var-str param-var-str-lst)
        (push param-type-str param-type-str-lst)))

    (setq param-types (reverse param-type-str-lst)
          param-vars (reverse param-var-str-lst))

    (push param-types result-datas)
    (push param-vars result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

;;
;; (@* "Writers" )
;;

(defun docstr-writers--valid-return-type-p (return-type-str ignore-lst)
  "Return non-nil if RETURN-TYPE-STR is valid compare to IGNORE-LST."
  (and docstr-show-return
       (stringp return-type-str)
       (not (string-empty-p return-type-str))
       (not (docstr-util-contain-list-string ignore-lst return-type-str))))

(defun docstr-writers--insert-param (param-types param-vars prefix &optional postfix)
  "Insert parameter section.

Argument PARAM-TYPES is a list of string contain type name data.
Argument PARAM-VARS is a list of string contain variable name data.
Argument PREFIX is string infront of each document string.
Argument POSTFIX is string behind of each document string."
  (let ((param-var-len (length param-vars)) (param-index 0))
    (while (< param-index param-var-len)
      (let ((type (nth param-index param-types)) (var (nth param-index param-vars)))
        (docstr-util-insert prefix)
        (docstr-util-insert-args (docstr-form-param type var docstr-desc-param))
        (when postfix (insert postfix)))
      (setq param-index (1+ param-index)))))

(defun docstr-writers--insert-return (return-type-str ignore-lst prefix &optional postfix)
  "Insert return section.

Argument RETURN-TYPE-STR is a string contain return type name.  Argument
IGNORE-LST is a list of string contain return type that we want to skip.
Argument PREFIX is string infront of return document string.
Argument POSTFIX is string behind of return document string."
  (when (docstr-writers--valid-return-type-p return-type-str ignore-lst)
    (docstr-util-insert prefix)
    (docstr-util-insert-args (docstr-form-return return-type-str "" docstr-desc-return))
    (when postfix (insert postfix))))

(defun docstr-writers-after (start &optional ind-r ind-l restore-pt)
  "Do stuff after document string insertion.

Argument START is the starting point ot the insertion.

If optional argument IND-R is non-nil, indent region once.  If optional
argument IND-L is non-nil, indent currnet line once.  If optional argument
RESTORE-PT is non-nil, go back to starting position."
  (when ind-r
    (indent-region start (point))  ; For single line comment
    (indent-region (docstr-util-start-comment-point)  ; For multi-line comment
                   (docstr-util-end-comment-point)))
  (when ind-l (indent-for-tab-command))
  (when restore-pt (goto-char start)))

;;
;; (@* "Configurations" )
;;

(defcustom docstr-writers-alist
  `((actionscript-mode . docstr-writers-actionscript)
    (c-mode            . docstr-writers-c)
    (c++-mode          . docstr-writers-c++)
    (csharp-mode       . docstr-writers-csharp)
    (go-mode           . docstr-writers-golang)
    (groovy-mode       . docstr-writers-groovy)
    (java-mode         . docstr-writers-java)
    (javascript-mode   . docstr-writers-javascript)
    (js-mode           . docstr-writers-javascript)
    (js2-mode          . docstr-writers-javascript)
    (js3-mode          . docstr-writers-javascript)
    (lua-mode          . docstr-writers-lua)
    (objc-mode         . docstr-writers-objc)
    (php-mode          . docstr-writers-php)
    (python-mode       . docstr-writers-python)
    (rjsx-mode         . docstr-writers-javascript)
    (ruby-mode         . docstr-writers-ruby)
    (rust-mode         . docstr-writers-rust)
    (scala-mode        . docstr-writers-scala)
    (swift-mode        . docstr-writers-swift)
    (typescript-mode   . docstr-writers-typescript)
    (web-mode          . docstr-writers-php))
  "List of writers to each `major-mode'."
  :type 'list
  :group 'docstr)

(provide 'docstr-writers)
;;; docstr-writers.el ends here
