;;; docstr-writers.el --- Writers for document string  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh <jcs090218@gmail.com>

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

(require 's)

(require 'docstr-util)

;;
;; (@* "Analyzer" )
;;

(defun docstr-writers-insert-jsdoc-type (type-name open-char close-char)
  "Insert the curly bracket by this order => OPEN-CHAR, TYPE-NAME, CLOSE-CHAR."
  (insert open-char)
  (insert type-name)
  (insert close-char))

(defun docstr-writers--param-empty-p (param-lst)
  "Check if the full PARAM-LST empty."
  (let ((param-lst-len (length param-lst))
        (index 0) (is-empty t))
    (while (and (< index param-lst-len) is-empty)
      (unless (string= "" (string-trim (nth index param-lst)))
        (setq is-empty nil))
      (setq index (1+ index)))
    is-empty))

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
  (let ((param-string "") (param-lst '())
        (param-type-str-lst '()) (param-var-str-lst '())
        (param-type-strings nil) (param-variable-strings nil)
        (result-datas '()))
    (setq param-string (docstr-writers--analyze-param-string search-string))

    (setq param-lst (split-string param-string ","))
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
            (if (string= param-type-str "") (setq sep "") (setq sep " "))
            (setq param-type-str (concat param-type-str sep (string-trim (nth index param-split-str-lst))))
            (setq index (1+ index))))

        (unless (string= "" param-var-str)
          (push param-var-str param-var-str-lst))
        (unless (string= "" param-type-str)
          (push param-type-str param-type-str-lst))))

    (setq param-type-strings (reverse param-type-str-lst))
    (setq param-variable-strings (reverse param-var-str-lst))

    (push param-type-strings result-datas)
    (push param-variable-strings result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

(defun docstr-writers--paren-param-list-behind (search-string &optional spi-sym last-word)
  "Like `docstr-writers--paren-param-list' but handle programming languages that use \
colon to separate the type.

Support format like

  `(var-name : type-name, var-name : type-name)`

or with default value

  `(var-name : type-name = default-val, var-name : type-name = default-val)`.

See `docstr-writers--paren-param-list' function for argument description SEARCH-STRING.

An optional argument SPI-SYM is the split symbol for return type.  In most cases,
this symbol often will be a 'colon'.

If optional argument LAST-WORD is non-nil; then limit the variable name to the
last word only."
  (let ((param-string "") (param-lst '())
        (param-type-str-lst '()) (param-var-str-lst '())
        (param-type-strings nil) (param-variable-strings nil)
        (result-datas '()))
    (setq param-string (docstr-writers--analyze-param-string search-string))

    (setq param-lst (split-string param-string ","))
    (when (docstr-writers--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '())
            (param-var-str "") (param-type-str ""))
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

    (setq param-type-strings (reverse param-type-str-lst)
          param-variable-strings (reverse param-var-str-lst))

    (push param-type-strings result-datas)
    (push param-variable-strings result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

;;
;; (@* "Writers" )
;;

(defun docstr-writers-actionscript (search-string)
  "Insert document string for ActionScript using SEARCH-STRING."
  (let* ((paren-param-list (jcs-paren-param-list-behind search-string ":"))
         (param-type-strings (nth 0 paren-param-list))
         (param-variable-strings (nth 1 paren-param-list))
         (param-var-len (length param-variable-strings))
         (param-index 0)
         ;; Get all return data types.
         (return-type-str (jcs--return-type-behind search-string ":"))
         (there-is-return (not (null return-type-str)))
         (start (point)))

    ;; Process param tag.
    (while (< param-index param-var-len)
      (insert "\n * ")  ; start from newline.
      (let ((type (nth param-index param-type-strings))
            (var (nth param-index param-variable-strings)))
        (insert (docstr-form-param type var docstr-desc-param)))
      ;; add up counter.
      (setq param-index (1+ param-index)))

    ;; Lastly, process returns tag.
    (when there-is-return
      (unless (string= return-type-str "void")
        (insert "\n * ")
        (insert (docstr-form-return return-type-str "" docstr-desc-return))))
    (indent-region start (point))
    (goto-char start)))

;;
;; (@* "Configurations" )
;;

(defcustom docstr-writer-alist
  `((actionscript-mode . docstr-writers-actionscript)
    (c-mode            . docstr-writers-c)
    (c++-mode          . docstr-writers-c++)
    (csharp-mode       . docstr-writers-csharp)
    (go-mode           . docstr-writers-go)
    (groovy-mode       . docstr-writers-groovy)
    (groovy-mode       . docstr-writers-groovy)
    (java-mode         . docstr-writers-java)
    (javascript-mode   . docstr-writers-javascript)
    (js-mode           . docstr-writers-javascript)
    (js2-mode          . docstr-writers-javascript)
    (js3-mode          . docstr-writers-javascript)
    (lua-mode          . docstr-writers-lua)
    (masm-mode         . docstr-writers-asm)
    (nasm-mode         . docstr-writers-asm)
    (php-mode          . docstr-writers-php)
    (python-mode       . docstr-writers-python)
    (rjsx-mode         . docstr-writers-rjsx)
    (rust-mode         . docstr-writers-rust)
    (scala-mode        . docstr-writers-scala)
    (typescript-mode   . docstr-writers-typescript)
    (web-mode          . docstr-writers-php))
  "List of writer to each `major-mode'."
  :type 'list
  :group 'docstr)

(provide 'docstr-writers)
;;; docstr-writers.el ends here
