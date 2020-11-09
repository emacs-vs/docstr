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

(require 'docstr-util)

;;
;; (@* "Analyzer" )
;;

(defun docstr-writers--analyze-param-string (search-string)
  "Get rid of the open and close parentheses, only get the center part.
SEARCH-STRING : string that use to analyze."
  (let (pos param-string)
    (setq param-string (substring search-string
                                  (1+ (jcs-last-regex-in-string "(" search-string))
                                  (length search-string))
          pos (jcs-last-regex-in-string ")" param-string)
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
    (when (jcs--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '())
            (param-split-str-lst-len -1) (param-split-str-lst-len-1 -1)
            (param-var-str "") (param-type-str ""))
        (setq param-sec-string (nth 0 (split-string param-sec-string "=")))
        (setq param-split-str-lst (jcs-chop param-sec-string " "))

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

See `jcs-paren-param-list' function for argument description SEARCH-STRING.

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
    (when (jcs--param-empty-p param-lst)
      (setq param-lst '()))

    (dolist (param-sec-string param-lst)
      (let ((param-split-str-lst '())
            (param-var-str "") (param-type-str ""))
        ;; First remove the possible default value.
        (setq param-sec-string (nth 0 (split-string param-sec-string "=[^>]")))
        (setq param-split-str-lst (split-string param-sec-string spi-sym))
        (setq param-var-str (string-trim (nth 0 param-split-str-lst)))
        (if (= (length param-split-str-lst) 1)
            ;; Set default type name string here.
            (setq param-type-str jcs--default-typename-string)
          (setq param-type-str (string-trim (nth 1 param-split-str-lst))))

        (when last-word
          (setq param-var-str (split-string param-var-str " " t)
                param-var-str (nth (1- (length param-var-str)) param-var-str)))

        (push param-var-str param-var-str-lst)
        (push param-type-str param-type-str-lst)))

    (setq param-type-strings (reverse param-type-str-lst))
    (setq param-variable-strings (reverse param-var-str-lst))

    (push param-type-strings result-datas)
    (push param-variable-strings result-datas)

    (setq result-datas (reverse result-datas))
    result-datas))

;;
;; (@* "Writers" )
;;

(defun docstr-actionscript-writer (search-string)
  "Insert document string for ActionScript using SEARCH-STRING."
  )

;;
;; (@* "Configurations" )
;;

(defcustom docstr-writer-alist
  `((actionscript-mode . docstr-actionscript-writer)
    (c-mode            . docstr-c-writer)
    (c++-mode          . docstr-c++-writer)
    (csharp-mode       . docstr-csharp-writer)
    (go-mode           . docstr-go-writer)
    (groovy-mode       . docstr-groovy-writer)
    (groovy-mode       . docstr-groovy-writer)
    (java-mode         . docstr-java-writer)
    (javascript-mode   . docstr-javascript-writer)
    (js-mode           . docstr-javascript-writer)
    (js2-mode          . docstr-javascript-writer)
    (js3-mode          . docstr-javascript-writer)
    (lua-mode          . docstr-lua-writer)
    (masm-mode         . docstr-asm-writer)
    (nasm-mode         . docstr-asm-writer)
    (php-mode          . docstr-php-writer)
    (python-mode       . docstr-python-writer)
    (rjsx-mode         . docstr-rjsx-writer)
    (rust-mode         . docstr-rust-writer)
    (scala-mode        . docstr-scala-writer)
    (typescript-mode   . docstr-typescript-writer)
    (web-mode          . docstr-php-writer))
  "List of writer to each `major-mode'."
  :type 'list
  :group 'docstr)

(provide 'docstr-writers)
;;; docstr-writers.el ends here
