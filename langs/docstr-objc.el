;;; docstr-objc.el --- Document string for Objective-C  -*- lexical-binding: t; -*-

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
;; Document string for Objective-C.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-objc-style 'header-doc
  "Style specification for document string in Objective-C."
  :type '(choice
          (const :tag "No specify" nil)
          (const :tag "HeaderDoc documentation generator developed by Apple Inc" header-doc))
  :group 'docstr)

(defcustom docstr-objc-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-objc-config-header-doc ()
  "Configure for convention, HeaderDoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-format-var "%s"))

(defun docstr-objc-config ()
  "Automatically configure style according to variable `docstr-objc-style'."
  (cl-case docstr-objc-style
    (header-doc (docstr-objc-config-header-doc))
    (t (docstr-util-default-format))))

(defun docstr-objc--param-list (search-string)
  "Parse SEARCH-STRING for Objective-C."
  (let ((param-split (split-string search-string ":"))
        lst-type lst-var)
    (pop param-split)  ; Remove function name section
    (dolist (param param-split)
      (let (tokens type var)
        (when (string-match "[(]\\([^)]*\\)[)]" param)
          (setq type (match-string 0 param))
          (setq param (s-replace type "" param))
          (setq type (s-replace "(" "" type)
                type (s-replace ")" "" type)
                type (string-trim type))
          (push type lst-type))
        (setq tokens (split-string param " " t)
              var (nth 0 tokens))
        (push var lst-var)))
    (list (reverse lst-type) (reverse lst-var))))

(defun docstr-objc--return-type (search-string)
  "Return the return type from SEARCH-STRING."
  (let ((return-type-str "void")
        (front-split (nth 0 (split-string search-string ":" t))))
    (when (string-match "[(]\\([^)]*\\)[)]" front-split)
      (setq return-type-str (match-string 0 front-split)
            return-type-str (s-replace "(" "" return-type-str)
            return-type-str (s-replace ")" "" return-type-str)
            return-type-str (string-trim return-type-str)))
    return-type-str))

;;; Writer

(defun docstr-writers-objc (search-string)
  "Insert document string for Objective-C using SEARCH-STRING."
  (docstr-objc-config)
  (let* ((start (point)) (prefix docstr-objc-prefix)
         (paren-param-list (docstr-objc--param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str (docstr-objc--return-type search-string)))
    (unless (= 0 param-var-len)
      (insert "\n" docstr-objc-prefix))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

(provide 'docstr-objc)
;;; docstr-objc.el ends here
