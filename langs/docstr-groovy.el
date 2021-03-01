;;; docstr-groovy.el --- Document string for Groovy  -*- lexical-binding: t; -*-

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
;; Document string for Groovy.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-groovy-style 'groovydoc
  "Style specification for document string in Groovy."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Document String in Groovy" groovydoc))
  :group 'docstr)

(defcustom docstr-groovy-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-groovy-config-groovydoc ()
  "Configre for convention, Groovydoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-groovy-prefix "* "
              docstr-format-var "%s"))

(defun docstr-groovy-config ()
  "Automatically configure style according to variable `docstr-groovy-style'."
  (cl-case docstr-groovy-style
    (groovydoc (docstr-groovy-config-groovydoc))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-groovy (search-string)
  "Insert document string for Groovy using SEARCH-STRING."
  (docstr-groovy-config)
  (let* ((start (point)) (prefix docstr-groovy-prefix)
         (paren-param-list (docstr-writers--paren-param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str (docstr-writers--return-type search-string)))
    (unless (= param-var-len 0)
      (docstr-util-insert docstr-groovy-prefix))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

(provide 'docstr-groovy)
;;; docstr-groovy.el ends here
