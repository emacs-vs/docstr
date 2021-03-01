;;; docstr-java.el --- Document string for Java  -*- lexical-binding: t; -*-

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
;; Document string for Java.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-java-style 'javadoc
  "Style specification for document string in Java."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Javadoc Style" javadoc))
  :group 'docstr)

(defcustom docstr-java-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-java-config-javadoc ()
  "Configre for convention, Javadoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-java-prefix "* "
              docstr-format-var "%s"))

(defun docstr-java-config ()
  "Automatically configure style according to variable `docstr-java-style'."
  (cl-case docstr-java-style
    (javadoc (docstr-java-config-javadoc))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-java (search-string)
  "Insert document string for Java using SEARCH-STRING."
  (docstr-java-config)
  (let* ((start (point)) (prefix docstr-java-prefix)
         (paren-param-list (docstr-writers--paren-param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str (docstr-writers--return-type search-string)))
    (unless (= param-var-len 0)
      (docstr-util-insert docstr-java-prefix))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

(provide 'docstr-java)
;;; docstr-java.el ends here
