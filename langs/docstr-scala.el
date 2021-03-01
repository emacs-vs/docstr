;;; docstr-scala.el --- Document string for Scala  -*- lexical-binding: t; -*-

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
;; Document string for Scala.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-scala-style 'scaladoc
  "Style specification for document string in Scala."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Scaladoc" scaladoc))
  :group 'docstr)

(defcustom docstr-scala-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-scala-config-scaladoc ()
  "Configure for convention, Scaladoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-scala-prefix "* "
              docstr-format-var "%s"))

(defun docstr-scala-config ()
  "Automatically configure style according to variable `docstr-scala-style'."
  (cl-case docstr-scala-style
    (scaladoc (docstr-scala-config-scaladoc))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-scala (search-string)
  "Insert document string for Scala using SEARCH-STRING."
  (docstr-scala-config)
  (let* ((start (point)) (prefix docstr-scala-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string ":" t))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         (return-type-str (docstr-writers--return-type-behind search-string ":")))
    (unless (= param-var-len 0)
      (docstr-util-insert docstr-scala-prefix))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

(provide 'docstr-scala)
;;; docstr-scala.el ends here
