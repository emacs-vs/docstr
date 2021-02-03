;;; docstr-ruby.el --- Document string for Ruby  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Document string for Ruby.
;;

;;; Code:

(defcustom docstr-ruby-style 'rdoc
  "Style specification for document string in Ruby."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Ruby Documentation System" rdoc))
  :group 'docstr)

(defcustom docstr-ruby-prefix "# "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-ruby-config-rdoc ()
  "Configure for convention, RDoc."
  (docstr-util-default-format :param "*" :ret "")
  (setq-local docstr-ruby-prefix "/// "
              docstr-format-var "`%s` -"
              docstr-show-type-name nil))

(defun docstr-ruby-config ()
  "Automatically configure style according to variable `docstr-ruby-style'."
  (cl-case docstr-ruby-style
    (rdoc (docstr-ruby-config-rdoc))
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-ruby (search-string)
  "Insert document string for Ruby using SEARCH-STRING."
  (docstr-ruby-config)
  (let* ((start (point)) (prefix docstr-ruby-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str "void"))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;;;###autoload
(defun docstr-trigger-ruby (&rest _)
  "Trigger document string inside Ruby."
  (when (and (docstr--doc-valid-p) (docstr-util-looking-back "##" 2))
    (insert " ")
    (docstr--insert-doc-string (docstr--c-style-search-string 2))))

(provide 'docstr-ruby)
;;; docstr-ruby.el ends here
