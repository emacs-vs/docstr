;;; docstr-lua.el --- Document string for Lua  -*- lexical-binding: t; -*-

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
;; Document string for Lua.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-lua-style 'lua-wiki
  "Style specification for document string in Lua."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Official Lua org style" lua-wiki))
  :group 'docstr)

(defcustom docstr-lua-prefix "-- "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defcustom docstr-lua-splitter
  "-------------------------------------------------------------"
  "Document string splitter for Lua programming language."
  :type 'string
  :group 'docstr)

(defun docstr-lua-config-lua-wiki ()
  "Configre for convention, Lua-Users Wiki."
  (docstr-util-default-format)
  (setq-local docstr-lua-prefix "-- "))

(defun docstr-lua-config ()
  "Automatically configure style according to variable `docstr-lua-style'."
  (cl-case docstr-lua-style
    (lua-wiki (docstr-lua-config-lua-wiki))
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-lua (search-string)
  "Insert document string for Lua using SEARCH-STRING."
  (docstr-lua-config)
  (let* ((start (point)) (prefix docstr-lua-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         (return-type-str "void"))  ; Get the return data type.
    (unless (= param-var-len 0)
      (insert (format "\n%s" docstr-lua-splitter)))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;;;###autoload
(defun docstr-trigger-lua (&rest _)
  "Trigger document string inside Lua."
  (when (and (docstr--doc-valid-p) (docstr-util-looking-back "---" 3))
    (backward-delete-char 3)
    (save-excursion
      (insert (format "%s\n" docstr-lua-splitter))
      (insert "-- \n")
      (insert (format "%s" docstr-lua-splitter)))
    (forward-line 1)
    (end-of-line)
    (docstr--insert-doc-string (docstr--generic-search-string 2 ")"))))

(provide 'docstr-lua)
;;; docstr-lua.el ends here
