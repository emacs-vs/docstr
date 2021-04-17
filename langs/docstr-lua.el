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

(defcustom docstr-lua-style 'luadoc
  "Style specification for document string in Lua."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Kepler's specification" luadoc)
                 (const :tag "doxygen/Javadoc-like style" doxygen)
                 (const :tag "Lua based document generator to Markdown" scriptum))
  :group 'docstr)

(defcustom docstr-lua-prefix "-- "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defcustom docstr-lua-splitter
  "-------------------------------------"
  "Document string splitter for Lua programming language."
  :type 'string
  :group 'docstr)

(defun docstr-lua-config-doxygen ()
  "Configre for convention, doxygen/Javadoc-like style."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-lua-prefix "-- "
              docstr-format-var "%s"))

(defun docstr-lua-config-luadoc ()
  "Configre for convention, LuaDoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-lua-prefix "-- "))

(defun docstr-lua-config-scriptum ()
  "Configre for convention, lua-scriptum."
  (docstr-util-default-format)
  (setq-local docstr-lua-prefix ""
              docstr-format-param (format "@param %s%s%s"  docstr-key-var
                                          docstr-key-type docstr-key-desc)
              docstr-format-var "%s"
              docstr-format-type "(%s)"))

(defun docstr-lua-config ()
  "Automatically configure style according to variable `docstr-lua-style'."
  (cl-case docstr-lua-style
    (luadoc (docstr-lua-config-luadoc))
    (doxygen (docstr-lua-config-doxygen))
    (scriptum (docstr-lua-config-scriptum))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-lua (search-string)
  "Insert document string for Lua using SEARCH-STRING."
  (docstr-lua-config)
  (let* ((start (point)) (prefix docstr-lua-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         (return-type-str "void"))  ; Get the return data type.
    (cl-case docstr-lua-style
      (doxygen
       (unless (= param-var-len 0)
         (insert (format "\n%s" docstr-lua-splitter))
         (forward-line -1)
         (end-of-line))))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;;; Trigger

(defcustom docstr-lua-modes '(lua-mode)
  "Lua major modes for document string insertion."
  :type 'list
  :group 'docstr)

(defun docstr-lua--before-insert (_search-string)
  "Before inserting parameters, etc."
  (cl-case docstr-lua-style
    (luadoc (insert " "))
    (doxygen
     (backward-delete-char 3)
     (save-excursion
       (insert (format "%s\n" docstr-lua-splitter))
       (insert (format "%s" docstr-lua-prefix)))
     (forward-line 1)
     (end-of-line))))

(defun docstr-trigger-lua (&rest _)
  "Trigger document string inside Lua."
  (when (and (memq major-mode '(lua-mode)) (docstr--doc-valid-p)
             (docstr-util-looking-back "---" 3)
             (memq docstr-lua-style '(luadoc doxygen)))
    (add-hook 'docstr-before-insert-hook #'docstr-lua--before-insert nil t)
    (docstr--insert-doc-string (docstr--generic-search-string 1 ")"))))

(defun docstr-trigger-lua-return (&rest _)
  "Trigger document string inside Lua multiline comment."
  (when (and (memq major-mode docstr-lua-modes)
             (docstr--doc-valid-p))
    (let ((ln-prev (docstr-util-line-relative -1 t))
          (ln-next (docstr-util-line-relative 1 t)))
      (when (and (string-prefix-p "--[[" ln-prev) (string-suffix-p "]]" ln-next)
                 (memq docstr-lua-style '(scriptum)))
        (docstr--insert-doc-string (docstr--generic-search-string 2 ")"))))))

(provide 'docstr-lua)
;;; docstr-lua.el ends here
