;;; docstr-csharp.el --- Document string for C#  -*- lexical-binding: t; -*-

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
;; Document string for C#.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-csharp-style nil
  "Style specification for document string in C#."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defcustom docstr-csharp-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-csharp-config ()
  "Automatically configure style according to variable `docstr-csharp-style'."
  (cl-case docstr-csharp-style
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-csharp (search-string)
  "Insert document string for C# using SEARCH-STRING."
  (docstr-csharp-config)
  (let* ((start (point)) (prefix docstr-csharp-prefix)
         (paren-param-list (docstr-writers--paren-param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         ;; Get the return data type.
         (return-type-str (docstr-writers--return-type search-string)))
    ;; Determine the docstring type.
    (if (docstr-util-multiline-comment-p)
        (progn
          (setq prefix "* ")
          (docstr-writers--insert-param param-types param-vars prefix)
          (docstr-writers--insert-return return-type-str '("void") prefix))
      (setq prefix "/// ")
      (let ((docstr-format-var "%s")
            (docstr-format-param (format "<param name=\"%s\"></param>" docstr-key-var))
            (docstr-format-return "<returns></returns>")
            (docstr-concat-var nil))
        (forward-line 1) (end-of-line)
        (docstr-writers--insert-param param-types param-vars prefix)
        (docstr-writers--insert-return return-type-str '("void") prefix)))
    (docstr-writers-after start t t t)))

;;; Trigger

(defcustom docstr-csharp-modes '(csharp-mode)
  "C# major modes for document string insertion."
  :type 'list
  :group 'docstr)

(defun docstr-trigger-csharp (&rest _)
  "Trigger document string inside C#."
  (when (and (memq major-mode docstr-csharp-modes)
             (docstr--doc-valid-p)
             (docstr-util-looking-back "///" 3))
    (save-excursion
      (insert " <summary>\n")
      (insert "/// \n")
      (insert "/// </summary>"))
    (forward-line 1)
    (end-of-line)
    (docstr--insert-doc-string (docstr--c-style-search-string 2))))

(provide 'docstr-csharp)
;;; docstr-csharp.el ends here
