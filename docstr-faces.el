;;; docstr-faces.el --- Faces for document string  -*- lexical-binding: t; -*-

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
;; Faces for document string.
;;

;;; Code:

(defface docstr-tag-face
  '((t (:foreground "SlateGray")))
  "Highlighting for Docstring tag."
  :group 'docstr)
(defvar docstr-tag-face 'docstr-tag-face)

(defface docstr-type-face
  '((t (:foreground "SteelBlue")))
  "Highlighting for Docstring type."
  :group 'docstr)
(defvar docstr-type-face 'docstr-type-face)

(defface docstr-value-face
  '((t (:foreground "gold4")))
  "Highlighting for Docstring value."
  :group 'docstr)
(defvar docstr-value-face 'docstr-value-face)

(defun docstr-apply-faces ()
  "Apply standard document string faces."
  (dolist (mode (docstr-major-modes))
    (font-lock-add-keywords
     mode
     '(;; `@param` { typename } val-tag : value tag description..
       ("\\(?:^\\|\\s-\\)\\(@[^ \"'{}()\t\r\n]+\\)" 1 'docstr-tag-face t)
       ;; @param `{ typename }` val-tag : value tag description..
       ("[ \t]+@[^ \t\r\n]+\\(?:^\\|\\s-\\)\\([\\[{][^}]*.\\)" 1 'docstr-type-face t)
       ;; @param { typename } `val-tag` : value tag description..
       ("[ \t]+@[^ \t\r\n].*[\]\|}]\\([^\r\n]*\\)[:-]" 1 'docstr-value-face t)
       ;; @param `val-tag` : value tag description..
       ("[ \t]+@[^ \t\r\n]*[ \t]*\\([a-zA-Z0-9_.*&]*\\)[ \t\n]*[{:-]" 1 'docstr-value-face t))
     'end)))

(provide 'docstr-faces)
;;; docstr-faces.el ends here
