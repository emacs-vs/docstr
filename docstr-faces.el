;;; docstr-faces.el --- Faces for document string  -*- lexical-binding: t; -*-

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
;; Faces for document string.
;;

;;; Code:

(declare-function docstr-major-modes "ext:docstr.el")

(defface docstr-faces-tag-face
  '((t (:foreground "SlateGray")))
  "Highlighting for Docstring tag."
  :group 'docstr)
(defvar docstr-faces-tag-face 'docstr-faces-tag-face)

(defface docstr-faces-type-face
  '((t (:foreground "SteelBlue")))
  "Highlighting for Docstring type."
  :group 'docstr)
(defvar docstr-faces-type-face 'docstr-faces-type-face)

(defface docstr-faces-value-face
  '((t (:foreground "gold4")))
  "Highlighting for Docstring value."
  :group 'docstr)
(defvar docstr-faces-value-face 'docstr-faces-value-face)

;;;###autoload
(defun docstr-faces-apply ()
  "Apply standard document string faces."
  (let ((modes (docstr-major-modes)))
    (dolist (mode modes)
      (font-lock-add-keywords
       mode
       '(;; `@param` { typename } val-tag : value tag description..
         ("\\(?:^\\|\\s-\\)\\([@\\][^ \"'{}()\t\r\n]+\\)" 1 'docstr-faces-tag-face t)
         ;; @param `{ typename }` val-tag : value tag description..
         ("[ \t]+@[^ \t\r\n]+\\(?:^\\|\\s-\\)\\([\\[{][^]}\n]*.\\)" 1 'docstr-faces-type-face t)
         ;; @param { typename } `val-tag` : value tag description..
         ("[ \t]+@[^ \t\r\n].*[\]\|}]\\([^\r\n]*\\)[:-]" 1 'docstr-faces-value-face t)
         ;; @param `val-tag` : value tag description..
         ("[ \t]+@[^ \t\r\n]*[ \t]*\\([a-zA-Z0-9_.*&]*\\)[ \t]*[{:-]" 1 'docstr-faces-value-face t))
       'end))))

(provide 'docstr-faces)
;;; docstr-faces.el ends here
