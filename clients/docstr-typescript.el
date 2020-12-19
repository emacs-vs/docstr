;;; docstr-typescript.el --- Document string for TypeScript  -*- lexical-binding: t; -*-

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
;; Document string for TypeScript.
;;

;;; Code:

(require 'docstr)

(declare-function docstr-writers-actionscript "ext:docstr-actionscript.el")

(defcustom docstr-typescript-style nil
  "Style specification for document string in TypeScript."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defun docstr-typescript-config ()
  "Automatically configure style according to variable `docstr-typescript-style'."
  (cl-case docstr-typescript-style
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-typescript (search-string)
  "Insert document string for TypesSript using SEARCH-STRING."
  (docstr-typescript-config)
  (docstr-writers-actionscript search-string))

(provide 'docstr-typescript)
;;; docstr-typescript.el ends here
