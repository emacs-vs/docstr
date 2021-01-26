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

(declare-function docstr-writers-rust "ext:docstr-rust.el")

(defcustom docstr-scala-style nil
  "Style specification for document string in Scala."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defun docstr-scala-config ()
  "Automatically configure style according to variable `docstr-scala-style'."
  (cl-case docstr-scala-style
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-scala (search-string)
  "Insert document string for Scala using SEARCH-STRING."
  (docstr-scala-config)
  (docstr-writers-rust search-string))

(provide 'docstr-scala)
;;; docstr-scala.el ends here
