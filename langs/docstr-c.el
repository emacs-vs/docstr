;;; docstr-c.el --- Document string for C  -*- lexical-binding: t; -*-

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
;; Document string for C.
;;

;;; Code:

(require 'docstr)

(declare-function docstr-writers-c++ "ext:docstr-c.el")

(defcustom docstr-c-style nil
  "Style specification for document string in C."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defcustom docstr-c-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-c-config ()
  "Automatically configure style according to variable `docstr-c-style'."
  (cl-case docstr-c-style
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-c (search-string)
  "Insert document string for C using SEARCH-STRING."
  (docstr-c-config)
  (docstr-writers-c++ search-string))

(provide 'docstr-c)
;;; docstr-c.el ends here
