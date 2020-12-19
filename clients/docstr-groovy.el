;;; docstr-groovy.el --- Document string for Groovy  -*- lexical-binding: t; -*-

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
;; Document string for Groovy.
;;

;;; Code:

(require 'docstr)

(declare-function docstr-writers-javascript "ext:docstr-js.el")

(defcustom docstr-groovy-style nil
  "Style specification for document string in Groovy."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defun docstr-groovy-config ()
  "Automatically configure style according to variable `docstr-groovy-style'."
  (cl-case docstr-groovy-style
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-groovy (search-string)
  "Insert document string for Groovy using SEARCH-STRING."
  (docstr-groovy-config)
  (docstr-writers-javascript search-string))

(provide 'docstr-groovy)
;;; docstr-groovy.el ends here
