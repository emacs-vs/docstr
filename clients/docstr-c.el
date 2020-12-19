;;; docstr-c.el --- Document string for C  -*- lexical-binding: t; -*-

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
;; Document string for C.
;;

;;; Code:

(require 'docstr)

(declare-function docstr-writers-c++ "ext:docstr-c.el")

;;;###autoload
(defun docstr-writers-c (search-string)
  "Insert document string for C using SEARCH-STRING."
  (docstr-writers-c++ search-string))

(provide 'docstr-c)
;;; docstr-c.el ends here
