;;; docstr-c++.el --- Document string for C++  -*- lexical-binding: t; -*-

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
;; Document string for C++.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-c++-style 'javadoc
  "Style specification for document string in C++."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Javadoc Style" javadoc)
                 (const :tag "Qt Style" qt))
  :group 'docstr)

(defcustom docstr-c++-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-c++-config-javadoc ()
  "Configre for convention, Javadoc Style."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-c++-prefix "* "
              docstr-format-var "%s"))

(defun docstr-c++-config-qt ()
  "Configre for convention, Qt Style."
  (docstr-util-default-format :param "\\param" :ret "\\return" :show-tn nil)
  (setq-local docstr-c++-prefix "    "
              docstr-format-var "%s"))

(defun docstr-c++-config ()
  "Automatically configure style according to variable `docstr-c++-style'."
  (cl-case docstr-c++-style
    (javadoc (docstr-c++-config-javadoc))
    (qt (docstr-c++-config-qt))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-c++ (search-string)
  "Insert document string for C++ using SEARCH-STRING."
  (docstr-c++-config)
  (let* ((start (point)) (prefix docstr-c++-prefix)
         (paren-param-list (docstr-writers--paren-param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         ;; Get the return data type.
         (return-type-str (docstr-writers--return-type search-string)))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str nil prefix)
    (docstr-writers-after start nil t t)))

(provide 'docstr-c++)
;;; docstr-c++.el ends here
