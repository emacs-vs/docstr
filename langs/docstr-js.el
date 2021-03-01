;;; docstr-js.el --- Document string for JavaScript  -*- lexical-binding: t; -*-

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
;; Document string for JavaScript.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-js-style 'jsdoc
  "Style specification for document string in JavaScript."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "JSdoc Style" jsdoc)
                 (const :tag "Google Style" google))
  :group 'docstr)

(defcustom docstr-js-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-js-config-jsdoc ()
  "Configre for convention, JSDoc."
  (docstr-util-default-format)
  (setq-local docstr-js-prefix "* "
              docstr-format-type "{%s}"
              docstr-format-var "%s -"))

(defun docstr-js-config-google ()
  "Configre for convention, Google."
  (docstr-util-default-format)
  (setq-local docstr-js-prefix "* "
              docstr-format-type "{%s}"
              docstr-format-var "%s"))

(defun docstr-js-config ()
  "Automatically configure style according to variable `docstr-js-style'."
  (cl-case docstr-js-style
    (jsdoc (docstr-js-config-jsdoc))
    (google (docstr-js-config-google))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-javascript (search-string)
  "Insert document string for JavaScript using SEARCH-STRING."
  (docstr-js-config)
  (let* ((start (point)) (prefix docstr-js-prefix)
         (paren-param-list (docstr-writers--paren-param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list)))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return nil '("void") prefix)
    (docstr-writers-after start  t t t)))

(provide 'docstr-js)
;;; docstr-js.el ends here
