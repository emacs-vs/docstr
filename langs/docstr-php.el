;;; docstr-php.el --- Document string for PHP  -*- lexical-binding: t; -*-

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
;; Document string for PHP.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-php-style 'phpdoc
  "Style specification for document string in PHP."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "PHPDoc Style" phpdoc))
  :group 'docstr)

(defcustom docstr-php-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-php-config-phpdoc ()
  "Configre for convention, PHPDoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-php-prefix "* "
              docstr-format-var "%s"))

(defun docstr-php-config ()
  "Automatically configure style according to variable `docstr-php-style'."
  (cl-case docstr-php-style
    (phpdoc (docstr-php-config-phpdoc))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-php (search-string)
  "Insert document string for PHP using SEARCH-STRING."
  (docstr-php-config)
  (let* ((start (point)) (prefix docstr-php-prefix)
         (paren-param-list (docstr-writers--paren-param-list search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list)))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return nil '("void") prefix)
    (docstr-writers-after start  t t t)))

(provide 'docstr-php)
;;; docstr-php.el ends here
