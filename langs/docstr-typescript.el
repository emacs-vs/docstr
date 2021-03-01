;;; docstr-typescript.el --- Document string for TypeScript  -*- lexical-binding: t; -*-

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
;; Document string for TypeScript.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-typescript-style 'typedoc
  "Style specification for document string in TypeScript."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "TypeDoc" typedoc)
                 (const :tag "TsDoc" tsdoc))
  :group 'docstr)

(defcustom docstr-typescript-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-typescript-config-typedoc ()
  "Configre for convention, TypeDoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-typescript-prefix "* "
              docstr-format-var "%s"))

(defun docstr-typescript-config-tsdoc ()
  "Configre for convention, TSDoc."
  (docstr-util-default-format :show-tn nil)
  (setq-local docstr-typescript-prefix "* "
              docstr-format-var "%s -"))

(defun docstr-typescript-config ()
  "Automatically configure style according to variable `docstr-typescript-style'."
  (cl-case docstr-typescript-style
    (typedoc (docstr-typescript-config-typedoc))
    (tsdoc (docstr-typescript-config-tsdoc))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-typescript (search-string)
  "Insert document string for TypesSript using SEARCH-STRING."
  (docstr-typescript-config)
  (let* ((start (point)) (prefix docstr-typescript-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string ":"))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         ;; Get all return data types.
         (return-type-str (docstr-writers--return-type-behind search-string ":")))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

(provide 'docstr-typescript)
;;; docstr-typescript.el ends here
