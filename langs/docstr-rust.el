;;; docstr-rust.el --- Document string for Rust  -*- lexical-binding: t; -*-

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
;; Document string for Rust.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-rust-style 'rfc-430
  "Style specification for document string in Rust."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "RFC 430 documentation conventions" rfc-430))
  :group 'docstr)

(defcustom docstr-rust-prefix "* "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defcustom docstr-rust-header-param ""
  "Header string before inserting paramters document string."
  :type 'string
  :group 'docstr)

(defun docstr-rust-config-rfc-430 ()
  "Configure for convention, RFC 430."
  (docstr-util-default-format :param "*" :ret "" :show-tn nil)
  (setq-local docstr-rust-prefix "/// "
              docstr-rust-header-param "# Arguments"
              docstr-format-var "`%s` -"))

(defun docstr-rust-config ()
  "Automatically configure style according to variable `docstr-rust-style'."
  (cl-case docstr-rust-style
    (rfc-430 (docstr-rust-config-rfc-430))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-rust (search-string)
  "Insert document string for Rust using SEARCH-STRING."
  (docstr-rust-config)
  (let* ((start (point)) (prefix docstr-rust-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string ":" t))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         (return-type-str (docstr-writers--return-type-behind search-string ":")))
    (unless (= param-var-len 0)
      (insert "\n")
      (insert docstr-rust-prefix)
      (insert "\n")
      (unless (string-empty-p docstr-rust-header-param)
        (insert docstr-rust-prefix)
        (insert docstr-rust-header-param)
        (insert "\n")
        (insert docstr-rust-prefix)))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;;; Trigger

(defcustom docstr-rust-modes '(rust-mode rustic-mode)
  "Rust major modes for document string insertion."
  :type 'list
  :group 'docstr)

(defun docstr-trigger-rust (&rest _)
  "Trigger document string inside Rust."
  (when (and (memq major-mode docstr-rust-modes)
             (docstr--doc-valid-p) (docstr-util-looking-back "///" 3))
    (insert " ")
    (docstr--insert-doc-string (docstr--c-style-search-string 2))))

(provide 'docstr-rust)
;;; docstr-rust.el ends here
