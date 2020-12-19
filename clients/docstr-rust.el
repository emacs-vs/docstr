;;; docstr-rust.el --- Document string for Rust  -*- lexical-binding: t; -*-

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
;; Document string for Rust.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-rust-style nil
  "Style specification for document string in Rust."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defun docstr-rust-config ()
  "Automatically configure style according to variable `docstr-rust-style'."
  (cl-case docstr-rust-style
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-rust (search-string)
  "Insert document string for Rust using SEARCH-STRING."
  (docstr-rust-config)
  (let* ((start (point)) (prefix "\n* ")
         (paren-param-list (docstr-writers--paren-param-list-behind search-string ":" t))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (return-type-str (docstr-writers--return-type-behind search-string ":")))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start)))

(provide 'docstr-rust)
;;; docstr-rust.el ends here
