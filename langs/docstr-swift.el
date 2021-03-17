;;; docstr-swift.el --- Document string for Swift  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Document string for Swift.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-swift-style 'swift-doc
  "Style specification for document string in Swift."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Generates documentation for Swift projects" swift-doc))
  :group 'docstr)

(defcustom docstr-swift-prefix ""
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-swift-config-swift-doc ()
  "Configure for convention, SwiftDoc."
  (docstr-util-default-format :param "" :ret "" :show-tn nil)
  (setq-local docstr-format-var "- %s:"
              docstr-format-return (format "- returns: %s" docstr-key-desc)))

(defun docstr-swift-config ()
  "Automatically configure style according to variable `docstr-swift-style'."
  (cl-case docstr-swift-style
    (swift-doc (docstr-swift-config-swift-doc))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-writers-swift (search-string)
  "Insert document string for Swift using SEARCH-STRING."
  (docstr-swift-config)
  (let* ((start (point)) (prefix docstr-swift-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string ":"))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get all return data types.
         (return-type-str (docstr-writers--return-type-behind search-string "->")))
    ;; Determine the docstring type.
    (if (docstr-util-multiline-comment-p) (setq prefix "") (setq prefix "/// "))

    (unless (= 0 param-var-len) (insert "\n" prefix))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;;; Trigger

(defcustom docstr-swift-modes '(swift-mode)
  "Swift major modes for document string insertion."
  :type 'list
  :group 'docstr)

(defun docstr-trigger-swift (&rest _)
  "Trigger document string inside Swift."
  (when (and (memq major-mode docstr-swift-modes)
             (docstr--doc-valid-p)
             (docstr-util-looking-back "///" 3))
    (insert " ")
    (docstr--insert-doc-string (docstr--c-style-search-string 1))))

(provide 'docstr-swift)
;;; docstr-swift.el ends here
