;;; docstr-ruby.el --- Document string for Ruby  -*- lexical-binding: t; -*-

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
;; Document string for Ruby.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-ruby-style 'rdoc
  "Style specification for document string in Ruby."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Ruby Documentation System" rdoc))
  :group 'docstr)

(defcustom docstr-ruby-prefix "# "
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defun docstr-ruby-config-rdoc ()
  "Configure for convention, RDoc."
  (docstr-util-default-format :param "" :ret "" :show-tn nil)
  (setq-local docstr-ruby-prefix "# "
              docstr-format-var "+%s+"))

(defun docstr-ruby-config ()
  "Automatically configure style according to variable `docstr-ruby-style'."
  (cl-case docstr-ruby-style
    (rdoc (docstr-ruby-config-rdoc))
    (t (docstr-util-default-format))))

(defun docstr-ruby--param-list (search-string)
  "Parse SEARCH-STRING without parenthesis."
  (let* ((fnc-split (split-string search-string "end"))
         (fnc-str (nth 0 fnc-split))
         (token-lst (split-string fnc-str " " t))
         (lst-type nil) lst-var (token (pop token-lst)))
    (while (not (string= "def" token)) (setq token (pop token-lst)))
    (pop token-lst)  ; Pop the function name
    (dolist (tkn token-lst)
      (setq lst-var (append lst-var (split-string tkn "," t))))
    (list lst-type lst-var)))

;;; Writer

(defun docstr-writers-ruby (search-string)
  "Insert document string for Ruby using SEARCH-STRING."
  (docstr-ruby-config)
  (let* ((start (point)) (prefix docstr-ruby-prefix)
         (paren-param-list
          (if (string-match-p "(" search-string)
              (docstr-writers--paren-param-list-behind search-string)
            (docstr-ruby--param-list search-string)))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str "void"))
    (unless (= 0 param-var-len)
      (insert "\n" docstr-ruby-prefix))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;;; Trigger

(defcustom docstr-ruby-modes '(ruby-mode)
  "Ruby major modes for document string insertion."
  :type 'list
  :group 'docstr)

(defun docstr-trigger-ruby (&rest _)
  "Trigger document string inside Ruby."
  (when (and (memq major-mode docstr-ruby-modes)
             (docstr--doc-valid-p) (docstr-util-looking-back "##" 2))
    (indent-for-tab-command)
    (insert "\n# ")
    (indent-for-tab-command) (end-of-line)
    (docstr--insert-doc-string (docstr--generic-search-string 1 ")"))))

(provide 'docstr-ruby)
;;; docstr-ruby.el ends here
