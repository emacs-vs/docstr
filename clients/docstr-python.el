;;; docstr-python.el --- Document string for Python  -*- lexical-binding: t; -*-

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
;; Document string for Python.
;;

;;; Code:

(require 'docstr)

(defcustom docstr-python-style 'pep-257
  "Style specification for document string in Python."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "PEP 257 convention" pep-257)
                 (const :tag "Google Style" google)
                 (const :tag "NumPy Style" numpy))
  :group 'docstr)

(defcustom docstr-python-prefix ""
  "Prefix you use on each newline."
  :type 'string
  :group 'docstr)

(defcustom docstr-python-header-param ""
  "Header string before inserting parameters document string."
  :type 'string
  :group 'docstr)

(defun docstr-python-config-pep-257 ()
  "Configre for convention, PEP 257."
  (docstr-util-default-format :param "")
  (setq-local docstr-python-prefix ""
              docstr-python-header-param "Keyword arguments:"
              docstr-format-var "%s --"
              docstr-show-type-name nil))

(defun docstr-python-config-google ()
  "Configre for convention, Google."
  (docstr-util-default-format :param "")
  (setq-local docstr-python-prefix "    "
              docstr-python-header-param "Args:"
              docstr-format-var (format "%%s (%s):" docstr-default-typename)
              docstr-show-type-name nil))

(defun docstr-python-config-numpy ()
  "Configre for convention, NumPy."
  (docstr-util-default-format :param "")
  (setq-local docstr-python-prefix ""
              docstr-python-header-param "Parameters\n----------"
              docstr-format-param (format "%s\n    %s" docstr-key-var docstr-key-desc)
              docstr-format-var (format "%%s : %s" docstr-default-typename)
              docstr-show-type-name nil))

(defun docstr-python-config ()
  "Automatically configure style according to variable `docstr-python-style'."
  (cl-case docstr-python-style
    (pep-257 (docstr-python-config-pep-257))
    (google (docstr-python-config-google))
    (numpy (docstr-python-config-numpy))
    (t (docstr-util-default-format))))

;;;###autoload
(defun docstr-writers-python (search-string)
  "Insert document string for Python using SEARCH-STRING."
  (docstr-python-config)
  (let* ((start (point)) (prefix docstr-python-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str "void"))
    ;; Remove `self' from list.
    (setq param-vars (remove "self" param-vars)
          param-var-len (length param-vars))
    ;; Line break between description and tags.
    (unless (= param-var-len 0)
      (insert "\n\n")
      (indent-for-tab-command)
      (forward-line -1)
      (unless (string-empty-p docstr-python-header-param)
        (docstr-util-insert docstr-python-header-param)))
    (docstr-writers--insert-param param-types param-vars prefix)
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start)))

(provide 'docstr-python)
;;; docstr-python.el ends here
