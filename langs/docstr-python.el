;;; docstr-python.el --- Document string for Python  -*- lexical-binding: t; -*-

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

(defcustom docstr-python-header-return ""
  "Header string before inserting returns document string."
  :type 'string
  :group 'docstr)

(defun docstr-python-config-pep-257 ()
  "Configre for convention, PEP 257."
  (docstr-util-default-format :param "" :show-tn nil :show-ret nil)
  (setq-local docstr-python-prefix ""
              docstr-python-header-param "Keyword arguments:"
              docstr-format-var "%s --"))

(defun docstr-python-config-google ()
  "Configre for convention, Google."
  (docstr-util-default-format
   :fmt-type "%s" :fmt-var "%s" :param "" :ret "" :con-type nil :con-var nil
   :show-tn t)
  (setq-local docstr-python-prefix "    "
              docstr-python-header-param "Args:"
              docstr-python-header-return "Returns:"
              docstr-format-param (format "%s (%s): %s"
                                          docstr-key-var
                                          docstr-key-type
                                          docstr-key-desc)
              docstr-format-return (format "%s: %s" docstr-key-type docstr-key-desc)))

(defun docstr-python-config-numpy ()
  "Configre for convention, NumPy."
  (docstr-util-default-format
   :fmt-type "%s" :fmt-var "%s" :param "" :ret "" :con-type nil :con-var nil
   :show-tn t)
  (setq-local docstr-python-prefix ""
              docstr-python-header-param "Parameters\n----------"
              docstr-python-header-return "Returns\n----------"
              docstr-format-param (format "%s : %s\n    %s"
                                          docstr-key-var
                                          docstr-key-type
                                          docstr-key-desc)
              docstr-format-return (format "%s\n    %s" docstr-key-type docstr-key-desc)))

(defun docstr-python-config ()
  "Automatically configure style according to variable `docstr-python-style'."
  (cl-case docstr-python-style
    (pep-257 (docstr-python-config-pep-257))
    (google (docstr-python-config-google))
    (numpy (docstr-python-config-numpy))
    (t (docstr-util-default-format))))

;;; Writer

(defun docstr-python--return-type (search-string)
  "Return return type from SEARCH-STRING."
  (let ((ret (docstr-writers--return-type-behind search-string "->")))
    (if (stringp ret) (s-replace ":" "" ret) nil)))

(defun docstr-writers-python (search-string)
  "Insert document string for Python using SEARCH-STRING."
  (docstr-python-config)
  (let* ((start (point)) (prefix docstr-python-prefix)
         (paren-param-list (docstr-writers--paren-param-list-behind search-string ":"))
         (param-types (nth 0 paren-param-list))
         (param-vars (nth 1 paren-param-list))
         (param-var-len (length param-vars))
         ;; Get the return data type.
         (return-type-str (docstr-python--return-type search-string)))
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
    (when (and return-type-str (not (string-empty-p docstr-python-header-return)))
      (insert "\n")
      (docstr-util-insert docstr-python-header-return))
    (docstr-writers--insert-return return-type-str '("void") prefix)
    (docstr-writers-after start t t t)))

;; Trigger

(defcustom docstr-python-modes '(python-mode)
  "Python major modes for document string insertion."
  :type 'list
  :group 'docstr)

(defun docstr-python--parse ()
  "Parse for search string."
  (let (empty-pt beg)
    (save-excursion
      (setq empty-pt (save-excursion (docstr-util-previous-blank-line) (line-beginning-position)))
      (search-backward "(") (setq beg (point))
      (when (< empty-pt beg)
        (search-forward ")")
        (search-forward ":")
        (buffer-substring beg (point))))))

(defun docstr-trigger-python (&rest _)
  "Trigger document string inside Python."
  (when (and (memq major-mode docstr-python-modes)
             docstr-mode
             (docstr-util-looking-back "\"\"\"" 3)
             ;; This should avoid pairing plugins inserting document string
             ;; twice by accident. e.g. `electric-pair-mode', `smartparens',
             ;; etc.
             ;;
             ;; See #5.
             (not (docstr-util-looking-back "\"\"\"\"" 4)))
    ;; If no pairing, help complete it!
    (unless (looking-at-p "\"\"\"") (save-excursion (insert "\"\"\"")))
    (docstr--insert-doc-string (docstr-python--parse))))

(provide 'docstr-python)
;;; docstr-python.el ends here
