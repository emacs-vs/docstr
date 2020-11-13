;;; docstr.el --- A document string minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-05 20:33:49

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: A document string minor mode.
;; Keyword: document string
;; Version: 5.2.2
;; Package-Requires: ((emacs "24.4") (s "1.9.0"))
;; URL: https://github.com/jcs-elpa/docstr

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
;; This package provides a simple solution to insert document string
;; into the code.
;;

;;; Code:

(require 'cl-lib)
(require 's)
(require 'subr-x)

(require 'docstr-util)
(require 'docstr-writers)
(require 'docstr-faces)

(defgroup docstr nil
  "A document string minor mode."
  :prefix "docstr-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/docstr"))

;;
;; (@* "Customization" )
;;

(defconst docstr-key-type "#T" "String key that going to replace type name.")
(defconst docstr-key-var "#V" "String key that going to replace variable name.")
(defconst docstr-key-desc "#D" "String key that going to replace description.")

(defcustom docstr-format-type "{ %s }"
  "Format string for type name section inside document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-format-var "%s :"
  "Format string for variable name section inside document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-format-param "@param #T #V #D"
  "Format string for parameter document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-format-return "@return #T #V #D"
  "Format string for return document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-desc-param "Param description here.."
  "Description for parameter document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-desc-return "Returns description here.."
  "Description for return document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-default-typename "typename"
  "Default typename when variable type is unknown."
  :type 'string
  :group 'docstr)

(defcustom docstr-show-type-name t
  "If non-nil, show the type name by default."
  :type 'boolean
  :group 'docstr)

(defcustom docstr-before-insert-hook nil
  "Hooks run before inserting document string."
  :type 'hook
  :group 'docstr)

(defcustom docstr-after-insert-hook nil
  "Hooks run after inserting document string."
  :type 'hook
  :group 'docstr)

(defun docstr--get-type-name (type)
  "Return TYPE's name."
  (if (not docstr-show-type-name) ""
    (when (or (null type) (string-empty-p type))
      (setq type docstr-default-typename))
    (format docstr-format-type type)))

(defun docstr--get-var-name (var)
  "Return VAR's name."
  (if (string-empty-p var) "" (format docstr-format-var var)))

(defun docstr-form-param (type var desc)
  "Return complete parameter document string.

Argument TYPE is the name of the name.  Argument VAR is the name of the
variable.  Argument DESC is the description of VAR."
  (let ((new docstr-format-param))
    (setq new (s-replace docstr-key-type (docstr--get-type-name type) new)
          new (s-replace docstr-key-var (docstr--get-var-name var) new)
          new (s-replace docstr-key-desc desc new)
          new (s-replace-regexp "[ ]+" " " new))
    new))

(defun docstr-form-return (type var desc)
  "Return complete return document string.

Argument TYPE is the name of the name.  Argument VAR is the name of the
variable.  Argument DESC is the description of VAR."
  (let ((new docstr-format-return))
    (setq new (s-replace docstr-key-type (docstr--get-type-name type) new)
          new (s-replace docstr-key-var (docstr--get-var-name var) new)
          new (s-replace docstr-key-desc desc new)
          new (s-replace-regexp "[ ]+" " " new))
    new))

;;
;; (@* "Entry" )
;;

(defcustom docstr-trigger-alist
  `((csharp-mode . ("/" docstr-trigger-csharp))
    (go-mode     . ("/" docstr-trigger-golang))
    (lua-mode    . ("-" docstr-trigger-lua))
    (python-mode . ("\"" docstr-trigger-python)))
  "List of trigger to each `major-mode'.

The data is a cons cell form by (mode-name . (key function)).  The first
data is the symbol of the `major-mode' and the second data is a list form
by the following order.

  <1> Key string
  <2> Trigger function name

You should customize this variable to add your own triggeration methods."
  :type 'hook
  :group 'docstr)

(defun docstr--enable-trigger (act)
  "Enable/Disable trigger base on boolean ACT."
  (dolist (tri docstr-trigger-alist)
    (let* ((mode (car tri)) (data (cdr tri))
           (key (nth 0 data)) (fnc (nth 1 data)))
      (when (eq major-mode mode)
        (if act (advice-add (key-binding (kbd key)) :after fnc)
          (advice-remove (key-binding (kbd key)) fnc))))))

(defun docstr--enable ()
  "Enable `docstr' in current buffer."
  (advice-add (key-binding (kbd "RET")) :after #'docstr--trigger-return)
  (docstr--enable-trigger t))

(defun docstr--disable ()
  "Disable `docstr' in current buffer."
  (advice-remove (key-binding (kbd "RET")) #'docstr--trigger-return)
  (docstr--enable-trigger nil))

;;;###autoload
(define-minor-mode docstr-mode
  "Minor mode 'docstr-mode'."
  :lighter " DocStr"
  :group docstr
  (if docstr-mode (docstr--enable) (docstr--disable)))

(defun docstr--turn-on-docstr-mode ()
  "Turn on the 'docstr-mode'."
  (docstr-mode 1))

;;;###autoload
(define-globalized-minor-mode global-docstr-mode
  docstr-mode docstr--turn-on-docstr-mode
  :require 'docstr)

;;
;; (@* "Core" )
;;

;;;###autoload
(defun docstr-supports-p ()
  "Return non-nil if current `major-mode' supports by `docstr'."
  (memq major-mode (docstr-major-modes)))

;;;###autoload
(defun docstr-major-modes ()
  "List of `major-mode' that supports document string."
  (let (lst)
    (dolist (m docstr-writers-alist) (push (car m) lst))
    (reverse lst)))

(defun docstr-get-writer ()
  "Return the writer from `docstr-writers-alist'."
  (assoc (buffer-local-value 'major-mode (current-buffer)) docstr-writers-alist))

(defun docstr--insert-doc-string (search-string)
  "Insert document string base on SEARCH-STRING."
  (let ((writer (docstr-get-writer)))
    (if writer
        (progn
          (run-hook-with-args 'docstr-before-insert-hook search-string)
          (save-excursion (funcall (cdr writer) search-string))
          (end-of-line)
          (run-hook-with-args 'docstr-after-insert-hook search-string))
      (user-error "[WARNING] No document string support for %s" major-mode))))

(defun docstr--get-search-string (type sr)
  "Return string that can be analyze by document string writer.

Argument TYPE can either be a function or an interger.  If it's function
execute it inside the buffer.  Otherwire, if it's an integer call function
`forward-line' instead.

Argument SR is the target symbol for us to stop looking for the end of declaration."
  (let (beg found)
    (save-excursion
      (cond ((functionp type) (funcall type))
            ((integerp type) (forward-line type)))
      (unless (docstr-util-current-line-empty-p)
        (setq beg (line-beginning-position))
        (while (and (not (docstr-util-current-line-empty-p)) (not found))
          (setq found (re-search-forward sr (line-end-position) t))
          (forward-line 1))
        (backward-char (length (match-string 0)))
        (buffer-substring beg (point))))))

(defun docstr--generic-search-string (type sr)
  "Return c-style search string.

See function `docstr--get-search-string' description for arguments TYPE
and SR."
  (let (search-string)
    (setq search-string (or (ignore-errors (docstr--get-search-string type sr))
                            "")
          search-string (string-trim search-string)
          search-string (s-replace "\n" " " search-string))))

(defun docstr--c-style-search-string (type)
  "Return c-style search string.

See function `docstr--get-search-string' description for argument TYPE."
  (docstr--generic-search-string type "{"))

(defun docstr--doc-valid-p ()
  "Return non-nil if current able to insert document string."
  (and docstr-mode (docstr-supports-p) (docstr-util-comment-block-p)))

(defun docstr--trigger-return (&rest _)
  "Trigger document string by pressing key return."
  (when (docstr--doc-valid-p)
    (let ((ln-prev (docstr-util-line-relative -1 t))
          (ln-current (docstr-util-line-relative 0 t))
          (ln-next (docstr-util-line-relative 1 t)))
      (when (and (string-prefix-p "/*" ln-prev) (string-suffix-p "*/" ln-next))
        (when (string-empty-p ln-current) (insert "* "))
        (docstr--insert-doc-string (docstr--c-style-search-string 2))))))

(defun docstr-trigger-csharp (&rest _)
  "Trigger document string inside C#."
  (when (and (docstr--doc-valid-p) (docstr-util-looking-back "///" 3))
    (save-excursion
      (insert " <summary>\n")
      (insert "/// \n")
      (insert "/// </summary>"))
    (forward-line 1)
    (end-of-line)
    (docstr--insert-doc-string (docstr--c-style-search-string 2))))

(defun docstr-trigger-golang (&rest _)
  "Trigger document string inside Golang."
  (when (and (docstr--doc-valid-p) (docstr-util-looking-back "//" 2))
    (docstr--insert-doc-string (docstr--c-style-search-string 1))))

(defun docstr-trigger-lua (&rest _)
  "Trigger document string inside Lua."
  (when (and (docstr--doc-valid-p) (docstr-util-looking-back "---" 3))
    (backward-delete-char 3)
    (save-excursion
      (insert (format "%s\n" docstr-lua-splitter))
      (insert "-- \n")
      (insert (format "%s" docstr-lua-splitter)))
    (forward-line 1)
    (end-of-line)
    (docstr--insert-doc-string (docstr--generic-search-string 2 ")"))))

(defun docstr-trigger-python (&rest _)
  "Trigger document string inside Python."
  ;; TODO: For some reason, '(nth 4 (syntax-ppss))' doesn't work.
  (when (and docstr-mode (docstr-util-looking-back "\"\"\"" 3))
    (if (looking-at-p "\"\"\"")
        (delete-char 3)
      (save-excursion (insert "\"\"\""))
      (docstr--insert-doc-string (docstr--generic-search-string -1 ":")))))

(provide 'docstr)
;;; docstr.el ends here
