;;; docstr.el --- A document string minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Shen, Jen-Chieh
;; Created date 2020-11-05 20:33:49

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: A document string minor mode.
;; Keyword: document string
;; Version: 5.7.0
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
(require 'docstr-key)

(defgroup docstr nil
  "A document string minor mode."
  :prefix "docstr-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/docstr"))

;;
;; (@* "Customization" )
;;

(defconst docstr-key-type "#T#" "String key that going to replace type name.")
(defconst docstr-key-var "#V#" "String key that going to replace variable name.")
(defconst docstr-key-desc "#D#" "String key that going to replace description.")

(defcustom docstr-format-type "{ %s }"
  "Format string for type name section inside document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-format-var "%s :"
  "Format string for variable name section inside document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-format-param
  (format "@param %s%s%s" docstr-key-type docstr-key-var docstr-key-desc)
  "Format string for parameter document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-format-return
  (format "@return %s%s%s" docstr-key-type docstr-key-var docstr-key-desc)
  "Format string for return document string."
  :type 'string
  :group 'docstr)

(defcustom docstr-desc-summary "[summary]"
  "Placeholder string for summary description."
  :type 'string
  :group 'docstr)

(defcustom docstr-desc-param "[description]"
  "Placeholder string for parameter description."
  :type 'string
  :group 'docstr)

(defcustom docstr-desc-return "[description]"
  "Placeholder string for return description."
  :type 'string
  :group 'docstr)

(defcustom docstr-default-typename "[type]"
  "Placeholder string for unknown type description."
  :type 'string
  :group 'docstr)

(defcustom docstr-show-type-name t
  "If non-nil, show the type name by default."
  :type 'boolean
  :group 'docstr)

(defcustom docstr-show-return t
  "If non-nil, write display document string."
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

(defvar docstr-concat-type t
  "Flag to see if add a space after getting type name.")
(defvar docstr-concat-var t
  "Flag to see if add a space after getting variable name.")

(defun docstr-insert-summary (_search-string)
  "Insert the default summary for user to replace."
  (insert docstr-desc-summary))

(defun docstr--get-type-name (type)
  "Return TYPE's name."
  (if (not docstr-show-type-name) ""
    (when (or (null type) (string-empty-p type))
      (setq type docstr-default-typename))
    (concat (format docstr-format-type type) (if docstr-concat-type " " ""))))

(defun docstr--get-var-name (var)
  "Return VAR's name."
  (if (string-empty-p var) ""
    (concat (format docstr-format-var var) (if docstr-concat-var " " ""))))

(defun docstr-form-param (type var desc)
  "Return complete parameter document string.

Argument TYPE is the name of the name.  Argument VAR is the name of the
variable.  Argument DESC is the description of VAR."
  (let ((new docstr-format-param))
    (setq new (s-replace docstr-key-type (docstr--get-type-name type) new)
          new (s-replace docstr-key-var (docstr--get-var-name var) new)
          new (s-replace docstr-key-desc desc new))
    new))

(defun docstr-form-return (type var desc)
  "Return complete return document string.

Argument TYPE is the name of the name.  Argument VAR is the name of the
variable.  Argument DESC is the description of VAR."
  (let ((new docstr-format-return))
    (setq new (s-replace docstr-key-type (docstr--get-type-name type) new)
          new (s-replace docstr-key-var (docstr--get-var-name var) new)
          new (s-replace docstr-key-desc desc new))
    new))

;;
;; (@* "Modules" )
;;

(defvar docstr-support-langs
  (append '(actionscript)
          '(c c++ csharp)
          '(go groovy)
          '(java js)
          '(lua)
          '(objc)
          '(php python)
          '(ruby rust)
          '(scala swift)
          '(typescript))
  "List of supported languages.")

(defun docstr-load (name)
  "Load docstr module by NAME."
  (let ((mode-name (intern (format "docstr-%s" name))))
    (require mode-name)))

(defun docstr-load-all ()
  "Load all supported language modules.

Please do not call this at the start up; this will slow down user's
configuration."
  (dolist (name docstr-support-langs) (docstr-load name)))

;;
;; (@* "Entry" )
;;

(defcustom docstr-trigger-alist
  `(("/"   . docstr-trigger-csharp)
    ("/"   . docstr-trigger-golang)
    ("-"   . docstr-trigger-lua)
    ("RET" . docstr-trigger-lua-return)
    ("\""  . docstr-trigger-python)
    ("#"   . docstr-trigger-ruby)
    ("/"   . docstr-trigger-rust)
    ("/"   . docstr-trigger-swift))
  "List of trigger to each `major-mode'.

The data is a cons cell form by (key . function).  The first string of the
key binding and the second data is a function name for triggeration.

You should customize this variable to add your own triggeration methods."
  :type 'hook
  :group 'docstr)

(defun docstr--enable-trigger (act)
  "Enable/Disable trigger base on boolean ACT."
  (dolist (tri docstr-trigger-alist)
    (let ((key (car tri)) (fnc (cdr tri)))
      (if act (docstr-util-key-advice-add key :after fnc)
        (docstr-util-key-advice-remove key fnc)))))

(defun docstr--enable ()
  "Enable `docstr' in current buffer."
  (docstr-load-all)
  (docstr-key-enable)  ; Be infront, in order to take effect
  (docstr-util-key-advice-add "RET" :after #'docstr--trigger-return)
  (docstr--enable-trigger t)
  (add-hook 'docstr-after-insert-hook #'docstr-insert-summary))

(defun docstr--disable ()
  "Disable `docstr' in current buffer."
  (docstr-key-disable)
  (docstr-util-key-advice-remove "RET" #'docstr--trigger-return)
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
        (while (and (not (docstr-util-current-line-empty-p)) (not (eobp)) (not found))
          (setq found (re-search-forward sr (line-end-position) t))
          (forward-line 1))
        (when found (goto-char found))
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
          search-string (s-replace "\n" " " search-string))
    search-string))

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
          (ln-next (docstr-util-line-relative 1 t)))
      (when (and (string-prefix-p "/*" ln-prev) (string-suffix-p "*/" ln-next))
        (docstr--insert-doc-string (docstr--c-style-search-string 2))))))

;;
;; (@* "Prefix" )
;;

(defvar docstr-prefix-alist
  '((actionscript-mode . docstr-actionscript-prefix)
    (c-mode            . docstr-c-prefix)
    (c++-mode          . docstr-c++-prefix)
    (csharp-mode       . docstr-csharp-prefix)
    (go-mode           . docstr-go-prefix)
    (groovy-mode       . docstr-groovy-prefix)
    (java-mode         . docstr-java-prefix)
    (javascript-mode   . docstr-js-prefix)
    (js-mode           . docstr-js-prefix)
    (js2-mode          . docstr-js-prefix)
    (js3-mode          . docstr-js-prefix)
    (lua-mode          . docstr-lua-prefix)
    (objc-mode         . docstr-objc-prefix)
    (php-mode          . docstr-php-prefix)
    (python-mode       . docstr-python-prefix)
    (rjsx-mode         . docstr-js-prefix)
    (ruby-mode         . docstr-ruby-prefix)
    (rust-mode         . docstr-rust-prefix)
    (scala-mode        . docstr-scala-prefix)
    (swift-mode        . docstr-swift-prefix)
    (typescript-mode   . docstr-typescript-prefix)
    (web-mode          . docstr-php-prefix))
  "Assocaition list for (major-mode . name).

`name` can either be a variable or function.")

(defun docstr-get-prefix ()
  "Return prefix from the corresponding mode."
  (let* ((prefix-cons (assoc major-mode docstr-prefix-alist))
         (prefix (cdr prefix-cons)))
    (cond ((functionp prefix) (funcall prefix))
          ((boundp prefix) (symbol-value prefix))
          (t ""))))

(provide 'docstr)
;;; docstr.el ends here
