;;; docstr-key.el --- Support key for document string.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-01-28 13:14:13

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
;; Helper functions that bind to specific key that may trigger document
;; string.  The purpose of this module is to help user fulfill conditions
;; from the document string triggerations.
;;

;;; Code:

(require 'cl-lib)

(declare-function docstr-load-all "ext:docstr.el")

(defcustom docstr-key-support nil
  "If non-nil, use key support to fulfill document string triggerations'
conditions."
  :type 'boolean
  :group 'docstr)

(defcustom docstr-key-javadoc-like-modes
  (append '(c-mode c++-mode objc-mode csharp-mode swift-mode)
          '(java-mode groovy-mode processing-mode)
          '(javascript-mode js-mode js2-mode js3-mode json-mode)
          '(web-mode php-mode)
          '(actionscript-mode typescript-mode)
          '(go-mode rust-mode scala-mode)
          '(css-mode ssass-mode scss-mode))
  "List of major-mode that can be use Javadoc style."
  :type 'list
  :group 'docstr)

(defun docstr-javadoc-like-p ()
  ""
  (memq major-mode docstr-key-javadoc-like-modes))

(defun docstr-key-insert-prefix ()
  "Insert prefix."
  (insert (docstr-get-prefix))
  (indent-for-tab-command))

(defun docstr-key-javadoc-asterik (fnc &rest args)
  "Asterik key for Javadoc like document string.

This fulfill condition, /* with */ into a pair."
  (apply fnc args)
  (when (docstr-javadoc-like-p)
    (save-excursion
      (when (and (docstr-util-is-behind-last-char-at-line-p)
                 (docstr-util-looking-back "/[*]" 2))
        (insert "*/")))))

(defun docstr-key-c-like-return (fnc &rest args)
  "Return key for C like programming languages.

This function will help insert the corresponding prefix on every line of the
document string."
  (if (not (docstr-javadoc-like-p)) (apply fnc args)
    (if (not (docstr-util-comment-block-p)) (apply fnc args)
      (let ((new-doc-p
             (and (save-excursion (search-backward "/*" (line-beginning-position) t))
                  (save-excursion (search-forward "*/" (line-end-position) t)))))
        (apply fnc args)
        (docstr-key-insert-prefix)
        ;; We can't use `newline-and-indent' here, or else the space will
        ;; be gone.
        (when new-doc-p
          (insert "\n") (indent-for-tab-command)
          (forward-line -1))
        (end-of-line)))))

(defun docstr-key-lua-return (fnc &rest args)
  "Return key for Lua document string.

This function has two features."
  (if (not (eq major-mode 'lua-mode)) (apply fnc args)
    (if (not (jcs-inside-comment-block-p)) (apply fnc args)
      (let ((new-doc-p
             (and (save-excursion (search-backward "--[[" (line-beginning-position) t))
                  (save-excursion (search-forward "]]" (line-end-position) t)))))
        (apply fnc args)
        (when new-doc-p
          (indent-for-tab-command)
          (end-of-line)))
      (unless (string= "--[[" (docstr-util-start-comment-symbol))
        (insert "-- ")))))

;;;###autoload
(defun docstr-key-init ()
  "Initailization for key functions."
  (when docstr-key-support
    (docstr-load-all)
    (docstr-util-key-advice-add "*" :around #'docstr-key-javadoc-asterik)
    (docstr-util-key-advice-add "RET" :around #'docstr-key-c-like-return)
    (docstr-util-key-advice-add "RET" :around #'docstr-key-lua-return)))

(provide 'docstr-key)
;;; docstr-key.el ends here
