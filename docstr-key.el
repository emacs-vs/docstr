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

(defun docstr-key-insert-prefix ()
  "Insert prefix."
  (insert (concat (docstr-get-prefix) " "))
  (indent-for-tab-command))

(defun docstr-key-javadoc-asterik (fnc &rest args)
  "Asterik key for Javadoc like document string.

This fulfill condition, /* with */ into a pair."
  (apply fnc args)
  (save-excursion
    (when (and (docstr-util-is-behind-last-char-at-line-p)
               (docstr-util-looking-back "/[*]" 2))
      (insert "*/"))))

(defun docstr-key-javadoc-return (fnc &rest args)
  "Return key for Javadoc like document string.

This function will help insert the corresponding prefix."
  (if (not (docstr-util-comment-block-p)) (apply fnc args)
    (let (new-doc-p)
      (setq new-doc-p
            (and (save-excursion (search-backward "/*" (line-beginning-position) t))
                 (save-excursion (search-forward "*/" (line-end-position) t))))
      (apply fnc args)
      ;; We can't use `newline-and-indent' here, or else the space will
      ;; be gone.
      (when new-doc-p (insert "\n") (indent-for-tab-command))
      (forward-line -1)
      (end-of-line))))

;;;###autoload
(defun docstr-key-init ()
  "Initailization for key functions."
  (when docstr-key-support
    (when (memq major-mode docstr-key-javadoc-like-modes)
      (docstr-util-key-advice-add "*" :around #'docstr-key-javadoc-asterik)
      (docstr-util-key-advice-add "RET" :around #'docstr-key-javadoc-return))))

(provide 'docstr-key)
;;; docstr-key.el ends here
