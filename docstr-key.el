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
(require 's)

(require 'docstr-util)

(declare-function docstr-get-prefix "ext:docstr.el")

(defcustom docstr-key-support nil
  "If non-nil, use key support to fulfill document triggerations' conditions."
  :type 'boolean
  :group 'docstr)

(defcustom docstr-key-javadoc-like-modes
  (append '(c-mode c++-mode objc-mode csharp-mode swift-mode)
          '(java-mode groovy-mode processing-mode)
          '(javascript-mode js-mode js2-mode js3-mode json-mode rjsx-mode)
          '(web-mode php-mode)
          '(actionscript-mode typescript-mode)
          '(go-mode scala-mode)
          '(rust-mode rustic-mode)
          '(css-mode ssass-mode scss-mode))
  "List of `major-mode' that can be use Javadoc style."
  :type 'list
  :group 'docstr)

(defcustom docstr-key-inhibit-doc-symbol
  '("//" "--" "#")
  "List of document symbol that are inhibit to insert for prefix."
  :type 'list
  :group 'docstr)

(defcustom docstr-key-sharp-doc-modes
  '(python-mode ruby-mode sh-mode)
  "List of `major-mode' that use # as document string prefix."
  :type 'list
  :group 'docstr)

(defun docstr-key-javadoc-like-p ()
  "Return non-nil if current `major-mode' use Javadoc style."
  (memq major-mode docstr-key-javadoc-like-modes))

(defun docstr-key-insert-prefix ()
  "Insert prefix."
  (let ((prefix (docstr-get-prefix)))
    (when prefix (insert prefix) (indent-for-tab-command))))

(defun docstr-key-single-line-prefix-insertion ()
  "Insertion for single line comment."
  (when (docstr-util-current-line-empty-p)  ; Ensure on newline
    (let* ((prev-line-text (save-excursion (forward-line -1) (thing-at-point 'line)))
           (prev-line-doc-symbol (docstr-util-comment-line-symbol -1))
           (current-line-doc-symbol (docstr-util-comment-line-symbol))
           (next-line-doc-symbol (docstr-util-comment-line-symbol 1))
           (prev-line-content (string-trim (s-replace prev-line-doc-symbol "" prev-line-text))))
      (when (or (docstr-util-string-match-mut-p prev-line-doc-symbol next-line-doc-symbol)
                (and (not (string-empty-p prev-line-content))
                     (not (docstr-util-contain-list-type-str
                           docstr-key-inhibit-doc-symbol prev-line-doc-symbol 'strict))
                     (string= current-line-doc-symbol next-line-doc-symbol)))
        (insert (concat (docstr-util-min-str prev-line-doc-symbol next-line-doc-symbol) " "))
        (indent-for-tab-command)))))

(defun docstr-key-javadoc-asterik (fnc &rest args)
  "Asterik key for Javadoc like document string.

This fulfill condition, /* with */ into a pair.

Arugments FNC and ARGS are for advice around."
  (apply fnc args)
  (when (docstr-key-javadoc-like-p)
    (save-excursion
      (when (and (docstr-util-is-behind-last-char-at-line-p)
                 (docstr-util-looking-back "/[*]" 2))
        (insert "*/")))))

(defun docstr-key-c-like-return (fnc &rest args)
  "Return key for C like programming languages.

This function will help insert the corresponding prefix on every line of the
document string.

Arugments FNC and ARGS are for advice around."
  (if (or (not (docstr-key-javadoc-like-p)) (not (docstr-util-comment-block-p)))
      (apply fnc args)
    (let ((new-doc-p (docstr-util-between-pair-p "/*" "*/")))
      (apply fnc args)
      (if (docstr-util-multiline-comment-p)
          (docstr-key-insert-prefix)
        (docstr-key-single-line-prefix-insertion))
      (when (and new-doc-p
                 ;; Make sure end symbol */ still at the back
                 (not (docstr-util-current-line-empty-p)))
        ;; We can't use `newline-and-indent' here, or else the space will be gone.
        (progn (insert "\n") (indent-for-tab-command))
        (forward-line -1))
      (end-of-line))))

(defun docstr-key-lua-return (fnc &rest args)
  "Return key for Lua document string.

This function has two features.

1. Extra indented newline with multi-line comment.

```lua
--[[
  > Extra newline inserted <
]]
```

2. Document prefix inserted with single line comment.

```lua
-- !Document line must presented before inserting a new prefix line!
-- !Cursor is here, prepare for return!
-- !The prefix inserted after hitting retun!
```

P.S. Prefix will matches the same as your document style selection.

Arugments FNC and ARGS are for advice around."
  (cond ((and (eq major-mode 'lua-mode) (docstr-util-comment-block-p))
         (let ((new-doc-p (docstr-util-between-pair-p "--[[" "]]")))
           (apply fnc args)
           (when new-doc-p (end-of-line)))
         (unless (string= "--[[" (docstr-util-start-comment-symbol))
           (docstr-key-single-line-prefix-insertion)))
        (t (apply fnc args))))

(defun docstr-key-sharp-return (fnc &rest args)
  "Return key for programming languages that can use # as document.

This is the same as function `docstr-key-lua-return' feature Pt 2
but instead of inserting two `-`, this will insert a `#` instead.

Arugments FNC and ARGS are for advice around."
  (cond ((and (memq major-mode docstr-key-sharp-doc-modes) (docstr-util-comment-block-p))
         (let ((start-comment (docstr-util-start-comment-symbol)))
           (apply fnc args)
           (when (string-match-p "#" start-comment)
             (docstr-key-single-line-prefix-insertion))))
        (t (apply fnc args))))

(defun docstr-key-enable ()
  "Enable key functions."
  (when docstr-key-support
    (docstr-util-key-advice-add "*" :around #'docstr-key-javadoc-asterik)
    (docstr-util-key-advice-add "RET" :around #'docstr-key-c-like-return)
    (docstr-util-key-advice-add "RET" :around #'docstr-key-lua-return)
    (docstr-util-key-advice-add "RET" :around #'docstr-key-sharp-return)))

(defun docstr-key-disable ()
  "Disable key functions."
  (docstr-util-key-advice-remove "*" #'docstr-key-javadoc-asterik)
  (docstr-util-key-advice-remove "RET" #'docstr-key-c-like-return)
  (docstr-util-key-advice-remove "RET" #'docstr-key-lua-return)
  (docstr-util-key-advice-remove "RET" #'docstr-key-sharp-return))

(provide 'docstr-key)
;;; docstr-key.el ends here
