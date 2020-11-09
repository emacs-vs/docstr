;;; docstr.el --- A document string minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-05 20:33:49

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: A document string minor mode.
;; Keyword: document string
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
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
;; A document string minor mode.
;;

;;; Code:

(require 'docstr-parsers)

(defgroup docstr nil
  "A document string minor mode."
  :prefix "docstr-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/docstr"))

(defun docstr-actionscript-parser (search-string)
  "Parser for ActionScript using SEARCH-STRING."
  )

(defun docstr-major-modes ()
  "List of `major-mode' that supports document string."
  (let (lst)
    (dolist (m docstr-parser-alist) (push (car m) docstr-parser-alist))
    (reverse lst)))


(defun docstr--trigger-return ()
  "Trigger document string by pressing key return."
  )

(defun docstr--trigger-csharp ()
  "Trigger document string inside C#."
  )

(defun docstr--trigger-lua ()
  "Trigger document string inside Lua."
  )

(defun docstr--trigger-python ()
  "Trigger document string inside Python."
  )

(defun docstr--enable ()
  "Enable `docstr' in current buffer."
  (advice-add (key-binding (kbd "RET")) :after #'docstr--trigger-return)
  (cl-case major-mode
    (csharp-mode (advice-add (key-binding (kbd "/")) :after #'docstr--trigger-csharp))
    (lua-mode (advice-add (key-binding (kbd "-")) :after #'docstr--trigger-lua))
    (python-mode (advice-add (key-binding (kbd "\"")) :after #'docstr--trigger-python))))

(defun docstr--disable ()
  "Disable `docstr' in current buffer."
  (advice-remove (key-binding (kbd "RET")) #'docstr--trigger-return)
  (cl-case major-mode
    (csharp-mode (advice-remove (key-binding (kbd "/")) #'docstr--trigger-csharp))
    (lua-mode (advice-remove (key-binding (kbd "-")) #'docstr--trigger-lua))
    (python-mode (advice-remove (key-binding (kbd "\"")) #'docstr--trigger-python))))

;;;###autoload
(define-minor-mode docstr-mode
  "Minor mode 'docstr-mode'."
  :lighter " DocStr"
  :group docstr
  (if docstr-mode (docstr--enable) (docstr--disable)))

(provide 'docstr)
;;; docstr.el ends here
