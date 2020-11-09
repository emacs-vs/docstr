;;; docstr.el --- A document string minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-11-05 20:33:49

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: A document string minor mode.
;; Keyword: document string
;; Version: 0.1.0
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
;; A document string minor mode.
;;

;;; Code:

(require 'cl-lib)
(require 's)
(require 'subr-x)

(require 'docstr-util)
(require 'docstr-writers)

(defgroup docstr nil
  "A document string minor mode."
  :prefix "docstr-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/docstr"))

;;
;; (@* "Entry" )
;;

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

;;
;; (@* "Core" )
;;

(defun docstr-major-modes ()
  "List of `major-mode' that supports document string."
  (let (lst)
    (dolist (m docstr-writer-alist) (push (car m) docstr-writer-alist))
    (reverse lst)))

(defun docstr-get-writer ()
  "Return the writer from `docstr-writer-alist'."
  (assoc (buffer-local-value 'major-mode (current-buffer)) docstr-writer-alist))

(defun docstr--insert-doc-string (search-string)
  "Insert document string base on SEARCH-STRING."
  (let ((writer (docstr-get-writer)))
    (if writer (funcall writer search-string)
      (user-error "[WARNING] No document string support for %s" major-mode))))

(defun docstr--get-search-string (type sr)
  "Return string that can be analyze by document string writer.

Argument TYPE can either be a function or an interger.  If it's function
execute it inside the buffer.  Otherwire, if it's an integer call function
`forward-line' instead.

Argument SR is the target symbol for us to stop looking for the end of declaration."
  (let (beg)
    (save-excursion
      (cond ((functionp type) (funcall type))
            ((integerp type) (forward-line type)))
      (unless (docstr-util-current-line-empty-p)
        (setq beg (line-beginning-position))
        (re-search-forward sr nil t)
        (substring (buffer-string) beg (point))))))

(defun docstr--trigger-return ()
  "Trigger document string by pressing key return."
  (interactive)
  (when docstr-mode
    (let ((ln-prev (docstr-util-line-relative -1 t))
          (ln-next (docstr-util-line-relative 1 t))
          search-string)
      (when (and (string-prefix-p "/*" ln-prev) (string-suffix-p "*/" ln-next))
        (setq search-string (docstr--get-search-string 2 "{")
              search-string (string-trim search-string)
              search-string (s-replace "\n" " " search-string))
        (docstr--insert-doc-string search-string)))))

(defun docstr--trigger-csharp ()
  "Trigger document string inside C#."
  (interactive)
  (when (and ;;docstr-mode
         (looking-back "///" 3))
    (save-excursion
      (insert " <summary>\n")
      (insert "/// \n")
      (insert "/// </summary>"))
    (forward-line 1)
    (end-of-line)
    ))

(defun docstr--trigger-lua ()
  "Trigger document string inside Lua."
  (interactive)
  (when (and docstr-mode (looking-back "---" 3))
    (save-excursion (insert "--\n"))
    ))

(defun docstr--trigger-python ()
  "Trigger document string inside Python."
  (when (and docstr-mode (looking-back "\"\"\"" 3))
    (save-excursion (insert "\"\"\""))
    ))

(provide 'docstr)
;;; docstr.el ends here
