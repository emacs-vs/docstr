;;; docstr-util.el --- Utility module  -*- lexical-binding: t; -*-

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
;; Utility module.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun docstr-util-line-relative (&optional n trim)
  "Return string of N line relatively.

If optional argument TRIM is non-nil; then trim the return string.

See function `forward-line' for argument N."
  (save-excursion
    (when n (forward-line n))
    (if trim (string-trim (thing-at-point 'line)) (thing-at-point 'line))))

(defun docstr-util-current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun docstr-util-last-regex-in-string (reg str)
  "Find the position in STR using REG from th end."
  (let ((pos -1) (run-it t))
    (while run-it
      (setq run-it (string-match-p reg str (1+ pos)))
      (when run-it (setq pos run-it)))
    (if (= pos -1) nil pos)))

(defun docstr-util-comment-block-p (&optional pos)
  "Return non-nil if POS is inside a comment block."
  (unless pos (setq pos (point)))
  (save-excursion (goto-char pos) (nth 4 (syntax-ppss))))

;;
;; (@* "List" )
;;

(defun docstr-util-chop (string separator)
  "Split a STRING without consuming a SEPARATOR."
  (cl-loop with seplen = (length separator)
           with len = (length string)
           with start = 0
           with next = seplen
           for end = (or (cl-search separator string :start2 next) len)
           for chunk = (substring string start end)
           collect chunk
           while (< end len)
           do (setf start end next (+ seplen end))))

(defun docstr-util-is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some (lambda (str) (string-match-p (regexp-quote str) in-str)) in-list))

;;
;; (@* "Insertion" )
;;

(defun docstr-util-insert (&rest args)
  "First newline and indent then insert ARGS."
  (insert "\n")
  (indent-for-tab-command)
  (apply 'insert args))

(defun docstr-util-insert-list (lst)
  "Insert list (LST) of strings with indnetation."
  (let (ignore-first)
    (dolist (str lst)
      (if ignore-first
          (docstr-util-insert str)
        (insert str))
      (setq ignore-first t))))

(defun docstr-util-insert-args (&rest args)
  "Insert after indentation with ARGS."
  (let ((split (split-string (car args) "\n")))
    (docstr-util-insert-list split))
  (when (cdr args)
    (apply 'docstr-util-insert-args (cdr args))))

(defun docstr-util-delete-current-line ()
  "Delete current line without consuming the newline."
  (delete-region (line-beginning-position) (line-end-position)))

;;
;; (@* "Searching" )
;;

(defun docstr-util-looking-back (regexp &optional limit greedy)
  "Wrapper for function `looking-back'.

See function `looking-back' description for arguments REGEXP, LIMIT,
and GREEDY."
  (ignore-errors (looking-back regexp limit greedy)))

;;
;; (@* "Character" )
;;

(defun docstr-util--get-current-char-string ()
  "Get the current character as the 'string'."
  (if (char-before) (string (char-before)) ""))

(defun docstr-util-current-char-equal-p (c)
  "Check the current character equal to C, C can be a list of character."
  (cond ((and (stringp c)
              (stringp (docstr-util--get-current-char-string)))
         (string= (docstr-util--get-current-char-string) c))
        ((listp c)
         (docstr-util-is-contain-list-string c (docstr-util--get-current-char-string)))
        (t nil)))

;;
;; (@* "Default" )
;;

(defun docstr-util-generate-format (param ret)
  ""
  (let ((fmt ""))
    (when (and docstr-show-type-name (not (string-empty-p docstr-format-type)))
      (setq fmt (concat fmt docstr-key-type)))

    fmt))

(cl-defun docstr-util-default-format
    (&key (fmt-type "{ %s }") (fmt-var "%s :") (param "@param") (ret "@return"))
  "Set default format for document string."
  (setq-local
   docstr-format-type fmt-type
   docstr-format-var fmt-var
   docstr-format-param (format "%s%s%s%s" param docstr-key-type docstr-key-var docstr-key-desc)
   docstr-format-return (format "%s%s%s%s" ret docstr-key-type docstr-key-var docstr-key-desc)))

(provide 'docstr-util)
;;; docstr-util.el ends here
