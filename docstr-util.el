;;; docstr-util.el --- Utility module  -*- lexical-binding: t; -*-

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
;; Utility module.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar docstr-format-type)
(defvar docstr-format-var)
(defvar docstr-format-param)
(defvar docstr-format-return)
(defvar docstr-key-type)
(defvar docstr-key-var)
(defvar docstr-key-desc)
(defvar docstr-concat-type)
(defvar docstr-concat-var)
(defvar docstr-show-type-name)
(defvar docstr-show-return)

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
  (when (stringp str)
    (let ((pos -1) (run-it t))
      (while run-it
        (setq run-it (string-match-p reg str (1+ pos)))
        (when run-it (setq pos run-it)))
      (if (= pos -1) nil pos))))

(defun docstr-util-comment-block-p (&optional pos)
  "Return non-nil if POS is inside a comment block."
  (unless pos (setq pos (point)))
  (save-excursion (goto-char pos) (nth 4 (syntax-ppss))))

(defun docstr-util-previous-blank-line ()
  "Move to the previous line containing nothing but whitespaces or tabs."
  (let ((sr-pt (save-excursion (re-search-backward "^[ \t]*\n" nil t))))
    (goto-char (if sr-pt sr-pt (point-min)))))

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

(defun docstr-util-contain-list-string (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'.
This function wrapped IN-STR with function `regexp-quote'."
  (cl-some (lambda (str) (string-match-p (regexp-quote str) in-str)) in-list))

(defun docstr-util-contain-list-type-str (in-list in-str type)
  "Return non-nil if IN-STR is listed in IN-LIST.

Argument TYPE see function `docstr-util-string-compare-p' for more information."
  (cl-some (lambda (elm) (docstr-util-string-compare-p elm in-str type)) in-list))

;;
;; (@* "String" )
;;

(defun docstr-util-min-str (str1 str2)
  "Return minimum string by comparing the lenght of STR1 and STR2."
  (cond ((and (null str1) (null str2)) "")
        ((null str1) str2)
        ((null str2) str1)
        (t (if (< (length str1) (length str2)) str1 str2))))

(defun docstr-util-string-match-mut-p (str1 str2)
  "Mutual way to check STR1 and STR2 with function `string-match-p'."
  (and (stringp str1) (stringp str2)
       (or (string-match-p str1 str2) (string-match-p str2 str1))))

(defun docstr-util-string-compare-p (regexp str type &optional ignore-case)
  "Compare STR with REGEXP by TYPE.

Argument TYPE can be on of the following symbol.

  * regex - uses function `string-match-p'.  (default)
  * strict - uses function `string='.
  * prefix - uses function `string-prefix-p'.
  * suffix - uses function `string-suffix-p'.

Optional argument IGNORE-CASE is only uses when TYPE is either symbol `prefix'
or `suffix'."
  (cl-case type
    (strict (string= regexp str))
    (prefix (string-prefix-p regexp str ignore-case))
    (suffix (string-suffix-p regexp str ignore-case))
    (t (string-match-p regexp str))))

;;
;; (@* "Insertion" )
;;

(defun docstr-util-insert (&rest args)
  "First newline and indent then insert ARGS."
  (insert "\n")
  (indent-for-tab-command)
  (apply #'insert args))

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
    (apply #'docstr-util-insert-args (cdr args))))

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
;; (@* "Point" )
;;

(defun docstr-util-is-behind-last-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing behind of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-forward "[^ \t]" (line-end-position) t))))

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
         (docstr-util-contain-list-string c (docstr-util--get-current-char-string)))
        (t nil)))

;;
;; (@* "Comment" )
;;

(defun docstr-util-between-pair-p (p1 p2)
  "Return non-nil if pair P1 and P2 on the same line."
  (and (save-excursion (search-backward p1 (line-beginning-position) t))
       (save-excursion (search-forward p2 (line-end-position) t))))

(defun docstr-util--goto-start-comment ()
  "Go to the start of the comment."
  (while (docstr-util-comment-block-p)
    (re-search-backward comment-start-skip nil t)))

(defun docstr-util--goto-end-comment ()
  "Go to the end of the comment."
  (when (docstr-util-comment-block-p)
    (forward-char 1)
    (docstr-util--goto-end-comment)))

(defun docstr-util-start-comment-point (&optional pt)
  "Point at the start of the comment point relative to PT."
  (save-excursion (when pt (goto-char pt)) (docstr-util--goto-start-comment) (point)))

(defun docstr-util-end-comment-point (&optional pt)
  "Point at the end of the comment point relative to PT."
  (save-excursion (when pt (goto-char pt)) (docstr-util--goto-end-comment) (point)))

(defun docstr-util-start-comment-symbol (&optional pt)
  "Return the starting comment symbol form the given PT."
  (when (docstr-util-comment-block-p)
    (let (start-pt)
      (save-excursion
        (when pt (goto-char pt))
        (docstr-util--goto-start-comment)
        (progn  ; Make sure to go outside of symbol
          (re-search-backward "[ \t\r\n]" nil t)
          (when (= (point) (line-end-position)) (forward-char 1)))
        (setq start-pt (point))
        (re-search-forward comment-start-skip (1+ (line-end-position)) t)
        (if (= start-pt (point)) nil
          (string-trim (buffer-substring start-pt (point))))))))

(defun docstr-util-end-comment-symbol (&optional pt)
  "Return the ending comment symbol form the given PT."
  (when (docstr-util-comment-block-p)
    (let (end-pt)
      (save-excursion
        (when pt (goto-char pt))
        (docstr-util--goto-end-comment)
        (setq end-pt (point))
        (re-search-backward "[ \t\r\n]" (1- (line-beginning-position)) t)
        (if (= end-pt (point)) nil
          (string-trim (buffer-substring (point) end-pt)))))))

(defun docstr-util-multiline-comment-p ()
  "Return non-nil, if current point inside multi-line comment block."
  (ignore-errors (string-match-p "/[*]" (docstr-util-start-comment-symbol))))

(defun docstr-util-comment-line-symbol (&optional n)
  "Forward N line and return starting comment symbol."
  (save-excursion
    (when n (forward-line n)) (end-of-line)
    (docstr-util-start-comment-symbol)))

;;
;; (@* "Key" )
;;

(defun docstr-util-key-advice-add (key where fnc)
  "Safe add advice KEY with FNC at WHERE."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-add key-fnc where fnc))))

(defun docstr-util-key-advice-remove (key fnc)
  "Safe remove advice KEY with FNC."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-remove key-fnc fnc))))

;;
;; (@* "Kill" )
;;

(defun docstr-util-kill-line (&optional pos)
  "Delete line from POS."
  (save-excursion
    (when pos (goto-char pos))
    (delete-region (1- (line-beginning-position)) (line-end-position))))

;;
;; (@* "Default" )
;;

(cl-defun docstr-util-default-format
    (&key (fmt-type "{ %s }") (fmt-var "%s :") (param "@param") (ret "@return")
          (con-type t) (con-var t) (show-tn t) (show-ret t))
  "Set default format for document string."
  (unless (string-empty-p param) (setq param (concat param " ")))
  (unless (string-empty-p ret) (setq ret (concat ret " ")))
  (setq-local
   docstr-format-type fmt-type
   docstr-format-var fmt-var
   docstr-format-param (format "%s%s%s%s" param docstr-key-type docstr-key-var docstr-key-desc)
   docstr-format-return (format "%s%s%s%s" ret docstr-key-type docstr-key-var docstr-key-desc)
   docstr-concat-type con-type
   docstr-concat-var con-var
   docstr-show-type-name show-tn
   docstr-show-return show-ret))

(provide 'docstr-util)
;;; docstr-util.el ends here
