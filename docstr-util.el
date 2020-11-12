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

(provide 'docstr-util)
;;; docstr-util.el ends here
