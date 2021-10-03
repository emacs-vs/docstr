;;; bootstrap.el --- CI test bootstrap -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jen-Chieh Shen
;;
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
;;
;;; Commentary:
;;
;; CI test bootstrap
;;
;;; Code:

(require 'package)

(setq network-security-level 'low)

(setq user-emacs-directory (expand-file-name (make-temp-name ".emacs.d")
                                             "~")
      package-user-dir (expand-file-name (make-temp-name "tmp-elpa")
                                         user-emacs-directory))

(let* ((package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
       (pkgs '(s)))
  (package-initialize)
  (package-refresh-contents)

  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg)))
        pkgs)

  (add-hook 'kill-emacs-hook
            `(lambda () (delete-directory ,user-emacs-directory t))))

;;; bootstrap.el ends here
