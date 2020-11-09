;;; docstr-parsers.el --- Parsers for document string  -*- lexical-binding: t; -*-

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
;; Parsers for document string.
;;

;;; Code:

(defcustom docstr-parser-alist
  `((actionscript-mode . docstr-actionscript-parser)
    (c-mode            . docstr-c-parser)
    (c++-mode          . docstr-c++-parser)
    (csharp-mode       . docstr-csharp-parser)
    (go-mode           . docstr-go-parser)
    (groovy-mode       . docstr-groovy-parser)
    (groovy-mode       . docstr-groovy-parser)
    (java-mode         . docstr-java-parser)
    (javascript-mode   . docstr-javascript-parser)
    (js-mode           . docstr-javascript-parser)
    (js2-mode          . docstr-javascript-parser)
    (js3-mode          . docstr-javascript-parser)
    (lua-mode          . docstr-lua-parser)
    (masm-mode         . docstr-asm-parser)
    (nasm-mode         . docstr-asm-parser)
    (php-mode          . docstr-php-parser)
    (python-mode       . docstr-python-parser)
    (rjsx-mode         . docstr-rjsx-parser)
    (rust-mode         . docstr-rust-parser)
    (scala-mode        . docstr-scala-parser)
    (typescript-mode   . docstr-typescript-parser)
    (web-mode          . docstr-php-parser))
  "List of parser to each `major-mode'."
  :type 'list
  :group 'docstr)

(provide 'docstr-parsers)
;;; docstr-parsers.el ends here
