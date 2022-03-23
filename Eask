(source "gnu")
(source "melpa")

(package-file "docstr.el")

(files "*el" "langs/*.el")

(depends-on "emacs" "24.4")
(depends-on "s")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-command-compile-hook
           (lambda () (setq byte-compile-error-on-warn t)))
