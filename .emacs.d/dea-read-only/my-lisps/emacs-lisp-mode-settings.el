;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-22 23:00:02 Tuesday by ahei>

(defun elisp-mode-settings ()
  "Settings for `emacs-lisp-mode'."
  (setq mode-name "ELisp"))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-settings)
