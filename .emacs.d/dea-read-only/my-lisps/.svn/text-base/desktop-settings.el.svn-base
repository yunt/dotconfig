;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-17 11:30:51 Thursday by ahei>

(require 'desktop)

(setq desktop-load-locked-desktop t)
(define-key global-map (kbd "C-x M-C") 'desktop-clear)
(if is-before-emacs-21 (desktop-load-default) (desktop-save-mode 1))
(desktop-read)
(dolist (var (list 'command-history 'kill-ring 'file-name-history 'find-symbol-last-symbol
                   'extended-command-history 'grep-history 'compile-history 'last-template
                   'minibuffer-history 'query-replace-history 'regexp-history
                   'shell-command-history 'recentf-open-last-file 'describe-symbol-last-symbol
                   'switch-major-mode-last-mode 'sb-keep-buffer))
  (add-to-list 'desktop-globals-to-save var))
