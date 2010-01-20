;;; yunt-viper.el ---

;;vim pulse vim in emacs 真邪恶:) ctrl+z 进行emacs vim之间的切换
(setq viper-mode t)                ; enable Viper at load time
(require 'viper)                   ; load Viper
(setq-default viper-mode t
viper-expert-level '5
;; ex-style don't allow you to move cross lines.
viper-ex-style-editing nil
viper-ex-style-motion nil
viper-auto-indent t
viper-inhibit-startup-message t
viper-eletric-mode t
viper-always t
viper-want-ctl-h-help t
viper-want-emacs-keys-in-insert t
viper-want-emacs-keys-in-vi t
viper-vi-style-in-minibuffer nil
viper-no-multiple-ESC t
viper-case-fold-search t
viper-re-search t
viper-re-query-replace t
viper-syntax-preference 'emacs
viper-delete-backwards-in-replace t
viper-parse-sexp-ignore-comments nil
viper-ESC-moves-cursor-back nil)

(define-key viper-vi-global-user-map "\C-f" 'forward-char)
(define-key viper-vi-global-user-map "\C-b" 'backward-char)
(define-key viper-vi-global-user-map "\M-v" 'scroll-down)
(define-key viper-vi-global-user-map "\C-v" 'scroll-up)
(define-key viper-vi-global-user-map "\C-y" 'yank)
(define-key viper-vi-global-user-map "\C-e"
(or (command-remapping 'move-end-of-line) 'move-end-of-line))

;; I don't like the default face in viper minibuffer
(setq viper-minibuffer-vi-face nil
viper-minibuffer-emacs-face nil)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'vimpulse)
(setq woman-use-own-frame nil)     ; don't create new frame for manpages
(setq woman-use-topic-at-point t)
(require 'redo)
(require 'rect-mark)

(provide 'yunt-viper)

;;; yunt-viper.el ends here
