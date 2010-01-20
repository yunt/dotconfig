;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-13 22:17:06 Friday by ahei>

(require 'browse-kill-ring)

(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-maximum-display-length nil)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-separator "------------------------------------------------------------")
(setq browse-kill-ring-display-duplicates nil)
(add-hook 'browse-kill-ring-hook 'browse-kill-ring-my-keys)
(defun browse-kill-ring-my-keys ()
  (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit-rdonly)
  (define-key browse-kill-ring-mode-map "<" 'beginning-of-buffer)
  (define-key browse-kill-ring-mode-map ">" 'end-of-buffer)
  (define-key browse-kill-ring-mode-map "j" 'next-line)
  (define-key browse-kill-ring-mode-map "k" 'previous-line)
  (define-key browse-kill-ring-mode-map "h" 'backward-char)
  (define-key browse-kill-ring-mode-map "l" 'forward-char)
  (define-key browse-kill-ring-mode-map (kbd "SPC") 'scroll-up)
  (define-key browse-kill-ring-mode-map (kbd "U") 'scroll-down)
  (define-key browse-kill-ring-mode-map "u" 'View-scroll-half-page-backward)
  (define-key browse-kill-ring-mode-map "o" 'other-window))
