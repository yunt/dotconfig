;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/26/2009 16:47:07 星期一 by ahei>

(require 'highlight-parentheses)

;; TODO: 最后一项不知道为啥不起作用
(setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))

(dolist (hook (list 'find-file-hook 'help-mode-hook 'Man-mode-hook 'log-view-mode-hook
                    'compilation-mode-hook 'gdb-mode-hook 'lisp-interaction-mode-hook
                    'browse-kill-ring-mode-hook 'completion-list-mode-hook 'hs-hide-hook
                    'inferior-ruby-mode-hook 'custom-mode-hook 'Info-mode-hook 'svn-log-edit-mode-hook
                    'package-menu-mode-hook 'dired-mode-hook 'apropos-mode-hook))
  (add-hook hook
            (lambda()
              (highlight-parentheses-mode t)) t))
