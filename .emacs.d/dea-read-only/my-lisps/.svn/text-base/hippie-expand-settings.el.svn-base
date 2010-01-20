;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/21/2009 16:26:37 星期三 by ahei>

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))
(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook))
  (add-hook hook
            '(lambda ()
               (make-local-variable 'hippie-expand-try-functions-list)
               (setq hippie-expand-try-functions-list
                     '(try-expand-dabbrev
                       try-expand-dabbrev-visible
                       try-expand-dabbrev-all-buffers
                       try-expand-dabbrev-from-kill
                       try-complete-file-name-partially
                       try-complete-file-name
                       try-expand-all-abbrevs
                       try-expand-list
                       try-expand-line
                       try-expand-whole-kill)))))
