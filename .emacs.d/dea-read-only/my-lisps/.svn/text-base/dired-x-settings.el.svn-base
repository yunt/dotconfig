;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/25/2009 17:43:11 星期日 by ahei>

;; dired-x, 忽略不感兴趣的文件
(when (not is-before-emacs-21)
  (add-hook 'dired-load-hook (lambda () (load "dired-x")))
  (add-hook 'dired-mode-hook
            (lambda ()
              (setq dired-omit-files "^#\\|^\\..*\\|semantic.cache")
              (if mswin
                  (setq dired-omit-files (concat dired-omit-files "\\|^_")))
              (dired-omit-mode 1))))

(setq dired-omit-size-limit 1000000)
