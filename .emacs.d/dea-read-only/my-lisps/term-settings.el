;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-22 15:20:09 Tuesday by ahei>

(defun term-mode-settings ()
  "Settings for `term-mode'"
  ;; emacs gui版本如果不把scroll-margin设为0
  ;; 当光标最屏幕底部时，有可能使得屏幕发生抖动
  (make-local-variable 'scroll-margin)
  (setq scroll-margin 0)
  (kill-buffer-when-shell-command-exit))

(add-hook 'term-mode-hook 'term-mode-settings)
