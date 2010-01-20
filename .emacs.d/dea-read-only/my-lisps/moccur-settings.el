;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/25/2009 23:12:55 星期日 by ahei>

(require 'color-moccur)

(defun occur-by-moccur-at-point ()
  "对当前光标所在的单词运行`occur-by-moccur'命令"
  (interactive)
  (if (current-word)
      (occur-by-moccur-displn (current-word) nil)))
(defun occur-by-moccur-at-point-displn ()
  "运行`occur-by-moccur-at-point'后显示行号"
  (interactive)
  (occur-by-moccur-at-point)
  (let ((buffer (get-buffer "*Moccur*")))
    (if buffer (with-current-buffer buffer (displn-mode t)))))
(defun occur-by-moccur-displn (regexp arg)
  "运行`occur-by-moccur'后显示行号"
  (interactive (list (moccur-regexp-read-from-minibuf) current-prefix-arg))
  (occur-by-moccur regexp arg)
  ;; TODO: 为什么这样第一次运行的时候按键设置没起作用
  ;;   (define-key moccur-mode-map "o" 'other-window)
  (let ((buffer (get-buffer "*Moccur*")))
    (if buffer
        (with-current-buffer buffer
          (displn-mode t)
          (moccur-my-keys)))))
(defun isearch-moccur-displn ()
  "运行`isearch-moccur'后显示行号"
  (interactive)
  (isearch-moccur)
  (let ((buffer (get-buffer "*Moccur*")))
    (if buffer
        (with-current-buffer buffer
          (displn-mode t)
          (moccur-my-keys)))))
(defun moccur-my-keys ()
  (local-set-key (kbd "o") 'other-window)
  (local-set-key (kbd "m") 'moccur-disp-cur-line)
  (local-set-key (kbd "h") 'backward-char)
  (local-set-key (kbd "l") 'forward-char)
  (local-set-key (kbd "b") 'backward-word)
  (local-set-key (kbd "w") 'forward-word-or-to-word)
  (local-set-key (kbd "f") 'forward-word)
  (local-set-key (kbd "y") 'copy-region-as-kill-nomark)
  (local-set-key (kbd "c") 'copy-region-as-kill-nomark)
  (local-set-key (kbd ".") 'set-mark-command)
  (local-set-key (kbd "L") 'count-brf-lines))
(defun moccur-disp-cur-line ()
  "moccur显示当前行"
  (interactive)
  (moccur-next 1)
  (moccur-prev 1))
(global-set-key (kbd "C-x C-u") 'occur-by-moccur-displn)
(global-set-key (kbd "C-x M-U") 'occur-by-moccur-at-point-displn)

;; TODO: following statement do not work well, why?
(define-key isearch-mode-map (kbd "M-o") 'isearch-moccur-displn)
(add-hook 'isearch-mode-hook
          '(lambda ()
             (define-key isearch-mode-map (kbd "M-o") 'isearch-moccur-displn)))
