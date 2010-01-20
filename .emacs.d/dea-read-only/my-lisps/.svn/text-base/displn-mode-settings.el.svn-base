;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-26 18:16:56 Saturday by ahei>

(require 'man)

;; 显示行号
;; 缺点:
;;      1.scroll bar去不掉
;;      2.补全窗口会全屏
;; (require 'wb-line-number)
;; (wb-line-number-enable)

;; setnu-plus
;; 缺点:
;;      1.hs-minor-mode下折叠的区域不能用C-n键向下滚动
;;      2.第一行显示有问题,有时候其他行显示也有问题
;;      3.用putty远程登录使用emacs时,粘贴时速度比较慢
;;      4.与doxymacs冲突,有doxymacs时不能显示行号
;; (load "setnu-plus")
;; (setq setnu-line-number-format "%4d ")

(setq use-linum t)
(if use-linum
    (progn
      (if is-after-emacs-23
          (require 'linum)
        (require 'linum "linum-for-22"))
      (setq linum-format "%4d|")

      (if is-before-emacs-21
          (progn
            (defalias 'displn-mode 'linum-mode)
            (defalias 'global-displn-mode 'global-linum-mode)
            (defvaralias 'displn-mode 'linum-mode)
            (defvaralias 'global-displn-mode 'global-linum-mode))
        (defalias 'displn-mode 'linum-mode "显示行号的命令")
        (defalias 'global-displn-mode 'global-linum-mode "显示行号的命令")
        (defvaralias 'displn-mode 'linum-mode "显示行号的命令")
        (defvaralias 'global-displn-mode 'global-linum-mode "显示行号的命令")))
  
  ;; 加载显示行号模块
  ;; 缺点:
  ;;      1.gdb调试时,源码的窗口不能显示行号,必须刷新一下才可以
  ;;      2.hs-minor-mode下折叠的区域不能用C-n键向下滚动
  ;;      3.用putty远程登录使用emacs时,粘贴时速度非常慢
  (require 'display-line-number)

  ;; 设置显示格式
  (setq display-line-number-format "%4d|")
  (if is-before-emacs-21
      (progn
        (defalias 'displn-mode 'display-line-number-mode)
        (defalias 'global-displn-mode 'global-display-line-number-mode)
        (defvaralias 'displn-mode 'display-line-number-mode)
        (defvaralias 'global-displn-mode 'global-display-line-number-mode))
    (defalias 'displn-mode 'display-line-number-mode "显示行号的命令")
    (defalias 'global-displn-mode 'global-display-line-number-mode "显示行号的命令")
    (defvaralias 'displn-mode 'display-line-number-mode "显示行号的命令")
    (defvaralias 'global-displn-mode 'global-display-line-number-mode "显示行号的命令")))

(dolist (hook `(find-file-hook help-mode-hook Man-mode-hook log-view-mode-hook
                               compilation-mode-hook gdb-mode-hook lisp-interaction-mode-hook
                               browse-kill-ring-mode-hook completion-list-mode-hook hs-hide-hook
                               inferior-ruby-mode-hook custom-mode-hook Info-mode-hook svn-log-edit-mode-hook
                               package-menu-mode-hook dired-mode-hook apropos-mode-hook svn-log-view-mode-hook
                               diff-mode-hook emacs-lisp-mode-hook ibuffer-mode-hook html-mode-hook
                               w3m-mode-hook data-debug-hook debugger-mode-hook eagle-mode-hook))
  (add-hook hook
            (lambda()
              (if (not (memq major-mode '(image-mode)))
                  (displn-mode t)) t)))

(global-set-key (kbd "C-x N") 'displn-mode)
