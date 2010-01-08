;;Fonts
;(create-fontset-from-fontset-spec
;   "-*-monaco-medium-R-normal--12-*-*-*-*-*-fontset-mymono,
;   chinese-gb2312:-*-yahei mono-medium-*-normal--14-*-*-*-*-*-iso10646-1,
;    chinese-gbk:-*-yahei mono-medium-*-normal--14-*-*-*-*-*-iso10646-1,
;     chinese-gb18030:-*-yahei mono-medium-*-normal--14-*-*-*-*-*-iso10646-1"
;     )
;(setq default-frame-alist (append '((font . "fontset-mymono")) default-frame-alist))
;(set-default-font "fontset-mymono")

;;; 显示时间
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
;;;; 关闭启动画面
(setq inhibit-startup-message t)
;;;;设置大的kill ring
(setq kill-ring-max 150)
(tool-bar-mode nil);去掉那个大大的工具栏
(scroll-bar-mode nil);去掉滚动条，因为可以使用鼠标滚轮了 ^_^
(setq x-select-enable-clipboard t);支持emacs和外部程序的粘贴
(font-lock-mode t) ; 开启语法高亮
(setq default-tab-width 4)

;; Themes
(require 'color-theme)
(color-theme-initialize)
;;(color-theme-robin-hood)
(color-theme-dark-laptop)

;; Shell

;;vim pulse vim in emacs 真邪恶:) ctrl+z 进行emacs vim之间的切换
(setq viper-mode t)                ; enable Viper at load time
(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
(require 'viper)                   ; load Viper
(add-to-list 'load-path "~/.emacs.d/plugins/vim")
(require 'vimpulse)                ; load Vimpulse
(setq woman-use-own-frame nil)     ; don't create new frame for manpages
(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)
;;ctrl + r -> redo
(require 'redo)
;;nice rectangle
(require 'rect-mark)


;; Python
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))


;; C/C++
;;;; Load CEDET
(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
(semantic-load-enable-code-helpers)
;;speedbar key binding  
(global-set-key [(f2)] 'speedbar-get-focus)  
;;alt+/ 自动补全  
;(define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(require 'ecb)
;(require 'ecb-autoloads)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
 (require 'ibuf-ext)
  (setq ibuffer-mode-hook
        (lambda ()
          (setq ibuffer-filter-groups
                '(
                  ("*buffer*" (name . "\\*.*\\*"))
                  ("dired" (mode . dired-mode))
                  ("perl" (or (mode . cperl-mode)
                              (mode . sepia-mode)
                              (mode . perl-mode)))
                  ("elisp" (or (mode . emacs-lisp-mode)
                               (mode . lisp-interaction-mode)))
                  ("prog" (or (mode . c++-mode)
                              (mode . c-mode)
                              (mode . java-mode)))
                  ("tags" (name . "^TAGS"))))))

(define-ibuffer-sorter file-name
  "Sort buffers by associated file name"
  (:description "file name")
  (apply 'string<
         (mapcar (lambda (buf)
                   (with-current-buffer (car buf)
                     (or buffer-file-name default-directory)))
                 (list a b))))
(define-key ibuffer-mode-map "sf" 'ibuffer-do-sort-by-file-name)

(defun ywb-ibuffer-rename-buffer ()
    (interactive)
    (call-interactively 'ibuffer-update)
    (let* ((buf (ibuffer-current-buffer))
           (name (generate-new-buffer-name
                  (read-from-minibuffer "Rename buffer(to new name): "
                                        (buffer-name buf)))))
      (with-current-buffer buf
        (rename-buffer name)))
    (call-interactively 'ibuffer-update))
  (define-key ibuffer-mode-map "r" 'ywb-ibuffer-rename-buffer)

