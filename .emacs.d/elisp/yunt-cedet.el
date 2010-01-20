;;; yunt-cedet.el --- cedet-ECB,semantic的设置
;;
;;; Code:
;;模板文件目录
(setq template-home-directory "~/.emacs.d/templates")
;;导入CEDET的各个插件
;;
;;Load CEDET
(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
;;Eieio
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/cedet/eieio"))
;;Semantic
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/cedet/semantic"))
;(load-file "~/lib/emacs-lisp/cedet-1.0pre4/eieio/eieio.el")
(require 'cc-mode)

(require 'semantic-ia)
(require 'semantic-gcc)
(global-ede-mode t)
(global-srecode-minor-mode 1)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)

;(when window-system
;  (global-semantic-tag-folding-mode 1))

;; Enable SRecode (Template management) minor-mode.
;(global-srecode-minor-mode 1)
(cogre-uml-enable-unicode)

;; TODO: 怎样可以不用这样取消`senator-prefix-key'的prefix command
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (make-local-variable 'senator-prefix-key)
;;              (setq senator-prefix-key nil)) t)
(dolist (map (list c-mode-base-map emacs-lisp-mode-map))
  (define-key map (kbd "C-c j") 'semantic-complete-jump-local)
  (define-key map (kbd "C-c n") 'senator-next-tag)
  (define-key map (kbd "C-c p") 'senator-previous-tag))

;; system include path
(if (or mswin cygwin)
    (dolist (mode '(c-mode c++-mode))
      (semantic-add-system-include "c:/cygwin/usr/include/" mode)))
(dolist (mode '(c-mode c++-mode))
	(semantic-add-system-include "/usr/local/pspdev/psp/include" mode)
	(semantic-add-system-include "/usr/local/pspdev/psp/sdk/include" mode))

;(unless mswin
;  ;; if you want to enable support for gnu global
;  (semanticdb-enable-gnu-global-databases 'c-mode)
;  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;(when (semantic-ectag-version)
;  (semantic-load-enable-primary-exuberent-ctags-support))
;;配置Sementic的检索范围
(setq semanticdb-project-roots
      (list
   (expand-file-name "/")))
;;配置semanticdb分析文件的目录
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))

(eval-after-load "semantic-complete"
		 '(setq semantic-complete-inline-analyzer-displayor-class
			semantic-displayor-ghost)) 

;; 配置 Emacs Code Browser
(add-to-list 'load-path
        "/usr/share/emacs/site-lisp/ecb")
;; ecb
(require 'ecb-autoloads)
(defun ecb ()
  "启动ecb"
  (interactive)
  (ecb-activate)
  (ecb-layout-switch "left9"))

(provide 'yunt-cedet)

;;; yunt-cedet.el ends here
