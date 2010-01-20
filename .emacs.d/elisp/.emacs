;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yunt's dotemacs file
;;; Last modified time
;;; Time-stamp: <yunt 07/15/2008 22:21:56>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;以上是我得time stamp，在后面将有详细讲解
;;设置你的全名和邮件，在发邮件时可以用到
(setq user-full-name "Yunt")
(setq user-mail-address "shangyunt@gmail.com")
;;设置你的书签文件，默认是~/.emacs.bmk，我喜欢把有关emacs的文件尽量放在一个文件夹，所以就修改了。
(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
;;设置缩略词的文件
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;;load-path就同bash中的$PATH相似，emacs所需要的Elisp包都得在load-path里的文件夹中
(setq load-path (cons "~/.emacs.d/elisp" load-path))
;;设置info的路径，也可通过Shell的全局变量$INFOPATH设置，我原来的22因为是自己编译的所以这里就注释了
;(add-to-list 'Info-default-directory-list "~/local/info/")
;;由菜单修改配置的东西将会保存在custom-file里，这里我设置他在我的elisp的集中营里
;(setq custom-file "~/.emacs.d/elisp/yunt-custom.el")
;;设置gnus启动的文件。默认是为~/.gnus.el
(setq gnus-init-file "~/.emacs.d/elisp/yunt-gnus.el")
;;由于我的配置文件很长，所以按照分类分别放在不同的文件里，方便管理
(load "yunt-basic")
(load "yunt-viper")
(load "yunt-language")
;(load "yunt-calendar")
(load "yunt-folding")
(load "yunt-ibuffer")
(load "yunt-ido")
;(load "yunt-dictionary")
(load "yunt-function")
;(load "yunt-mew")
;(load "yunt-w3m")
(load "yunt-cedet")
;(load "yunt-org")
;(load "yunt-dired")
;(load "yunt-mode")
;(load "yunt-wiki")
(load "yunt-other-elisp")
;(load "yunt-key-bindings")

;;这个东西必须放在最后
;;desktop.el是一个可以保存你上次emacs关闭时的状态，下一次启动时恢复为上次关闭的状态。就和vmware的suspend一样。
;;因为我要使用sawfish-mode,wiki-mode,html-helper-mode，放在这里才能保证下次启动时能正确辨认文件需要的模式。
(load "desktop")
(desktop-save-mode)
;;(desktop-read)
