;;; yunt-dired.el --- dired的配置
;;; Code:
;;设定显示文件的参数，以版本/人性化的显示 就是ls的参数
(setq dired-listing-switches "-vhl")
;; NB C-u s 就可以编辑 dired 的 dired-listing-switches
;;    这个变量，从而达到控制排序的方法的目的。
;;允许复制和删除时将文件夹里所有内容一起带上
(setq dired-recursive-copies t)
(setq dired-recursive-deletes t)
;;cvs时的一个浏览设置
(setq cvs-dired-use-hook 'always)
;;主要可以详细设置的地方
;;对于特定的文件，用什么程序打开
;;记住是用！，而不是enter键，enter键表示编辑，同v
;;记住在后面带上&，为后台运行，我们还要用Emacs做别的事情呢
;;下面的文件用的是正则表达式，要表达清楚
(add-hook 'dired-load-hook
          (lambda ()
       ;;记住这里要加载另一个有关文件管理器的包dired-x.el，这个是dired的扩展，非常强大
       (load "dired-x")
       (setq dired-guess-shell-alist-user
        (list
         (list "\\.tar\\.bz2$" "tar jxvf *   &")
         '("\\.tar\\.gz$" "tar zxvf *   &")
         '("\\.chm$" "chmsee *  &")
         '("\\.tar$" "tar xvvf *  &")
         '("\\.ps$" "gv *   &")
         '("\\.html?$" "swiftweasel * &" "firefox *   &" "urxvt -e w3m *   &")
         '("\\.pdf$"  "apvlv * &" "acroread *  &" "evince *   &")
         '("\\.\\(jpe?g\\|gif\\|png\\|bmp\\|xbm\\|xpm\\|fig\\|eps\\)$" "feh * &" "gthumb * &" "gqview *  &" "display *   &" "xloadimage *   &" )
         '("\\.\\([Ww][Mm][Vv]\\|[Vv][Oo][Bb]\\|[Mm][Pp][Ee]?[Gg]\\|asf\\|[Rr][Aa]?[Mm]\\|[Mm]4[Vv]\\)$" "mplayer -quiet *   &")
         '("\\.rmvb$" "mplayer * &")
         '("\\.RMVB$" "mplayer * &")
         ))
       ;; 可以使用 M-o 来方便地切换忽略与显示。
       ;; 使用dired-x,忽略所有以.开头的文件,隐藏文件
       ;; omit all hidden file which starts with '
       ;; initially omit unintrested files
       (setq dired-omit-files "^#\\|^\\..*")
            (dired-omit-mode 1)
       ;;加载dired+增强模式
       ;(require 'dired+)
       ;; NB 使用图形化的sort menu
       (require 'dired-sort-menu+)
       ;; NB  让dired只使用一个buffer
       (require 'dired-single)
       ;; NB 能够在 dired 里面使用只对文件名部分执行 i-search
       (require 'dired-isearch)
       ;; 输入文件名的首字母快速定位到文件
       ;;他是重新定义了所有的字母以及数字键说绑定的函数，所以如果文件名
       ;;是中文的话也没有办法，而且按键会和 dired 本身的很多按键冲突
       (require 'dired-view)
       (define-key dired-mode-map (kbd "RET") 'joc-dired-single-buffer)
            (define-key dired-mode-map (kbd "<mouse-1>") 'joc-dired-single-buffer-mouse)

       (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
       (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
       (define-key dired-mode-map (kbd "M-C-s") 'dired-isearch-forward-regexp)
       (define-key dired-mode-map (kbd "M-C-r") 'dired-isearch-backward-regexp)
       (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
       (define-key dired-mode-map (kbd "^")
              (lambda ()
                (interactive)
                (joc-dired-single-buffer "..")))
            (setq joc-dired-use-magic-buffer t)
            (setq joc-dired-magic-buffer-name "*dired*")
       ))

(provide 'yunt-dired)

;;; yunt-dired.el ends here
