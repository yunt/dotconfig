;;; yunt-basic.el ---

(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
;;我还是使用color-theme.el修改颜色吧,天天看着电脑,视力下降的厉害啊
;;使用M-x color-theme-select就可以选择配色方案，在配色方案上按I就
;;可以改变当前frame的配色，按i就可以改变所有frame的配色
;;按p就可以把当前配色方案的lisp打印出来，加入你的.emacs，就可以不用加
;;载color-theme了，这样可以加快起动速度
(require 'color-theme);;不用了,真慢，如果上网的话，这个就很慢了，所以我注释掉了
;(load-file "/home/yunt/lib/emacs-lisp/color-theme-6.6.0/color-theme.el")
(color-theme-initialize)
;;(color-theme-tty-dark)
(color-theme-comidia)
;(color-theme-dark-laptop)
;(color-theme-arjen)
;(color-theme-euphoria);;颜色很好
;(color-theme-kingsajz)
;(color-theme-cooper-dark)
;(color-theme-oswald)
;(color-theme-hober)
;( my-color-theme t )
;(color-theme-pok-wob)
;(color-theme-whateveryouwant);;字体设置的很好
(require 'ahei-face)

;;中文字体设定－这里是一部分，还有.Xresources中也有
;; 我是近视眼，喜欢大字体。这个pixelsize大多数人是设成13吧。
;;(set-default-font "Monaco:pixelsize=13")
(set-default-font "DejaVu Sans Mono:pixelsize=14")
  ;; 中文字体的设定，网上很多资料都是gb18030，但我的locale是UTF-8
(set-fontset-font (frame-parameter nil 'font)
  'unicode '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))

;;外观设置
;;去掉工具栏
(tool-bar-mode nil)
;;我将F10绑定为显示菜单栏，万一什么东西忘了
;;需要菜单栏了可以摁F10调出，再摁F10就去掉菜单
(menu-bar-mode nil)
;;不要滚动栏，现在都用滚轴鼠标了，可以不用滚动栏了
(scroll-bar-mode nil)

;;修改中文文本的行距,3个象素就可以了吧
(setq-default line-spacing 2)
;;启用C-x,C-v,C-s这些通用设置
;(cua-mode t)

;;备份设置
;;emacs还有一个自动保存功能，默认在~/.emacs.d/auto-save-list里，这个非常有用，我
;;这里没有改动，具体可以参见Sams teach yourself emacs in 24hours(我简称为sams24)
;;启用版本控制，即可以备份多次
(setq version-control t)
;;备份最原始的版本两次，记第一次编辑前的文档，和第二次编辑前的文档
(setq kept-old-versions 2)
;;备份最新的版本十次，理解同上
(setq kept-new-versions 10)
;;删掉不属于以上12中版本的版本
(setq delete-old-versions t)
;;设置备份文件的路径
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
;;备份设置方法，直接拷贝
(setq backup-by-copying t)
(setq make-backup-file t)

;;自动补全功能，从王垠的网站直接Copy过来的，引用一些他对此的说明
;;你可以设置以下 hippie-expand 的补全方式。它是一个优先列表， hippie-expand 会优
;;先使用表最前面的函数来补全这是说，首先使用当前的buffer补全，如果找不到，就到别的可见
;;的窗口里寻找，如果还找不到，那么到所有打开的buffer去找，如果还那么到kill-ring
;;里，到文件名，到简称列表里，到list, 当前使用的匹配方式会在 echo 区域显示。
;;特别有意思的是 try-expand-line，它可以帮你补全整整一行文字。我很多时后有两行文字大致
;;相同，只有几个字不一样，但是我懒得去拷贝粘贴以下。那么我就输入这行文字的前面几个字。然后
;;多按几下 M-/ 就能得到那一行。
;;;;自动补齐策略
(defun my-indent-or-complete ()
   (interactive)
   (if (looking-at "\\>")
          (hippie-expand nil)
          (indent-for-tab-command))
)
(global-set-key [(control tab)] 'my-indent-or-complete)
(autoload 'senator-try-expand-semantic "senator")
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
  '(
  senator-try-expand-semantic ;优先调用了senator的分析结果-很慢-还是使用吧
  try-expand-line ; 补全当前行
  try-expand-line-all-buffers
  try-expand-list ; 补全一个列表
  try-expand-list-all-buffers
  try-expand-dabbrev ; 搜索当前 buffer
  try-expand-dabbrev-visible ; 搜索当前可见窗口
  try-expand-dabbrev-all-buffers ; 搜索所有 buffer
  try-expand-dabbrev-from-kill ; 从 kill-ring 中搜索
  try-complete-file-name ; 文件名匹配
  try-complete-file-name-partially ; 文件名部分匹配
  try-complete-lisp-symbol ; 补全 lisp symbol
  try-complete-lisp-symbol-partially ; 部分补全 elisp symbol
  try-expand-whole-kill
  ))

;;时间戳设置(time-stamp)，设定文档上次保存的信息
;;只要里在你得文档里有Time-stamp:的设置，就会自动保存时间戳
;;启用time-stamp
(setq time-stamp-active t)
;;去掉time-stamp的警告？
(setq time-stamp-warn-inactive t)
;;设置time-stamp的格式，我如下的格式所得的一个例子：
(setq time-stamp-format "%:u %02m/%02d/%04y %02H:%02M:%02S")
;;将修改时间戳添加到保存文件的动作里。
(add-hook 'write-file-hooks 'time-stamp)

;;时间显示设置
;;启用时间显示设置，在minibuffer上面的那个杠上
(display-time-mode 1)
;;时间使用24小时制
(setq display-time-24hr-format t)
;;时间显示包括日期和具体时间
(setq display-time-day-and-date t)
;;时间栏旁边启用邮件设置
(setq display-time-use-mail-icon t)
;;时间的变化频率
(setq display-time-interval 10)
;;显示时间的格式
(setq display-time-format "%m月%d日 %A %H:%M")

;;将默认模式从fundemental-mode改为text-mode
(setq default-major-mode 'text-mode)
;;启用minibuffer，好像是默认设置吧
(minibuffer-electric-default-mode 1)
;;启用部分补全功能，如输入M-x q r r相当于M-x query-replace-regexp
(partial-completion-mode 1)
;;在minibuffer里启用自动补全函数和变量
(icomplete-mode 1)
;;所有的问题用y/n方式，不用yes/no方式。有点懒，只想输入一个字母
(fset 'yes-or-no-p 'y-or-n-p)
;;允许minibuffer自由变化其大小（指宽度）
(setq resize-mini-windows t)
;;当寻找一个同名的文件，改变两个buffer的名字,前面加上目录名
(setq uniquify-buffer-name-style 'forward)
;;在emacs读man文档时，使用当前buffer
(setq Man-notify-method 'pushy)
;;鼠标自动避开指针，如当你输入的时候，指针到了鼠标的位置，鼠标有点挡住视线了
;;不喜欢这种方式
(mouse-avoidance-mode 'animate)
;;允许自动打开图片，如wiki里面
(auto-image-file-mode t)
;;可以操作压缩文档
(auto-compression-mode 1)
;;在minibuffer上面可以显示列号,列号
(column-number-mode t)
(line-number-mode t)
;;显示默认的文档的宽度，看起来比较舒服？
;;latex90分钟介绍里说66是最大限度,看来不错.
;(setq default-fill-column 60)
;; 缩进设置
;不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候也
;不用担心，因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的。
(require 'cl)
;; 不用TAB字符来indent
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list nil)
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x tab-width) tab-stop-list)))
;;(setq default-tab-width 4)
;; (setq tab-stop-list ())
;; (loop for x downfrom 40 to 1 do
;; (setq tab-stop-list (cons (* x 4) tab-stop-list)))
;;指针不要闪，我得眼睛花了
(blink-cursor-mode -1)
(transient-mark-mode 1)
;;当指针到一个括号时，自动显示所匹配的另一个括号
(show-paren-mode 1)
;;是用滚轴鼠标
(mouse-wheel-mode t)
;; 没有提示音,也不闪屏
(setq ring-bell-function 'ignore)
;;去掉烦人的警告铃声
;(setq visible-bell nil)
;;滚动页面时比较舒服，不要整页的滚动
(setq scroll-step 1
  scroll-margin 3
  scroll-conservatively 10000)
;;设定句子结尾，主要是针对中文设置
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;;去掉Emacs和gnus启动时的引导界面
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)
;;当指针移到另一行，不要新增这一行？
(setq next-line-add-newlines nil)
;;在文档最后自动插入空白一行，好像某些系统配置文件是需要这样的
(setq require-final-newline t)
(setq track-eol t)
;;使用C-k删掉指针到改行末的所有东西
(setq-default kill-whole-line t)
;;设定删除保存记录为200，可以方便以后无限恢复
(setq kill-ring-max 200)
;;增大使用查找函数和变量的寻找范围
(setq apropos-do-all t)
;;使用aspell程序作为Emacs的拼写检查程序
;;没有aspell的字典，还是用ispell吧
(setq-default ispell-program-name "ispell")
;;启动Emacs自动设置为两个窗口(上下各一个)
;(split-window-vertically)
;;改变emacs标题栏的标题,显示buffer的名字
(setq frame-title-format "emacs-snapshot@%b")
;;允许emacs和外部其他程序的粘贴
(setq x-select-enable-clipboard t)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
;;启动Emacs Server,然后用emacsclient起动emacs
;;加快emacs的起动速度
(server-start)
;;把这些缺省禁用的功能打开。
(put 'set-goal-column 'disabled nil)
;;使用narrow功能时的一个设置
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'narrow-to-page 'disabled nil) ; C-x n p
(put 'narrow-to-defun 'disabled nil) ; C-x n d

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'LaTeX-hide-environment 'disabled nil)
;;向左右滚动的命令激活：C-x < 和 C-x >
(put 'scroll-left 'disabled nil)
;;可以递归的使用 minibuffer。
(setq enable-recursive-minibuffers t)
;;Save bookmarks file each time a bookmark is added, not just on exit.
(setq bookmark-save-flag 1)
;;使用GTK风格的toolbar
(setq icon-map-list '(x-gtk-stock-map))
;;使用旧的dialog风格,不使用gtk的
;(setq x-gtk-use-old-file-dialog t)
;;自动重载更改的文件
(global-auto-revert-mode 1)
;;custome的风格改为单一的树状风格
(setq custom-buffer-style 'brackets)
;;发送mail的时候使用fortune添加amuses
;(add-hook 'mail-setup-hook 'spook)
;;当鼠标移动的时候自动转换frame，window或者minibuffer
(setq mouse-autoselect-window t)
(autoload 'list-processes+ "list-processes+" "增强的ist-processes'命令" t)
;;天气预报
(require 'cn-weather)
(setq cn-weather-city "洛宁")
;; 统计命令使用频率
(require 'command-frequence)
;; 显示行号
(load "displn-mode-settings")
;(load "maxframe-settings")

(provide 'yunt-basic)

;;; yunt-basic.el ends here
