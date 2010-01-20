;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-23 22:05:58 Monday by ahei>

(require 'wuxch-dired "my-wuxch-dired")
(require 'wuxch-dired-copy-paste "my-wuxch-dired-copy-paste")

(require 'dired-details+)
(setq dired-details-initially-hide nil)

(load "dired+-settings")

(define-key global-map (kbd "C-x d") 'dired-jump)

(defun his-dired-sort ()
  "dired-mode中让目录显示在文件前"
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'his-dired-sort)
(add-hook 'dired-lood-hook 'his-dired-sort)

;; dired中用T就把一个目录压缩为一个.tar.gz文件
(require 'dired-tar)

(defun ywb-dired-filter-regexp (regexp &optional arg)
  "dired mode中只显示后缀名符合正则表达式的文件和目录"
  (interactive
   (list (dired-read-regexp
          (concat (if current-prefix-arg "只显示不" "只显示") "匹配的后缀名(regexp): "))
         current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (or arg (if (fboundp 'dired-do-toggle) (dired-do-toggle) (dired-toggle-marks)))
  (dired-do-kill-lines))

(defun ywb-dired-filter-extension (extension &optional arg)
  "dired mode中只显示后缀名为EXTENSION的文件和目录"
  (interactive
   (list (read-from-minibuffer
          (concat "只显示后缀名为" (if current-prefix-arg "" "") ": "))
         current-prefix-arg))
  (ywb-dired-filter-regexp (concat "\\." extension "\\'") arg))

(defvar ywb-dired-quickview-buffer nil)
(defun ywb-dired-quickview ()
  "类似TC的一个命令,可以使用同一个buffer浏览多个文件，每次打开新文件就把前一个buffer关了"
  (interactive)
  (if (buffer-live-p ywb-dired-quickview-buffer)
      (kill-buffer ywb-dired-quickview-buffer))
  (setq ywb-dired-quickview-buffer
        (find-file-noselect (dired-get-file-for-visit)))
  (display-buffer ywb-dired-quickview-buffer))

;; wdired提供修改文件名的一种非常方便方法。它把dired-mode当作一般的
;; 文本处理，这样无论是修改一个文件，还是批量修改文件都不是一般的爽。
(if is-before-emacs-21 (load "wdired-for-21"))

;; 让你能够在dired-mode里面使用只对文件名部分执行i-search
(require 'dired-isearch "my-dired-isearch")

(defun dired-up-directory-same-buffer ()
  "Goto parent directory in the smae buffer."
  (interactive)
  (let* ((dir (dired-current-directory))
         (dir-file-name (directory-file-name dir)))
    (unless (string= dir dir-file-name)
      (find-alternate-file "..")
      (dired-goto-file dir-file-name))))

(defun dired-keys ()
  "dired-mode中的快捷键定义"
  (define-prefix-command 'dired-slash-map)
  (apply-define-key
   dired-mode-map
   `(("C-h"         dired-up-directory-same-buffer)
     ("<backspace>" dired-up-directory-same-buffer)
     ("'"           switch-to-other-buffer)
     ("/"           dired-slash-map)
     ("/m"          ywb-dired-filter-regexp)
     ("/."          ywb-dired-filter-extension)
     ("C-q"         ywb-dired-quickview)
     ("r"           wdired-change-to-wdired-mode)
     ("C-s"         dired-lis-isearch-forward-always?)
     ("C-r"         dired-lis-isearch-backward-always?)
     ("ESC C-s"     dired-lis-isearch-forward-regexp-always?)
     ("ESC C-r"     dired-lis-isearch-backward-regexp-always?)
     ;; 让dired只使用一个buffer
     ("RET"         diredp-find-file-reuse-dir-buffer)
     ("<return>"    diredp-find-file-reuse-dir-buffer)
     ("M"           wuxch-mark-all-files-directories)
     ("g"           revert-buffer)
     ("M-o"         dired-omit-mode)
     ("M-Y"         dired-redo)
     ("SPC"         scroll-up)
     ("C-k"         dired-do-delete)
     
     ("C-c C-m"     make-sb)
     ("C-c m"       make-check-sb)
     ("C-c M"       make-clean-sb)
     ("C-c c"       compile-buffer-sb)
     ("C-c r"       run-program-sb)
     ("C-c C"       smart-compile-sb)
     ("C-c g"       gdb)
     ("C-c b"       gud-break)
     ("C-c B"       gud-remove))))
(defun dired-settings ()
  "Settings for `dired-mode'."
  (dired-keys)
  (setq mode-line-buffer-identification (propertized-buffer-identification "%b")))
(add-hook 'dired-mode-hook 'dired-settings)

(load "dired-x-settings")

(defun dired-sort-by-size ()
  "sort by Size"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))
(defun dired-sort-by-extension ()
  "sort by eXtension"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))
(defun dired-sort-by-time ()
  "sort by Time"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))
(defun dired-sort-by-name ()
  "sort by Name"
  (interactive)
  (dired-sort-other dired-listing-switches))

(load "dired-lis-settings")

(def-redo-command dired-redo 'dired-redo 'dired-undo)
