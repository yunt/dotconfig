;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-21 11:34:09 Monday by ahei>

(require 'cc-mode)
(require 'misc)
(require 'compile-settings)

(defun forward-word-or-to-word ()
  "如果`forward-to-word'后移到下一行, 就`forward-word'"
  (interactive)
    (let ((noo (line-number-at-pos)) no)
      (save-excursion
        (forward-to-word 1)
        (setq no (line-number-at-pos)))
      (if (> no noo)
          (forward-word)
        (forward-to-word 1))))

(defun backward-kill-word-or-kill-region ()
  "`mark-active'时, 剪切选择的区域, 平时向后删除word, 和bash下面一样."
  (interactive)
  (if rm-mark-active
      (call-interactively 'rm-kill-region)
    (if mark-active
        (if cua--rectangle
            (progn
              (cua-cut-rectangle t)
              (cua-cancel))
          (call-interactively 'kill-region))
      (call-interactively 'backward-kill-word))))

(defun mark-whole-sexp ()
  "Mark whole sexp."
  (interactive)
  (let ((region (bounds-of-thing-at-point 'sexp)))
    (if (not region)
        (message "Can not found sexp.")
      (goto-char (car region))
      (call-interactively 'set-mark-command)
      (forward-sexp))))
(defun kill-sexp ()
  "删除一个sexp"
  (interactive)
  (mark-whole-sexp)
  (backward-kill-word-or-kill-region))
(defun copy-sexp ()
  "拷贝一个sexp"
  (interactive)
  (save-excursion
    (mark-whole-sexp)
    (if mark-active
        (copy-region (region-beginning) (region-end)))))
(defun my-kill-word ()
  "删除一个单词, 当光标处于单词中间时也删除整个单词, 这是与`kill-word'的区别"
  (interactive)
  (wcy-mark-some-thing-at-point)
  (backward-kill-word-or-kill-region))
(defun mark-function ()
  "Mark function."
  (interactive)
  (cond
   ((or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))
    (c-mark-function))
   ((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-mode) (equal major-mode 'lisp-interaction-mode))
    (lisp-mark-function))))
(defmacro def-action-on-function-command (fun-name action action-str)
  `(defun ,fun-name ()
     ,(concat (capitalize action-str) " function.")
     (interactive)
     (save-excursion
       (mark-function)
       (call-interactively ,action))))
(defmacro def-action-on-area-command (fun-name action mark-area doc)
  `(defun ,fun-name ()
     ,doc
     (interactive)
     (save-excursion
       (funcall ,mark-area)
       (call-interactively ,action))))
(defun comment-function (&optional arg)
  "Comment function."
  (interactive "P")
  (save-excursion
    (mark-function)
    (comment-region (region-beginning) (region-end) arg)))

(apply-args-list-to-fun
 'def-action-on-area-command
  `((copy-function        'copy-region   'mark-function     "Copy function.")
    (kill-function        'kill-region   'mark-function     "Kill function.")
    (indent-function      'indent-region 'mark-function     "Indent function.")
    (indent-paragraph     'indent-region 'mark-paragraph    "Indent paragraph.")
    (copy-whole-buffer    'copy-region   'mark-whole-buffer "Copy whole buffer.")
    (kill-whole-buffer    'kill-region   'mark-whole-buffer "Kill whole buffer.")
    (indent-whole-buffer  'indent-region 'mark-whole-buffer "Indent whole buffer.")))

(defun kill-whole-paragraph (&optional arg)
  "Kill whole paragraph."
  (interactive "P")
  (if arg
      (kill-paragraph nil)
    (call-interactively 'mark-paragraph)
    (call-interactively 'kill-region)))

(defun copy-whole-paragraph (&optional arg)
  "Copy whole paragraph."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (mark-command t)
          (forward-paragraph))
      (call-interactively 'mark-paragraph))
    (call-interactively 'copy-region)))

(defun copy-cur-line ()
  "拷贝当前行"
  (interactive)
  (let ((end (min (point-max) (1+ (line-end-position)))))
    (copy-region-as-kill-nomark (line-beginning-position) end)))
(defun copy-lines (&optional number)
  "从当前行开始拷贝NUMBER行"
  (interactive "p")
  (if (null number)
      (copy-cur-line)
    (let ((lineNo))
      (save-excursion
        (if (< number 0)
            (next-line))
        (setq lineNo (line-number-at-pos nil))
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (goto-line (+ number lineNo))
        (call-interactively 'copy-region-as-kill-nomark)))))

(defun copy-line-left ()
  "拷贝当前行光标后面的文字"
  (interactive)
  (copy-region-as-kill-nomark (point) (1+ (line-end-position))))
(defun smart-copy ()
  "智能拷贝, 如果`mark-active'的话, 则`copy-region', 否则`copy-lines'"
  (interactive)
  (if mark-active (call-interactively 'copy-region) (call-interactively 'copy-lines)))
(defun copy-region-and-paste ()
  "拷贝region并且粘贴到region后"
  (interactive)
  (call-interactively 'copy-region)
  (call-interactively 'yank))
(defun which-copy ()
  "如果`mark-active'的话, 则`copy-region-and-paste', 否则`copy-line-left'"
  (interactive)
  (if mark-active (copy-region-and-paste) (copy-line-left)))
(defun insert-cur-line ()
  "拷贝当前行并粘贴进当前buffer"
  (interactive)
  (copy-cur-line)
  (forward-line)
  (beginning-of-line)
  (call-interactively 'yank)
  (previous-line)
  (end-of-line))
(defun insert-cur-sexp ()
  "拷贝当前sexp并粘贴进当前buffer"
  (interactive)
  (copy-sexp)
  (call-interactively 'yank))
(defun copy-sentence ()
  "拷贝sentence"
  (interactive)
  (save-excursion
    (call-interactively 'mark-end-of-sentence)
    (call-interactively 'copy-region-as-kill-nomark)))

;; 删除当前光标到行首的字符
(defun del-to-begin (&optional arg)
  (interactive "P")
  (if (not arg)
      (kill-line 0)
    (copy-region-as-kill-nomark (1+ (line-beginning-position)) (point))))

(defun lisp-mark-function (&optional allow-extend)
  "`mark-defun'有时候会多mark一个空白行, 这个函数就是解决这个bug的"
  (interactive "p")
  (mark-defun allow-extend)
  (let (next-is-fun)
    (save-excursion (forward-line) (setq next-is-fun (looking-at "[ \t]*(defun")))
    (if (or (looking-at "$") (and next-is-fun (not (looking-at "[ \t]*(defun"))))
      (forward-line))))

(defun case-trans ()
  "大小写转换当前字符"
  (interactive)
  (let* ((ochar (char-after (point))) (char ochar))
    (if (and (>= char ?a) (<= char ?z))
        (setq char (upcase char))
      (setq char (downcase char)))
    (if (/= ochar char)
        (save-excursion
          (delete-char 1)
          (insert-char char 1)))))

(defun comment (&optional arg)
  "如果`mark-active'的话,就`comment-region',否则注释光标所在行"
  (interactive "P")
  (if mark-active
      (comment-region (region-beginning) (region-end) arg)
    (let (fun)
      (if arg (setq fun 'uncomment-region) (setq fun 'comment-region))
      (funcall fun (line-beginning-position) (line-end-position)))))
(defun uncomment (&optional arg)
  "如果`mark-active'的话,就`uncomment-region',否则取消注释光标所在行"
  (interactive "P")
  (comment (not arg)))

(dolist (map (append (list c-mode-base-map emacs-lisp-mode-map lisp-interaction-mode-map) makefile-mode-map-list))
  (define-key map "\C-c\C-c" 'comment))

(defmacro def-redo-command (fun-name redo undo)
  "Make redo command."
  `(defun ,fun-name ()
     (interactive)
     (if (equal last-command ,redo)
         (setq last-command 'undo)
       (setq last-command nil))
     (call-interactively ,undo)
     (setq this-command ,redo)))
(def-redo-command redo 'redo 'undo)

(defun mark-invisible-region ()
  "Mark invisible region."
  (interactive)
  (if (not (and last-region-beg last-region-end))
      (message "No previous region.")
    (goto-char last-region-beg)
    (if last-region-is-rect
        (if last-region-use-cua
            (call-interactively 'cua-set-rectangle-mark)
          (call-interactively 'rm-set-mark))
      (call-interactively 'set-mark-command))
    (goto-char last-region-end)
    (if (and last-region-is-rect last-region-use-cua)
        (cua--activate-rectangle))))

(apply-define-key
 global-map
 `(("M-k" kill-whole-paragraph)
   ("M-C-k" kill-paragraph)
   ("M-C" copy-whole-paragraph)
   ("C-x c" copy-whole-buffer)
   ("C-x C" kill-whole-buffer)
   ("M-W" which-copy)
   ("M-w" smart-copy)
   ("C-x M-w" insert-cur-line)
   ("C-x M-W" insert-cur-sexp)
   ("C-M-w" copy-sentence)
   ;; 删除整行
   ("M-K" kill-line)
   ("C-k" kill-whole-line)
   ("C-\\" delete-indentation)
   ("C-x M-M" mark-invisible-region)
   ("M-U" del-to-begin)
   ("C-^" case-trans)
   ("C-w" backward-kill-word-or-kill-region)
   ("C-x S" mark-whole-sexp)
   ("C-x W" kill-sexp)
   ("C-x w" copy-sexp)
   ("M-D" my-kill-word)
   ("C-x TAB" indent-whole-buffer)
   ("C-h" c-electric-backspace-kill)
   ,(if window-system '("C-z" undo))
   ("M-Y" redo)))

(apply-define-key
 c-mode-base-map
  `(("C-c f" copy-function)
    ("C-c F" kill-function)
    ("C-c C" comment-function)
    ("C-M-h" mark-function)))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (apply-define-key
   map
   `(("C-M-h" mark-function)
     ("C-c D"  edebug-defun)
     ("C-c C-d" eval-defun)
     ("C-c B"  eval-buffer)
     ("C-c f" copy-function)
     ("C-c F" kill-function)
     ("C-c C-q" indent-function)
     ("C-c C" comment-function))))

(defun c-electric-backspace-kill ()
  "If `mark-active', run `kill-region', otherwise run `c-electric-backspace'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'c-electric-backspace)))

(defun delete-blank-lines-region (beg end)
  "Execute `delete-blank-lines' in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (let ((blank-line "^\\s-*$")
          (nonblank-line "^.*\\S-.*$")
          blank-beg blank-end)
      (while (and (< (point) end) (setq blank-beg (search-forward-regexp blank-line end t)))
        (save-excursion
          (setq blank-end (search-forward-regexp nonblank-line end t)))
        (if blank-end
            (setq end (- end (- blank-end blank-beg)))
          (setq end 0))
        (previous-line)
        (delete-blank-lines)))))

(defun smart-delete-blank-lines (&optional no-region)
  "Smart `delete-blank-lines'.

If NO-REGION is non-nil, always execute `delete-blank-lines',
otherwise, if `mark-active', execute `delete-blank-lines-region',
and execute `delete-blank-lines' if there no mark."
  (interactive "P")
  (if (or no-region (not mark-active))
      (delete-blank-lines)
    (call-interactively 'delete-blank-lines-region)))
