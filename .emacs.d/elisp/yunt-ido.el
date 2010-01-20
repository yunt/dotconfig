;;; yunt-ido.el --- ido模式的配置
;; emacs 22，23都内含
(require 'ido)
;;; Code:
(ido-mode t)
(add-hook 'ido-define-mode-map-hook 'ido-yunt-keys)
(defun ido-yunt-keys ()
  "Set up the keymap for `ido'."
  ;; 基本配置
  (define-key ido-mode-map "C-e" 'ido-edit-input);;ido编辑模式
  ;(define-key ido-mode-map "t" 'ido-complete) ;; 部分补全 complete partial
  (define-key ido-mode-map "C-c" 'ido-complete)
  (define-key ido-mode-map "C-j" 'ido-select-text)
  (define-key ido-mode-map "C-m" 'ido-exit-minibuffer);;推出minibuffer
  (define-key ido-mode-map "?" 'ido-completion-help) ;; 列出符合的
  ;;逐步筛选你需要的文件
  (define-key ido-mode-map [(control ? )] 'ido-restrict-to-matches)
  (define-key ido-mode-map [(control ?@)] 'ido-restrict-to-matches)
  ;; 在符合条件的循环
  (define-key ido-mode-map "C-r" 'ido-prev-match)
  (define-key ido-mode-map "C-s" 'ido-next-match)
  (define-key ido-mode-map [right] 'ido-next-match)
  (define-key ido-mode-map [left] 'ido-prev-match)
  ;; 切换
  (define-key ido-mode-map "C-t" 'ido-toggle-regexp) ;;正则匹配
  (define-key ido-mode-map "C-p" 'ido-toggle-prefix) ;;部分匹配
  (define-key ido-mode-map "C-c" 'ido-toggle-case)   ;;切换大小写匹配
  (define-key ido-mode-map "C-a" 'ido-toggle-ignore) ;;忽略某些文件
  ;; 在文件和目录环境中的快捷键
  (when (memq ido-cur-item '(file dir))
    (define-key ido-mode-map "C-b" 'ido-enter-switch-buffer);;选择buffer模式
    (define-key ido-mode-map "C-d" 'ido-enter-dired) ;;Dired模式
    (define-key ido-mode-map "C-f" 'ido-fallback-command)
    ;; 目录里的循环
    ;; 使用[left]和[right]匹配文件
    (define-key ido-mode-map [down] 'ido-next-match-dir)
    (define-key ido-mode-map [up]   'ido-prev-match-dir)
    ;; backspace删除键的配置
    (define-key ido-mode-map [backspace] 'ido-delete-backward-updir)
    ;(define-key ido-mode-map "d"        'ido-delete-backward-updir)
    (define-key ido-mode-map [(meta backspace)] 'ido-delete-backward-word-updir)
    (define-key ido-mode-map [(control backspace)] 'ido-up-directory)
    ;; 搞不懂这些配置
    (define-key ido-mode-map [(meta ?d)] 'ido-wide-find-dir)
    (define-key ido-mode-map [(meta ?f)] 'ido-wide-find-file)
    (define-key ido-mode-map [(meta ?k)] 'ido-forget-work-directory)
    (define-key ido-mode-map [(meta ?m)] 'ido-make-directory)
    (define-key ido-mode-map [(meta down)] 'ido-next-work-directory)
    (define-key ido-mode-map [(meta up)] 'ido-prev-work-directory)
    (define-key ido-mode-map [(meta left)] 'ido-prev-work-file)
    (define-key ido-mode-map [(meta right)] 'ido-next-work-file)
    ;; 在directories目录中的搜索
    ;; 使用C-_来undo
    (define-key ido-mode-map [(meta ?s)] 'ido-merge-work-directories)
    (define-key ido-mode-map [(control ?\_)] 'ido-undo-merge-work-directory)
    )
  (when (eq ido-cur-item 'file)
    (define-key ido-mode-map "C-k" 'ido-delete-file-at-head)
    (define-key ido-mode-map "C-l" 'ido-toggle-literal)
    (define-key ido-mode-map "C-o" 'ido-copy-current-word)
    (define-key ido-mode-map "C-v" 'ido-toggle-vc)
    (define-key ido-mode-map "C-w" 'ido-copy-current-file-name)
    )

  (when (eq ido-cur-item 'buffer)
    (define-key ido-mode-map "C-b" 'ido-fallback-command)
    (define-key ido-mode-map "C-f" 'ido-enter-find-file)
    (define-key ido-mode-map "C-k" 'ido-kill-buffer-at-head)
    ))

(provide 'yunt-ido)

;;; yunt-ido.el ends here
