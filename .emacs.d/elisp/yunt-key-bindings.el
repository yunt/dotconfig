;;; yunt-key-bindings.el ---

;;传说中可以提高效率的按键
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;; 按下C-x k立即关闭掉当前的buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "M-]") 'comment-dwim)
(global-set-key [delete] 'delete-region)
(global-set-key "%" 'match-paren)

;; 这个命令配合 comment-dwim 基本上能满足所有的注释命令
(global-set-key (kbd "C-c g") 'comment-or-uncomment-region)

(provide 'yunt-key-bindings.el)

;;yunt-key-bindings.el ends here
