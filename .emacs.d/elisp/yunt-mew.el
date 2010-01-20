;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Last modified time
;;; Time-stamp: <yunt 07/15/2008 22:21:56>
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; start yunt-mew.el ---
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(if (boundp 'read-mail-command)
(setq read-mail-command 'mew))
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
(setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
(define-mail-user-agent
'mew-user-agent
'mew-user-agent-compose
'mew-draft-send-message
'mew-draft-kill
'mew-send-hook))

(set-default 'mew-decode-quoted 't)
(when (boundp 'utf-translate-cjk)
(setq utf-translate-cjk t)
(custom-set-variables
'(utf-translate-cjk t)))
(if (fboundp 'utf-translate-cjk-mode)
(utf-translate-cjk-mode 1))
;(require 'flyspell)

(provide 'yunt-mew)

;;; yunt-mew.el ends here
