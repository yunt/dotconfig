;;; yunt-other-elisp.el ---

;;;;; Twitter on Emacs ;;;;;;
;; Define M-x commands
(autoload 'twit-show-recent-tweets	"twit" "" t) ; most recent direct tweets (!)
(autoload 'twit-show-at-tweets		"twit" "" t) ; directed to you
(autoload 'twit-show-friends 		"twit" "" t) ; your friends
(autoload 'twit-show-followers 		"twit" "" t) ; your followers

(autoload 'twit-follow-recent-tweets	"twit" "" t) ; at idle, check at background

(autoload 'twit-post			"twit" "" t)
(autoload 'twit-post-region		"twit" "" t)
(autoload 'twit-post-buffer		"twit" "" t)
(autoload 'twit-direct			"twit" "" t) ; tweet to person

(autoload 'twit-add-favorite		"twit" "" t) ; Add to favourite: (*) star
(autoload 'twit-remove-favorite 	"twit" "" t)

(autoload 'twit-add-friend  		"twit" "" t) ; follow a friend
(autoload 'twit-remove-friend 		"twit" "" t) ; emove a frienda

;; Customize twit-multi-accounts in order to use these: ((user . pass) ...)
(autoload 'twit-switch-account 		"twit" "" t)
(autoload 'twit-direct-with-account  	"twit" "" t)
(autoload 'twit-post-with-account 	"twit" "" t)

(autoload 'twit-show-direct-tweets-with-account "twit" "" t)
(autoload 'twit-show-at-tweets-with-account 	"twit" "" t)

(setq twit-show-user-images t) ;; 显示好友头像
(setq twit-user-image-dir "~/.emacs.d/twit") ;; 设置头像保存路径
(global-set-key (kbd "C-c t") 'twit-show-recent-tweets)
(global-set-key (kbd "C-c w") 'twit-post)

(require 'erc)                          ;IRC聊天
(require 'erc-nicklist)
(require 'erc-highlight-nicknames)      ;不同昵称的颜色

(provide 'yunt-yunt-other-elisp)

;;; yunt-other-elisp.el ends here
