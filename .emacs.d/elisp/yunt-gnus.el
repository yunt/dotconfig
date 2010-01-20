;; Email & NewsGroup
;;编码设置
;(set-language-environment 'Chinese-GB)
;(setq gnus-default-charset 'chinese-iso-8bit
;gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit))
;gnus-summary-show-article-charset-alist
;'((1 . chinese-iso-8bit)
;(2 . gbk)
;(3 . big5)
;(4 . utf-8))
(setq gnus-newsgroup-ignored-charsets
'(unknown-8bit x-unknown x-gbk gb18030 iso-8859-1))
;;html
(eval-after-load "mm-decode"
'(progn
(add-to-list 'mm-discouraged-alternatives "text/html")
(add-to-list 'mm-discouraged-alternatives "text/richtext")))
;;news
;;新闻组服务器设置
(setq gnus-select-method '(nntp "news.cn99.com"))
;;(setq gnus-select-method '(nntp "news.yaako.com"))
;;(add-to-list 'gnus-secondary-select-methods '(nntp "news.newsgroup.com.hk"))
;;ordering
(setq gnus-default-subscribed-newsgroups
'("gnu.emacs.help"
"cn.comp.os.linux"
))
;;邮件基础设置
(load-library "nnimap")
(require 'nnir)
;;imap收信服务器设置
(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)))

;; Smtp发信服务器设置
(setq starttls-use-gnutls t)
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-debug-info t)
(require 'smtpmail)

(require 'init-gnus)

;; Keymaps
(define-key gnus-summary-mode-map (kbd "Backspace") 
	'gnus-summary-prev-page)
