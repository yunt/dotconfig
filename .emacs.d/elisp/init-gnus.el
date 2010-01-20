;;; init-gnus.el --- Configuration file for gnus

(require 'gnus-notify+)

;; 常规设置
(gnus-agentize)                                     ;开启代理功能, 以支持离线浏览
(setq gnus-inhibit-startup-message t)               ;关闭启动时的画面
(setq gnus-novice-user nil)                         ;关闭新手设置, 不进行确认
(setq gnus-expert-user t)                           ;不询问用户
(setq gnus-show-threads t)                          ;显示邮件线索
(setq gnus-interactive-exit nil)                    ;退出时不进行交互式询问
(setq gnus-use-dribble-file nil)                    ;不创建恢复文件
(setq gnus-always-read-dribble-file nil)            ;不读取恢复文件
(setq gnus-asynchronous t)                          ;异步操作
(setq gnus-large-newsgroup 100)                     ;设置大容量的新闻组默认显示的大小
(setq gnus-large-ephemeral-newsgroup nil)           ;和上面的变量一样, 只不过对于短暂的新闻组
(setq gnus-summary-ignore-duplicates t)             ;忽略具有相同ID的消息
(setq gnus-treat-fill-long-lines t)                 ;如果有很长的行, 不提示
(setq message-confirm-send t)                       ;防止误发邮件, 发邮件前需要确认
(setq message-kill-buffer-on-exit t)                ;设置发送邮件后删除buffer
(setq message-from-style 'angles)                   ;`From' 头的显示风格
(setq message-syntax-checks '((sender . disabled))) ;语法检查
(setq nnmail-expiry-wait 7)                         ;邮件自动删除的期限 (单位: 天)
(setq nnmairix-allowfast-default t)                 ;加快进入搜索结果的组
;; 窗口布局
(gnus-add-configuration
 '(article
   (vertical 1.0
             (summary .35 point)
             (article 1.0))))
;; 显示设置
(setq mm-text-html-renderer 'w3m)                     ;用W3M显示HTML格式的邮件
(setq mm-inline-large-images t)                       ;显示内置图片
(auto-image-file-mode)                                ;自动加载图片
(add-to-list 'mm-attachment-override-types "image/*") ;附件显示图片
;; 概要显示设置
(setq gnus-summary-gather-subject-limit 'fuzzy) ;聚集题目用模糊算法
(setq gnus-summary-line-format "%4P %U%R%z%O %{%5k%} %{%14&user-date;%}   %{%-20,20n%} %{%ua%} %B %(%I%-60,60s%)\n")
(defun gnus-user-format-function-a (header) ;用户的格式函数 `%ua'
  (let ((myself (concat "<shangyunt@gmail.com>"))
        (references (mail-header-references header))
        (message-id (mail-header-id header)))
    (if (or (and (stringp references)
                 (string-match myself references))
            (and (stringp message-id)
                 (string-match myself message-id)))
        "X" "│")))
(setq gnus-user-date-format-alist             ;用户的格式列表 `user-date'
      '(((gnus-seconds-today) . "TD %H:%M")   ;当天
        (604800 . "W%w %H:%M")                ;七天之内
        ((gnus-seconds-month) . "%d %H:%M")   ;当月
        ((gnus-seconds-year) . "%m-%d %H:%M") ;今年
        (t . "%y-%m-%d %H:%M")))              ;其他
;; 线程的可视化外观, `%B'
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "    ")
(setq gnus-sum-thread-tree-single-indent "◎ ")
(setq gnus-sum-thread-tree-root "● ")
(setq gnus-sum-thread-tree-false-root "☆")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf "╰─► ")
;; 时间显示
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ;将邮件的发出时间转换为本地时间
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ;跟踪组的时间轴
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)              ;新闻组分组
;; 设置邮件报头显示的信息
(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic")
                 "\\|"))
;; 用 Supercite 显示多种多样的引文形式
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)
;; 线程设置
(setq
 gnus-use-trees t                                                       ;联系老的标题
 gnus-tree-minimize-window nil                                          ;用最小窗口显示
 gnus-fetch-old-headers 'some                                           ;抓取老的标题以联系线程
 gnus-generate-tree-function 'gnus-generate-horizontal-tree             ;生成水平树
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;聚集函数根据标题聚集
 )
;; 排序
(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)                               ;时间的逆序
        (not gnus-thread-sort-by-number)))                           ;跟踪的数量的逆序
;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group) ;gnus切换时
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group)    ;退出Summary时
;; 自动更新新消息
;(add-hook 'gnus-summary-exit-hook 'gnus-notify+)        ;退出summary模式后
;(add-hook 'gnus-group-catchup-group-hook 'gnus-notify+) ;当清理当前组后
;(add-hook 'mail-notify-pre-hook 'gnus-notify+)          ;更新邮件时
;; 斑纹化
(setq gnus-summary-stripe-regexp        ;设置斑纹化匹配的正则表达式
      (concat "^[^"
              gnus-sum-thread-tree-vertical
              "]*"))
(add-hook 'gnus-started-hook
	  (lambda ()
	    (require 'gnus-demon)
	    (setq gnus-use-demon t)
	    (gnus-demon-add-handler 'gnus-group-get-new-news 3 1)
	    (gnus-demon-init)))

;; 最后设置
(gnus-compile)                          ;编译一些选项, 加快速度

(provide 'init-gnus)

;;; init-gnus.el ends here
