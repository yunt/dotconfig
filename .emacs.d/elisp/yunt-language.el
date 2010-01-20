;;; yunt-language.el ---
;;; Code:

(set-language-environment 'UTF-8)
;;
(set-keyboard-coding-system 'utf-8)
;;终端的编码
(set-terminal-coding-system 'utf-8)
;;buffer内文字的编码
(set-buffer-file-coding-system 'utf-8)
;;
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
;;使用utf-8显示文件名
(set-file-name-coding-system 'utf-8)
;;
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)

;; Emacs 不支持 gb18030，如果 gb2312 能看到，就看到，看不到就看不到了，
;; 索性认为 gb18030 也是 gb2312 罢。
;(define-coding-system-alias 'gb18030 'gb2312)

;; ;;
; (set-language-environment 'utf-8)
;; ;;
;; (setq locale-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-clipboard-coding-system 'ctext)
;; (set-buffer-file-coding-system 'utf-8)

;;字体解码
;;
(setq font-encoding-alist
  (append
  '(("MuleTibetan-0" (tibetan . 0))
  ("GB2312" (chinese-gb2312 . 0))
  ("GBK" (chinese-gbk . 0))
  ("JISX0208" (japanese-jisx0208 . 0))
  ("JISX0212" (japanese-jisx0212 . 0))
  ("VISCII" (vietnamese-viscii-lower . 0))
  ("KSC5601" (korean-ksc5601 . 0))
  ("MuleArabic-0" (arabic-digit . 0))
  ("MuleArabic-1" (arabic-1-column . 0))
  ("MuleArabic-2" (arabic-2-column . 0)))
  font-encoding-alist))

(require 'unicad) ;;自动探测文件编码

(provide 'yunt-language)

;;; yunt-language.el ends here
