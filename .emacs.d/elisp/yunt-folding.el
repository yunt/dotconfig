;;; yunt-folding.el --- folding mode的配置
;;小知识：autoload和load相比，前者是在需要时启动folding.el，
;而在emacs启动时只加载这个函数名而已，也就是让Emacs知道有这个函数。
;而使用load，则在Emacs启动时便加载，如果加载东西很多，emacs启动会很慢。
(autoload 'folding-mode "folding"
  "Minor mode that simulates a folding editor" t)
(load-library  "folding")
(defun folding-mode-find-file-hook ()
  "One of the hooks called whenever a `find-file' is successful."
  (and (assq 'folded-file (buffer-local-variables))
       folded-file
       (folding-mode 1)
       (kill-local-variable 'folded-file)))
(setq fold-fold-on-startup t)
(folding-mode-add-find-file-hook)

;; ;; FIXME :是否通用,还没有试验
;; ;;使用通用的folding模式，只要有{{{和}}}就开启，挺好用吧
;; (defun my-folding-load-hook ()
;;   "Folding setup."
;;   (folding-install)  ;; just to be sure
;;   (defvar folding-mode-marks-alist nil)
;;   (let* ((ptr (assq 'text-mode folding-mode-marks-alist)))
;;     (setcdr ptr (list "# {{{" "# }}}")))
;;   )
;; (add-hook 'folding-load-hook 'my-folding-load-hook)

;;修改folding.el默认的快捷键，以方便自己使用
;;我大部分的快捷键在最后一帖中将会讲到
(setq fold-keys-already-setup nil)
(add-hook 'folding-mode-hook
     (function (lambda()
            (unless fold-keys-already-setup
         (setq fold-keys-already-setup t)
         (define-prefix-command 'ctl-f-folding-mode-prefix)
         (define-key 'ctl-f-folding-mode-prefix "f" 'fold-fold-region)
         (define-key 'ctl-f-folding-mode-prefix "e" 'fold-enter)
         (define-key 'ctl-f-folding-mode-prefix "x" 'fold-exit)
         (define-key 'ctl-f-folding-mode-prefix "b" 'fold-whole-buffer)
         (define-key 'ctl-f-folding-mode-prefix "o" 'fold-open-buffer)
         (define-key 'ctl-f-folding-mode-prefix "h" 'fold-hide)
         (define-key 'ctl-f-folding-mode-prefix "s" 'fold-show)
         (define-key 'ctl-f-folding-mode-prefix "t" 'fold-top-level)
         (define-key 'ctl-f-folding-mode-prefix "f" 'fold-fold-region)
         )
            (local-set-key "C-f" 'ctl-f-folding-mode-prefix))))

;;设定各个模式下，折叠的具体内容和方式
(folding-add-to-marks-list 'sgml-mode
         "<!-- {"
         "<!-- } -->" " --> ")
(folding-add-to-marks-list 'c-mode "/* <" "/* > */" "*/")
(folding-add-to-marks-list 'c++-mode
         "//<" "//>" "")
(folding-add-to-marks-list 'LaTeX-mode "%%% {{{" "%%% }}}" " ")
(folding-add-to-marks-list 'latex2e-mode "%%% {{{" "%%% }}}" " ")
(folding-add-to-marks-list 'latex-mode "%%%% {{{" "%%%% }}}" " ")
(folding-add-to-marks-list 'BibTeX-mode "%%% {{{" "%%% }}}" " ")
(folding-add-to-marks-list 'lisp-mode ";;{{{" ";;}}}" "")
(folding-add-to-marks-list 'emacs-lisp-mode ";;{{{" ";;}}}" "")
(folding-add-to-marks-list 'lisp-mode ";;; {" ";;; }" "")
(folding-add-to-marks-list 'lex-mode" /* {{{ " " /* }}} */ " "*/")
(folding-add-to-marks-list 'html-mode "<!-- { " "<!-- } -->" "-->")
(folding-add-to-marks-list 'shell-script-mode "# {{{" "# }}}" nil)
(folding-add-to-marks-list 'sh-mode "# {{{ " "# }}}" nil)

(provide 'yunt-foldinging)

;;; yunt-folding.el ends here
