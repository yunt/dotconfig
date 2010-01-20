;;; begin lisp code
(require 'highlight)

;zjl:这些感觉没有什么用处实际上...
(custom-set-variables
 '(jit-lock-chunk-size 1000000)
 '(jit-lock-stealth-load 100)
 '(jit-lock-stealth-nice 0.1))

;; extra syntax highlighting
(defface zjl-c-font-lock-bracket-face
  '((t (:foreground "firebrick3")))
  "Font lock mode face for brackets, e.g. '(', ']', etc."
  :group 'font-lock-faces)
(defvar zjl-c-font-lock-bracket-face 'zjl-c-font-lock-bracket-face
  "Font lock mode face for backets.  Changing this directly
  affects only new buffers.")

(setq c-operators-regexp
      (regexp-opt '("+" "-" "*" "/" "%" "!"
                    "&" "^" "~" "|"
                    "=" "<" ">"
                    "." "," ";" ":")))
(setq c-brackets-regexp
      (regexp-opt '("(" ")" "[" "]" "{" "}")))
(setq c-types-regexp
      (concat
       "\\_<[_a-zA-Z][_a-zA-Z0-9]*_t\\_>" "\\|"       
      (concat "\\_<" (regexp-opt '("unsigned" "int" "char" "float" "void") 'words) "\\_>")))

(setq warning-words-regexp
      (concat "\\_<" (regexp-opt '("FIXME" "TODO" "BUG" "XXX" "DEBUG")) "\\_>"))
(eval-after-load "cc-mode"
  '(progn
     (font-lock-add-keywords
      'c-mode
      (list
;       (cons c-operators-regexp 'font-lock-builtin-face)
       (cons c-operators-regexp 'zjl-c-hl-operators-face)
       (cons c-brackets-regexp 'zjl-c-font-lock-bracket-face)
       (cons c-types-regexp 'font-lock-type-face)
       (cons warning-words-regexp 'font-lock-warning-face)))
     ))

(defface zjl-c-hl-operators-face
  '((t (:foreground "DarkGoldenrod4")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-operators-face 'zjl-c-hl-operators-face)

(defface zjl-c-hl-function-call-face
  '((t (:foreground "SpringGreen4" :bold t)))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-function-call-face 'zjl-c-hl-function-call-face)

(defface zjl-c-hl-member-reference-face
;  '((t (:foreground "dark green")))
;  '((t (:foreground "CadetBlue")))
  '((t (:foreground "ForestGreen")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-member-reference-face 'zjl-c-hl-member-reference-face)

(defface zjl-c-hl-local-variable-reference-face
;  '((t (:foreground "purple")))
;  '((t (:foreground "cyan4")))
;  '((t (:foreground "turquoise4")))
;  '((t (:foreground "maroon4")))
  '((t (:foreground "magenta3")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-local-variable-reference-face 'zjl-c-hl-local-variable-reference-face)

(defface zjl-c-hl-parameters-reference-face
;  '((t (:foreground "NavyBlue")))
  '((t (:foreground "RoyalBlue4" :bold t)))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-parameters-reference-face 'zjl-c-hl-parameters-reference-face)

(defface zjl-c-hl-number-face
  '((t (:foreground "red")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-number-face 'zjl-c-hl-number-face)

(defface zjl-c-hl-if0-face
  '((t (:foreground "gray53")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-if0-face 'zjl-c-hl-if0-face)

(defface zjl-c-hl-global-reference-face
  '((t (:foreground "firebrick")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-global-reference-face 'zjl-c-hl-global-reference-face)

(defface zjl-c-hl-MACRO-reference-face
  '((t (:foreground "red")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-MACRO-reference-face 'zjl-c-hl-MACRO-reference-face)

(defface zjl-c-hl-member-point-face
  '((t (:foreground "SpringGreen4")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-c-faces)
(defvar zjl-c-hl-member-point-face 'zjl-c-hl-member-point-face)

 (add-hook 'c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(("->"
          0  zjl-c-hl-member-point-face t))
     1)))

 (add-hook 'c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(("\\(\\_<\\(?:\\(?:0x[0-9a-fA-F]*\\)\\|\\(?:[0-9]+\\)\\|\\(?:0[0-7]*\\)\\|\\(?:[01]+b\\)\\)\\_>\\)"
          0  zjl-c-hl-number-face keep))
     1)))

 (add-hook 'c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(("\\(\\(\\.\\|\\(->\\)\\)\\_<\\(\\w\\|\\s_\\)+\\)"
          1  zjl-c-hl-member-reference-face keep))
     1)))

 (add-hook 'c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(("\\(\\_<\\(\\w\\|\\s_\\)+\\_>\\)[ 	]*("
          1  zjl-c-hl-function-call-face keep))
     1)))

(defun zjl-c-hl-local-variable-and-parameter-in-func-region (start end)
  (interactive)
  (save-excursion
    (goto-char start)
    (let* (symbol-boundaries
           symbol-substring
           (local-variable-list '())
           (parameter-list '())                
           (func-body-begin (save-excursion (zjl-c-hl-find-symbol-end start end "{")))
           (temp-case-fold-search  case-fold-search)
           symbol-end
           )
      (setq case-fold-search nil)
      (when func-body-begin
        (while   (< (point) func-body-begin) 
          (setq symbol-end (zjl-c-hl-find-symbol-end (point) func-body-begin "\\_<\\(?:\\w\\|\\s_\\)+\\_>"))
          (if symbol-end
              (progn (goto-char symbol-end)
                     (backward-char)
                     (when (equal (get-char-property (point) 'face) 'font-lock-variable-name-face)              
                       (setq symbol-boundaries (bounds-of-thing-at-point 'symbol))
                       (setq symbol-substring (buffer-substring-no-properties (car symbol-boundaries) (cdr symbol-boundaries)))
                       (setq parameter-list (cons symbol-substring parameter-list)))
                     (forward-char))
            (goto-char func-body-begin)))
        (goto-char func-body-begin)
        (while   (< (point) end) 
          (setq symbol-end (zjl-c-hl-find-symbol-end (point) end "\\_<\\(?:\\w\\|\\s_\\)+\\_>"))
          (if symbol-end
              (progn (goto-char symbol-end)
                     (backward-char)
                     (when (equal (get-char-property (point) 'face) 'font-lock-variable-name-face)              
                       (setq symbol-boundaries (bounds-of-thing-at-point 'symbol))
                       (setq symbol-substring (buffer-substring-no-properties (car symbol-boundaries) (cdr symbol-boundaries)))
                       (setq local-variable-list (cons symbol-substring local-variable-list)))
                     (forward-char))
            (goto-char end)))

        (save-excursion
          (zjl-c-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-c-hl-local-variable-reference-face nil t t)
          (zjl-c-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-c-hl-parameters-reference-face nil t t)))
      (setq  case-fold-search temp-case-fold-search))))

(defun zjl-c-hl-symbol-region (start end symbol-exp symbol-face override-c-mode override-my override-nil)
  (let (target-this-end target-next-start (temp-case-fold-search  case-fold-search))
    (setq case-fold-search nil)
    (save-excursion
      (goto-char start)
      (while   (< (point) end)
        (zjl-c-hl-next-comment-pos (point) end)
        (if zjl-c-hl-next-comment-exist
            (progn
              (setq target-this-end zjl-c-hl-next-comment-start)
              (setq target-next-start zjl-c-hl-next-comment-end))
          (setq target-this-end end)
          (setq target-next-start end))
        (if override-c-mode
            (hlt-highlight-regexp-region (point) target-this-end symbol-exp symbol-face)
          (while (re-search-forward symbol-exp target-this-end t)
            (backward-char)
            (when (or 
                   (and override-my
                        (or (equal (get-char-property (point) 'face) 'zjl-c-hl-parameters-reference-face)
                            (equal (get-char-property (point) 'face) 'zjl-c-hl-local-variable-reference-face)
                            (not (zjl-c-hl-what-face))))
                   (and override-nil
                        (not (zjl-c-hl-what-face))))
              (let ((symbol-boundaries (bounds-of-thing-at-point 'symbol)))
                (hlt-highlight-regexp-region (car symbol-boundaries) (cdr symbol-boundaries) ".*" symbol-face)))
            (forward-char))
          (goto-char target-next-start))))
    (setq  case-fold-search temp-case-fold-search)))

(defun zjl-c-hl-find-symbol-end (start end symbol-exp)
  (let (target-this-end target-next-start (temp-case-fold-search  case-fold-search) find-p)
    (setq case-fold-search nil)
    (save-excursion
      (goto-char start)
      (while   (< (point) end)
        (zjl-c-hl-next-comment-pos (point) end)
        (if zjl-c-hl-next-comment-exist
            (progn
              (setq target-this-end zjl-c-hl-next-comment-start)
              (setq target-next-start zjl-c-hl-next-comment-end))
          (setq target-this-end end)
          (setq target-next-start end))
        (setq find-p (re-search-forward symbol-exp target-this-end t))
        (if find-p
            (progn  (setq find-p (point))
                    (goto-char end))
          (goto-char target-next-start))))
        (setq  case-fold-search temp-case-fold-search)
        find-p))


(setq zjl-c-hl-next-comment-start nil)
(setq zjl-c-hl-next-comment-end nil)
(setq zjl-c-hl-next-comment-exist nil)
(defun zjl-c-hl-next-comment-pos (start end)
  (let (comment-pos-1 comment-pos-2 min-pos)
    (setq zjl-c-hl-next-comment-start nil)
    (setq zjl-c-hl-next-comment-end nil)
    (setq zjl-c-hl-next-comment-exist nil)
    (save-excursion
      (goto-char start)
      (save-excursion
        (if (re-search-forward "/\\*" end t)
            (progn 
              (setq comment-pos-1 (point))
              (setq zjl-c-hl-next-comment-exist t))
          (setq comment-pos-1 end)))
      (save-excursion 
        (if (re-search-forward "//" end t)
            (progn
              (setq comment-pos-2 (point))
              (setq zjl-c-hl-next-comment-exist t))
          (setq comment-pos-2 end)))
      (when zjl-c-hl-next-comment-exist
        (setq min-pos (min comment-pos-1 comment-pos-2))
        (setq zjl-c-hl-next-comment-start (- min-pos 2))
        (cond
         ((equal comment-pos-2 min-pos) 
          (end-of-line)
          (when (< (point) end)
            (forward-char)))
         ((equal comment-pos-1 min-pos)
          (unless (re-search-forward "\\*/" end t)
            (goto-char end))))
        (setq zjl-c-hl-next-comment-end (point))))))


(defun zjl-c-hl-local-variable-and-parameter-region (start end)
  (save-excursion
    (goto-char start)
    (let (defun-start defun-end)
      (while (and (setq defun-end 
                        (if (c-end-of-defun) 
                            (point)
                          nil))
                  (setq defun-start
                        (if (c-beginning-of-defun)
                            (point)
                          nil))
                  (< defun-start end))
        (zjl-c-hl-local-variable-and-parameter-in-func-region defun-start defun-end)
        (goto-char defun-end)))))

(defun zjl-c-hl-global-region (start end)
  (save-excursion
    (goto-char start)
    (let (defun-start defun-end)
      (while (and (setq defun-end 
                        (if (c-end-of-defun) 
                            (point)
                          nil))
                  (setq defun-start
                        (if (c-beginning-of-defun)
                            (point)
                          nil))
                  (< defun-start end))
         (zjl-c-hl-symbol-region defun-start defun-end "\\_<[gm]\\(\\w\\|\\s_\\)+\\_>" zjl-c-hl-global-reference-face nil nil t)
        (goto-char defun-end)))))



                                                     
(defun zjl-c-hl-local-variable-and-parameter-init ()
  (zjl-c-hl-local-variable-and-parameter-region (point-min) (point-max)))

(defun zjl-c-hl-global-init ()
  (zjl-c-hl-global-region (point-min) (point-max)))

(defun zjl-c-hl-MACRO-init ()
  (zjl-c-hl-symbol-region (point-min) (point-max) "\\_<[A-Z]\\([A-Z0-9]\\|\\s_\\)+\\_>" zjl-c-hl-MACRO-reference-face nil nil t))

;borrow from http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun zjl-c-hl-what-face ()
  (interactive)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    face))

(defun zjl-c-hl-init ()
  (interactive)
  (when (eq major-mode 'c-mode)
    (zjl-c-hl-local-variable-and-parameter-init)
;    (zjl-c-hl-MACRO-init)
    (zjl-c-hl-global-init)
))

(setq zjl-c-hl-window-start 0)
(setq zjl-c-hl-window-end 0)
(setq zjl-c-hl-timer-count 0)
(defun zjl-c-hl-timer ()
  (interactive)
  (when (eq major-mode 'c-mode)
    (let(start end)
      (if (window-end)
      (setq end (window-end))
      (setq end (point-max)))
      (if (window-start)
      (setq start (window-start))
      (setq start (point-min)))
      (zjl-c-hl-local-variable-and-parameter-region start end)
      (when (or zjl-c-hl-buffer-changed
                (not (equal start zjl-c-hl-window-start))
                (not (equal end zjl-c-hl-window-end))      
                (equal zjl-c-hl-timer-count 10))                
        (seq zjl-c-hl-timer-count 0)
        (zjl-c-hl-global-region start end)
        (zjl-c-hl-symbol-region start end "\\_<[gm]\\(\\w\\|\\s_\\)+\\_>" zjl-c-hl-global-reference-face nil nil t)
;      (zjl-c-hl-symbol-region start end (point-max) "\\_<[A-Z]\\([A-Z0-9]\\|\\s_\\)+\\_>" zjl-c-hl-MACRO-reference-face nil nil t)
      ))))

(run-with-idle-timer 0.5 1 'zjl-c-hl-timer)
;reference: http://forum.ubuntu.org.cn/viewtopic.php?f=68&t=97499&start=0&sid=d9f0add2f103eb05ece1827c7ee10f5c
;;; end lisp code 

(setq zjl-c-hl-buffer-changed nil)
(add-hook 'after-change-functions 'zjl-c-hl-buffer-changed t t)
(defun zjl-c-hl-buffer-changed ()
  (setq zjl-c-hl-buffer-changed t))
(add-hook 'c-mode-hook 'zjl-c-hl-init t t)

(provide 'zjl-c-hl-aggressive)

