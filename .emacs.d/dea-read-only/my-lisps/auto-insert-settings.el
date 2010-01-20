;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-11 17:05:26 Wednesday by ahei>

(auto-insert-mode)
(setq auto-insert-query nil)
(setq auto-insert-directory my-emacs-templates-path)
(define-auto-insert "build.properties" "build.properties.tpl")

(defun expand-template (template)
  "Expand template."
  (template-expand-template (concat my-emacs-templates-path template)))

(define-auto-insert
  '("\\.\\([Hh]\\|hh\\|hxx\\|hpp\\)$" . "C/C++ header")
  (lambda ()
    (expand-template "h.tpl")))
(define-auto-insert
  '("\\.c$" . "C")
  (lambda ()
    (expand-template "c.tpl")))
(define-auto-insert
  '("\\.cpp$" . "Cpp")
  (lambda ()
    (expand-template "cpp.tpl")))

(defun expand-elisp-template ()
  "Expand elisp template when create a new elisp file."
  (insert-abbrev "headx"))

(defun expand-html-template ()
  "Expand html template when create a new html file."
  (insert "headx")
  (yas/expand))

(define-auto-insert "\\.el$" 'expand-elisp-template)
(define-auto-insert "\\.htm\\(l\\)?$" 'expand-html-template)

(defun insert-abbrev (abbrev-name)
  "Insert abbrev ABBREV-NAME"
  (interactive "s")
  (insert abbrev-name)
  (expand-abbrev))
