;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-03 00:08:50 Thursday by ahei>

(require 'eldoc)

(dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

(defun eldoc-print-current-symbol-info-anyway ()
  "Print current symbol info."
  (interactive)
  (condition-case err
      (if eldoc-documentation-function
          (eldoc-message (funcall eldoc-documentation-function))
        (let* ((current-symbol (eldoc-current-symbol))
               (current-fnsym  (eldoc-fnsym-in-current-sexp))
               (doc (cond
                     ((null current-fnsym)
                      nil)
                     ((eq current-symbol (car current-fnsym))
                      (or (apply 'eldoc-get-fnsym-args-string
                                 current-fnsym)
                          (eldoc-get-var-docstring current-symbol)))
                     (t
                      (or (eldoc-get-var-docstring current-symbol)
                          (apply 'eldoc-get-fnsym-args-string
                                 current-fnsym))))))
          (eldoc-message doc)))
    ;; This is run from post-command-hook or some idle timer thing,
    ;; so we need to be careful that errors aren't ignored.
    (error (message "eldoc error: %s" err))))

(defun eldoc-pre-command-refresh-echo-area ())

(setq eldoc-idle-delay 0.5)
(custom-set-faces '(eldoc-highlight-function-argument
                    ((((type tty)) :bold t :foreground "green")
                     (t :bold nil :foreground "green"))))
(eldoc-add-command 'describe-symbol-at-point 'View-scroll-half-page-backward 'l-command
                   'save-buffer-sb 'switch-to-other-buffer)
(eldoc-remove-command 'goto-paren)
