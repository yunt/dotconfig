;;; yunt-ibuffer.el --- IBuffer的配置

;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
 (require 'ibuf-ext)
  (setq ibuffer-mode-hook
        (lambda ()
          (setq ibuffer-filter-groups
                '(
                  ("*buffer*" (name . "\\*.*\\*"))
                  ("dired" (mode . dired-mode))
                  ("perl" (or (mode . cperl-mode)
                              (mode . sepia-mode)
                              (mode . perl-mode)))
                  ("elisp" (or (mode . emacs-lisp-mode)
                               (mode . lisp-interaction-mode)))
                  ("prog" (or (mode . c++-mode)
                              (mode . c-mode)
                              (mode . java-mode)))
                  ("tags" (name . "^TAGS"))))))

(define-ibuffer-sorter file-name
  "Sort buffers by associated file name"
  (:description "file name")
  (apply 'string<
         (mapcar (lambda (buf)
                   (with-current-buffer (car buf)
                     (or buffer-file-name default-directory)))
                 (list a b))))
(define-key ibuffer-mode-map "sf" 'ibuffer-do-sort-by-file-name)

(defun ywb-ibuffer-rename-buffer ()
    (interactive)
    (call-interactively 'ibuffer-update)
    (let* ((buf (ibuffer-current-buffer))
           (name (generate-new-buffer-name
                  (read-from-minibuffer "Rename buffer(to new name): "
                                        (buffer-name buf)))))
      (with-current-buffer buf
        (rename-buffer name)))
    (call-interactively 'ibuffer-update))
  (define-key ibuffer-mode-map "r" 'ywb-ibuffer-rename-buffer)

(provide 'yunt-ibuffer)

;;; yunt-ibuffer.el ends here
