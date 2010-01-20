;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-24 17:45:57 Tuesday by ahei>

(apply-define-key
 help-mode-map
 `(("B"   help-go-back)
   ("F"   help-go-forward)
   ("C-h" help-go-back)
   ("C-;" help-go-forward)
   ("n"   forward-button)
   ("p"   backward-button)
   ("."   find-symbol-at-point)))

(defun goto-help-buffer ()
  "Goto *Help* buffer."
  (interactive)
  (let ((buffer (get-buffer "*Help*")))
    (if buffer
        (switch-to-buffer buffer)
      (message "*Help* buffer dose not exist!"))))

(define-key global-map (kbd "C-x H") 'goto-help-buffer)
