;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-18 16:41:01 Friday by ahei>

(setq todochiku-command
      (case system-type 
        (windows-nt (concat my-emacs-lisps-path "snarl_command.exe"))
        (darwin "/usr/local/bin/growlnotify")
        (t "/usr/bin/notify-send")))

(require 'todochiku)

(let ((non-exist (not (file-exists-p todochiku-command))))
  (setq todochiku-tooltip-too (and non-exist window-system))
  (setq todochiku-message-too (and (or non-exist (not window-system)) (not todochiku-tooltip-too))))

(setq todochiku-icons-directory (concat my-emacs-lisps-path "todochiku/icons"))
