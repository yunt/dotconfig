;; -*- Emacs-Lisp -*-

;; Time-stamp: <11/01/2009 15:50:13 星期日 by ahei>

(require 'compile)
(require 'ruby-mode)
(require 'make-mode)
(require 'cc-mode)
(require 'psvn)
(require 'sh-script)

;; 设置编译命令
(setq compile-command "make -k")

(if is-before-emacs-21
    (defalias 'cpl 'compile)
  (defalias 'cpl 'compile "简化`compile'命令"))

(defvar makefile-mode-map-list nil "the list of `makefile-mode-map'")
(if is-before-emacs-21
    (setq makefile-mode-map-list (list makefile-mode-map))
  (setq makefile-mode-map-list (list makefile-gmake-mode-map makefile-automake-mode-map)))

;; 智能编译
(require 'my-smart-compile)
(defun compile-buffer ()
  "编译当前buffer文件"
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name)) base-name)
    (if (not file)
        (message "此buffer不与任何文件关联")
      (setq base-name (file-name-nondirectory file))
      (let ((extension (file-name-extension file)))
        (cond
         ((equal extension "cpp")
          (compile (format "g++ -g %s -o %s" file (file-name-sans-extension base-name))))
         ((equal (downcase extension) "c")
          (compile (format "gcc -g %s -o %s" file (file-name-sans-extension base-name))))
         ((equal extension "java")
          (compile (format "javac -g %s" file)))
         ((equal extension "jj")
          (compile (format "javacc %s" file)))
         ((equal extension "sh")
          (compile (format "sh -n %s" file)))))))
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))

(defun run-program (command)
  "以命令COMMAND运行当前源程序对应的程序"
  (interactive
   (let* ((file (buffer-file-name)) base-name default-command (input ""))
     (if (not file)
         (error "此buffer不与任何文件关联")
       (setq base-name (file-name-nondirectory file))
       (setq default-command 
             (let ((extension (file-name-extension file)))
               (if (not extension)
                   (setq extension ""))
               (cond
                ((or (equal extension "cpp") (equal (downcase extension) "c"))
                 (format "./%s" (file-name-sans-extension base-name)))
                ((equal extension "java")
                 (format "java %s" (file-name-sans-extension base-name)))
                ((or (equal extension "sh") (equal major-mode 'sh-mode))
                 (format "sh %s" base-name)))))
       (while (string= input "")
         (setq input (read-from-minibuffer "Command to run: " default-command nil nil 'shell-command-history default-command)))
       (list input))))
  (let ((buffer "*Shell Command Output*"))
    (shell-command command buffer)
    (sleep-for 1)
    (end-of-buffer-other-window buffer)))

(defun make ()
  "在当前目录下执行\"make\"命令"
  (interactive)
  (if (or (file-readable-p "Makefile") (file-readable-p "makefile") (file-readable-p "GNUmakefile"))
      (compile "make -k")
    (if (file-readable-p "build.xml")
        (compile "ant -e -k compile")
      (smart-compile)))
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))
(defun ant ()
  "执行\"ant\"命令"
  (interactive)
  (compile "ant compile -e -k -s")
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))
(let ((list makefile-mode-map-list))
  (setq list (append list (list c-mode-base-map svn-status-mode-map sh-mode-map
				compilation-mode-map ruby-mode-map)))
  (dolist (map list)
    (define-key map (kbd "C-c C-m") 'make-sb)
    (define-key map (kbd "C-c m") 'make-check-sb)
    (define-key map (kbd "C-c M") 'make-clean-sb)
    (define-key map (kbd "C-c c") 'compile-buffer-sb)
    (define-key map (kbd "C-c r") 'run-program-sb)
    (define-key map (kbd "C-c C") 'smart-compile-sb)))
(dolist (map (list java-mode-map))
  (define-key map (kbd "C-c C-m") 'ant-sb)
  (define-key map (kbd "C-c M") 'ant-clean-sb)
  (define-key map (kbd "C-c m") 'ant-test-sb))

(defun make-check ()
  "在当前目录下执行\"make check\"命令"
  (interactive)
  (if (or (file-readable-p "Makefile") (file-readable-p "makefile") (file-readable-p "GNUmakefile"))
      (compile "make -k check")
    (if (file-readable-p "build.xml")
        (compile "ant test -e -k")))
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))

(defun make-clean ()
  "在当前目录下执行\"make clean\"命令"
  (interactive)
  (if (or (file-readable-p "Makefile") (file-readable-p "makefile") (file-readable-p "GNUmakefile"))
      (compile "make -k clean")
    (if (file-readable-p "build.xml")
        (compile "ant clean -e -k")))
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))
(defun ant-clean ()
  "执行\"ant clean\"命令"
  (interactive)
  (compile "ant clean -e -k -s")
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))
(defun ant-test ()
  "执行\"ant test\"命令"
  (interactive)
  (compile "ant test -e -k -s")
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))

(defun make-install ()
  "在当前目录下执行\"make install\"命令"
  (interactive)
  (compile "make -k install")
  (sleep-for 1)
  (end-of-buffer-other-window "*compilation*"))

;; 显示上一个编译错误
(global-set-key (kbd "M-p") 'previous-error)
(dolist (map makefile-mode-map-list)
  (define-key map (kbd "M-p") 'previous-error)
  (define-key map (kbd "M-n") 'next-error)
  (define-key map (kbd "C-c p") 'makefile-previous-dependency)
  (define-key map (kbd "C-c n") 'makefile-next-dependency))

(define-key compilation-mode-map (kbd "n") 'compilation-next-error)
(define-key compilation-mode-map (kbd "p") 'compilation-previous-error)

(provide 'compile-settings)
