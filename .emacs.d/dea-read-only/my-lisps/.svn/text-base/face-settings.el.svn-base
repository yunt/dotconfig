;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-21 10:10:18 Monday by ahei>

;; usage:
;; (require 'face-settings)

(require 'ahei-face)
(require 'color-theme-ahei)

;; 是否使用黑色背景
(setq use-black-background t)

(when (facep 'semantic-tag-highlight-face)
  (set-face-foreground 'semantic-tag-highlight-face "red")
  (set-face-background 'semantic-tag-highlight-face "blue"))

(setq browse-kill-ring-separator-face 'font-lock-comment-delimiter-face)

;; ido face
(if is-before-emacs-21
    (progn
      (custom-set-faces '(ido-first-match-face ((((type tty pc)) :foreground "yellow")
                                                (t :bold nil :foreground "yellow"))))
      (custom-set-faces '(ido-only-match ((((class color)) (:bold nil :foreground "green"))))))
  (require 'ido)
  (custom-set-faces '(ido-first-match ((((type tty pc)) :foreground "yellow")
                                       (t :bold nil :foreground "yellow"))))
  (custom-set-faces '(ido-only-match ((((class color)) (:bold nil :foreground "green"))))))

(if use-black-background
    (set-face-foreground 'linum "cyan")
  (set-face-foreground 'linum "gray"))

;; 颜色配置
;; 高亮显示
(set-face-foreground 'highlight "red")
(set-face-background 'highlight "blue")

;; 区域显示
(custom-set-faces '(region
  ((((class color) (min-colors 88) (background dark))
     :background "#4CAA4CAA4CAA")
    (((class color) (min-colors 88) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 16) (background dark))
     :background "wheat")
    (((class color) (min-colors 16) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 8))
     :background "blue" :foreground "red")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))))

(copy-face 'region 'region-invert)
(invert-face 'region-invert)

(load "font-lock-face-settings")

;; `completion-mode'颜色设置
(defun completion-faces ()
  (unless is-before-emacs-21
    (custom-set-faces
     '(completions-first-difference
       ((((class color) (background dark)) (:foreground "red")))))
    (set-face-foreground 'completions-common-part "yellow")))
(add-hook 'completion-setup-hook 'completion-faces)

;; 设置界面
(if use-black-background
    (progn
      (set-background-color "black")
      (set-foreground-color "white")
      (set-cursor-color "green"))
  (set-background-color "white")
  (set-foreground-color "black")
  (set-cursor-color "blue"))

;; `apropos-mode'颜色设置
(setq apropos-match-face 'red-face)
(setq apropos-symbol-face 'magenta-face)
(setq apropos-keybinding-face 'cyan-face)
(setq apropos-label-face 'underline-green-face)
(setq apropos-property-face 'font-yellow-face)

;; mode-line颜色设置
;; `which-func'颜色设置
(unless is-before-emacs-21
  (custom-set-faces '(mode-line-buffer-id
                      ((((class grayscale) (background light)) (:foreground "LightGray" :background "yellow" :weight bold))
                       (((class grayscale) (background dark)) (:foreground "DimGray" :background "yellow" :weight bold))
                       (((class color) (min-colors 88) (background light)) (:foreground "Orchid" :background "yellow"))
                       (((class color) (min-colors 88) (background dark)) (:foreground "yellow" :background "HotPink3"))
                       (((class color) (min-colors 16) (background light)) (:foreground "Orchid" :background "yellow"))
                       (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue" :background "yellow"))
                       (((class color) (min-colors 8)) (:foreground "blue" :background "yellow" :weight bold))
                       (t (:weight bold))))))
(if window-system
    (progn
      (set-face-foreground 'mode-line "black")
      (set-face-background 'mode-line "lightgreen")
      (unless is-before-emacs-21
        (set-face-foreground 'mode-line-inactive "black")
        (set-face-background 'mode-line-inactive "white")
        (set-face-foreground 'which-func "yellow2")
        (set-face-background 'which-func "dark magenta")))
  (set-face-foreground 'mode-line "green")
  (set-face-background 'mode-line "black")
  (unless is-before-emacs-21
    (set-face-foreground 'mode-line-buffer-id "blue")
    (set-face-background 'mode-line-buffer-id "yellow")
    (set-face-foreground 'mode-line-inactive "white")
    (set-face-background 'mode-line-inactive "black")
    (set-face-foreground 'which-func "yellow")
    (set-face-background 'which-func "black")))

(unless is-before-emacs-21
  ;; 括号颜色设置
  (set-face-background 'show-paren-match "magenta")
  (set-face-foreground 'show-paren-match "yellow")
  (set-face-background 'show-paren-mismatch "red")

  ;; `help-mode'
  (set-face-foreground 'help-argument-name "green"))

;; `isearch'颜色设置
(set-face-foreground 'isearch "red")
(set-face-background 'isearch "blue")
(when (not is-before-emacs-21)
  (set-face-foreground 'lazy-highlight "black")
  (set-face-background 'lazy-highlight "white"))
(custom-set-faces '(isearch-fail ((((class color)) (:background "red")))))

(load "diff-face-settings")

(load "ediff-face-settings")

(require 'color-moccur)
(set-face-foreground 'moccur-current-line-face "red")
(set-face-background 'moccur-current-line-face "blue")
(custom-set-faces '(moccur-face
                    ((((type tty)) :bold t :foreground "red")
                     (t :bold nil :foreground "red"))))
(set-face-background 'moccur-face "white")

(setq Man-overstrike-face 'yellow-face)
(setq Man-underline-face 'underline-green-face)
(setq Man-reverse-face 'red-face)

(load "info-face-settings")
(load "info+-face-settings")

(require 'mic-paren)
(set-face-background 'paren-face-match "magenta")
(set-face-foreground 'paren-face-match "yellow")
(set-face-background 'paren-face-mismatch "red")

(require 'log-view)
(if is-before-emacs-21
    (progn
      (set-face-foreground 'log-view-file-face "green")
      (set-face-foreground 'log-view-message-face "yellow"))
  (setq log-view-file-face 'darkgreen-face)
  (setq log-view-message-face 'darkyellow-face))

(require 'woman)
(set-face-foreground 'woman-italic "green")
(set-face-foreground 'woman-bold "red")
(set-face-foreground 'woman-addition "yellow")
(set-face-foreground 'woman-unknown "blue")

;; icomplete+
(custom-set-faces '(icompletep-nb-candidates
                    ((((type tty)) :bold t :foreground "green")
                     (t :foreground "green"))))
(custom-set-faces '(icompletep-determined
                    ((((type tty)) :bold t :foreground "green")
                     (t :foreground "green"))))
(custom-set-faces '(icompletep-choices
                    ((((type tty)) :bold t :foreground "yellow")
                     (t :foreground "yellow"))))
(custom-set-faces '(icompletep-nb-candidates
                    ((((type tty)) :bold t :foreground "green")
                     (t :foreground "green"))))

;; compile
(custom-set-faces '(compilation-info
                    ((((type tty)) :bold t :foreground "green")
                     (t :foreground "green"))))
(setq compilation-message-face nil)
(setq compilation-warning-face 'red-face)
(setq compilation-enter-directory-face 'beautiful-blue-face)
(setq compilation-leave-directory-face 'magenta-face)

;; svn
(custom-set-faces '(svn-status-filename-face
                    ((((type tty)) :bold t :foreground "yellow")
                     (t :foreground "yellow"))))

;; dired+
(require 'dired+)
(custom-set-faces '(diredp-display-msg
                    ((((type tty)) :foreground "blue")
                     (t :foreground "cornflower blue"))))
(custom-set-faces '(diredp-date-time
                    ((((type tty)) :foreground "yellow")
                     (t :foreground "goldenrod1"))))
(custom-set-faces '(diredp-dir-heading
                    ((((type tty)) :background "yellow" :foreground "blue")
                     (t :background "Pink" :foreground "DarkOrchid1"))))
(setq diredp-ignored-file-name 'green-face)
(setq diredp-file-name 'darkred-face)
(setq diredp-file-suffix 'magenta-face)
(setq diredp-exec-priv 'darkred-face)
(setq diredp-other-priv 'white-face)
(setq diredp-no-priv 'darkmagenta-face)
(setq diredp-write-priv 'darkcyan-face)
(setq diredp-read-priv 'darkyellow-face)
(setq diredp-link-priv 'lightblue-face)
(setq diredp-symlink 'darkcyan-face)
(setq diredp-rare-priv 'white-red-face)
(setq diredp-dir-priv 'beautiful-blue-face)
(setq diredp-compressed-file-suffix 'darkyellow-face)

(custom-set-faces '(match
                    ((((class color) (min-colors 88) (background light))
                      :background "yellow1")
                     (((class color) (min-colors 88) (background dark))
                      :background "RoyalBlue3" :foreground "cyan")
                     (((class color) (min-colors 8) (background light))
                      :background "yellow" :foreground "black")
                     (((class color) (min-colors 8) (background dark))
                      :background "blue" :foreground "white")
                     (((type tty) (class mono))
                      :inverse-video t)
                     (t :background "gray"))))

(custom-set-faces
 '(sh-heredoc
   ((((min-colors 88) (class color)
      (background dark))
     (:foreground "deeppink"))
    (((class color)
      (background dark))
     (:foreground "deeppink"))
    (((class color)
      (background light))
     (:foreground "tan1" ))
    (t
     (:weight bold)))))

;; w3m
(require 'w3m)
(set-face-foreground 'w3m-arrived-anchor-face "magenta")

(defface mode-line-lines-face
  '((((type tty pc)) :background "red" :foreground "white")
    (t (:background "dark slate blue" :foreground "yellow")))
  "Face used to highlight lines on mode-line.")

(custom-set-faces
 '(header-line
   ((default
      :inherit mode-line)
    (((type tty))
     :foreground "black" :background "yellow" :inverse-video nil)
    (((class color grayscale) (background light))
     :background "grey90" :foreground "grey20" :box nil)
    (((class color grayscale) (background dark))
     :background "#D58EFFFFFC18" :foreground "blue")
    (((class mono) (background light))
     :background "white" :foreground "black"
     :inverse-video nil
     :box nil
     :underline t)
    (((class mono) (background dark))
     :background "black" :foreground "white"
     :inverse-video nil
     :box nil
     :underline t))))

;; highlight-symbol
(custom-set-faces
 '(highlight-symbol-face
   ((((type tty))
     :background "white" :foreground "black")
    (((class color) (background dark))
     :background "gray30" :foreground "#AD0DE2FAFFFF")
    (((class color) (background light))
     :background "gray90"))))

(when (fboundp 'color-theme-adjust-hl-line-face)
  (color-theme-adjust-hl-line-face))

(load "cedet-face-settings")

(load "twit-face-settings")

(defface hl-line-nonunderline-face
  '((((type tty)))
    (t :background "AntiqueWhite4" :inverse-video nil))
  "`hl-line-face' without `underline'.")

(provide 'face-settings)
