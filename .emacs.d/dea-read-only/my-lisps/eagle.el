;;; eagle.el --- Twitter client

(defvar eagle-version-number "1.0")

;; Copyright (C) 2009 ahei

;; Time-stamp: <2010-01-12 17:20:14 Tuesday by ahei>
;; Author: ahei <ahei0802@126.com>
;; Keywords: twitter eagle

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; This library is enhacement of twit.el.
;; 1.
;; If you in some country which "forbit" great people to use twitter, twit.el
;; can not help you, but eagle can let you set twitter api address which your
;; network can reach.
;; 2.
;; With twit, you must write your twitter password in el file, that is very unsecure,
;; unless you must type your twitter password many times when twit connect twitter server.
;; but eagle will prompt you to input your twitter password when you have not set it. and,
;; after you input it, eagle use it to build authentication, but not save it which twit do.
;;
;; see article http://ahei.yo2.cn/twitter.htm

;;; Installation:
;;
;; Copy eagle.el to your load-path and add to your .emacs:
;;
;; (require 'eagle)

;; then use eagle-show-recent-tweets to show recent tweets.

;;; History:
;;
;; 2009-12-6
;;      * initial version 1.0.

;;; Code:

(require 'twitter4e)
(require 'image-file)

(defgroup eagle nil
  "Enhacement for twit."
  :prefix "eagle-")

(defcustom eagle-twitter-username
  ""
  "Your twitter username."
  :group 'eagle
  :type 'string)

(defcustom eagle-password
  ""
  "Your twitter password."
  :group 'eagle
  :type 'string)

(defcustom eagle-http-port 80
  "Http port."
  :type 'integer
  :group 'eagle)

(defcustom eagle-https-port 443
  "Https port."
  :type 'integer
  :group 'eagle)

(defcustom eagle-recent-tweets-buffer-name "*Twitter-recent*"
  "Recent twits buffer name."
  :type 'string
  :group 'eagle)

(defcustom eagle-cur-time-format "%Y-%m-%d %T"
  "Time format."
  :type 'string
  :group 'eagle)

(defcustom eagle-unknown-screenname "unknown"
  "Unknow screen name."
  :type 'string
  :group 'eagle)

(defface eagle-message-face
  `((((type tty pc)) :foreground "yellow")
    (t :foreground "#FFFFFEA78873"))
  "The font face to use for a twitter message."
  :group 'eagle)

(defface eagle-twitter-username-face
  '((((type tty pc)) :foreground "magenta")
    (t :foreground "#F1C280F1FFFF"))
  "The face to use for twitter username."
  :group 'eagle)

(defface eagle-screenname-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 88) (background dark)) (:foreground "cornflower blue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "The face to use for twitter username."
  :group 'eagle)

(defface eagle-info-face
  `((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :foreground "#D58EE0FAFFFF"))
  "Normal info face."
  :group 'eagle)

(defface eagle-small-info-face
  `((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#D58EE0FAFFFF"))
  "Small info face."
  :group 'eagle)

(defface eagle-time-face
  '((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#AA7CF7B2FFFF"))
  "The face to use for a twitter source."
  :group 'eagle)

(defface eagle-location-face
  '((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#FFFFDC99D00E"))
  "The face to use for a twitter source."
  :group 'eagle)

(defface eagle-source-face
  '((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#6777FFFF6CA5"))
  "The face to use for a twitter source."
  :group 'eagle)

(defface eagle-tcrt-mode-line-face
  '((((type tty pc)) :foreground "yellow" :background "magenta")
    (t (:foreground "#FFFFBCA80000" :background "#00005E177C05")))
  "Face used highlight `eagle-tcrt-mode-line-format'.")

(defface eagle-title-face
  `((((background light))
     :background "PowderBlue" :bold t
     :box (:line-width 2 :color "PowderBlue" :style 0))
    (((background dark))
     :background "#D58EFFFFFC18" :foreground "blue")
    (t :underline "white"))
  "Title Area of the recent tweets buffer."
  :group 'eagle)

(defface eagle-hash-at-face
    '((((class color) (background light))
       (:foreground "GoldenRod3"))
      (((class color) (background dark))
       (:foreground "GoldenRod"))
      (t (:underline "white")))
  "Face to show @msgs in"
  :group 'eagle)

(defface eagle-too-long-face
    '((((supports :strike-through t)) :strike-through t )
      (t :inherit 'font-lock-warning-face))

  "Face for highlighting a twit that's too long to post"
  :group 'eagle)

(defcustom eagle-tcrt-mode-line-format
  (propertize "TCRT" 'face 'eagle-tcrt-mode-line-face)
  "Mode line format of function `eagle-tcrt-mode'."
  :group 'eagle)

(defcustom eagle-time-format 'eagle-format-time-for-display
  "Function or string describing the format to display time stamps in.
If the value is a string it should be a format string with %
characters and it will be passed to format-time-string.

If the value is a function it will be called with a single
argument which will be a two element list containing the high and
low part of the number of seconds since epoch. The value can be
converted to broken down time using decode-time.

Otherwise the variable can be nil in which case the time string
from Twitter will be displayed directly."
  :type '(choice (const :tag "No translation" nil)
                 string
                 function)
  :group 'eagle)

(defcustom eagle-status-format
  (concat 
   "%p" 
   (propertize "%u" 'face 'eagle-twitter-username-face)
   (propertize "("  'face 'eagle-info-face)
   (propertize "%n" 'face 'eagle-screenname-face)
   (propertize ")"  'face 'eagle-info-face)
   "\n"
   (propertize "%m" 'face 'eagle-message-face)
   "\n"
   (propertize "%t"   'face 'eagle-time-face)
   (propertize " @"   'face 'eagle-small-info-face)
   (propertize " %l"  'face 'eagle-location-face)
   (propertize " via" 'face 'eagle-small-info-face)
   (propertize " %s"  'face 'eagle-source-face)
   "\n\n")
  "Format string describing how to display twitter statuses
It should be a string containing '%' characters followed by one
of the following commands:

%n - the full name of the person posting the tweet
%u - the screen name of the person posting the tweet
%t - the time the tweet was created. This gets formatted
     according to twitter-time-format
%r - a reply button
%m - the tweet's text
%M - the tweet's text but filled with fill-region
%s - the name of the program used to send the tweet

%i - the numeric id of the tweet
%T - whether the tweet was truncated

%U - the screen name of the person who the tweet was a reply to
%R - the status id of the tweet that this is a reply to
%S - the user id of the tweet that this is a reply to

%I - the id of the user posting the tweet
%l - the location of the user posting the tweet
%d - a description of the user posting the tweet
%A - a URL to the image for the person posting the tweet
%L - a URL to the home page for the person posting the tweet
%p - photo of the person posting the tweet
%F - the number of followers of the person posting the tweet
%P - whether posts from this user are protected

%% - a literal percent character

Any other text is copied directly into the buffer. Text
properties are preserved and the properties of the % markers will
be applied to the resulting string.

The marker can optionally be given a padding value after the %
symbol. If the value is negative, the padding will be added to
the right otherwise it will be added to the left."
  :type 'string
  :group 'eagle)

(defcustom eagle-time-format 'eagle-format-time-for-display
  "Function or string describing the format to display time stamps in.
If the value is a string it should be a format string with %
characters and it will be passed to format-time-string.

If the value is a function it will be called with a single
argument which will be a two element list containing the high and
low part of the number of seconds since epoch. The value can be
converted to broken down time using decode-time.

Otherwise the variable can be nil in which case the time string
from Twitter will be displayed directly."
  :type '(choice (const :tag "No translation" nil)
                 string
                 function)
  :group 'eagle)

(defcustom eagle-recent-tweets-header-format "Recent tweets (Page %s) [%s]"
  "Format of recent tweets header."
  :type 'string
  :group 'eagle)

(defcustom eagle-use-tcrt-mode t
  "Use `eagle-tcrt-mode' or not."
  :type 'boolean
  :group 'eagle)

(defcustom eagle-debug-mem nil
  "Turn on memory debugging."
  :type 'boolean
  :group 'eagle)

(defcustom eagle-follow-idle-interval
  90
  "How long in time to wait before checking for new tweets.
Right now it will check every 90 seconds, Which will generate a maximum
of 40 requests, leaving you another 30 per hour to play with.

The variable name is a bit of a misnomer, because it is not actually
based on idle time (anymore)."
  :type 'integer
  :group 'twit)

(defconst eagle-status-commands
  '((?i . id)
    (?R . in_reply_to_status_id)
    (?S . in_reply_to_user_id)
    (?U . in_reply_to_screen_name)
    (?T . truncated))
  "Alist mapping format commands to XML nodes in the status element.")

(defconst eagle-user-commands
  '((?n . name)
    (?u . screen_name)
    (?I . id)
    (?l . location)
    (?d . description)
    (?A . profile_image_url)
    (?L . url)
    (?F . followers_count)
    (?P . protected))
  "Alist mapping format commands to XML nodes in the user element.")

(defconst eagle-request-headers
  `(("X-Twitter-Client" . "Eagle")
    ("X-Twitter-Client-Version" . ,eagle-version-number)
    ("X-Twitter-Client-URL" . "http://www.emacswiki.org/cgi-bin/emacs/eagle.el"))
  "Headers sent by every eagle.el request.")

(defconst eagle-month-map
  '(("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Nov" . 11)
    ("Dec" . 12))
  "Assoc list mapping month abbreviations to month numbers")

(defconst eagle-hash-at-regex "\\([#@][a-zA-Z0-9_.]+\\)"
  "Regular expression form for matching hashtags (#) and directions (@).")

(defconst eagle-url-regex "\\(http://[a-zA-Z0-9.]+\.[a-zA-Z0-9%#;~/.=+&$,?@-]+\\)"
   "Regular expression for urls.")

(defconst eagle-emacs-lisp-regex "\\([a-zA-Z0-9-.]+\\)\\.el"
  "Regex for Emacs Lisp files.")

(defconst eagle-max-tweet 140 "Maximum length of a tweet.")

(defvar eagle-status-compiled-format nil
  "Compiled format of `eagle-status-format'.")

(defvar eagle-tcrt-status nil
  "Status of `eagle-tcrt'.

Nil means have no timing check recent tweets,
and non-nil means already enable function `eagle-tcrt-status'.")

(defvar eagle-need-timing-check nil
  "Need enter `eagle-tcrt-mode' or not.

This variable used for `eagle-show-recent-tweets-async-timing-check'.")

(defvar eagle-last-tweet '()
  "The last tweet that was posted.
This is a bit of an ugly hack to store the last tweet that was shown through
`eagle-write-recent-tweets'.

It is in the format of (timestamp user-id message)")

(defvar eagle-mode-map (make-sparse-keymap) "Keymap for `eagle-mode'.")
(let ((map eagle-mode-map))
  (define-key map (kbd "C-k")     'eagle-remove-friend)
  (define-key map (kbd "a")       'eagle-add-friend)
  (define-key map (kbd "A")       'eagle-analyse-user)
  (define-key map (kbd "S")       'eagle-search)
  (define-key map (kbd "G")       'eagle-analyse-graph-user)
  (define-key map (kbd "g")       'eagle-show-recent-tweets-async)
  (define-key map (kbd "n")       'eagle-next-tweet)
  (define-key map (kbd "p")       'eagle-previous-tweet)
  
  (define-key map (kbd "d")       'eagle-direct)
  (define-key map (kbd "N")       'eagle-post)
  (define-key map (kbd "t")       'eagle-post-to)
  (define-key map (kbd "C-c n")   'eagle-post-url)
  (define-key map (kbd "r")       'eagle-post-loud-reply)
  (define-key map (kbd "R")       'eagle-post-reply)
  (define-key map (kbd "c")       'eagle-post-retweet)

  (define-key map (kbd "C-c C-a") 'eagle-show-at-tweets)
  (define-key map (kbd "SPC")     'scroll-up)
  (define-key map (kbd "u")       'View-scroll-page-backward)
  (define-key map (kbd "e")       'move-end-of-line)
  (define-key map (kbd "f")       'eagle-show-followers)
  (define-key map (kbd "F")       'eagle-show-friends)
  (define-key map (kbd "h")       'backward-char)
  (define-key map (kbd "l")       'forward-char)
  (define-key map (kbd "j")       'next-line)
  (define-key map (kbd "k")       'previous-line)
  (define-key map (kbd "J")       'roll-down)
  (define-key map (kbd "K")       'roll-up)
  (define-key map (kbd "o")       'other-window)
  (define-key map (kbd "O")       'eagle-open-link)

  (define-key map (kbd "1")       'delete-other-windows)
  (define-key map (kbd "q")       'bury-buffer)
  (define-key map (kbd "Q")       'kill-this-buffer)
  (define-key map (kbd "<")       'beginning-of-buffer)
  (define-key map (kbd ">")       'end-of-buffer)
  (define-key map (kbd "G")       'end-of-buffer))

(defvar eagle-is-recent-tweets-first-page nil
  "Current is display first page of recent tweets or not.")

(defvar eagle-user-image-list 'nil "List containing all user images.")

(defconst eagle-text-logo "eagle" "Text log of eagle.")

;; must do this
(put 'eagle-tcrt-mode-line-format 'risky-local-variable t)

(setq minor-mode-alist
      (append
       `((eagle-tcrt-mode " ")
         (eagle-tcrt-mode ,eagle-tcrt-mode-line-format))
       (delq (assq 'eagle-tcrt-mode minor-mode-alist) minor-mode-alist)))

(defun eagle-write-title (title &rest args)
  "Helper function to write out a title bar for a eagle buffer.

TITLE is the title to display, and it is formatted with ARGS."
  (setq header-line-format
        (propertize (apply 'format title args) 'face 'eagle-title-face)))

(defun eagle-show-recent-tweets (page)
  "Run like `eagle-show-recent-tweets', but prompt user to input if user's `eagle-password' is not set."
  (interactive "p")
  (eagle-set-username-password)
  (switch-to-buffer
   (eagle-write-recent-tweets-with-buffer
    (twitter4e-get-home-timeline page)
    page)))

(defun eagle-show-recent-tweets-timing-check (&optional page)
  "Set up a timer, and show you the most recent tweets approx every 90 seconds.

You can change the time between each check by customizing
`eagle-follow-idle-interval'."
  (interactive "P")
  (setq eagle-is-recent-tweets-first-page (= page 1))
  (eagle-show-recent-tweets page)
  (eagle-tcrt-mode 1))

(defun eagle-show-recent-tweets-async (page)
  "Run like `eagle-show-recent-tweets', but it is asynchronously."
  (interactive "p")
  (eagle-set-username-password)
  (twitter4e-get-home-timeline page 'eagle-show-recent-tweets-async-callback page))

(defun eagle-show-recent-tweets-async-timing-check (page)
  "Like `eagle-show-recent-tweets-timing-check', but is asynchronously."
  (interactive "p")
  (unless eagle-use-tcrt-mode
      (setq eagle-need-timing-check t))
  (setq eagle-is-recent-tweets-first-page (= page 1))
  (eagle-show-recent-tweets-async page)
  (if eagle-use-tcrt-mode
    (eagle-tcrt-mode 1)))

(defun eagle-show-recent-tweets-async-callback (status url xml page)
  "Callback function, called by `eagle-follow-recent-tweets-timer-function'.

STATUS, URL and XML are all set by `eagle-follow-recent-tweets-timer-function'."
  (when (not status)
    (switch-to-buffer
     (eagle-write-recent-tweets-with-buffer xml page))
    (unless eagle-use-tcrt-mode
      (when eagle-need-timing-check
        (eagle-tcrt-mode 1)
        (setq eagle-need-timing-check nil)))))

(defun eagle-follow-recent-tweets-async-callback (status url xml)
  "Callback function, called by `eagle-follow-recent-tweets-timer-function'.

STATUS, URL and XML are all set by `eagle-follow-recent-tweets-timer-function'."
  (when (not status)
    (save-window-excursion
      (save-excursion
        (eagle-write-recent-tweets-with-buffer xml)))))

(defun eagle-follow-recent-tweets-timer-function ()
  "Timer function for recent tweets, called via a timer."
  (twitter4e-get-home-timeline 1 'eagle-follow-recent-tweets-async-callback))

(defun eagle-write-recent-tweets-with-buffer (xml page)
  "Write recent tweets XML.

This function return buffer `eagle-recent-tweets-buffer-name',
if page PAGE is nil, it default to 1."
  (with-eagle-buffer
    eagle-recent-tweets-buffer-name
    (eagle-enter-eagle-mode)
    (eagle-write-title
     eagle-recent-tweets-header-format
     page
     (format-time-string eagle-cur-time-format))
    (eagle-write-tweets xml)))

(defmacro with-eagle-buffer (buffer-name &rest forms)
  "Create a twitter buffer with name BUFFER-NAME, and execute FORMS.

The value returned is the current buffer."
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (buffer-disable-undo)
     (toggle-read-only 0)
     (delete-region (point-min) (point-max))
     ,@forms
     (set-buffer-modified-p nil)
     (toggle-read-only 1)
     (goto-char (point-min))
     (current-buffer)))

(define-derived-mode eagle-mode text-mode
  "eagle"
  "Major mode for twitter.

  \\{eagle-mode-map}")

(defun eagle-enter-eagle-mode ()
  "Enter `eagle-mode'."
  (unless (eq major-mode 'eagle-mode)
     (eagle-mode)))

(define-minor-mode eagle-tcrt-mode
  "Toggle timing check recent tweets."
  :group 'eagle
  :global t
  (if eagle-tcrt-mode
      (unless eagle-tcrt-status
        (setq eagle-timer
              (run-with-timer
               eagle-follow-idle-interval
               eagle-follow-idle-interval
               'eagle-follow-recent-tweets-timer-function)))
    (when eagle-tcrt-status
      (cancel-timer eagle-timer)))
  (setq eagle-tcrt-status eagle-tcrt-mode))

(defun eagle-tcrt-mode-on ()
  "Trun on `eagle-tcrt-mode'."
  (interactive)
  (eagle-tcrt-mode 1))

(defun eagle-tcrt-mode-off ()
  "Trun off `eagle-tcrt-mode'."
  (interactive)
  (eagle-tcrt-mode -1))

(defun eagle-tcrt-mode-off-right ()
  "Trun off `eagle-tcrt-mode'
only when `major-mode' is `eagle-mode'."
  (if (eq major-mode 'eagle-mode)
      (eagle-tcrt-mode -1)))

(defun eagle-show-at-tweets ()
  "Show @ tweets."
  (interactive)
  (eagle-set-password)
  (with-current-buffer
      (call-interactively 'eagle-show-at-tweets)
    (delete-other-windows)))

(defun eagle-compile-format-string (format-string)
  "Converts FORMAT-STRING into a list that is easier to scan.
See twitter-status-format for a description of the format. The
returned list contains elements that are one of the following:

- A string. This should be inserted directly into the buffer.

- A four element list like (RIGHT-PAD WIDTH COMMAND
  PROPERTIES). RIGHT-PAD is t if the - flag was specified or nil
  otherwise. WIDTH is the amount to pad the string to or nil if
  no padding was specified. COMMAND is an integer representing
  the character code for the command. PROPERTIES is a list of
  text properties that should be applied to the resulting
  string."
  (let (parts last-point)
    (with-temp-buffer
      (insert format-string)
      (goto-char (point-min))
      (setq last-point (point))
      (while (re-search-forward "%\\(-?\\)\\([0-9]*\\)\\([a-zA-Z%]\\)" nil t)
        ;; Push the preceeding string (if any) to copy directly into
        ;; the buffer
        (when (> (match-beginning 0) last-point)
          (push (buffer-substring last-point (match-beginning 0)) parts))
        ;; Make the three element list describing the command
        (push (list (> (match-end 1) (match-beginning 1)) ; is - flag given?
                    (if (> (match-end 2) (match-beginning 2)) ; is width given?
                        (string-to-number (match-string 2)) ; extract the width
                      nil) ; otherwise set to nil
                    ;; copy the single character for the command number directly
                    (char-after (match-beginning 3))
                    ;; extract all of the properties so they can be
                    ;; copied into the final string
                    (text-properties-at (match-beginning 0)))
              parts)
        ;; Move last point to the end of the last match
        (setq last-point (match-end 0)))
      ;; Add any trailing text
      (when (< last-point (point-max))
        (push (buffer-substring last-point (point-max)) parts)))
    (nreverse parts)))

(defun eagle-get-attrib-node (node attrib)
  "Get the text of a child attribute node.
If the children of XML node NODE are formatted like
<attrib1>data</attrib1> <attrib2>data</attrib2> ... then this
fuction will return the text of the child node named ATTRIB or
nil if it isn't found."
  (let ((child (xml-get-children node attrib)))
    (if (consp child)
        (eagle-get-node-text (car child))
      nil)))

(defun eagle-get-node-text (node)
  "Return the text of XML node NODE.
All of the text elements are concatenated together and returned
as a single string."
  (let (text-parts)
    (dolist (part (xml-node-children node))
      (when (stringp part)
        (push (eagle-decode-entity-encoding part) text-parts)))
    (apply 'concat (nreverse text-parts))))

(defun eagle-decode-entity-encoding (str)
  (let (result)
    (setq result (replace-regexp-in-string "&gt;" ">" str))
    (setq result (replace-regexp-in-string "&lt;" "<" result))))

(defun eagle-time-to-time (time)
  "Convert TIME to a number of seconds since some epoch."
  (let ((case-fold-search t))
    (unless (string-match
             (concat "\\`[a-z]\\{3\\} "
                     "\\([a-z]\\{3\\}\\) "
                     "\\([0-9]\\{1,2\\}\\) "
                     "\\([0-9]\\{2\\}\\):"
                     "\\([0-9]\\{2\\}\\):"
                     "\\([0-9]\\{2\\}\\) "
                     "\\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\) "
                     "\\([0-9]\\{4\\}\\)\\'") time)
      (error "Invalid time string: %s" time))
    (encode-time (string-to-number (match-string 5 time))
                 (string-to-number (match-string 4 time))
                 (string-to-number (match-string 3 time))
                 (string-to-number (match-string 2 time))
                 (cdr (assoc (match-string 1 time) eagle-month-map))
                 (string-to-number (match-string 8 time))
                 (concat (match-string 6 time) ":" (match-string 7 time)))))

(defun eagle-format-time-for-display (time)
  "Convert TIME to a friendly human readable string.
TIME should be a high/low pair as returned by encode-time."
  ;; This is based on a similar function from Tweet
  (let* ((now (current-time))
         (age (subtract-time now time))
         (age-days (- (time-to-days now) (time-to-days time))))
    (if (or (< (car age) 0)
            (>= (car age) 16) ; more than about 12 days
            (>= age-days 7))
        (format-time-string "%x at %H:%M" time)
      (let* ((age-seconds (logior (lsh (car age) 16) (cadr age)))
             (age-minutes (/ age-seconds 60))
             (age-hours (/ age-minutes 60)))
        (cond ((< age-seconds 60)
               "Less than a minute ago")
              ((<= age-minutes 1)
               "About a minute ago")
              ((< age-minutes 60)
               (format "About %d minutes ago" age-minutes))
              ((<= age-hours 1)
               "About an hour ago")
              ((< age-minutes 360)
               (format "About %d hours ago" age-hours))
              ((<= age-days 0)
               (format-time-string "Today at %H:%M" time))
              ((<= age-days 1)
               (format-time-string "Yesterday at %H:%M" time))
              (t
               (format-time-string "Last %A at %H:%M" time)))))))

(defun eagle-reply (pos)
  "Sets up a status edit buffer to reply to the message at POS.
eagle-reply-status-id is set to the id of the status
corresponding to the status so that it will be marked as a
reply. The status' screen name is initially entered into the
buffer.

When called interactively POS is set to point."
  (interactive "d")
  (let ((status-screen-name (get-text-property pos 'eagle-status-screen-name))
        (status-id (get-text-property pos 'eagle-status-id)))
    (when (null status-screen-name)
      (error "Missing screen name in status"))
    (when (null status-id)
      (error "Missing status id"))
    (eagle-status-edit)
    (setq eagle-reply-status-id status-id)
    (insert "@" status-screen-name " ")))

(defun eagle-reply-button-pressed (button)
  "Calls eagle-reply for the position where BUTTON is."
  (eagle-reply (overlay-start button)))

(defun eagle-insert-image (status-node)
  "Insert user's image to buffer."
  (let* ((user-node (eagle-get-user-node status-node))
         (screenname (eagle-get-screenname status-node))
         (user-img (eagle-get-user-image (xml-first-childs-value user-node 'profile_image_url) screenname)))
    (when user-img
      (insert-image user-img))))

(defun eagle-insert-tweet-time (status-node)
  "Insert tweet time to buffer."
  (let ((val (eagle-get-attrib-node status-node 'created_at)))
    (when val
      (cond
       ((stringp eagle-time-format)
        (insert (format-time-string eagle-time-format (eagle-time-to-time val))))
       ((functionp eagle-time-format)
        (insert (funcall eagle-time-format (eagle-time-to-time val))))
       ((null eagle-time-format)
        (insert val))
       (t (error "Invalid value for `eagle-time-format'"))))))

(defun eagle-insert-source (status-node)
  "Insert tweet's source to buffer."
  (let ((link-source (eagle-get-attrib-node status-node 'source))
        source)
    (when link-source
      (with-temp-buffer
        (insert link-source)
        (setq source (eagle-get-node-text (car (xml-parse-region (point-min) (point-max))))))
      (when (or (null source) (string= source "")) (setq source link-source))
      (insert source))))

(defun eagle-insert-tweet-attribute (status-node command)
  "Insert tweet's attribute to buffer."
  (let ((user-node (xml-first-child status-node 'user))
        val elem)
    (cond ((setq elem (assoc command eagle-user-commands))
           (setq val (eagle-get-attrib-node user-node (cdr elem))))
          ((setq elem (assoc command eagle-status-commands))
           (setq val (eagle-get-attrib-node status-node (cdr elem)))))
    (when val
      (insert val))))

(defun eagle-fill-string (str)
  "Run `fill-region' on string STR."
  (with-temp-buffer
    (fill-region (prog1 (point) (insert str)) (point))
    (buffer-substring (point-min) (point-max))))

(defun eagle-insert-tweet-message (status-node command)
  "Insert tweet's message to buffer."
  (let ((val (eagle-get-attrib-node status-node 'text)))
    (when (and val (= command ?M))
      (setq val (eagle-fill-string val)))
    (insert (eagle-keymap-and-fontify-message val))))

(defun eagle-insert-status-part-for-command (status-node command)
  "Extract the string for COMMAND from STATUS-NODE and insert.
The command should be integer representing one of the characters
supported by eagle-status-format."
  (case command
    (?t (eagle-insert-tweet-time status-node))
    ;; ((= command ?r)
    ;;  (insert-button "reply"
    ;;                 'action 'eagle-reply-button-pressed))
    (?p (eagle-insert-image status-node))
    ((?m ?M) (eagle-insert-tweet-message status-node command))
    (?s (eagle-insert-source status-node))
    (?% (insert ?%))
    (t (eagle-insert-tweet-attribute status-node command))))

(defun eagle-get-user-node (status-node)
  "Get user node by status node STATUS-NODE."
  (or (xml-first-child status-node 'user) (xml-first-child status-node 'sender)))

(defun eagle-get-screenname (user-node)
  "Get user screen name by status node STATUS-NODE."
  (or (xml-first-childs-value user-node 'screen_name) eagle-unknown-screenname))

(defun eagle-write-tweet (status-node &optional filter-tweets times-through)
  "Insert a tweet STATUS-NODE into the current buffer.
TWEET should be an xml parsed node, which could be a message or a status node.
FILTER-TWEETS is an optional boolean to disregard filtering.
TIMES-THROUGH is an integer representing the number of times a tweet has been
  displayed, for zebra-tabling."
  (dolist (element eagle-status-compiled-format)
    (let ((part-start (point))
          properties command)
      (if (stringp element)
          (progn
            (setq properties (text-properties-at 0 element))
            (insert (substring-no-properties element)))
        (let ((right-pad (nth 0 element))
              (padding (nth 1 element)))
          (setq command (nth 2 element))
          (setq properties (nth 3 element))
          (eagle-insert-status-part-for-command status-node command)
          (when (and padding
                     (< (- (point) part-start) padding))
            (setq padding (make-string
                           (+ padding (- part-start (point))) ? ))
            (if right-pad
                (insert padding)
              (let ((part-end (point)))
                (goto-char part-start)
                (insert padding)
                (goto-char (+ part-end (length padding))))))))
      (let ((overlay (make-overlay part-start (point)))
            (key (cdr (or (assoc command eagle-user-commands) (assoc command eagle-status-commands)))))
        (overlay-put overlay 'face properties)
        (if key
            (overlay-put overlay 'key key))
        (let* ((tweet-id (xml-first-childs-value status-node 'id))
               (user-node (eagle-get-user-node status-node))
               (screenname (eagle-get-screenname user-node)))
          (overlay-put overlay 'eagle-tweet-id tweet-id)
          (overlay-put overlay 'eagle-tweet-username screenname))))))

(defun eagle-write-tweets (xml-data)
  "Function that writes the recent tweets to the buffer.

XML-DATA is the sxml (with http header)."
  (if (not xml-data)
      (message "No xml data.")
    (if (twitter4e-header-error-p (twitter4e-parse-header (car xml-data)))
        (eagle-display-error xml-data)
	  (let* ((status-nodes (cadr xml-data))
             (first-tweet (xml-first-child status-nodes 'status))
			 (most-recent-tweet (list (xml-first-childs-value first-tweet 'created_at)
									  (eagle-get-screenname (xml-first-child first-tweet 'user))
									  (xml-substitute-special (xml-first-childs-value first-tweet 'text))))
			 (times-through 1))
		(dolist (status-node (xml-get-children status-nodes 'status))
          (let ((message (xml-substitute-special (xml-first-childs-value status-node 'text)))
                (author (xml-first-childs-value (or (xml-first-child status-node 'user)
                                                    (xml-first-child status-node 'sender))
                                                'name)))
            (when (not (eagle-filter-tweet message author))
              (eagle-write-tweet status-node nil times-through)
              (setq times-through (+ 1 times-through)))))
        
        (when (not (equal most-recent-tweet eagle-last-tweet))
          (setq eagle-last-tweet most-recent-tweet)
          (run-hooks 'eagle-new-tweet-hook))))
    
    ;; this needs more TLC
    (if eagle-debug-mem (message (garbage-collect)))))

(defun eagle-todochiku ()
  "Helper function for use by the todochiku package."
  (todochiku-message
   eagle-text-logo
   (format "From %s:\n%s" (cadr eagle-last-tweet) (caddr eagle-last-tweet))
   (todochiku-icon 'social)))

(defun eagle-filter-tweet (message author)
  "Return t if the user wants MESSAGE filtered, nil if not."
  (when (> eagle-filter-diarrhea 0)
        (if (string-equal author (car eagle-last-author))
            (setcdr eagle-last-author (+ 1 (cdr eagle-last-author)))
            (setq eagle-last-author (cons author 1))))
  (not (and  (or (string-equal "" eagle-filter-tweets-regex)
                 (null eagle-filter-tweets-regex)
                 (not (string-match eagle-filter-tweets-regex message)))
             (or (not eagle-filter-at-tweets)
                 (not (string-match "@" message))
                 (string-match eagle-filter-at-tweets-retweet-regex message)
                 (and eagle-filter-at-tweets
                      (eagle-at-message-was-from-friend message)))
             (or (= 0 eagle-filter-diarrhea)
                 (>= eagle-filter-diarrhea
                     (cdr eagle-last-author))))))

(defun eagle-twitter-username-password-is-set (value)
  "Username or pass is set or not.

t means set, and nil no set."
  (not (or (not value) (string= "" value))))

(defun eagle-set-username-password ()
  "Check set twitter's password.
 If have not set, just prompt user to input password, and set."
  (unless (eagle-twitter-username-password-is-set eagle-twitter-username)
    (call-interactively 'eagle-set-username))
  (unless (eagle-twitter-username-password-is-set eagle-password)
    (call-interactively 'eagle-set-password)))

(defun eagle-set-username (username)
  "Set twitter username."
  (interactive "MInput your twitter username: ")
  (setq eagle-twitter-username username))

(defun eagle-set-password (pass)
  "Set twitter's password."
  (interactive
   (list
    (read-passwd
     (format "Input your twitter password of account %s: " eagle-twitter-username))))
  (twitter4e-set-auth eagle-twitter-username pass)
  (setq eagle-password t))

(defun eagle-set-api-protocol (protocol)
  "Set twitter api protocol."
  (interactive "MProtocol to use: ")
  (twitter4e-set-api-protocol protocol)
  (eagle-set-api-domain twitter4e-api-domain))

(defun eagle-set-status-format (&optional format)
  "set `eagle-status-format'."
  (interactive)
  (if format (setq eagle-status-format format))
  (setq eagle-status-compiled-format (eagle-compile-format-string eagle-status-format)))

(defun eagle-display-error (xml)
  "Given an XML fragment that contain an error, display it to the user."
  (let ((header (twitter4e-parse-header (car xml))))
    (when (twitter4e-header-error-p header)
       (twitter4e-insert-with-overlay-attributes
         "(_x___}<"
         '((face "twitter4e-fail-whale-face")))
       (twitter4e-insert-with-overlay-attributes
          (concat "         HTTP ERROR!  "
                  "(" (cadr header) ") "
                  (caddr header) "\n\n"
                  "  "(twitter4e-get-header-error header) "\n\n"
                  "  The response from twitter was: "
                  (format "%s" (xml-first-childs-value (cadr xml) 'error))
                  "\n\n")
          '((face "twitter4e-error-face"))))))

(defun eagle-get-header-error (header)
   "Given a parsed HEADER from `twitter4e-parse-header', return human readable error."
   (if (null header)
       "Null header, probably an error with twit.el."
       (case (string-to-number (cadr header))
         ((200) "Everything is A OK!")
         ((304) "Nothing Changed.")
         ((400) "Bad Request. (probably rate limited)")
         ((401) "Not Authorized.  You need to log in, or your login/password is incorrect.\nIf this is the first time you have tried to use a twitter command\nenter your password and try again.")
         ((403) "You are FORBIDDEN")
         ((404) "Not Found.  404'ed!")
         ((406) "Something is bropken with twit.el's search!")
         ((500) "Something is horribly broken with twit.el, or even Twitter!")
         ((502) "Twitter is down. FAIL WHALE!")
         ((503) "Rate limited on search."))))

(defun eagle-keymap-and-fontify-message (message)
  "Scan through MESSAGE, and fontify and keymap all #foo and @foo."
  (let ((original-txt (substring message 0))) ;; Just to be sure we're using a copy
    (when (string-match eagle-hash-at-regex message) ;; usernames
      (setq message (replace-regexp-in-string
                     eagle-hash-at-regex
                     (lambda (str)
                       (let ((type (substring str 0 1))
                             (thing (substring str 1)))
                         (setq str (propertize str
                                               'face 'eagle-hash-at-face
                                               'pointer 'hand))
                         (when (string-equal "@" type)
                           (setq str (propertize str 'eagle-user thing)))
                         (propertize str 'eagle-search (concat type thing))))
                     message)))

    (when (string-match eagle-url-regex message) ;; URLs
      (setq message (replace-regexp-in-string
                     eagle-url-regex
                     (lambda (str)
                       (let ((map (make-sparse-keymap)))
                         (define-key map [enter] 'eagle-visit-link)
                         (define-key map [(control) (enter)] 'eagle-visit-link)
                         (define-key map [mouse-1] 'eagle-visit-link)
                         (define-key map [mouse 2] 'eagle-visit-link)
                         (define-key map [mouse-3] 'eagle-visit-link)
                         (propertize str
                                     'face 'eagle-url-face
                                     'pointer 'hand
                                     'eagle-url str
                                     'keymap map)))
                     message)))
    (when (string-match eagle-emacs-lisp-regex message) ;; .el's
      (setq message (replace-regexp-in-string
                     eagle-emacs-lisp-regex
                     (lambda (str)
                       (propertize str
                                   'face 'eagle-url-face
                                   'elisp str))
                     message)))

    ;; message content (plaintext)
    (propertize message 'eagle-message original-txt)))

(defun eagle-get-user-image (url user-id)
  "Retrieve the user image from the list, or from the URL.
USER-ID must be provided."
  (let ((img (assoc url eagle-user-image-list)))
    (if (and img (not (bufferp (cdr img))))
        (cdr (assoc url eagle-user-image-list))
        (if (file-exists-p (concat eagle-user-image-dir
                                   "/" user-id "-"
                                   (file-name-nondirectory url)))
            (let ((img (create-image
                        (concat eagle-user-image-dir ;; What's an ana for? lol
                                "/" user-id "-"
                                (file-name-nondirectory url)))))
              (add-to-list 'eagle-user-image-list (cons url img))
              img)
            (let* ((url-request-method "GET")
                   (url-show-status nil)
                   (url-buffer (url-retrieve url 'eagle-write-user-image
                                             (list url user-id))))
              (if url-buffer
                  (progn
                    (add-to-list 'eagle-user-image-list (cons url url-buffer))
                    (if eagle-debug (message "Added image. List is %s" eagle-user-image-list)))
                  (eagle-alert (format "Warning, couldn't load %s " url)))
              nil)))))

(defun eagle-write-user-image (status url user-id)
  "Called by `eagle-get-user-image', to write the image to disk.

STATUS, URL and USER-ID are all set by `url-retrieve'."
  (let ((image-file-name
         (concat eagle-user-image-dir
                 "/" user-id "-"
                 (file-name-nondirectory url))))
    (when (not (file-directory-p eagle-user-image-dir))
      (make-directory eagle-user-image-dir))
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\C-j\C-j"))
    (save-buffer)
    (delete (cons url (current-buffer)) eagle-user-image-list)
    (kill-buffer (current-buffer))
    (add-to-list 'eagle-user-image-list (cons url (create-image image-file-name)))))

(defun eagle-insert-with-overlay-attributes (text attributes &optional prefix justify)
  "Helper function to insert text into a buffer with an overlay.

Inserts TEXT into buffer, add an overlay and apply ATTRIBUTES to the overlay."
  (let ((start (point)))
    (insert text)
    (let ((overlay (make-overlay start (point))))
      (dolist (spec attributes)
        (overlay-put overlay (car spec) (cdr spec))))))

(defun eagle-alert (msg &optional title)
  "Send some kind of alert MSG to the user, with the title TITLE.

If todochiku is available, use that.  Instead, just message the user."
  (when (null title) (setq title twitter4e-alert-default-title))
  (when (featurep 'todochiku)
    (todochiku-message title msg (todochiku-icon 'social)))
  (message "%s: %s" title msg))

(defun eagle-set-api-domain (domain)
  "Set twitter api domain."
  (interactive "MTwitter api domain to use: ")
  (twitter4e-set-api-domain domain)
  ;; reset password for prompting user to reinput it later
  (setq eagle-password nil))

(defun eagle-next-tweet (&optional arg)
  "Move forward to the next tweet.

With argument ARG, move to the ARGth next tweet."
  (interactive "p")
  (mapc (lambda (n)
          (goto-char (next-single-char-property-change (point) 'eagle-tweet-id nil
                                                       (point-max))))
        (number-sequence 1 (or arg 1))))

;;* interactive nav
(defun eagle-previous-tweet (&optional arg)
  "Move backward to the previous tweet.

With argument ARG, move to the ARGth previous tweet."
  (interactive "p")
  (mapc (lambda (n)
          (goto-char (previous-single-char-property-change (point) 'eagle-tweet-id nil
                                                           (point-min))))
        (number-sequence 1 (or arg 1))))

;;* interactive direct
;;;###autoload
(defun eagle-direct (user msg)
  "Send USER a direct message MSG.

If you are currently positioned over a tweet, then it will fill in the author
of that tweet as the default recipient.

This will attempt to do a completing read based on the people you
are following, if you have images turned on."
  (interactive
   (list (eagle-read-friend "Direct Message To: " t)
         (eagle-query-for-post "Message: " "")))
  (if (> (length msg) eagle-max-tweet)
      (error eagle-too-long-msg)
      (eagle-direct-message user msg)))

;;* interactive direct multi-account
(defun eagle-direct-with-account (account)
  "Send a user a direct tweet with `eagle-direct' under a different ACCOUNT."
  (interactive (list (eagle-read-account)))
  (with-eagle-account account
    (eagle-direct (eagle-read-friend "Direct Message To: " t)
                 (read-string "Message: "))))

;;* post interactive tweet
;;;###autoload
(defun eagle-post (prefix)
  "Send a post to twitter.com.
Prompt the first time for password and username \(unless
`eagle-user' and/or `eagle-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= `eagle-max-tweet' chars
long.

A PREFIX argument will prompt you for your post in reply to a
specific author that the cursor is nearest to.  This behavior may
become deprecated."
  (interactive "P")
  (let* ((reply-to (when prefix
                     (eagle-get-text-property 'eagle-user)))
         (post (eagle-query-for-post
                (if reply-to
                    (concat "Reply to " reply-to)
                    "Post")
                (when reply-to
                  (concat "@" reply-to " ")))))
    (if (> (length post) eagle-max-tweet)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

;;* post interactive tweet multi-account
(defun eagle-post-with-account (account post)
  "Like `eagle-post' but under a different ACCOUNT POST a tweet."
  (interactive (list (eagle-read-account)
                     (eagle-query-for-post "Post: " "")))
  (with-eagle-account account
    (if (> (length post) eagle-max-tweet)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

;;* post interactive keymap
(defun eagle-post-to ()
  "Posts to a particular user.  Mostly used by keymaps."
  (interactive)
  (let ((post (eagle-query-for-post "Reply To: " (concat "@" (eagle-get-text-property 'eagle-user) " "))))
    (if (> (length post) eagle-max-tweet)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

;;* post reply interactive
(defun eagle-post-reply ()
  "Reply to a status on twitter.com."
  (interactive)
  (let* ((reply-to (eagle-get-text-property 'eagle-user))
         (parent-id (eagle-get-text-property 'eagle-id))
         (post (eagle-query-for-post (concat "Reply to " reply-to)
                                    (concat "@" reply-to " "))))
    (if (> (length post) 140)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post parent-id))))

;;* post "loud" reply interactive
(defun eagle-post-loud-reply ()
  "Reply to a status on twitter.com.

When you write @foo, only those followers of yours who are also
following @foo will see the reply. To get around this, if you
want all of your followers to see the reply anyways begin the
tweet with \".@\" or some other filler character."
  (interactive)
  (let* ((reply-to (eagle-get-text-property 'eagle-user))
         (parent-id (eagle-get-text-property 'eagle-id))
         (post (eagle-query-for-post (concat "Loud reply to " reply-to)
                                    (concat ".@" reply-to " "))))
    (if (> (length post) 140)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post parent-id))))

;;* post retweet
;;  Begins a post with "RT @foo: waka waka waka || "
(defun eagle-post-retweet ()
  "Retweet someone else's post."
  (interactive)
  (let* ((reply-to (eagle-get-text-property 'eagle-user))
         (parent-id (eagle-get-text-property 'eagle-id))
         (retweet-text (eagle-get-text-property 'eagle-message))
         (post (eagle-query-for-post
                (concat "Retweeting " reply-to)
                (concat "RT @" reply-to ": " retweet-text " || "))))
    (if (> (length post) 140)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post parent-id))))

(defun eagle-get-text-property (propname)
  "Return a property named PROPNAME or nil if not available.

This is the reverse of `get-char-property', it checks text properties first."
  (or (get-text-property (point) propname)
      (get-char-property (point) propname)))

(defun eagle-query-for-post (prompt-heading initial-input)
  "Query for a Twitter.com post text in the minibuffer.

PROMPT-HEADING is the prompt, and has \" (140 char max): \" appended to it.
INITIAL-INPUT is what it is."
  (let ((minibuffer-setup-hook
         (cons #'eagle--query-for-post-minibuffer-setup minibuffer-setup-hook)))
    (read-string (concat prompt-heading
                         (format " (  0/%3d characters): "
                                 eagle-max-tweet))
                 initial-input)))

;;* post url
;;  Prompts for a URL, then compresses it and starts a tweet with the shortened URL in the body
(defun eagle-post-url ()
  "Compress a URL, then start posting a tweet with the result."
  (interactive)
  (let* ((url (compress-url (read-string "URL: ")))
         (post (eagle-query-for-post "Post" url)))
    (if (> (length post) eagle-max-tweet)
        (error eagle-too-long-msg)
        (eagle-post-status eagle-update-url post))))

(defun eagle--query-for-post-minibuffer-setup ()
  "Prepare the minibuffer for a twit entry.
Limit main field length to `eagle-max-tweet' characters"

  (eagle--query-for-post-update)
  (local-set-key [remap exit-minibuffer]
  #'eagle--query-for-post-exit-minibuffer)
  (add-hook 'after-change-functions
  #'eagle--query-for-post-update t t))

(defun eagle--query-for-post-exit-minibuffer ()
  (interactive)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin)))

    (if (<= field-length eagle-max-tweet)
        (exit-minibuffer)
        (beep)
        (eagle--query-for-post-update nil nil nil t)
        (sit-for 1)
        (eagle--query-for-post-update nil nil nil))))

(defun eagle--query-for-post-update (&optional beg end length invert)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin))
         overlay)

    ;; remove old overlays
    (mapcar #'(lambda (overlay)
                (when (overlay-get overlay 'eagle--query-for-post)
                  (delete-overlay overlay)))
            (overlays-in (point-min) (point-max)))

    ;; if necessary, add a new one
    (when (> field-length eagle-max-tweet)
      (setq overlay (make-overlay (+ field-begin eagle-max-tweet) field-end))
      (overlay-put overlay 'face 'eagle-too-long-face)
      (overlay-put overlay 'eagle--query-for-post t))

    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "(\\(  0\\)/[0-9]+ characters)" nil t)
        (setq overlay (make-overlay (match-beginning 1)
                                    (match-end 1)))

        (overlay-put overlay 'eagle--query-for-post t)
        (overlay-put overlay 'display (format "%3d" field-length))

        (let ((face
               `(:foreground
                 ,(if (<= field-length eagle-max-tweet) "green" "red"))))

          (when invert
            (setq face (append '(:inverse-video t) face)))

          (overlay-put overlay 'face face))))))

(setq eagle-user-image-dir "~/.twitter/images")
(make-directory eagle-user-image-dir t)

(eagle-set-status-format)

(add-hook 'kill-buffer-hook 'eagle-tcrt-mode-off-right)

(provide 'eagle)

;;; eagle.el ends here
