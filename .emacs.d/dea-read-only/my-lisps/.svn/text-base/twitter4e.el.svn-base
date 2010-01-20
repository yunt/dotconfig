;;; twitter4e.el --- Twitter for Emacs Lisp

;; Copyright (C) 2009 ahei

;; Time-stamp: <2010-01-09 21:37:26 Saturday by ahei>
;; Author: ahei <ahei0802@126.com>
;; Keywords: twitter

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

(defgroup twitter4e nil
  "Group of twitter4e."
  :prefix "twitter4e-")

(defcustom twitter4e-protocol "http"
  "Which protocol to use for twitter.

If you use http, the requests will be much faster, and there will be
a lot less messages at the bottom of the screen.   However, your
password will be sent plaintext.

If you use https, it is noisier, and slower, but (obviously) secure.

Note, if you change this, you will need to restart emacs for it to
take effect."
  :type '(choice (const :tag "http" "http")
                 (const :tag "https" "https"))

  :group 'twit)

(defcustom twitter4e-api-domain "twip.fishnote.net"
  "Twitter api domain."
  :type 'string
  :group 'twitter4e)

(defcustom twitter4e-http-port 80
  "Http port."
  :type 'integer
  :group 'twitter4e)

(defcustom twitter4e-https-port 443
  "Https port."
  :type 'integer
  :group 'twitter4e)

;;*const url analyse
(defcustom twitter4e-analyse-user-url "http://twanalyst.com/%s"
  "Url for twanalyst.")
(defcustom twitter4e-analyse-graph-user-url "http://twanalyst.com/%s/track"
  "Url for twanlyst graph.")
(defcustom twitter4e-analyse-suggest-user-url "http://twanalyst.com/%s/suggest"
  "Url for twanlyst suggest")

(defcustom twitter4e-alert-default-title "twitter4e"
  "Default title for `twitter4e-alert'."
  :type 'string
  :group 'twitter4e)

(defcustom twitter4e-debug nil
  "Whether or not to run twitter4e.el in debug mode."
  :type 'boolean
  :group 'twitter4e)

(defconst twitter4e-method-get  "GET"  "Get method.")
(defconst twitter4e-method-post "POST" "Post method.")

(defun twitter4e-set-api-protocol (protocol)
  "Set twitter api protocol."
  (interactive "MProtocol to use: ")
  (setq twitter4e-protocol protocol)
  (twitter4e-set-api-domain twitter4e-api-domain))

(defun twitter4e-set-api-domain (domain)
  "Set twitter api domain."
  (interactive "MTwitter api domain to use: ")
  (setq twitter4e-api-domain domain)
  
  (setq twitter4e-base-search-url (concat "http://" domain "/search"))
  (setq twitter4e-base-url (concat twitter4e-protocol "://" domain "/api"))
  ;; statuses
  (setq twitter4e-update-url
        (concat twitter4e-base-url "/statuses/update.xml"))
  (setq twitter4e-puplic-timeline-file
        (concat twitter4e-base-url "/statuses/public_timeline.xml?page=%s"))
  (setq twitter4e-home-timeline-file
        (concat twitter4e-base-url "/statuses/home_timeline.xml"))
  (setq twitter4e-friend-timeline-file
        (concat twitter4e-base-url "/statuses/friends_timeline.xml?page=%s"))
  (setq twitter4e-followers-list-url
        (concat twitter4e-base-url "/statuses/followers.xml?page=%s"))
  (setq twitter4e-friend-list-url
        (concat twitter4e-base-url "/statuses/friends.xml"))
  (setq twitter4e-mentions-url
        (concat twitter4e-base-url "/statuses/mentions.xml?page=%s"))
  ;; rate limit
  (setq twitter4e-rate-limit-file
        (concat twitter4e-base-url "/account/rate_limit_status.xml"))
  ;; direct messages
  (setq twitter4e-direct-msg-send-url
        (concat twitter4e-base-url "/direct_messages/new.xml"))
  (setq twitter4e-direct-msg-get-url
        (concat twitter4e-base-url "/direct_messages.xml?page=%s"))
  ;; friends
  (setq twitter4e-add-friend-url
        (concat twitter4e-base-url "/friendships/create/%s.xml"))
  (setq twitter4e-remove-friend-url
        (concat twitter4e-base-url "/friendships/destroy/%s.xml"))
  ;; favorites
  (setq twitter4e-favorites-url
        (concat twitter4e-base-url "/favorites.xml?page=%s"))
  (setq twitter4e-add-favorite-url
        (concat twitter4e-base-url "/favorites/create/%s.xml"))
  (setq twitter4e-remove-favorite-url
        (concat twitter4e-base-url "/favorites/destroy/%s.xml"))
  ;; search
  (setq twitter4e-search-url
        (concat twitter4e-base-search-url "/search.atom?q=%s"))

  ;;* graph const url
  (setq twitter4e-graph-friends-url
        (concat twitter4e-base-url "/friends/ids.xml?user_id=%s"))
  (setq twitter4e-graph-followers-url
        (concat twitter4e-base-url "/followers/ids.xml?user_id=%s")))

(defun twitter4e-set-auth (user pass)
  "Set the http url authentication string from USER and PASS."
  (let* ((api-http  (format "%s:%d" twitter4e-api-domain twitter4e-http-port))
         (api-https (format "%s:%d" twitter4e-api-domain twitter4e-https-port))
         (old-http-storage
          (assoc api-http (symbol-value url-basic-auth-storage)))
         (old-https-storage
          (assoc api-https (symbol-value url-basic-auth-storage)))
         (auth-pair
          (cons "Twitter API"
                (base64-encode-string (format "%s:%s" user pass)))))
    (when old-http-storage
      (set url-basic-auth-storage
           (delete old-http-storage (symbol-value url-basic-auth-storage))))
    (when old-https-storage
      (set url-basic-auth-storage
           (delete old-https-storage (symbol-value url-basic-auth-storage))))
    (set url-basic-auth-storage
         (cons (list api-https auth-pair)
               (cons (list api-http auth-pair)
                     (symbol-value url-basic-auth-storage))))))

(defun twitter4e-parse-header (header-frag)
  "Parse the HEADER-FRAG, and come back with some status codes.

This returns the HTTP status (for now) as a list of three elements.
 (HTTP/Version code Description)

The header fragment should be the first text node from the parsed
xml.

The header fragment is actually quite important, it will tell us
if we have run into some kind of condition where we cannot
display tweets or other information.  This will ease the fresh-install
pain where uesrs can't see why they have blank timelines."
  "Header format:  (or part we care about)" "HTTP/1.0 <status> <status text>\n"


  (string-match "HTTP/\\([0-9]+\\.[0-9]+\\) \\([1-5][0-9][0-9]\\) \\(.*\\)$"
                header-frag)
  (if (match-string 3 header-frag)
      (list (match-string 1 header-frag)
            (match-string 2 header-frag)
            (match-string 3 header-frag))
    (error "Malformed Header sent to twitter4e-parse-header.   Header: %s"
           header-frag)))

(defun twitter4e-header-error-p (header)
  "Let us know if the HEADER is an error or not.  Null headers are errors."
  (and header (<= 400 (string-to-number (cadr header)))))

(defun twitter4e-parse-xml-sync (url method)
  "Retrieve xml file at url URL with method METHOD and parse it with `xml-parse-fragment'.
Emacs' url package will prompt for authentication info if required.

Note that this parses the entire HTTP request as an xml fragment
and not the response."
  (let ((result nil)
        (url-request-method method)
        (url-show-status nil))
    (save-window-excursion
      (set-buffer (url-retrieve-synchronously url))
      (let* ((first-header-line
              (buffer-substring (goto-char (point-min)) (search-forward "\n")))
             (http-header (twitter4e-parse-header first-header-line)))
        (if (twitter4e-header-error-p http-header)
            (string-to-number (cadr http-header))
          (goto-char (point-min))
          (setq result (xml-parse-fragment))
          (kill-buffer (current-buffer)))
        result))))

(defun twitter4e-parse-xml-async (url callback &optional cbargs)
  "Retrieve the resource at URL, and when retrieved call CALLBACK.

This is the asyncronous version of `twitter4e-parse-xml'.  Once that function is
refactored, and its named changed, so should this one."
  (let ((url-request-method "GET")
        (url-show-status nil))
    (url-retrieve url 'twitter4e-parse-xml-async-retrieve
                  (list url callback cbargs))))

(defun twitter4e-parse-xml-async-retrieve (status url callback &optional cbargs)
  "Called by `twitter4e-parse-xml-async'.

STATUS is the status from HTTP, URL and CALLBACK were the args from `twitter4e-parse-xml-async'."
  (let ((result nil))
    (if (null status)   ; no news is good news.
        (save-excursion
          (goto-char (point-min))
          (setq result (xml-parse-fragment))
          (kill-buffer (current-buffer))))
    (funcall callback status url result cbargs)))

(defun twitter4e-parse-xml (url &optional method callback cbargs)
  "If CALLBACK is nil, call `twitter4e-parse-xml-async',
otherwise call `twitter4e-parse-xml-sync'."
  (if callback
      (twitter4e-parse-xml-async url callback cbargs)
    (twitter4e-parse-xml-sync url method)))

(defun twitter4e-get-home-timeline (&optional page callback cbargs)
  "Get home timeline."
  (let ((url twitter4e-home-timeline-file))
    (when page
      (setq url (format "%s?page=%d" url page)))
    (twitter4e-parse-xml url twitter4e-method-get callback cbargs)))

(defun twitter4e-get-home-timeline-since (&optional id callback cbargs)
  "Get home timeline since id ID."
  (let ((url (concat twitter4e-home-timeline-file "?since_id=" id)))
    (twitter4e-parse-xml url twitter4e-method-get callback cbargs)))

(defun twitter4e-get-rate-limit-xml (&optional callback cbargs)
  "Get rate limit xml."
  (twitter4e-parse-xml twitter4e-rate-limit-file twitter4e-method-get callback cbargs))

(defun twitter4e-get-rate-limit ()
  "Return the rate limit as a number from the xml."
  (interactive)
  (twit-parse-rate-limit (cadr (twitter4e-get-rate-limit-xml))))

(defun twitter4e-parse-rate-limit (xml)
  "Parse the rate limit, and return the hourly limit.
XML should be the twitter ratelimit sxml and should not have any HTTP header
information in its car."
  (let ((limit (assoc 'hourly-limit xml)))
    (if twitter4e-debug (message "Parsed limit %s from xml %s" limit xml))
    (if limit
        (string-to-number (caddr limit)))))

(twitter4e-set-api-domain twitter4e-api-domain)

(provide 'twitter4e)

;;; twitter4e.el ends here
