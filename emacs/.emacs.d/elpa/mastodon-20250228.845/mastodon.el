;;; mastodon.el --- Client for fediverse services using the Mastodon API  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Copyright (C) 2021 Abhiseck Paira <abhiseckpaira@disroot.org>
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <mousebot@disroot.org>
;; Maintainer: Marty Hiatt <mousebot@disroot.org>
;; Package-Version: 20250228.845
;; Package-Revision: 9561697f7b62
;; Package-Requires: ((emacs "28.1") (request "0.3.0") (persist "0.4") (tp "0.7"))
;; Homepage: https://codeberg.org/martianh/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon.el is a client for fediverse services that implement the Mastodon
;; API. See <https://github.com/mastodon/mastodon>.

;; For set up and usage details, see the Info documentation, or the readme
;; file at <https://codeberg.org/martianh/mastodon.el>.

;;; Code:
(require 'cl-lib) ; for `cl-some' call in `mastodon'
(eval-when-compile (require 'subr-x))
(require 'url)
(require 'thingatpt)
(require 'shr)

(require 'mastodon-http)
(require 'mastodon-toot)
(require 'mastodon-search)
(require 'mastodon-transient)
(require 'mastodon-tl)

(declare-function discover-add-context-menu "discover")
(declare-function emojify-mode "emojify")
(declare-function request "request")

(autoload 'mastodon-auth--get-account-name "mastodon-auth")
(autoload 'mastodon-auth--user-acct "mastodon-auth")
(autoload 'mastodon-discover "mastodon-discover")
(autoload 'mastodon-notifications-follow-request-accept "mastodon-notifications")
(autoload 'mastodon-notifications-follow-request-reject "mastodon-notifications")
(autoload 'mastodon-notifications-get-mentions "mastodon-notifications")
(autoload 'mastodon-notifications--timeline "mastodon-notifications")
(autoload 'mastodon-notifications-policy "mastodon-notifications")
(autoload 'mastodon-notifications-requests "mastodon-notifications")

(autoload 'mastodon-profile--fetch-server-account-settings "mastodon-profile")
(autoload 'mastodon-profile-get-toot-author "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile-my-profile "mastodon-profile")
(autoload 'mastodon-profile-show-user "mastodon-profile")
(autoload 'mastodon-profile-update-user-profile-note "mastodon-profile")
(autoload 'mastodon-profile-view-bookmarks "mastodon-profile")
(autoload 'mastodon-profile-view-favourites "mastodon-profile")

(autoload 'mastodon-toot-edit-toot-at-point "mastodon-toot")
(when (require 'lingva nil :no-error)
  (autoload 'mastodon-toot-translate-toot-text "mastodon-toot"))
(autoload 'mastodon-toot--view-toot-history "mastodon-tl")

(autoload 'mastodon-views-view-follow-suggestions "mastodon-views"
  nil :interactive) ;; for M-x visibility
(autoload 'mastodon-views-view-filters "mastodon-views"
  nil :interactive)
(autoload 'mastodon-views-view-follow-requests "mastodon-views"
  nil :interactive)
(autoload 'mastodon-views-view-own-instance "mastodon-views"
  nil :interactive)
(autoload 'mastodon-views-view-instance-description "mastodon-views"
  nil :interactive)
(autoload 'mastodon-views-view-lists "mastodon-views"
  nil :interactive)
(autoload 'mastodon-views-view-scheduled-toots "mastodon-views"
  nil :interactive)

(autoload 'special-mode "simple")

(defvar mastodon-tl--highlight-current-toot)
(defvar mastodon-notifications--map)

(defgroup mastodon nil
  "Interface with Mastodon."
  :prefix "mastodon-"
  :group 'external)

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the fediverse instance you want to be active.
For example, if your username is
\"example_user@social.instance.org\", and you want this account
to be active, the value of this variable should be
\"https://social.instance.org\".

Also for completeness, the value of `mastodon-active-user' should
be \"example_user\".

After setting these variables you should restart Emacs for these
changes to take effect."
  :type 'string)

(defcustom mastodon-active-user nil
  "Username of the active user.
For example, if your username is
\"example_user@social.instance.org\", and you want this account
to be active, the value of this variable should be
\"example_user\".

Also for completeness, the value of `mastodon-instance-url'
should be \"https://social.instance.org\".

After setting these variables you should restart Emacs for these
changes to take effect."
  :type 'string)

(defcustom mastodon-toot-timestamp-format "%F %T"
  "Format to use for timestamps.
For valid formatting options see `format-time-string`.
The default value \"%F %T\" prints ISO8601-style YYYY-mm-dd HH:MM:SS.
Use. e.g. \"%c\" for your locale's date and time format."
  :type 'string)

(defcustom mastodon-use-emojify nil
  "Whether to use emojify.el to display emojis.
From version 28, Emacs can display emojis natively. But
currently, it doesn't seem to have a way to handle custom emoji,
while emojify,el has this feature and mastodon.el implements it."
  :type 'boolean)

;; notifications customizes
;; moved here because we can load notifs without first loading mastodon.el
;; or mastodon-notifications.el

(defcustom mastodon-profile-note-in-foll-reqs t
  "If non-nil, show a user's profile note in follow request notifications."
  :type '(boolean))

(defcustom mastodon-profile-note-in-foll-reqs-max-length nil
  "The max character length for user profile note in follow requests.
Profile notes are only displayed if
`mastodon-profile-note-in-foll-reqs' is non-nil.
If unset, profile notes of any size will be displayed, which may
make them unweildy."
  :type '(integer))

(defcustom mastodon-images-in-notifs nil
  "Whether to display attached images in notifications."
  :type '(boolean))

(defcustom mastodon-group-notifications nil
  "Whether to use grouped notifications.
Requires that your instance actually implements grouped notifications.
Mastodon implemented them in 4.3."
  :type '(boolean))

(defcustom mastodon-notifications-grouped-names-count 2
  "The number of notification authors to display.
A count of 2 for example means to display like so: \"Bob, Jenny
and X others...\"."
  :type '(integer))

(defun mastodon-kill-window ()
  "Quit window and delete helper."
  (interactive)
  (quit-window 'kill))

(defvar mastodon-mode-map
  (let ((map (make-sparse-keymap)))
    ;; navigation inside a timeline
    (define-key map (kbd "n")      #'mastodon-tl-goto-next-item)
    (define-key map (kbd "p")      #'mastodon-tl-goto-prev-item)
    (define-key map (kbd "M-n")    #'mastodon-tl-next-tab-item)
    (define-key map (kbd "M-p")    #'mastodon-tl-previous-tab-item)
    (define-key map [?\t]          #'mastodon-tl-next-tab-item)
    (define-key map [backtab]      #'mastodon-tl-previous-tab-item)
    (define-key map [?\S-\t]       #'mastodon-tl-previous-tab-item)
    (define-key map [?\M-\t]       #'mastodon-tl-previous-tab-item)
    (define-key map (kbd "l")      #'recenter-top-bottom)
    ;; navigation between timelines
    (define-key map (kbd "#")      #'mastodon-tl-get-tag-timeline)
    (define-key map (kbd "\"")     #'mastodon-tl-list-followed-tags)
    (define-key map (kbd "'")      #'mastodon-tl-followed-tags-timeline)
    (define-key map (kbd "C-'")   #'mastodon-tl-tag-group-timeline)
    (define-key map (kbd "A")      #'mastodon-profile-get-toot-author)
    (define-key map (kbd "F")      #'mastodon-tl-get-federated-timeline)
    (define-key map (kbd "H")      #'mastodon-tl-get-home-timeline)
    (define-key map (kbd "L")      #'mastodon-tl-get-local-timeline)
    (define-key map (kbd "N")      #'mastodon-notifications-get)
    (define-key map (kbd "S-C-n")  #'mastodon-notifications-requests)
    (define-key map (kbd "@")      #'mastodon-notifications-get-mentions)
    (define-key map (kbd "P")      #'mastodon-profile-show-user)
    (define-key map (kbd "s")      #'mastodon-search-query)
    (define-key map (kbd "/")      #'mastodon-switch-to-buffer)
    (define-key map (kbd "\\")     #'mastodon-tl-get-remote-local-timeline)
    ;; quitting mastodon
    (define-key map (kbd "q")      #'kill-current-buffer)
    (define-key map (kbd "Q")      #'mastodon-kill-window)
    (define-key map (kbd "M-C-q")  #'mastodon-kill-all-buffers)
    ;; toot actions
    (define-key map (kbd "c")      #'mastodon-tl-toggle-spoiler-text-in-toot)
    (define-key map (kbd "b")      #'mastodon-toot-toggle-boost)
    (define-key map (kbd "f")      #'mastodon-toot-toggle-favourite)
    (define-key map (kbd "k")      #'mastodon-toot-toggle-bookmark)
    (define-key map (kbd "r")      #'mastodon-toot-reply)
    (define-key map (kbd "C")      #'mastodon-toot-copy-toot-url)
    (define-key map (kbd "o")      #'mastodon-toot-browse-toot-url)
    (define-key map (kbd "v")      #'mastodon-tl-poll-vote)
    (define-key map (kbd "E")      #'mastodon-toot-view-toot-edits)
    (define-key map (kbd "T")      #'mastodon-tl-thread)
    (define-key map (kbd "RET")    #'mastodon-tl-thread)
    (define-key map (kbd "m")      #'mastodon-tl-dm-user)
    (when (require 'lingva nil :no-error)
      (define-key map (kbd "a")    #'mastodon-toot-translate-toot-text))
    (define-key map (kbd ",")      #'mastodon-toot-list-favouriters)
    (define-key map (kbd ".")      #'mastodon-toot-list-boosters)
    (define-key map (kbd ";")      #'mastodon-views-view-instance-description)
    ;; override special mode binding
    (define-key map (kbd "g")      #'undefined)
    (define-key map (kbd "g")      #'mastodon-tl-update)
    ;; this is now duplicated by 'g', cd remove/use for else:
    (define-key map (kbd "u")      #'mastodon-tl-update)
    ;; own toot actions:
    (define-key map (kbd "t")      #'mastodon-toot)
    (define-key map (kbd "d")      #'mastodon-toot-delete-toot)
    (define-key map (kbd "D")      #'mastodon-toot-delete-and-redraft-toot)
    (define-key map (kbd "i")      #'mastodon-toot-pin-toot-toggle)
    (define-key map (kbd "e")      #'mastodon-toot-edit-toot-at-point)
    ;; user actions
    (define-key map (kbd "W")      #'mastodon-tl-follow-user)
    (define-key map (kbd "C-S-W")  #'mastodon-tl-unfollow-user)
    (define-key map (kbd "B")      #'mastodon-tl-block-user)
    (define-key map (kbd "C-S-B")  #'mastodon-tl-unblock-user)
    (define-key map (kbd "M")      #'mastodon-tl-mute-user)
    (define-key map (kbd "C-S-M")  #'mastodon-tl-unmute-user)
    (define-key map (kbd "Z")      #'mastodon-tl-report-to-mods)
    ;; own profile
    (define-key map (kbd "O")      #'mastodon-profile-my-profile)
    (define-key map (kbd "U")      #'mastodon-profile-update-user-profile-note)
    (define-key map (kbd "V")      #'mastodon-profile-view-favourites)
    (define-key map (kbd "K")      #'mastodon-profile-view-bookmarks)
    (define-key map (kbd ":")      #'mastodon-user-settings)
    (define-key map (kbd "C-:")    #'mastodon-notifications-policy)
    ;; minor views
    (define-key map (kbd "R")      #'mastodon-views-view-follow-requests)
    (define-key map (kbd "S")      #'mastodon-views-view-scheduled-toots)
    (define-key map (kbd "I")      #'mastodon-views-view-filters)
    (define-key map (kbd "G")      #'mastodon-views-view-follow-suggestions)
    (define-key map (kbd "X")      #'mastodon-views-view-lists)
    (define-key map (kbd "SPC")    #'mastodon-tl-scroll-up-command)
    (define-key map (kbd "!")      #'mastodon-tl-fold-post-toggle)
    (define-key map (kbd "z")      #'bury-buffer)
    map)
  "Keymap for `mastodon-mode'.")

(defcustom mastodon-mode-hook nil
  "Hook run when entering Mastodon mode."
  :type 'hook
  :options '(provide-discover-context-menu))

(defface mastodon-handle-face
  '((t :inherit default))
  "Face used for user handles in bylines.")

(defface mastodon-display-name-face
  '((t :inherit warning))
  "Face used for user display names.")

(defface mastodon-boosted-face
  '((t :inherit success :weight bold))
  "Face to indicate that a toot is boosted.")

(defface mastodon-boost-fave-face
  '((t :inherit success))
  "Face to indicate that you have boosted or favourited a toot.")

(defface mastodon-cw-face
  '((t :inherit success))
  "Face used for content warning.")

(defface mastodon-toot-docs-face
  `((t :inherit shadow))
  "Face used for documentation in toot compose buffer.
If `mastodon-tl--enable-proportional-fonts' is changed,
mastodon.el needs to be re-loaded for this to be correctly set.")

(defface mastodon-toot-docs-reply-text-face
  `((t :inherit font-lock-comment-face
       :family ,(face-attribute 'variable-pitch :family)))
  "Face used for reply text in toot compose buffer.
See `mastodon-toot-display-orig-in-reply-buffer'.")

(defface mastodon-cursor-highlight-face
  `((t :inherit highlight :extend t))
  "Face for `mastodon-tl--highlight-current-toot'.")

;;;###autoload
(defun mastodon ()
  "Connect client to `mastodon-instance-url' instance.
If there are any open mastodon.el buffers, switch to one instead.
Prority in switching is given to timeline views."
  (interactive)
  (let* ((tls (list "home"
                    "local"
                    "federated"
                    (concat (mastodon-auth--user-acct) "-statuses") ; own profile
                    "favourites"
                    "search"))
         (buffer (or (cl-some (lambda (el)
                                (get-buffer (concat "*mastodon-" el "*")))
                              tls) ; return first buff that exists
                     (cl-some (lambda (x)
                                (when (string-prefix-p "*mastodon-"
                                                       (buffer-name x))
                                  (get-buffer x)))
                              (buffer-list))))) ; catch any other masto buffer
    (if buffer
        (pop-to-buffer buffer '(display-buffer-same-window))
      ;; we need to update credential-account in case setting have been changed
      ;; outside mastodon.el in the meantime:
      (mastodon-return-credential-account :force)
      (mastodon-tl-get-home-timeline)
      (message "Loading fediverse account %s on %s..."
               (mastodon-auth--user-acct)
               mastodon-instance-url))))

(defvar mastodon-profile-credential-account nil)

;; TODO: the get request in mastodon-http--get-response often returns nil
;; after waking from sleep, not sure how to fix, or if just my pc.
;; interestingly it only happens with this function tho.
(defun mastodon-return-credential-account (&optional force)
  "Return the CredentialAccount entity.
Either from `mastodon-profile-credential-account' or from the
server if that var is nil.
FORCE means to fetch from the server in any case and update
`mastodon-profile-credential-account'."
  (if (or force (not mastodon-profile-credential-account))
      (setq mastodon-profile-credential-account
            ;; TODO: we should signal a quit condition after 5 secs here
            (condition-case nil
                (mastodon-http--get-json
                 (mastodon-http--api "accounts/verify_credentials")
                 nil :silent)
              (t ; req fails, return old value
               mastodon-profile-credential-account)))
    ;; else just return the var:
    mastodon-profile-credential-account))

(defvar mastodon-instance-data nil
  "Instance data from the instance endpoint.")

(defun mastodon-instance-data ()
  "Return `mastodon-instnace-data' or else fetch from instance endpoint."
  (or mastodon-instance-data
      (setq mastodon-instance-data
            (mastodon-http--get-json (mastodon-http--api "instance")))))

(defun mastodon-instance-version ()
  "Return the version string of user's instance."
  (alist-get 'version (mastodon-instance-data)))

;;;###autoload
(defun mastodon-toot (&optional user reply-to-id reply-json)
  "Update instance with new toot. Content is captured in a new buffer.
If USER is non-nil, insert after @ symbol to begin new toot.
If REPLY-TO-ID is non-nil, attach new toot to a conversation.
If REPLY-JSON is the json of the toot being replied to."
  (interactive)
  (mastodon-toot--compose-buffer user reply-to-id reply-json))

;;;###autoload
(defun mastodon-notifications-get (&optional type buffer-name max-id)
  "Display NOTIFICATIONS in buffer.
Optionally only print notifications of type TYPE, a string.
BUFFER-NAME is added to \"*mastodon-\" to create the buffer name.
MAX-ID is a request parameter for pagination."
  (interactive)
  (let* ((buffer-name (or buffer-name "notifications"))
         (buffer (concat "*mastodon-" buffer-name "*")))
    (message "Loading your notifications...")
    (mastodon-tl--init-sync
     buffer-name
     "notifications"
     'mastodon-notifications--timeline
     type
     (when max-id
       `(("max_id" . ,(mastodon-tl--buffer-property 'max-id))))
     nil nil nil
     (if (or (not mastodon-group-notifications)
             ;; if version less than 1st grouped notifs release:
             (> 4.3 (string-to-number
                     (mastodon-instance-version))))
         "v1"
       "v2"))
    (with-current-buffer (get-buffer-create buffer)
      (use-local-map mastodon-notifications--map))
    (message "Loading your notifications... Done")))

;; URL lookup: should be available even if `mastodon.el' not loaded:

;;;###autoload
(defun mastodon-url-lookup (&optional query-url force)
  "If a URL resembles a fediverse link, try to load in `mastodon.el'.
Does a WebFinger lookup on the server.
URL can be arg QUERY-URL, or URL at point, or provided by the user.
If a status or account is found, load it in `mastodon.el', if
not, just browse the URL in the normal fashion.
If FORCE, do a lookup regardless of the result of `mastodon--fedi-url-p'."
  (interactive)
  (let* ((query (or query-url
                    (mastodon-tl--property 'shr-url :no-move)
                    (thing-at-point-url-at-point)
                    (read-string "Lookup URL: "))))
    (if (and (not force)
             (not (mastodon--fedi-url-p query)))
        ;; (shr-browse-url query) ; doesn't work (keep our shr keymap)
        (progn (message "Using external browser")
               (browse-url query))
      (message "Performing lookup...")
      (let* ((url (format "%s/api/v2/search" mastodon-instance-url))
             (params `(("q" . ,query)
                       ("resolve" . "t"))) ; webfinger
             (response (mastodon-http--get-json url params :silent)))
        (cond ((not (seq-empty-p (alist-get 'statuses response)))
               (let* ((statuses (assoc 'statuses response))
                      (status (seq-first (cdr statuses)))
                      (status-id (alist-get 'id status)))
                 (mastodon-tl--thread-do status-id)))
              ((not (seq-empty-p (alist-get 'accounts response)))
               (let* ((accounts (assoc 'accounts response))
                      (account (seq-first (cdr accounts))))
                 (mastodon-profile--make-author-buffer account)))
              (t
               (message "Lookup failed. Using external browser")
               (browse-url query)))))))

(defun mastodon-url-lookup-force ()
  "Call `mastodon-url-lookup' without checking if URL is fedi-like."
  (interactive)
  (mastodon-url-lookup nil :force))

(defun mastodon--fedi-url-p (query)
  "Check if QUERY resembles a fediverse URL."
  ;; calqued off https://github.com/tuskyapp/Tusky/blob/c8fc2418b8f5458a817bba221d025b822225e130/app/src/main/java/com/keylesspalace/tusky/BottomSheetActivity.kt
  ;; thx to Conny Duck!
  ;; mastodon at least seems to allow only [a-z0-9_] for usernames, plus "."
  ;; but not at beginning or end, see https://github.com/mastodon/mastodon/issues/6830
  ;; objects may have - in them
  (let* ((uri-parsed (url-generic-parse-url query))
         (query (url-filename uri-parsed)))
    (save-match-data
      (or (string-match "^/@[^/]+$" query)
          (string-match "^/@[^/]+/[[:digit:]]+$" query)
          (string-match "^/user[s]?/@?[[:alnum:]_]+$" query) ; @: pleroma or soapbox
          (string-match "^/notice/[[:alnum:]]+$" query)
          (string-match "^/objects/[-a-f0-9]+$" query)
          (string-match "^/notes/[a-z0-9]+$" query)
          (string-match "^/display/[-a-f0-9]+$" query)
          (string-match "^/profile/[[:alpha:]_]+$" query)
          (string-match "^/p/[[:alpha:]_]+/[[:digit:]]+$" query)
          (string-match "^/[[:alpha:]_]+$" query)
          (string-match "^/u/[[:alpha:]_]+$" query)
          (string-match "^/c/[[:alnum:]_]+$" query)
          (string-match "^/post/[[:digit:]]+$" query)
          (string-match "^/comment/[[:digit:]]+$" query) ; lemmy
          (string-match "^/@[^/]+/statuses/[[:alnum:]]" query) ; GTS
          (string-match "^/user[s]?/[[:alnum:]_]+/statuses/[[:digit:]]+$" query) ; hometown
          (string-match "^/notes/[[:alnum:]]+$" query))))) ; misskey post

(defun mastodon-live-buffers ()
  "Return a list of open mastodon buffers.
Calls `mastodon-tl--get-buffer-type', which see."
  (cl-loop for x in (buffer-list)
           when (with-current-buffer x (mastodon-tl--get-buffer-type))
           collect (get-buffer x)))

(defun mastodon-buffer-p (&optional buffer)
  "Non-nil if BUFFER or `current-buffer' is a mastodon one."
  (let ((buf (or buffer (current-buffer))))
    (member buf (mastodon-live-buffers))))

(defun mastodon-kill-all-buffers ()
  "Kill any and all open mastodon buffers, hopefully."
  (interactive)
  (let ((mastodon-buffers (mastodon-live-buffers)))
    (cl-loop for x in mastodon-buffers
             do (kill-buffer x))))

(defun mastodon-switch-to-buffer ()
  "Switch to a live mastodon buffer."
  (interactive)
  (let ((choice (completing-read
                 "Switch to mastodon buffer: "
                 (mapcar #'buffer-name (mastodon-live-buffers))
                 nil :match)))
    (switch-to-buffer choice)))

(defun mastodon--url-at-point ()
  "`thing-at-point' provider function."
  (get-text-property (point) 'shr-url))

(defun mastodon-mode-hook-fun ()
  "Function to add to `mastodon-mode-hook'."
  (when (and mastodon-use-emojify
             (require 'emojify nil :noerror))
    (emojify-mode t)
    (when mastodon-toot--enable-custom-instance-emoji
      (mastodon-toot-enable-custom-emoji)))
  (mastodon-profile--fetch-server-account-settings)
  (when (and mastodon-tl--highlight-current-toot
             (fboundp #'cursor-face-highlight-mode))
    (cursor-face-highlight-mode)) ; 29.1
  ;; make `thing-at-point' functions work:
  (setq-local thing-at-point-provider-alist
              (append thing-at-point-provider-alist
                      '((url . mastodon--url-at-point)))))

;;;###autoload
(add-hook 'mastodon-mode-hook #'mastodon-mode-hook-fun)

(define-derived-mode mastodon-mode special-mode "Mastodon"
  "Major mode for fediverse services using the Mastodon API."
  (read-only-mode 1))

(provide 'mastodon)
;;; mastodon.el ends here
