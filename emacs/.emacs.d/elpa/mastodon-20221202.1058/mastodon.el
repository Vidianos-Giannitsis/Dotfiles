;;; mastodon.el --- Client for Mastodon, a federated social network  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Copyright (C) 2021 Abhiseck Paira <abhiseckpaira@disroot.org>
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (persist "0.4") (ts "0.3"))
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

;; mastodon.el is an Emacs client for Mastodon <https://github.com/mastodon/mastodon>,
;; the federated microblogging social network. It also works with Pleroma instances and other services that implement the Mastodon API.
;; See the readme file at https://codeberg.org/martianh/mastodon.el for set up and usage details.

;;; Code:
(require 'cl-lib) ; for `cl-some' call in mastodon
(eval-when-compile (require 'subr-x))
(require 'mastodon-http)
(require 'mastodon-toot)
(require 'url)
(require 'thingatpt)
(require 'shr)

(declare-function discover-add-context-menu "discover")
(declare-function emojify-mode "emojify")
(declare-function request "request")
(autoload 'special-mode "simple")
(autoload 'mastodon-tl--get-federated-timeline "mastodon-tl")
(autoload 'mastodon-tl--get-home-timeline "mastodon-tl")
(autoload 'mastodon-tl--get-local-timeline "mastodon-tl")
(autoload 'mastodon-tl--get-tag-timeline "mastodon-tl")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl")
(autoload 'mastodon-tl--goto-prev-toot "mastodon-tl")
(autoload 'mastodon-tl--next-tab-item "mastodon-tl")
(autoload 'mastodon-tl--previous-tab-item "mastodon-tl")
(autoload 'mastodon-tl--thread "mastodon-tl")
(autoload 'mastodon-tl--toggle-spoiler-text-in-toot "mastodon-tl")
(autoload 'mastodon-tl--update "mastodon-tl")
(autoload 'mastodon-profile--get-toot-author "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile--show-user "mastodon-profile")
(autoload 'mastodon-discover "mastodon-discover")

(autoload 'mastodon-tl--block-user "mastodon-tl")
(autoload 'mastodon-tl--unblock-user "mastodon-tl")
(autoload 'mastodon-tl--mute-user "mastodon-tl")
(autoload 'mastodon-tl--unmute-user "mastodon-tl")
(autoload 'mastodon-tl--follow-user "mastodon-tl")
(autoload 'mastodon-tl--unfollow-user "mastodon-tl")
(autoload 'mastodon-profile--my-profile "mastodon-profile")
(autoload 'mastodon-profile--view-favourites "mastodon-profile")
(autoload 'mastodon-profile--view-follow-requests "mastodon-profile")
(autoload 'mastodon-notifications--follow-request-accept "mastodon-notifications")
(autoload 'mastodon-notifications--follow-request-reject "mastodon-notifications")
(autoload 'mastodon-search--search-query "mastodon-search")
(autoload 'mastodon-auth--get-account-name "mastodon-auth")
;; (autoload 'mastodon-async--stream-federated "mastodon-async")
;; (autoload 'mastodon-async--stream-local "mastodon-async")
;; (autoload 'mastodon-async--stream-home "mastodon-async")
;; (autoload 'mastodon-async--stream-notifications "mastodon-async")
;; (autoload 'mastodon-async-mode "mastodon-async")
(autoload 'mastodon-profile--update-user-profile-note "mastodon-profile")
(autoload 'mastodon-auth--user-acct "mastodon-auth")
(autoload 'mastodon-tl--poll-vote "mastodon-http")
(autoload 'mastodon-profile--view-bookmarks "mastodon-profile")
(autoload 'mastoton-tl--view-filters "mastodon-tl")
(autoload 'mastodon-tl--view-filters "mastodon-tl")
(autoload 'mastodon-tl--get-follow-suggestions "mastodon-tl")
(when (require 'lingva nil :no-error)
  (autoload 'mastodon-toot--translate-toot-text "mastodon-toot"))
(autoload 'mastodon-search--trending-tags "mastodon-search")
(autoload 'mastodon-profile--fetch-server-account-settings "mastodon-profile")
(autoload 'mastodon-notifications--get-mentions "mastodon-notifications")
(autoload 'mastodon-tl--view-lists "mastodon-tl")
(autoload 'mastodon-toot--edit-toot-at-point "mastodon-toot")
(autoload 'mastodon-toot--view-toot-history "mastodon-tl")
(autoload 'mastodon-tl--init-sync "mastodon-tl")
(autoload 'mastodon-notifications--timeline "mastodon-notifications")

(defvar mastodon-notifications--map)

(defgroup mastodon nil
  "Interface with Mastodon."
  :prefix "mastodon-"
  :group 'external)

(defcustom mastodon-instance-url "https://mastodon.social"
  "Base URL for the Mastodon instance you want to be active.

For example, if your mastodon username is
\"example_user@social.instance.org\", and you want this account
to be active, the value of this variable should be
\"https://social.instance.org\".

Also for completeness, the value of `mastodon-active-user' should
be \"example_user\".

After setting these variables you should restart Emacs for these
changes to take effect."
  :group 'mastodon
  :type 'string)

(defcustom mastodon-active-user nil
  "Username of the active user.

For example, if your mastodon username is
\"example_user@social.instance.org\", and you want this account
to be active, the value of this variable should be
\"example_user\".

Also for completeness, the value of `mastodon-instance-url'
should be \"https://social.instance.org\".

After setting these variables you should restart Emacs for these
changes to take effect."
  :group 'mastodon
  :type 'string)

(defcustom mastodon-toot-timestamp-format "%F %T"
  "Format to use for timestamps.

For valid formatting options see `format-time-string`.
The default value \"%F %T\" prints ISO8601-style YYYY-mm-dd HH:MM:SS.
Use. e.g. \"%c\" for your locale's date and time format."
  :group 'mastodon
  :type 'string)

(defvar mastodon-mode-map
  (let ((map (make-sparse-keymap)))
    ;; navigation inside a timeline
    (define-key map (kbd "n") #'mastodon-tl--goto-next-toot)
    (define-key map (kbd "p") #'mastodon-tl--goto-prev-toot)
    (define-key map (kbd "M-n") #'mastodon-tl--next-tab-item)
    (define-key map (kbd "M-p") #'mastodon-tl--previous-tab-item)
    (define-key map [?\t] #'mastodon-tl--next-tab-item)
    (define-key map [backtab] #'mastodon-tl--previous-tab-item)
    (define-key map [?\S-\t] #'mastodon-tl--previous-tab-item)
    (define-key map [?\M-\t] #'mastodon-tl--previous-tab-item)
    ;; navigation between timelines
    (define-key map (kbd "#") #'mastodon-tl--get-tag-timeline)
    (define-key map (kbd "A") #'mastodon-profile--get-toot-author)
    (define-key map (kbd "F") #'mastodon-tl--get-federated-timeline)
    (define-key map (kbd "H") #'mastodon-tl--get-home-timeline)
    (define-key map (kbd "L") #'mastodon-tl--get-local-timeline)
    (define-key map (kbd "N") #'mastodon-notifications-get)
    (define-key map (kbd "P") #'mastodon-profile--show-user)
    (define-key map (kbd "T") #'mastodon-tl--thread)
    ;; navigation out of mastodon
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "Q") #'kill-buffer-and-window)
    ;; timeline actions
    (define-key map (kbd "b") #'mastodon-toot--toggle-boost)
    (define-key map (kbd "c") #'mastodon-tl--toggle-spoiler-text-in-toot)
    (define-key map (kbd "f") #'mastodon-toot--toggle-favourite)
    (define-key map (kbd "r") #'mastodon-toot--reply)
    ;; this is now duplicated by 'g', cd remove/use for else:
    (define-key map (kbd "u") #'mastodon-tl--update)
    ;; new toot
    (define-key map (kbd "t") #'mastodon-toot)
    ;; override special mode binding
    (define-key map (kbd "g") #'undefined)
    (define-key map (kbd "g") #'mastodon-tl--update)
    ;; mousebot additions
    (define-key map (kbd "W") #'mastodon-tl--follow-user)
    (define-key map (kbd "C-S-W") #'mastodon-tl--unfollow-user)
    (define-key map (kbd "B") #'mastodon-tl--block-user)
    (define-key map (kbd "C-S-B") #'mastodon-tl--unblock-user)
    (define-key map (kbd "M") #'mastodon-tl--mute-user)
    (define-key map (kbd "C-S-M") #'mastodon-tl--unmute-user)
    (define-key map (kbd "O") #'mastodon-profile--my-profile)
    (define-key map (kbd "S") #'mastodon-search--search-query)
    (define-key map (kbd "d") #'mastodon-toot--delete-toot)
    (define-key map (kbd "D") #'mastodon-toot--delete-and-redraft-toot)
    (define-key map (kbd "C") #'mastodon-toot--copy-toot-url)
    (define-key map (kbd "i") #'mastodon-toot--pin-toot-toggle)
    (define-key map (kbd "V") #'mastodon-profile--view-favourites)
    (define-key map (kbd "R") #'mastodon-profile--view-follow-requests)
    (define-key map (kbd "U") #'mastodon-profile--update-user-profile-note)
    (define-key map (kbd "v") #'mastodon-tl--poll-vote)
    (define-key map (kbd "k") #'mastodon-toot--bookmark-toot-toggle)
    (define-key map (kbd "K") #'mastodon-profile--view-bookmarks)
    (define-key map (kbd "I") #'mastodon-tl--view-filters)
    (define-key map (kbd "G") #'mastodon-tl--get-follow-suggestions)
    (define-key map (kbd "X") #'mastodon-tl--view-lists)
    (define-key map (kbd "@") #'mastodon-notifications--get-mentions)
    (define-key map (kbd "e") #'mastodon-toot--edit-toot-at-point)
    (define-key map (kbd "E") #'mastodon-toot--view-toot-edits)
    (when (require 'lingva nil :no-error)
      (define-key map (kbd "s") #'mastodon-toot--translate-toot-text))
    map)
  "Keymap for `mastodon-mode'.")

(defcustom mastodon-mode-hook nil
  "Hook run when entering Mastodon mode."
  :type 'hook
  :options '(provide-discover-context-menu)
  :group 'mastodon)

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

;;;###autoload
(defun mastodon ()
  "Connect Mastodon client to `mastodon-instance-url' instance."
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
                                (when
                                    (string-prefix-p "*mastodon-" (buffer-name x))
                                  (get-buffer x)))
                              (buffer-list))))) ; catch any other masto buffer
    (if buffer
        (switch-to-buffer buffer)
      (mastodon-tl--get-home-timeline)
      (message "Loading Mastodon account %s on %s..."
               (mastodon-auth--user-acct)
               mastodon-instance-url))))

;;;###autoload
(defun mastodon-toot (&optional user reply-to-id reply-json)
  "Update instance with new toot. Content is captured in a new buffer.
If USER is non-nil, insert after @ symbol to begin new toot.
If REPLY-TO-ID is non-nil, attach new toot to a conversation.
If REPLY-JSON is the json of the toot being replied to."
  (interactive)
  (mastodon-toot--compose-buffer user reply-to-id reply-json))

;;;###autoload
(defun mastodon-notifications-get (&optional type buffer-name)
  "Display NOTIFICATIONS in buffer.
Optionally only print notifications of type TYPE, a string.
BUFFER-NAME is added to \"*mastodon-\" to create the buffer name."
  (interactive)
  (let ((buffer (or (concat "*mastodon-" buffer-name "*")
                    "*mastodon-notifications*")))
    (if (get-buffer buffer)
        (progn (switch-to-buffer buffer)
               (mastodon-tl--update))
      (message "Loading your notifications...")
      (mastodon-tl--init-sync
       (or buffer-name "notifications")
       "notifications"
       'mastodon-notifications--timeline
       type)
      (use-local-map mastodon-notifications--map))))

;; URL lookup: should be available even if `mastodon.el' not loaded:

;;;###autoload
(defun mastodon-url-lookup (&optional query-url)
  "If QUERY-URL resembles a mastodon link, try to load in `mastodon.el'.
Does a WebFinger lookup.
URL can be arg QUERY-URL, or URL at point, or provided by the user.
If a status or account is found, load it in `mastodon.el', if
not, just browse the URL in the normal fashion."
  (interactive)
  (let* ((query (or query-url
                    (thing-at-point-url-at-point)
                    (get-text-property (point) 'shr-url)
                    (read-string "Lookup URL: "))))
    (if (not (mastodon--masto-url-p query))
        ;; this doesn't work as shr-browse-url doesn't take a url arg
        ;; and with no args it can't use our read-string query, but only
        ;; looks for a url at point
        ;; (if (equal major-mode 'mastodon-mode)
        ;; (shr-browse-url query) ;; keep our shr keymap
        (browse-url query)
      (message "Performing lookup...")
      (let* ((url (format "%s/api/v2/search" mastodon-instance-url))
             (params `(("q" . ,query)
                       ("resolve" . "t"))) ; webfinger
             (response (mastodon-http--get-json url params :silent)))
        (cond ((not (seq-empty-p
                     (alist-get 'statuses response)))
               (let* ((statuses (assoc 'statuses response))
                      (status (seq-first (cdr statuses)))
                      (status-id (alist-get 'id status)))
                 (mastodon-tl--thread status-id)))
              ((not (seq-empty-p
                     (alist-get 'accounts response)))
               (let* ((accounts (assoc 'accounts response))
                      (account (seq-first (cdr accounts))))
                 (mastodon-profile--make-author-buffer account)))
              (t
               (browse-url query)))))))

(defun mastodon--masto-url-p (query)
  "Check if QUERY resembles a fediverse URL."
  ;; calqued off https://github.com/tuskyapp/Tusky/blob/c8fc2418b8f5458a817bba221d025b822225e130/app/src/main/java/com/keylesspalace/tusky/BottomSheetActivity.kt
  ;; thx to Conny Duck!
  (let* ((uri-parsed (url-generic-parse-url query))
         (query (url-filename uri-parsed)))
    (save-match-data
      (or (string-match "^/@[^/]+$" query)
          (string-match "^/@[^/]+/[[:digit:]]+$" query)
          (string-match "^/users/[[:alnum:]]+$" query)
          (string-match "^/notice/[[:alnum:]]+$" query)
          (string-match "^/objects/[-a-f0-9]+$" query)
          (string-match "^/notes/[a-z0-9]+$" query)
          (string-match "^/display/[-a-f0-9]+$" query)
          (string-match "^/profile/[[:alpha:]]+$" query)
          (string-match "^/p/[[:alpha:]]+/[[:digit:]]+$" query)
          (string-match "^/[[:alpha:]]+$" query)
          (string-match "^/u/[[:alpha:]]+$" query)))))

;;;###autoload
(add-hook 'mastodon-mode-hook (lambda ()
                                (when (require 'emojify nil :noerror)
                                  (emojify-mode t)
                                  (when mastodon-toot--enable-custom-instance-emoji
                                    (mastodon-toot--enable-custom-emoji)))))

;;;###autoload
(add-hook 'mastodon-mode-hook #'mastodon-profile--fetch-server-account-settings)

(define-derived-mode mastodon-mode special-mode "Mastodon"
  "Major mode for Mastodon, the federated microblogging network."
  :group 'mastodon
  (read-only-mode 1))

(provide 'mastodon)
;;; mastodon.el ends here
