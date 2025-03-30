;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2024 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <mousebot@disroot.org>
;; Maintainer: Marty Hiatt <mousebot@disroot.org>
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

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'mastodon-widget)
(require 'map)

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-params-async-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-tl--byline "mastodon-tl")
(autoload 'mastodon-tl--byline-author "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--content "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl")
(autoload 'mastodon-tl--init "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--spoiler "mastodon-tl")
(autoload 'mastodon-tl--item-id "mastodon-tl")
(autoload 'mastodon-tl-update "mastodon-tl")
(autoload 'mastodon-views-view-follow-requests "mastodon-views")
(autoload 'mastodon-tl--current-filters "mastodon-views")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-notifications-get "mastodon")
(autoload 'mastodon-tl--byline-uname-+-handle "mastodon-tl")
(autoload 'mastodon-tl--byline-handle "mastodon-tl")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-media--get-avatar-rendering "mastodon-media")
(autoload 'mastodon-tl--image-trans-check "mastodon-tl")
(autoload 'mastodon-tl--symbol "mastodon-tl")
(autoload 'mastodon-tl--display-or-uname "mastodon-tl")
(autoload 'mastodon-tl-goto-next-item "mastodon-tl")
(autoload 'mastodon-tl--buffer-type-eq "mastodon-tl")
(autoload 'mastodon-tl--buffer-property "mastodon-tl")
(autoload 'mastodon-http--patch "mastodon-http")
(autoload 'mastodon-views--minor-view "mastodon-views")
(autoload 'mastodon-tl--goto-first-item "mastodon-tl")
(autoload 'mastodon-tl--init-sync "mastodon-tl")

;; notifications defcustoms moved into mastodon.el
;; as some need to be available without loading this file

(defvar mastodon-tl--shr-map-replacement)
(defvar mastodon-tl--horiz-bar)
(defvar mastodon-active-user)
(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--display-media-p)
(defvar mastodon-mode-map)
(defvar mastodon-tl--fold-toots-at-length)
(defvar mastodon-tl--show-avatars)
(defvar mastodon-profile-note-in-foll-reqs)
(defvar mastodon-profile-note-in-foll-reqs-max-length)
(defvar mastodon-group-notifications)
(defvar mastodon-notifications-grouped-names-count)
(defvar mastodon-tl--link-keymap)

;;; VARIABLES

(defvar mastodon-notifications--map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-mode-map)
    (define-key map (kbd "a") #'mastodon-notifications-follow-request-accept)
    (define-key map (kbd "j") #'mastodon-notifications-follow-request-reject)
    (define-key map (kbd "C-k") #'mastodon-notifications-clear-current)
    (define-key map (kbd "C-c C-c") #'mastodon-notifications-cycle-type)
    map)
  "Keymap for viewing notifications.")

(defvar mastodon-notifications--types
  '("all" "favourite" "reblog" "mention" "poll"
    "follow_request" "follow" "status" "update"
    "severed_relationships" "moderation_warning")
  "A list of notification types according to their name on the server, plus \"all\".")

(defvar mastodon-notifications--filter-types-alist
  '(("all"                    . mastodon-notifications-get)
    ("favourite"              . mastodon-notifications-get-favourites)
    ("reblog"                 . mastodon-notifications-get-boosts)
    ("mention"                . mastodon-notifications-get-mentions)
    ("poll"                   . mastodon-notifications-get-polls)
    ("follow_request"         . mastodon-notifications-get-follow-requests)
    ("follow"                 . mastodon-notifications-get-follows)
    ("status"                 . mastodon-notifications-get-statuses)
    ("update"                 . mastodon-notifications-get-edits))
  "An alist of notification types and their corresponding load functions.
Notification types are named according to their name on the server.")

(defvar mastodon-notifications--response-alist
  '(("Followed"             . "you")
    ("Favourited"           . "your post")
    ("Boosted"              . "your post")
    ("Mentioned"            . "you")
    ("Posted a poll"        . "that has now ended")
    ("Requested to follow"  . "you")
    ("Posted"               . "a post")
    ("Edited"               . "their post"))
  "Alist of subjects for notification types.")

(defvar mastodon-notifications-grouped-types
  '(follow reblog favourite)
  "List of notification types for which grouping is implemented.")

(defvar mastodon-notifications--action-alist
  '((reblog                . "Boosted")
    (favourite             . "Favourited")
    (follow_request        . "Requested to follow")
    (follow                . "Followed")
    (mention               . "Mentioned")
    (status                . "Posted")
    (poll                  . "Posted a poll")
    (update                . "Edited")
    (severed_relationships . "Relationships severed")
    (moderation_warning    . "Moderation warning"))
  "Action strings keyed by notification type.
Types are those of the Mastodon API.")

(defvar mastodon-notifications--no-status-notif-alist
  '(("moderation_warning"    . moderation_warning)
    ("severed_relationships" . event)
    ("follow"                . follow)
    ("follow_request"        . follow_request)))

;;; VAR FETCHERS

(defun mastodon-notifications--action-alist-get (type)
  "Return an action string for notification TYPE.
Fetch from `mastodon-notifications--action-alist'.
If no match, return empty string."
  (or (alist-get type mastodon-notifications--action-alist)
      ""))

(defun mastodon-notifications--response-alist-get (message)
  "Return a response string for MESSAGE.
Fetch from `mastodon-notifications--response-alist'.
If no match, return empty string."
  (or (alist-get
       message
       mastodon-notifications--response-alist nil nil #'equal)
      ""))

;;; UTILS

(defun mastodon-notifications--api (endpoint)
  "Return a notifications API ENDPOINT.
If `mastodon-group-notifications' is non-nil, use API v2."
  (mastodon-http--api endpoint
                      (when mastodon-group-notifications "v2")))

;;; FOLL REQS

(defun mastodon-notifications--follow-request-process (&optional reject)
  "Process the follow request at point.
With no argument, the request is accepted. Argument REJECT means
reject the request. Can be called in notifications view or in
follow-requests view."
  (if (not (mastodon-tl--find-property-range 'item-json (point)))
      (user-error "No follow request at point?")
    (let* ((item-json (mastodon-tl--property 'item-json))
           (f-reqs-view-p (string= "follow_requests"
                                   (plist-get mastodon-tl--buffer-spec 'endpoint)))
           (f-req-p (or (string= "follow_request"
                                 (mastodon-tl--property 'notification-type
                                                        :no-move))
                        f-reqs-view-p)))
      (if (not f-req-p)
          (user-error "No follow request at point?")
        (let-alist (or (alist-get 'account item-json) ;notifs
                       item-json) ;f-reqs
          (if (not .id)
              (user-error "No account result at point?")
            (let ((response
                   (mastodon-http--post
                    (mastodon-http--api
                     (format "follow_requests/%s/%s"
                             .id (if reject "reject" "authorize"))))))
              (mastodon-http--triage
               response
               (lambda (_)
                 (if f-reqs-view-p
                     (mastodon-views-view-follow-requests)
                   (mastodon-tl--reload-timeline-or-profile))
                 (message "Follow request of %s (@%s) %s!"
                          .username .acct (if reject "rejected" "accepted")))))))))))

(defun mastodon-notifications-follow-request-accept ()
  "Accept a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process))

(defun mastodon-notifications-follow-request-reject ()
  "Reject a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process :reject))

;;; FORMAT NON-STANDARD NOTIFS

(defun mastodon-notifications--severance-body (json)
  "Return a body for a severance notification JSON."
  ;; https://docs.joinmastodon.org/entities/RelationshipSeveranceEvent/
  (let-alist json
    (concat .type ": "
            .target_name
            "\nRelationships affected: "
            "\nFollowers: " (number-to-string .followers_count)
            "\nFollowing: " (number-to-string .following_count))))

(defun mastodon-notifications--mod-warning-body (json)
  "Return a body for a moderation warning notification JSON."
  ;; https://docs.joinmastodon.org/entities/AccountWarning/
  (let-alist json
    (concat .action ": \"" (string-trim .text) "\""
            "\nStatuses: "
            (mastodon-notifications--render-mod-status-links .status_ids)
            "\nfor account: "
            .target_account.acct
            (if .appeal
                (concat "\nYour appeal: \""
                        (alist-get 'text .appeal)
                        "\"")
              "")
            "\nMore info/appeal: "
            (mastodon-notifications--render-mod-link .id))))

(defun mastodon-notifications--propertize-link (url help-echo)
  "Render a plain URL link with HELP-ECHO."
  (propertize
   url
   'face 'shr-link ;; mastodon-display-name-face
   'keymap mastodon-tl--shr-map-replacement
   'mastodon-tab-stop 'shr-url
   'help-echo help-echo
   'follow-link t
   'mouse-face 'highlight
   'shr-url url
   'keymap mastodon-tl--shr-map-replacement))

(defun mastodon-notifications--render-mod-status-links (ids)
  "Render moderation status IDS as URLs."
  (mapconcat (lambda (id)
               (let ((str (format "%s/@%s/%s"
                                  mastodon-instance-url
                                  mastodon-active-user id)))
                 (mastodon-notifications--propertize-link str "view toot")))
             ids ", "))

(defun mastodon-notifications--render-mod-link (id)
  "Render a moderation link for item with ID."
  (let ((str (format "%s/disputes/strikes/%s"
                     mastodon-instance-url id)))
    (mastodon-notifications--propertize-link str "View mod warning")))

;;; FORMAT/INSERT SINGLE NOTIF

(defun mastodon-notifications--format-note (note)
  "Format for a NOTE, a non-grouped notification."
  (let* ((type (intern (alist-get 'type note)))
         (profile-note
          (when (eq 'follow_request type)
            (let ((str (mastodon-tl--field
                        'note
                        (mastodon-tl--field 'account note))))
              (if mastodon-profile-note-in-foll-reqs-max-length
                  (string-limit str mastodon-profile-note-in-foll-reqs-max-length)
                str))))
         (status (mastodon-tl--field 'status note))
         (follower (alist-get 'account note))
         (follower-name (mastodon-notifications--follower-name follower))
         (filtered (mastodon-tl--field 'filtered status))
         (filters (when filtered
                    (mastodon-tl--current-filters filtered))))
    (if (and filtered (assoc "hide" filters))
        nil
      (mastodon-notifications--insert-note
       ;; toot
       ;; should always be note, otherwise notif data not avail
       ;; later on:
       note
       ;; body
       (mastodon-notifications--body-arg
        type filters status profile-note follower-name nil note)
       ;; action-byline (top)
       (mastodon-notifications--action-byline
        type nil nil note follower-name)
       ;; base toot (always provide)
       status
       nil nil nil type))))

(defun mastodon-notifications--format-group-note (group status accounts)
  "Format for a GROUP notification.
STATUS is the status's JSON.
ACCOUNTS is data of the accounts that have reacted to the notification."
  (let ((folded nil))
    ;; FIXME: apply/refactor filtering as per/with `mastodon-tl--toot'
    (let-alist group
      (let* ((type (intern .type))
             (profile-note
              (when (member type '(follow_request))
                (let ((str (mastodon-tl--field 'note (car accounts))))
                  (if mastodon-profile-note-in-foll-reqs-max-length
                      (string-limit str mastodon-profile-note-in-foll-reqs-max-length)
                    str))))
             (follower (when (member type '(follow follow_request))
                         (car accounts)))
             (follower-name (mastodon-notifications--follower-name follower))
             (filtered (mastodon-tl--field 'filtered status))
             (filters (when filtered
                        (mastodon-tl--current-filters filtered))))
        (unless (and filtered (assoc "hide" filters))
          (mastodon-notifications--insert-note
           ;; toot
           (if (member type '(follow follow_request))
               follower
             status)
           ;; body
           (mastodon-notifications--body-arg
            type filters status profile-note follower-name group)
           ;; action-byline
           (mastodon-notifications--action-byline
            type accounts group)
           ;; base toot (no need for update/poll/?)
           (when (member type '(favourite reblog))
             status)
           folded group accounts))))))

(defun mastodon-notifications--follower-name (follower)
  "Return display_name or username of FOLLOWER."
  (if (not (string= "" (alist-get 'display_name follower)))
      (alist-get 'display_name follower)
    (alist-get 'username follower)))

(defun mastodon-notifications--comment-note-text (str)
  "Add comment face to all text in STR with `shr-text' face only."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'face 'shr-text t))
        (add-text-properties (prop-match-beginning prop)
                             (prop-match-end prop)
                             '(face (mastodon-toot-docs-face shr-text)))))
    (buffer-string)))

(defun mastodon-notifications--body-arg
    (type &optional filters status profile-note follower-name group note)
  "Prepare a notification body argument.
The string returned is passed to `mastodon-notifications--insert-note'.
TYPE is a symbol, a member of `mastodon-notifiations--types'.
FILTERS STATUS PROFILE-NOTE FOLLOWER-NAME GROUP NOTE."
  (let ((body
         (if-let* ((match (assoc "warn" filters)))
             (mastodon-tl--spoiler status (cadr match))
           (mastodon-tl--clean-tabs-and-nl
            (cond ((mastodon-tl--has-spoiler status)
                   (mastodon-tl--spoiler status))
                  ((eq type 'follow_request)
                   (mastodon-tl--render-text profile-note))
                  (t (mastodon-tl--content status)))))))
    (cond
     ((not (member (symbol-name type)
                   mastodon-notifications--types))
      "Unknown notification type.")
     ((eq type 'follow)
      (propertize "Congratulations, you have a new follower!"
                  'face 'default
                  'item-type 'follow-request)) ;; nav
     ((eq type 'follow_request)
      (concat
       (propertize (format "You have a follow request from %s"
                           follower-name)
                   'face 'default
                   'item-type 'follow-request) ;; nav
       (when mastodon-profile-note-in-foll-reqs
         (concat
          ":\n"
          (mastodon-notifications--comment-note-text body)))))
     ((eq type 'severed_relationships)
      (mastodon-notifications--severance-body
       (alist-get 'event (or group note))))
     ((eq type 'moderation_warning)
      (mastodon-notifications--mod-warning-body
       (alist-get 'moderation_warning (or group note))))
     ((member type '(favourite reblog))
      (propertize
       (mastodon-notifications--comment-note-text body)))
     (t body))))

(defun mastodon-notifications--insert-note
    (toot body action-byline
          &optional base-toot unfolded group accounts type)
  "Display the content and byline of timeline element TOOT.
BODY will form the section of the toot above the byline.
AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.
ACTION-BYLINE is a string, obtained by calling
`mastodon-notifications--action-byline'.
BASE-TOOT is the JSON of the toot responded to.
UNFOLDED is a boolean meaning whether to unfold or fold item if
foldable.
GROUP is the notification group data.
ACCOUNTS is the notification accounts data.
TYPE is notification type, used for non-group notifs."
  (let* ((type (if type
                   (symbol-name type) ;; non-group
                 (alist-get 'type group)))
         (toot-foldable
          (and mastodon-tl--fold-toots-at-length
               (length> body mastodon-tl--fold-toots-at-length)))
         (ts ;; types listed here use base item timestamp, else we use
          ;; group's latest timestamp:
          (when (and group
                     (not
                      (member type '("favourite" "reblog" "edit" "poll"))))
            (mastodon-tl--field 'latest_page_notification_at group))))
    (insert
     (propertize ;; top byline, body + byline:
      (concat
       (if (equal type "mention") ;; top (action) byline
           ""
         action-byline)
       (propertize body ;; body only
                   'toot-body t) ;; includes newlines etc. for folding
       "\n"
       ;; actual byline:
       (if (member type '("severed_relationships" "moderation_warning"))
           (propertize
            (concat mastodon-tl--horiz-bar "\n")
            'byline t)
         (mastodon-tl--byline toot nil nil base-toot group ts)))
      'item-type     'toot ;; for nav, actions, etc.
      'item-id       (or (alist-get 'page_max_id group) ;; newest notif
                         (alist-get 'id toot)) ; toot id
      'base-item-id  (mastodon-tl--item-id
                      ;; if status is a notif, get id from base-toot
                      ;; (-tl--item-id toot) will not work here:
                      (or base-toot
                          toot)) ; else normal toot with reblog check
      'item-json     toot
      'base-toot     base-toot
      'cursor-face   'mastodon-cursor-highlight-face
      'toot-foldable toot-foldable
      'toot-folded   (and toot-foldable (not unfolded))
      ;; grouped notifs data:
      'notification-type type
      'notification-id (alist-get 'group_key group)
      'notification-group group
      'notification-accounts accounts
      ;; for pagination:
      'notifications-min-id (alist-get 'page_min_id group)
      'notifications-max-id (alist-get 'page_max_id group))
     "\n")))

;;; BYLINES

(defun mastodon-notifications--action-byline
    (type &optional accounts group note follower-name)
  "Return an action (top) byline for notification of TYPE.
ACCOUNTS and GROUP group are used by grouped notifications.
NOTE and FOLLOWER-NAME are used for non-grouped notifs."
  (let* ((str-prefix (mastodon-notifications--action-alist-get type))
         (action-str
          (unless (member type '(follow follow_request mention))
            (downcase
             (mastodon-notifications--byline-action-str
              str-prefix))))
         (action-symbol (if (eq type 'mention)
                            ""
                          (mastodon-tl--symbol type)))
         (action-authors
          (cond
           ((not (member (symbol-name type)
                         mastodon-notifications--types))
            "")
           ((member type
                    '(follow follow_request mention
                             severed_relationships moderation_warning))
            "") ;; mentions are normal statuses
           (group
            (mastodon-notifications--byline-accounts accounts group))
           (t (mastodon-tl--byline-handle note nil
                                          follower-name
                                          'mastodon-display-name-face)))))
    (propertize
     (concat action-symbol " " action-authors action-str)
     'byline-top t)))

(defun mastodon-notifications--byline-action-str (message)
  "Return an action (top) byline string for TOOT with MESSAGE."
  (let ((resp (mastodon-notifications--response-alist-get message)))
    (concat " "
            (propertize message 'face 'mastodon-boosted-face)
            " " resp "\n")))

(defun mastodon-notifications--alist-by-value (str field json)
  "From JSON, return the alist whose FIELD value matches STR.
JSON is a list of alists."
  (cl-some (lambda (y)
             (when (string= str (alist-get field y))
               y))
           json))

(defun mastodon-notifications--group-accounts (ids json)
  "For IDS, return account data in JSON."
  (cl-loop
   for x in ids
   collect (mastodon-notifications--alist-by-value x 'id json)))

(defun mastodon-notifications--byline-accounts
    (accounts group &optional avatar)
  "Propertize author byline ACCOUNTS.
GROUP is the group notification data.
When AVATAR, include the account's avatar image."
  (let ((total (alist-get 'notifications_count group))
        (accts mastodon-notifications-grouped-names-count))
    (concat
     (string-trim ;; remove trailing newline
      (cl-loop
       for account in accounts
       repeat accts
       concat
       (let-alist account
         (concat
          ;; avatar insertion moved up to `mastodon-tl--byline' by
          ;; default to be outside 'byline propt.
          (when (and avatar ; used by `mastodon-profile--format-user'
                     mastodon-tl--show-avatars
                     mastodon-tl--display-media-p
                     (mastodon-tl--image-trans-check))
            (mastodon-media--get-avatar-rendering .avatar))
          (let ((uname (mastodon-tl--display-or-uname account)))
            (mastodon-tl--byline-handle account nil
                                        uname 'mastodon-display-name-face))
          ", ")))
      nil ", ")
     (if (< accts total)
         (let ((diff (- total accts)))
           (propertize ;; help-echo remaining notifs authors:
            (format " and %s other%s" diff (if (= 1 diff) "" "s"))
            'help-echo (mapconcat (lambda (a)
                                    (propertize (alist-get 'username a)
                                                'face 'mastodon-display-name-face))
                                  (cddr accounts) ;; not first two
                                  ", ")))))))

;;; LOAD TIMELINE

(defun mastodon-notifications--render (json no-group)
  "Display grouped notifications in JSON.
NO-GROUP means don't render grouped notifications."
  ;; (setq masto-grouped-notifs json)
  (if no-group
      (cl-loop for x in json
               do (mastodon-notifications--format-note x))
    (let ((groups (alist-get 'notification_groups json)))
      (cl-loop
       for g in groups
       for start-pos = (point)
       for accounts = (mastodon-notifications--group-accounts
                       (alist-get 'sample_account_ids g)
                       (alist-get 'accounts json))
       for type = (alist-get 'type g)
       for status = (mastodon-notifications--status-or-event g type json)
       do (mastodon-notifications--format-group-note g status accounts)
       (when mastodon-tl--display-media-p
         ;; images-in-notifs custom is handeld in
         ;; `mastodon-tl--media-attachment', not here
         (mastodon-media--inline-images start-pos (point)))))))

(defun mastodon-notifications--status-or-event (group type json)
  "Return a notification's status or event data.
Using GROUP data, notification TYPE, and overall notifs JSON."
  (if (member type (map-keys mastodon-notifications--no-status-notif-alist))
      ;; notifs w no status data:
      (let ((key (alist-get type mastodon-notifications--no-status-notif-alist
                            nil nil #'equal)))
        (alist-get key group))
    (mastodon-notifications--alist-by-value
     (alist-get 'status_id group)
     'id
     (alist-get 'statuses json))))

(defun mastodon-notifications--timeline (json &optional type update)
  "Format JSON in Emacs buffer.
Optionally specify TYPE.
UPDATE means we are updating, so skip some things."
  (if (seq-empty-p json)
      (user-error "Looks like you have no (more) notifications for now")
    (unless update
      (mastodon-widget--create
       "Filter" mastodon-notifications--types
       (or type "all")
       (lambda (widget &rest _ignore)
         (let ((value (widget-value widget)))
           (mastodon-notifications-get-type value)))
       :newline)
      (insert "\n"))
    ;; filtered/requests message:
    (when (mastodon-notifications--notif-requests)
      (insert
       (substitute-command-keys
        "You have filtered notifications. \
\\[mastodon-notifications-requests] to view requests.\n\n")))
    ;; render:
    (mastodon-notifications--render json
                                    (not mastodon-group-notifications))
    (goto-char (point-min))
    ;; set last read notif ID:
    (save-excursion
      (mastodon-tl-goto-next-item :no-refresh)
      (let ((id (mastodon-tl--property 'item-id))) ;; notif not base
        (mastodon-notifications--set-last-read id)))
    (unless update ;; already in tl--update
      (mastodon-tl-goto-next-item))))

;;; VIEW LOADING FUNCTIONS

(defun mastodon-notifications-get-type (&optional type)
  "Read a notification type and load its timeline.
Optionally specify TYPE."
  (interactive)
  (let ((choice (or type
                    (completing-read
                     "View notifications: "
                     mastodon-notifications--filter-types-alist))))
    (funcall (alist-get
              choice mastodon-notifications--filter-types-alist
              nil nil #'equal))))

(defun mastodon-notifications-cycle-type (&optional prefix)
  "Cycle the current notifications view.
With arg PREFIX, `completing-read' a type and load it."
  (interactive "P")
  ;; FIXME: do we need a sept buffer-type result for all notifs views?
  (if (not (or (mastodon-tl--buffer-type-eq 'notifications)
               (mastodon-tl--buffer-type-eq 'mentions)))
      (user-error "Not in a notifications view")
    (let* ((choice
            (if prefix
                (completing-read "Notifs by type: "
                                 mastodon-notifications--types)
              (mastodon-notifications--get-next-type)))
           (fun (alist-get choice mastodon-notifications--filter-types-alist
                           nil nil #'equal)))
      (funcall fun))))

(defun mastodon-notifications--current-type ()
  "Return the current notification type or nil."
  (let* ((update-params (mastodon-tl--buffer-property
                         'update-params nil :no-error)))
    (alist-get "types[]" update-params nil nil #'equal)))

(defun mastodon-notifications--get-next-type ()
  "Return the next notif type based on current buffer spec."
  (let* ((type (mastodon-notifications--current-type)))
    (if (not type)
        (cadr mastodon-notifications--types)
      (or (cadr (member type mastodon-notifications--types))
          (car mastodon-notifications--types)))))

(defun mastodon-notifications-get-mentions ()
  "Display mention notifications in buffer."
  (interactive)
  (mastodon-notifications-get "mention" "mentions"))

(defun mastodon-notifications-get-favourites ()
  "Display favourite notifications in buffer."
  (interactive)
  (mastodon-notifications-get "favourite" "favourites"))

(defun mastodon-notifications-get-boosts ()
  "Display boost notifications in buffer."
  (interactive)
  (mastodon-notifications-get "reblog" "boosts"))

(defun mastodon-notifications-get-polls ()
  "Display poll notifications in buffer."
  (interactive)
  (mastodon-notifications-get "poll" "polls"))

(defun mastodon-notifications-get-statuses ()
  "Display status notifications in buffer.
Status notifications are created when you call
`mastodon-tl-enable-notify-user-posts'."
  (interactive)
  (mastodon-notifications-get "status" "statuses"))

(defun mastodon-notifications-get-follows ()
  "Display follow notifications in buffer."
  (interactive)
  (mastodon-notifications-get "follow" "follows"))

(defun mastodon-notifications-get-follow-requests ()
  "Display follow request notifications in buffer."
  (interactive)
  (mastodon-notifications-get "follow_request" "follow-requests"))

(defun mastodon-notifications-get-edits ()
  "Display edited post notifications in buffer."
  (interactive)
  (mastodon-notifications-get "update" "edits"))

(defun mastodon-notifications--filter-types-list (type)
  "Return a list of notification types with TYPE removed."
  (remove type mastodon-notifications--types))

;;; CLEAR/DISMISS NOTIFS

(defun mastodon-notifications-clear-all ()
  "Clear all notifications."
  (interactive)
  (when (y-or-n-p "Clear all notifications?")
    (let ((response
           (mastodon-http--post
            (mastodon-notifications--api "notifications/clear"))))
      (mastodon-http--triage
       response (lambda (_)
                  (when mastodon-tl--buffer-spec
                    (mastodon-tl--reload-timeline-or-profile))
                  (message "All notifications cleared!"))))))

(defun mastodon-notifications-clear-current ()
  "Dismiss the notification at point."
  (interactive)
  (let* ((id (or ;; grouping enabled
              ;; (*should* also work for ungrouped items):
              (mastodon-tl--property 'notification-id)
              ;; FIXME: are these all required?
              (mastodon-tl--property 'item-id)
              (mastodon-tl--field
               'id
               (mastodon-tl--property 'item-json))))
         (endpoint (mastodon-notifications--api
                    (format "notifications/%s/dismiss" id)))
         (response (mastodon-http--post endpoint)))
    (mastodon-http--triage
     response (lambda (_)
                (when mastodon-tl--buffer-spec
                  (mastodon-tl--reload-timeline-or-profile))
                (message "Notification dismissed!")))))

;;; MISC

(defun mastodon-notifications--set-last-read (id)
  "Set the last read notification ID on the server."
  (let ((endpoint (mastodon-http--api "markers"))
        (params `(("notifications[last_read_id]" . ,id))))
    (mastodon-http--post endpoint params)))

(defun mastodon-notifications--get-last-read ()
  "Return the last read notification ID from the server."
  (let* ((params '(("timeline[]" . "notifications")))
         (endpoint (mastodon-http--api "markers"))
         (resp (mastodon-http--get-json endpoint params)))
    (map-nested-elt resp '(notifications last_read_id))))

(defun mastodon-notifications-get-single-notif ()
  "Return a single notification JSON for v2 notifs."
  (interactive)
  (let* ((id ;; grouped (should work for ungrouped items):
          (mastodon-tl--property 'notification-id))
         (endpoint (mastodon-notifications--api
                    (format "notifications/%s" id)))
         (response (mastodon-http--get-json endpoint)))
    (message "%s" (prin1-to-string response))))

(defun mastodon-notifications--get-unread-count ()
  "Return the number of unread notifications for the current account."
  ;; params: limit - max 1000, default 100, types[], exclude_types[], account_id
  (let* ((endpoint "notifications/unread_count")
         (url (mastodon-http--api endpoint
                                  (when mastodon-group-notifications "v2")))
         (resp (mastodon-http--get-json url)))
    (alist-get 'count resp)))

;;; NOTIFICATION REQUESTS / FILTERING / POLICY

(defvar mastodon-notifications--requests-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-mode-map)
    (define-key map (kbd "j") #'mastodon-notifications-request-reject)
    (define-key map (kbd "a") #'mastodon-notifications-request-accept)
    (define-key map (kbd "g") #'mastodon-notifications-requests)
    map)
  "Keymap for viewing follow requests.")

;; FIXME: these are only for grouped notifs, else the fields are JSON bools
(defvar mastodon-notifications-policy-vals
  '("accept" "filter" "drop"))

(defun mastodon-notifications-get-policy ()
  "Return the notification filtering policy."
  (let ((url (mastodon-notifications--api "notifications/policy")))
    (mastodon-http--get-json url)))

(defun mastodon-notifications--notif-requests ()
  "Non-nil if the user currently has pending/filtered notifications.
Returns"
  (let* ((policy (mastodon-notifications-get-policy))
         (count (map-nested-elt policy '(summary pending_notifications_count))))
    (if (and count (> count 0))
        count)))

(defun mastodon-notifications--pending-p ()
  "Non-nil if there are any pending requests or notifications."
  (let* ((json (mastodon-notifications-get-policy))
         (summary (alist-get 'summary json)))
    (or (not (= 0 (alist-get 'pending_requests_count summary)))
        (not (= 0 (alist-get 'pending_notifications_count summary))))))

(defun mastodon-notifications--update-policy (&optional params)
  "Update notifications filtering policy.
PARAMS is an alist of parameters."
  ;; https://docs.joinmastodon.org/methods/notifications/#update-the-filtering-policy-for-notifications
  (let ((url (mastodon-notifications--api "notifications/policy")))
    (mastodon-http--patch url params)))

(defun mastodon-notifications--get-requests (&optional params)
  "Get a list of notification requests data from the server.
PARAMS is an alist of parameters."
  ;; NB: link header pagination
  (let ((url (mastodon-notifications--api "notifications/requests")))
    (mastodon-http--get-json url params)))

(defun mastodon-notifications-request-accept (&optional reject)
  "Accept a notification request for a user.
This will merge any filtered notifications from them into the main
notifications and accept any future notification from them.
REJECT means reject notifications instead."
  ;; POST /api/v1/notifications/requests/:id/accept
  (interactive)
  (let* ((id (mastodon-tl--property 'item-id))
         (user (mastodon-tl--property 'notif-req-user))
         (url (mastodon-http--api
               (format "notifications/requests/%s/%s"
                       id (if reject "dismiss" "accept"))))
         (resp (mastodon-http--post url)))
    (mastodon-http--triage
     resp
     (lambda (_resp)
       (message "%s notifications from %s"
                (if reject "Not accepting" "Accepting") user)
       ;; reload view:
       (mastodon-notifications-requests)))))

(defun mastodon-notifications-request-reject ()
  "Reject a notification request for a user.
Rejecting a request means any notifications from them will continue to
be filtered."
  (interactive)
  (mastodon-notifications-request-accept :reject))

(defun mastodon-notifications-requests ()
  "Open a new buffer displaying the user's notification requests."
  ;; calqued off `mastodon-views-view-follow-requests'
  (interactive)
  (mastodon-tl--init-sync
   "notification-requests"
   "notifications/requests"
   'mastodon-views--insert-notification-requests
   nil
   '(("limit" . "40")) ; server max is 80
   :headers
   "notification requests"
   "a/j - accept/reject request at point\n\
 n/p - go to next/prev request\n\
 \\[mastodon-notifications-policy] - set filtering policy")
  (mastodon-tl--goto-first-item)
  (with-current-buffer "*mastodon-notification-requests*"
    (use-local-map mastodon-notifications--requests-map)))

(defun mastodon-views--insert-notification-requests (json)
  "Insert the user's current notification requests.
JSON is the data returned by the server."
  (mastodon-views--minor-view
   "notification requests"
   #'mastodon-notifications--insert-users
   json))
;; masto-notif-req))

(defun mastodon-notifications--insert-users (json)
  "Insert users list into the buffer.
JSON is the data from the server."
  ;; calqued off `mastodon-views--insert-users-propertized-note'
  ;; and `mastodon-search--insert-users-propertized'
  (mapc (lambda (req)
          (insert
           (concat
            (mastodon-notifications--format-req-user req)
            mastodon-tl--horiz-bar "\n\n")))
        json))

(defun mastodon-notifications--format-req-user (req &optional note)
  "Format a notification request user, REQ.
NOTE means to include a profile note."
  ;; calqued off `mastodon-search--propertize-user'
  (let-alist req
    (propertize
     (concat
      (propertize .account.username
                  'face 'mastodon-display-name-face
                  'byline t
                  'notif-req-user .account.username
                  'item-type 'notif-req
                  'item-id .id) ;; notif req id
      " : \n : "
      (propertize (concat "@" .account.acct)
                  'face 'mastodon-handle-face
                  'mouse-face 'highlight
		  'mastodon-tab-stop 'user-handle
		  'keymap mastodon-tl--link-keymap
                  'mastodon-handle (concat "@" .account.acct)
		  'help-echo (concat "Browse user profile of @" .account.acct))
      " : \n"
      (when note
        (mastodon-tl--render-text .account.note .account))
      "\n")
     'item-json req)))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
