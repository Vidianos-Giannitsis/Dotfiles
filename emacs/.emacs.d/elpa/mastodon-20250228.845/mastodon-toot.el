;;; mastodon-toot.el --- Minor mode for sending Mastodon toots  -*- lexical-binding: t -*-

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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:
(eval-when-compile (require 'subr-x))


(defvar mastodon-use-emojify)
(require 'emojify nil :noerror)
(declare-function emojify-insert-emoji "emojify")
(declare-function emojify-set-emoji-data "emojify")
(declare-function emojify-mode "emojify")
(declare-function emojify-emojis-each "emojify")
(defvar emojify-emojis-dir)
(defvar emojify-user-emojis)
(defvar emojify-emoji-styles)

(require 'cl-lib)
(require 'persist)
(require 'mastodon-iso)
(require 'facemenu)
(require 'text-property-search)

(eval-when-compile
  (require 'mastodon-tl))

(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--enable-proportional-fonts)
(defvar mastodon-profile-account-settings)
(defvar mastodon-profile-acccount-preferences-data)

(autoload 'iso8601-parse "iso8601")
(autoload 'ht-get "ht")
(autoload 'mastodon-auth--user-acct "mastodon-auth")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--build-array-params-alist "mastodon-http")
(autoload 'mastodon-http--delete "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--get-json-async "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--post-media-attachment "mastodon-http")
(autoload 'mastodon-http--process-json "mastodon-http")
(autoload 'mastodon-http--put "mastodon-http")
(autoload 'mastodon-http--read-file-as-string "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-kill-window "mastodon")
(autoload 'mastodon-profile--fetch-server-account-settings "mastodon-profile")
(autoload 'mastodon-profile--fetch-server-account-settings-maybe "mastodon-profile")
(autoload 'mastodon-profile--get-source-pref "mastodon-profile")
(autoload 'mastodon-profile-show-user "mastodon-profile")
(autoload 'mastodon-profile--update-preference "mastodon-profile")
(autoload 'mastodon-search--search-accounts-query "mastodon-search")
(autoload 'mastodon-search--search-tags-query "mastodon-search")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-tl--buffer-type-eq "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl-goto-next-item "mastodon-tl")
(autoload 'mastodon-tl--map-alist "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-tl--set-buffer-spec "mastodon-tl")
(autoload 'mastodon-tl--symbol "mastodon-tl")
(autoload 'mastodon-tl--item-id "mastodon-tl")
(autoload 'mastodon-toot "mastodon")
(autoload 'mastodon-views-cancel-scheduled-toot "mastodon-views")
(autoload 'mastodon-views-view-scheduled-toots "mastodon-views")
(autoload 'org-read-date "org")
(autoload 'mastodon-tl--toot-or-base "mastodon-tl")
(autoload 'mastodon-profile--get-source-value "mastodon-toot")
(autoload 'mastodon-tl--get-buffer-type "mastodon-tl")
(autoload 'mastodon-tl--human-duration "mastodon-tl")
(autoload 'mastodon-profile--get-preferences-pref "mastodon-profile")
(autoload 'mastodon-views--get-own-instance "mastodon-views")
(autoload 'mastodon-tl--image-trans-check "mastodon-tl")
(autoload 'mastodon-instance-data "mastodon")
(autoload 'mastodon-create-poll "mastodon-transient")

;; for mastodon-toot-translate-toot-text
(autoload 'mastodon-tl--content "mastodon-tl")
(when (require 'lingva nil :no-error)
  (declare-function lingva-translate "lingva"))

(defgroup mastodon-toot nil
  "Tooting in Mastodon."
  :prefix "mastodon-toot-"
  :group 'mastodon)

(defcustom mastodon-toot--default-media-directory "~/"
  "The default directory when prompting for a media file to upload."
  :type 'string)

(defcustom mastodon-toot--attachment-height 80
  "Height of the attached images preview in the toot draft buffer."
  :type 'integer)

(defcustom mastodon-toot--enable-completion t
  "Whether to enable completion of mentions and hashtags.
Used for completion in toot compose buffer."
  :type 'boolean)

(defcustom mastodon-toot--use-company-for-completion nil
  "Whether to enable company for completion.
When non-nil, `company-mode' is enabled in the toot compose
buffer, and mastodon completion backends are added to
`company-capf'.

You need to install company yourself to use this."
  :type 'boolean)

(defcustom mastodon-toot--completion-style-for-mentions "all"
  "The company completion style to use for mentions."
  :type '(choice
          (const :tag "off" nil)
          (const :tag "following only" "following")
          (const :tag "all users" "all")))

(defcustom mastodon-toot-display-orig-in-reply-buffer nil
  "Display a copy of the toot replied to in the compose buffer."
  :type 'boolean)

(defcustom mastodon-toot-orig-in-reply-length 191
  ;; three lines of divider width: (- (* 3 67) (length " Reply to: "))
  "Length to crop toot replied to in the compose buffer to."
  :type 'integer)

(defcustom mastodon-toot--default-reply-visibility "public"
  "Default visibility settings when replying.
If the original toot visibility is different we use the more restricted one."
  :type '(choice
          (const :tag "public" "public")
          (const :tag "unlisted" "unlisted")
          (const :tag "followers only" "private")
          (const :tag "direct" "direct")))

(defcustom mastodon-toot--enable-custom-instance-emoji nil
  "Whether to enable your instance's custom emoji by default."
  :type 'boolean)

(defcustom mastodon-toot--proportional-fonts-compose nil
  "Nonnil to enable using proportional fonts in the compose buffer.
By default fixed width fonts are used."
  :type '(boolean :tag "Enable using proportional rather than fixed \
width fonts"))

(defcustom mastodon-toot-poll-use-transient t
  "Whether to use the transient menu to create a poll."
  :type '(boolean))

(defvar-local mastodon-toot--content-warning nil
  "The content warning of the current toot.")

(defvar-local mastodon-toot--content-nsfw nil
  "A flag indicating whether the toot should be marked as NSFW.")

(defvar mastodon-toot-visibility-list
  '(public unlisted private direct)
  "A list of the available toot visibility settings.")

(defvar mastodon-toot-visibility-settings-list
  '("public" "unlisted" "private")
  "A list of the available default toot visibility settings.
Like `mastodon-toot-visibility-list' but without direct.")

(defvar-local mastodon-toot--visibility nil
  "A string indicating the visibility of the toot being composed.
Valid values are \"direct\", \"private\" (followers-only),
\"unlisted\", and \"public\".

This is determined by the account setting on the server. To
change the setting on the server, see
`mastodon-toot-set-default-visibility'.")

(defvar-local mastodon-toot--media-attachments nil
  "A list of the media attachments of the toot being composed.")

(defvar-local mastodon-toot--media-attachment-ids nil
  "A list of any media attachment ids of the toot being composed.")

(defvar mastodon-toot-poll nil
  "A plist of poll options for the toot being composed.")

(defvar-local mastodon-toot--language nil
  "The language of the toot being composed, in ISO 639 (two-letter).")

(defvar-local mastodon-toot--scheduled-for nil
  "An ISO 8601 timestamp that specifying when the post should be published.
Should be at least 5 minutes into the future.")

(defvar-local mastodon-toot--scheduled-id nil
  "The id of the scheduled post that we are now editing.")

(defvar-local mastodon-toot--reply-to-id nil
  "Buffer-local variable to hold the id of the toot being replied to.")

(defvar-local mastodon-toot--edit-item-id nil
  "The id of the toot being edited.")

(defvar-local mastodon-toot-previous-window-config nil
  "A list of window configuration prior to composing a toot.
Takes its form from `window-configuration-to-register'.")

(defvar mastodon-toot--max-toot-chars nil
  "The maximum allowed characters count for a single toot.")

(defvar-local mastodon-toot-completions nil
  "The data of completion candidates for the current completion at point.")

(defvar mastodon-toot-current-toot-text nil
  "The text of the toot being composed.")

(persist-defvar mastodon-toot-draft-toots-list nil
  "A list of toots that have been saved as drafts.
For the moment we just put all composed toots in here, as we want
to also capture toots that are \"sent\" but that don't successfully
send.")


;;; REGEXES

(defvar mastodon-toot-handle-regex
  (rx (| (any ?\( "\n" "\t "" ") bol) ; preceding things
      (group-n 2 (+ ?@ (* (any ?- ?_ ?. "A-Z" "a-z" "0-9" ))) ; handle
               (? ?@ (* (not (any "\n" "\t" " "))))) ; optional domain
      (| "'" word-boundary))) ; boundary or possessive

(defvar mastodon-toot-tag-regex
  (rx (| (any ?\( "\n" "\t" " ") bol)
      (group-n 2 ?# (+ (any "A-Z" "a-z" "0-9")))
      (| "'" word-boundary))) ; boundary or possessive

(defvar mastodon-toot-emoji-regex
  (rx (| (any ?\( "\n" "\t" " ") bol)
      (group-n 2 ?: ; opening :
               (+ (any "A-Z" "a-z" "0-9" "_"))
               (? ?:)) ; closing :
      word-boundary)) ; boundary

(defvar mastodon-toot-url-regex
  ;; adapted from ffap-url-regexp
  (concat
   "\\(?2:\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)" ; uri prefix
   "[^ \n\t]*\\)" ; any old thing, that is, i.e. we allow invalid/unwise chars
   ;; "[ .,:;!?]\\b"))
   ;; "/" ; poss an ending slash? incompat with boundary end:
   "\\>")) ; boundary end


;;; UTILS

(defun mastodon-toot--base-toot-or-item-json ()
  "Return the JSON data of either base-toot or item-json property.
The former is for boost or favourite notifications, returning
data about the item boosted or favourited."
  (or (mastodon-tl--property 'base-toot :no-move) ; fave/boost notifs
      (mastodon-tl--property 'item-json)))

(defun mastodon-toot--inc-or-dec (count subtract)
  "If SUBTRACT, decrement COUNT, else increment."
  (if subtract
      (1- count)
    (1+ count)))


;;; MACRO

(defmacro mastodon-toot--with-toot-item (&rest body)
  "Execute BODY if we have a toot object at point.
Includes boosts, and notifications that display toots.
This macro makes the local variable ID available."
  (declare (debug t))
  `(if (or (not (eq 'toot (mastodon-tl--property 'item-type :no-move)))
           (member (mastodon-tl--property 'notification-type :no-move)
                   '("follow" "follow_request")))
       (user-error "Looks like there's no toot at point?")
     (mastodon-tl--with-toot-helper
      (lambda (id)
        ,@body))))

(defun mastodon-tl--with-toot-helper (body-fun)
  "Helper function for `mastodon-tl--with-toot-item'.
Extract any common variables needed, such as base-item-id
property, and call BODY-FUN on them."
  (let ((id (mastodon-tl--property 'base-item-id)))
    (funcall body-fun id)))


;;; MODE MAP

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot-send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot-cancel)
    (define-key map (kbd "C-c C-w") #'mastodon-toot-set-content-warning)
    (define-key map (kbd "C-c C-n") #'mastodon-toot-toggle-nsfw)
    (define-key map (kbd "C-c C-v") #'mastodon-toot-change-visibility)
    (define-key map (kbd "C-c C-e") #'mastodon-toot-insert-emoji)
    (define-key map (kbd "C-c C-a") #'mastodon-toot-attach-media)
    (define-key map (kbd "C-c !") #'mastodon-toot-clear-all-attachments)
    (define-key map (kbd "C-c C-p") #'mastodon-toot-create-poll)
    (define-key map (kbd "C-c C-o") #'mastodon-toot-clear-poll)
    (define-key map (kbd "C-c C-l") #'mastodon-toot-set-toot-language)
    (define-key map (kbd "C-c C-s") #'mastodon-toot-schedule-toot)
    map)
  "Keymap for `mastodon-toot'.")

(defun mastodon-toot-set-default-visibility ()
  "Set the default visibility for toots on the server."
  (interactive)
  (let ((vis (completing-read "Set default visibility to:"
                              mastodon-toot-visibility-list
                              nil t)))
    (mastodon-profile--update-preference "privacy" vis :source)))

(defun mastodon-toot--get-max-toot-chars (&optional no-toot)
  "Fetch max_toot_chars from `mastodon-instance-url' asynchronously.
NO-TOOT means we are not calling from a toot buffer."
  (mastodon-http--get-json-async
   (mastodon-http--api "instance")
   nil
   'mastodon-toot--get-max-toot-chars-callback no-toot))

(defun mastodon-toot--get-max-toot-chars-callback (json-response
                                                   &optional no-toot)
  "Set max_toot_chars returned in JSON-RESPONSE and display in new toot buffer.
NO-TOOT means we are not calling from a toot buffer."
  (let ((max-chars
         (or (alist-get 'max_toot_chars json-response)
             (alist-get 'max_characters ; some servers have this instead
                        (alist-get 'statuses
                                   (alist-get 'configuration
                                              json-response))))))
    (setq mastodon-toot--max-toot-chars max-chars)
    (unless no-toot
      (with-current-buffer "*new toot*"
        (mastodon-toot--update-status-fields)))))

(defun mastodon-toot--action-success (marker byline-region remove &optional json)
  "Insert/remove the text MARKER with `success' face in byline.
BYLINE-REGION is a cons of start and end pos of the byline to be
modified.
Remove MARKER if REMOVE is non-nil, otherwise add it.
JSON is added to the string as its item-json."
  (let ((inhibit-read-only t)
        (bol (car byline-region))
        (eol (cdr byline-region))
        (at-byline-p (eq t (mastodon-tl--property 'byline :no-move))))
    (save-excursion
      (when remove
        (goto-char bol)
        (beginning-of-line) ;; The marker is not part of the byline
        (if (search-forward (format "(%s) " marker) eol :no-error)
            (replace-match "")
          (user-error "Oops: could not find marker '(%s)'" marker)))
      (unless remove
        (goto-char bol)
        (insert
         (propertize
          (format "(%s) "
                  (propertize marker
                              'face 'success))
          'cursor-face 'mastodon-cursor-highlight-face
          'item-json json)))) ;; for (un)folding items
    (when at-byline-p
      ;; leave point after the marker:
      (unless remove
        ;; if point is inside the byline, back up first so
        ;; we don't move to the following toot:
        (beginning-of-line)
        (forward-line -1)
        (mastodon-tl-goto-next-item)))))

(defun mastodon-toot--action (action callback)
  "Take ACTION, a string, on toot at point, then execute CALLBACK.
Makes a POST request to the server. Used for favouriting,
boosting, or bookmarking toots."
  (let* ((id (mastodon-tl--property 'base-item-id))
         (url (mastodon-http--api
               (concat "statuses/" (mastodon-tl--as-string id) "/" action)))
         (response (mastodon-http--post url)))
    (mastodon-http--triage response callback)))

(defun mastodon-toot--toggle-boost-or-favourite (action)
  "Toggle boost or favourite of toot at point.
ACTION is a symbol, either `favourite' or `boost.'"
  (mastodon-toot--with-toot-item
   (let* ((n-type (mastodon-tl--property 'notification-type :no-move))
          (byline-region (mastodon-tl--find-property-range 'byline (point)))
          (boost-p (eq action 'boost))
          (action-str (symbol-name action))
          (item-json (mastodon-tl--property 'item-json))
          (vis (mastodon-tl--field 'visibility item-json)))
     (cond
      ((not byline-region)
       (user-error "Nothing to %s here?!?" action-str))
      ;; there's nothing wrong with faving/boosting own toots
      ;; & nothing wrong with faving/boosting own toots from notifs,
      ;; it boosts/faves the base toot, not the notif status
      ((or (string= n-type "follow")
           (string= n-type "follow_request"))
       (user-error "Can't %s %s notifications" action n-type))
      ((and boost-p
            (or (string= vis "direct")
                (string= vis "private")))
       (user-error "Can't boost posts with visibility: %s" vis))
      (t
       (let* ((boosted (when byline-region
                         (get-text-property (car byline-region) 'boosted-p)))
              (faved (when byline-region
                       (get-text-property (car byline-region) 'favourited-p)))
              (str-api (if boost-p "reblog" action-str))
              (action-str-api (mastodon-toot--str-negify str-api faved boosted
                                                         action))
              (action-pp (concat
                          (mastodon-toot--str-negify action-str faved boosted
                                                     action)
                          (if boost-p "ed" "d")))
              (remove-p (if boost-p boosted faved)))
         (mastodon-toot--action
          action-str-api
          (lambda (_)
            (let ((inhibit-read-only t))
              (add-text-properties (car byline-region)
                                   (cdr byline-region)
                                   (if boost-p
                                       (list 'boosted-p (not boosted))
                                     (list 'favourited-p (not faved))))
              (mastodon-toot--update-stats-on-action action remove-p)
              (mastodon-toot--action-success (mastodon-tl--symbol action)
                                             byline-region remove-p item-json))
            (message "%s #%s" action-pp id)))))))))

(defun mastodon-toot--str-negify (str faved boosted action)
  "Add \"un\" to STR if item is already FAVED or BOOSTED.
ACTION is the action currently being taken."
  (if (eq action 'boost)
      (if boosted (concat "un" str) str)
    (if faved
        (concat "un" str)
      str)))

(defun mastodon-toot--update-stats-on-action (action &optional subtract)
  "Increment the toot stats display upon ACTION.
ACTION is a symbol, either `favourite' or `boost'.
SUBTRACT means we are un-favouriting or unboosting, so we decrement."
  (if (not (symbolp action))
      (error "Invalid argument: symbolp %s" action)
    (let* ((count-prop (if (eq action 'favourite)
                           'favourites-count
                         'boosts-count))
           (count-range (mastodon-tl--find-property-range count-prop (point)))
           (count (get-text-property (car count-range) count-prop))
           (inhibit-read-only 1))
      ;; TODO another way to implement this would be to async fetch counts again
      ;;  and re-display from count-properties
      (add-text-properties (car count-range)
                           (cdr count-range)
                           (list 'display
                                 (number-to-string
                                  (mastodon-toot--inc-or-dec count subtract))
                                 ;; update the count prop
                                 ;; we rely on this for any subsequent actions:
                                 count-prop
                                 (mastodon-toot--inc-or-dec count subtract))))))

(defun mastodon-toot-toggle-boost ()
  "Boost/unboost toot at `point'."
  (interactive)
  (mastodon-toot--toggle-boost-or-favourite 'boost))

(defun mastodon-toot-toggle-favourite ()
  "Favourite/unfavourite toot at `point'."
  (interactive)
  (mastodon-toot--toggle-boost-or-favourite 'favourite))

;; TODO maybe refactor into boost/fave fun
(defun mastodon-toot-toggle-bookmark ()
  "Bookmark or unbookmark toot at point."
  (interactive)
  (mastodon-toot--with-toot-item
   (let* ((n-type (mastodon-tl--property 'notification-type :no-move))
          (byline-region (mastodon-tl--find-property-range 'byline (point)))
          (bookmarked-p (when byline-region
                          (get-text-property (car byline-region) 'bookmarked-p)))
          (action (if bookmarked-p "unbookmark" "bookmark")))
     (cond ((or (string= n-type "follow")
                (string= n-type "follow_request"))
            (user-error "Can't bookmark %s notifications" n-type))
           ((not byline-region)
            (user-error "Nothing to %s here?!?" action))
           (t
            (let* ((bookmark-str (mastodon-tl--symbol 'bookmark))
                   (message (if bookmarked-p
                                "Bookmark removed!"
                              "Toot bookmarked!"))
                   (item-json (mastodon-tl--property 'item-json)))
              (mastodon-toot--action
               action
               (lambda (_)
                 (let ((inhibit-read-only t))
                   (add-text-properties (car byline-region)
                                        (cdr byline-region)
                                        (list 'bookmarked-p (not bookmarked-p)))
                   (mastodon-toot--action-success bookmark-str
                                                  byline-region bookmarked-p item-json)
                   (message "%s #%s" message id))))))))))

(defun mastodon-toot-list-boosters ()
  "List the boosters of toot at point."
  (interactive)
  ;; use grouped notifs data if present:
  ;; only send accounts as arg if type matches notif type we are acting
  ;; on, to prevent showing accounts for a boost notif when asking for
  ;; favers, and vice versa.
  (let* ((type (mastodon-tl--property 'notification-type :no-move))
         (accounts (when (string= type "reblog")
                     (mastodon-tl--property 'notification-accounts :no-move))))
    (mastodon-toot--list-boosters-or-favers nil accounts)))

(defun mastodon-toot-list-favouriters ()
  "List the favouriters of toot at point."
  (interactive)
  (let* ((type (mastodon-tl--property 'notification-type :no-move))
         (accounts (when (string= type "favourite")
                     (mastodon-tl--property 'notification-accounts :no-move))))
    (mastodon-toot--list-boosters-or-favers :favourite accounts)))

(defun mastodon-toot--list-boosters-or-favers (&optional favourite accounts)
  "List the favouriters or boosters of toot at point.
With FAVOURITE, list favouriters, else list boosters.
ACCOUNTS is notfications accounts if any."
  (mastodon-toot--with-toot-item
   (let* ((endpoint (unless accounts
                      (if favourite "favourited_by" "reblogged_by")))
          (url (unless accounts
                 (mastodon-http--api (format "statuses/%s/%s" id endpoint))))
          (params (unless accounts '(("limit" . "80"))))
          (json (or accounts (mastodon-http--get-json url params))))
     (if (eq (caar json) 'error)
         (user-error "%s (Status does not exist or is private)"
                     (alist-get 'error json))
       (let ((handles (mastodon-tl--map-alist 'acct json))
             (type-string (if favourite "Favouriters" "Boosters")))
         (if (not handles)
             (user-error "Looks like this toot has no %s" type-string)
           (let ((choice (completing-read
                          (format "%s (enter to view profile): " type-string)
                          handles nil t)))
             (mastodon-profile-show-user choice))))))))

(defun mastodon-toot-copy-toot-url ()
  "Copy URL of toot at point.
If the toot is a fave/boost notification, copy the URL of the
base toot."
  (interactive)
  (let* ((url (mastodon-toot--toot-url)))
    (kill-new url)
    (message "Toot URL copied to the clipboard.")))

(defun mastodon-toot-browse-toot-url ()
  "Browse URL of toot at point.
Calls `browse-url'."
  (interactive)
  (browse-url
   (mastodon-toot--toot-url)))

(defun mastodon-toot--toot-url ()
  "Return the URL of the base toot at point."
  (let* ((toot (mastodon-toot--base-toot-or-item-json)))
    (if (mastodon-tl--field 'reblog toot)
        (alist-get 'url (alist-get 'reblog toot))
      (alist-get 'url toot))))

(defun mastodon-toot-copy-toot-text ()
  "Copy text of toot at point.
If the toot is a fave/boost notification, copy the text of the
base toot."
  (interactive)
  (let* ((toot (mastodon-toot--base-toot-or-item-json)))
    (kill-new (mastodon-tl--content toot))
    (message "Toot content copied to the clipboard.")))

(defun mastodon-toot-translate-toot-text ()
  "Translate text of toot at point.
Uses `lingva.el'."
  (interactive)
  (if mastodon-tl--buffer-spec
      (if-let* ((toot (mastodon-tl--property 'item-json)))
          (condition-case x
              (lingva-translate nil
                                (mastodon-tl--content toot)
                                (when mastodon-tl--enable-proportional-fonts
                                  t))
            (void-function
             (user-error "Looks like you need to install lingva.el. Error: %s"
                         (error-message-string x))))
        (user-error "No toot to translate?"))
    (user-error "No mastodon buffer?")))

(defun mastodon-toot--own-toot-p (toot)
  "Check if TOOT is user's own, for deleting, editing, or pinning it."
  ;; this check needs to allow acting on own toots displayed as boosts, so we
  ;; call `mastodon-tl--toot-or-base'.
  (let ((json (mastodon-tl--toot-or-base toot)))
    (string= (alist-get 'acct (alist-get 'account json))
             (mastodon-auth--user-acct))))

(defun mastodon-toot-pin-toot-toggle ()
  "Pin or unpin user's toot at point."
  (interactive)
  (let* ((toot (mastodon-toot--base-toot-or-item-json))
         (pinnable-p (mastodon-toot--own-toot-p toot))
         (pinned-p (eq t (alist-get 'pinned toot)))
         (action (if pinned-p "unpin" "pin"))
         (msg (if pinned-p "unpinned" "pinned")))
    (if (not pinnable-p)
        (user-error "You can only pin your own toots")
      (when (y-or-n-p (format "%s this toot? " (capitalize action)))
        (mastodon-toot--action action
                               (lambda (_)
                                 (when mastodon-tl--buffer-spec
                                   (mastodon-tl--reload-timeline-or-profile))
                                 (message "Toot %s!" msg)))))))


;;; DELETE, DRAFT, REDRAFT

(defun mastodon-toot-delete-toot ()
  "Delete user's toot at point synchronously."
  (interactive)
  (mastodon-toot-delete-and-redraft-toot t))

;; TODO: handle media/poll for redrafting toots
(defun mastodon-toot-delete-and-redraft-toot (&optional no-redraft)
  "Delete and redraft user's toot at point synchronously.
NO-REDRAFT means delete toot only."
  (interactive)
  (let* ((toot (mastodon-toot--base-toot-or-item-json))
         (id (mastodon-tl--as-string (mastodon-tl--item-id toot)))
         (url (mastodon-http--api (format "statuses/%s" id)))
         (pos (point)))
    (let-alist toot
      (if (not (mastodon-toot--own-toot-p toot))
          (user-error "You can only delete (and redraft) your own toots")
        (when (y-or-n-p (if no-redraft
                            (format "Delete this toot? ")
                          (format "Delete and redraft this toot? ")))
          (let* ((response (mastodon-http--delete url)))
            (mastodon-http--triage
             response
             (lambda (_)
               (if no-redraft
                   (progn
                     (when mastodon-tl--buffer-spec
                       (mastodon-tl--reload-timeline-or-profile pos))
                     (message "Toot deleted!"))
                 (mastodon-toot--redraft response
                                         .in_reply_to_id
                                         .visibility
                                         .spoiler_text))))))))))

(defun mastodon-toot--set-cw (&optional cw)
  "Set content warning to CW if it is non-nil."
  (unless (or (null cw) ; cw is nil for `mastodon-tl-dm-user'
              (string-empty-p cw))
    (setq mastodon-toot--content-warning cw)))


;;; REDRAFT

(defun mastodon-toot--redraft (response &optional reply-id toot-visibility
                                        toot-cw)
  "Opens a new toot compose buffer using values from RESPONSE buffer.
REPLY-ID, TOOT-VISIBILITY, and TOOT-CW of deleted toot are preseved."
  (with-current-buffer response
    (let* ((response (mastodon-http--process-json))
           (content (alist-get 'text response)))
      (mastodon-toot--compose-buffer)
      (goto-char (point-max))
      (insert content)
      ;; adopt reply-to-id, visibility and CW from deleted toot:
      (mastodon-toot--set-toot-properties
       reply-id toot-visibility toot-cw
       ;; TODO set new lang/scheduled props here
       nil))))

(defun mastodon-toot--set-toot-media-attachments (media)
  "Set the media attachments variables.
MEDIA is the media_attachments data for a status from the server."
  (mapcar (lambda (x)
            (cl-pushnew (alist-get 'id x)
                        mastodon-toot--media-attachment-ids)
            (cl-pushnew `((:contents . ,(mastodon-http--read-file-as-string
                                         (alist-get 'url x) :url))
                          (:description . ,(alist-get 'description x)))
                        mastodon-toot--media-attachments))
          media))

(defun mastodon-toot--set-toot-properties
    (reply-id visibility cw lang &optional scheduled scheduled-id media poll)
  "Set the toot properties for the current redrafted or edited toot.
REPLY-ID, VISIBILITY, CW, SCHEDULED, and LANG are the properties to set.
MEDIA is the media_attachments data for a status from the server."
  (with-current-buffer "*edit toot*"
    (when reply-id
      (setq mastodon-toot--reply-to-id reply-id))
    (setq mastodon-toot--visibility visibility)
    (setq mastodon-toot--scheduled-for scheduled)
    (setq mastodon-toot--scheduled-id scheduled-id)
    (when (not (string-empty-p lang))
      (setq mastodon-toot--language lang))
    (mastodon-toot--set-cw cw)
    (when media
      (mastodon-toot--set-toot-media-attachments media))
    (when poll
      (mastodon-toot--server-poll-to-local poll))
    (mastodon-toot--refresh-attachments-display)
    (mastodon-toot--update-status-fields)))

(defun mastodon-toot--kill (&optional cancel)
  "Kill `mastodon-toot-mode' buffer and window.
CANCEL means the toot was not sent, so we save the toot text as a draft."
  (let ((prev-window-config mastodon-toot-previous-window-config))
    (unless (eq mastodon-toot-current-toot-text nil)
      (when cancel
        (cl-pushnew mastodon-toot-current-toot-text
                    mastodon-toot-draft-toots-list :test #'string=)))
    ;; prevent some weird bug when cancelling a non-empty toot:
    (delete #'mastodon-toot--save-toot-text after-change-functions)
    (quit-window 'kill)
    (mastodon-toot--restore-previous-window-config prev-window-config)))

(defun mastodon-toot-cancel ()
  "Kill new-toot buffer/window. Does not POST content.
If toot is not empty, prompt to save text as a draft."
  (interactive)
  (when (and (not (mastodon-toot--empty-p))
             (y-or-n-p "Save draft toot?"))
    (mastodon-toot-save-draft))
  (mastodon-toot--kill))

(defun mastodon-toot-save-draft ()
  "Save the current compose toot text as a draft.
Pushes `mastodon-toot-current-toot-text' to
`mastodon-toot-draft-toots-list'."
  (interactive)
  (unless (string= mastodon-toot-current-toot-text nil)
    (cl-pushnew mastodon-toot-current-toot-text
                mastodon-toot-draft-toots-list :test 'string=)
    (message "Draft saved!")))

(defun mastodon-toot--empty-p (&optional text-only)
  "Return t if toot has no text, attachments, or polls.
TEXT-ONLY means don't check for attachments or polls."
  (and (if text-only
           t
         (and (not mastodon-toot--media-attachments)
              (not mastodon-toot-poll)))
       (string-empty-p (mastodon-tl--clean-tabs-and-nl
                        (mastodon-toot--remove-docs)))))


;;; EMOJIS

(defun mastodon-toot-insert-emoji ()
  "Prompt to insert an emoji."
  (interactive)
  (if mastodon-use-emojify
      (emojify-insert-emoji)
    (emoji-search))) ;; 29.1

(defun mastodon-toot--emoji-dir ()
  "Return the file path for the mastodon custom emojis directory."
  (concat (expand-file-name emojify-emojis-dir)
          "/mastodon-custom-emojis/"))

(defun mastodon-toot-download-custom-emoji ()
  "Download `mastodon-instance-url's custom emoji.
Emoji images are stored in a subdir of `emojify-emojis-dir'.
To use the downloaded emoji, run `mastodon-toot-enable-custom-emoji'."
  (interactive)
  (let* ((url (mastodon-http--api "custom_emojis"))
         (custom-emoji (mastodon-http--get-json url))
         (mastodon-custom-emoji-dir (mastodon-toot--emoji-dir)))
    (if (not (file-directory-p emojify-emojis-dir))
        (user-error "Looks like you need to set up emojify first")
      (unless (file-directory-p mastodon-custom-emoji-dir)
        (make-directory mastodon-custom-emoji-dir nil)) ; no add parent
      (mapc (lambda (x)
              (let ((url (alist-get 'url x))
                    (shortcode (alist-get 'shortcode x)))
                ;; skip anything that contains unexpected characters
                (when (and url shortcode
                           (string-match-p "^[a-zA-Z0-9-_]+$" shortcode)
                           (string-match-p "^[a-zA-Z]+$" (file-name-extension url)))
                  (url-copy-file url
                                 (concat mastodon-custom-emoji-dir
                                         shortcode
                                         "."
                                         (file-name-extension url))
                                 t))))
            custom-emoji)
      (message "Custom emoji for %s downloaded to %s"
               mastodon-instance-url
               mastodon-custom-emoji-dir))))

(defun mastodon-toot--collect-custom-emoji ()
  "Return a list of `mastodon-instance-url's custom emoji.
The list is formatted for `emojify-user-emojis', which see."
  (let* ((mastodon-custom-emojis-dir (mastodon-toot--emoji-dir))
         (custom-emoji-files (directory-files mastodon-custom-emojis-dir
                                              nil ; not full path
                                              "^[^.]")) ; no dot files
         mastodon-emojify-user-emojis)
    (mapc (lambda (x)
            (push
             `(,(concat ":"
                        (file-name-base x) ":")
               . (("name" . ,(file-name-base x))
                  ("image" . ,(concat mastodon-custom-emojis-dir x))
                  ("style" . "github")))
             mastodon-emojify-user-emojis))
          custom-emoji-files)
    (reverse mastodon-emojify-user-emojis)))

(defun mastodon-toot-enable-custom-emoji ()
  "Add `mastodon-instance-url's custom emoji to `emojify'.
Custom emoji must first be downloaded with
`mastodon-toot-download-custom-emoji'. Custom emoji are appended
to `emojify-user-emojis', and the emoji data is updated."
  (interactive)
  (unless (file-exists-p (mastodon-toot--emoji-dir))
    (when (y-or-n-p "Looks like you haven't downloaded your
    instance's custom emoji yet. Download now? ")
      (mastodon-toot-download-custom-emoji)))
  (let ((masto-emojis (mastodon-toot--collect-custom-emoji)))
    (unless (cl-find (car masto-emojis)
                     emojify-user-emojis
                     :test #'equal)
      (setq emojify-user-emojis
            (append masto-emojis
                    emojify-user-emojis))
      ;; if already loaded, reload
      (when (featurep 'emojify)
        ;; we now only do this within the unless test above, as it is extremely
        ;; slow and runs in `mastodon-mode-hook'.
        (emojify-set-emoji-data)))))

(defun mastodon-toot--remove-docs ()
  "Get the body of a toot from the current compose buffer."
  (let ((header-region (mastodon-tl--find-property-range 'toot-post-header
                                                         (point-min))))
    (string-trim-left
     (buffer-substring (cdr header-region) (point-max)))))

(defun mastodon-toot--build-poll-params ()
  "Return an alist of parameters for POSTing a poll status."
  (if mastodon-toot-poll-use-transient
      (let-alist mastodon-toot-poll
        (append
         (mastodon-http--build-array-params-alist
          "poll[options][]"
          (list .one .two .three .four))
         (list (cons "poll[expires_in]" .expiry)
               (cons "poll[multiple]" .multi)
               (cons "poll[hide_totals]" .hide))))
    (append
     (mastodon-http--build-array-params-alist
      "poll[options][]"
      (plist-get mastodon-toot-poll :options))
     `(("poll[expires_in]" .  ,(plist-get mastodon-toot-poll :expiry)))
     `(("poll[multiple]" . ,(symbol-name (plist-get mastodon-toot-poll :multi))))
     `(("poll[hide_totals]" . ,(symbol-name (plist-get mastodon-toot-poll :hide)))))))


;;; SEND TOOT FUNCTION

(defun mastodon-toot-send ()
  "POST contents of new-toot buffer to fediverse instance and kill buffer.
If media items have been attached and uploaded with
`mastodon-toot-attach-media', they are attached to the toot.
If `mastodon-toot--edit-item-id' is non-nil, PUT contents to
instance to edit a toot."
  (interactive)
  (let* ((toot (mastodon-toot--remove-docs))
         (scheduled mastodon-toot--scheduled-for)
         (scheduled-id mastodon-toot--scheduled-id)
         (edit-id mastodon-toot--edit-item-id)
         (endpoint (mastodon-http--api (if edit-id ; we are sending an edit:
                                           (format "statuses/%s" edit-id)
                                         "statuses")))
         (args-no-media
          (append
           `(("status" . ,toot)
             ("in_reply_to_id" . ,mastodon-toot--reply-to-id)
             ("visibility" . ,mastodon-toot--visibility)
             ("sensitive" . ,(when mastodon-toot--content-nsfw
                               (symbol-name t)))
             ("spoiler_text" . ,mastodon-toot--content-warning)
             ("language" . ,mastodon-toot--language))
           ;; Pleroma instances can't handle null-valued
           ;; scheduled_at args, so only add if non-nil
           (when scheduled `(("scheduled_at" . ,scheduled)))))
         (args-media (when mastodon-toot--media-attachment-ids
                       (mastodon-http--build-array-params-alist
                        "media_ids[]"
                        mastodon-toot--media-attachment-ids)))
         (args-poll (when mastodon-toot-poll
                      (mastodon-toot--build-poll-params)))
         ;; media || polls:
         (args (if mastodon-toot--media-attachment-ids
                   (append args-media args-no-media)
                 (if mastodon-toot-poll
                     (append args-no-media args-poll)
                   args-no-media)))
         (prev-window-config mastodon-toot-previous-window-config))
    (cond ((and mastodon-toot--media-attachment-ids
                ;; make sure we have media args
                ;; and the same num of ids as attachments
                (or (not args-media)
                    (not (= (length mastodon-toot--media-attachments)
                            (length mastodon-toot--media-attachment-ids)))))
           (user-error "Something is wrong with your uploads. Wait for them to complete or try again"))
          ((and mastodon-toot--max-toot-chars
                (> (mastodon-toot--count-toot-chars toot mastodon-toot--content-warning)
                   mastodon-toot--max-toot-chars))
           (user-error "Looks like your toot (inc. CW) is longer than that maximum allowed length"))
          ;; polls must have text, so we use poll as flag for text-only
          ;; check here:
          ((mastodon-toot--empty-p mastodon-toot-poll)
           (user-error "Empty toot. Cowardly refusing to post this"))
          (t
           (let ((response (funcall (if edit-id ; we are sending an edit:
                                        #'mastodon-http--put
                                      #'mastodon-http--post)
                                    endpoint args)))
             (mastodon-http--triage
              response
              (lambda (_)
                ;; kill buffer:
                (mastodon-toot--kill)
                ;; nil our poll var:
                (setq mastodon-toot-poll nil)
                (message "Toot %s!" (if scheduled "scheduled" "toot"))
                ;; cancel scheduled toot if we were editing it:
                (when scheduled-id
                  (mastodon-views-cancel-scheduled-toot
                   scheduled-id :no-confirm))
                ;; window config:
                (mastodon-toot--restore-previous-window-config prev-window-config)
                ;; reload: - when we have been editing
                ;;         - when we are in thread view
                ;; (we don't reload in every case as it can be slow and we
                ;; may lose our place in a timeline.)
                (let ((type (mastodon-tl--get-buffer-type)))
                  (when (or edit-id
                            (eq 'single-status type)
                            (eq 'thread type))
                    (let ((pos (marker-position (cadr prev-window-config))))
                      (mastodon-tl--reload-timeline-or-profile pos)))))))))))


;;; EDITING TOOTS:

(defun mastodon-toot-edit-toot-at-point ()
  "Edit the user's toot at point."
  (interactive)
  (mastodon-toot--with-toot-item
   (let ((toot (mastodon-toot--base-toot-or-item-json)))
     (if (not (mastodon-toot--own-toot-p toot))
         (user-error "You can only edit your own toots")
       (let* ((source (mastodon-toot--get-toot-source id))
              (content (alist-get 'text source))
              (source-cw (alist-get 'spoiler_text source)))
         (let-alist toot
           (when (y-or-n-p "Edit this toot? ")
             (mastodon-toot--compose-buffer nil .in_reply_to_id nil
                                            content :edit)
             (goto-char (point-max))
             ;; adopt reply-to-id, visibility, CW, language, and media:
             (mastodon-toot--set-toot-properties .in_reply_to_id .visibility
                                                 source-cw .language nil nil
                                                 ;; maintain media order:
                                                 (reverse .media_attachments) .poll)
             (setq mastodon-toot--edit-item-id id))))))))

(defun mastodon-toot--get-toot-source (id)
  "Fetch the source JSON of toot with ID."
  (let ((url (mastodon-http--api (format "/statuses/%s/source" id))))
    (mastodon-http--get-json url nil :silent)))

(defun mastodon-toot--get-toot-edits (id)
  "Return the edit history of toot with ID."
  (let* ((url (mastodon-http--api (format "statuses/%s/history" id))))
    (mastodon-http--get-json url)))

(defun mastodon-toot-view-toot-edits ()
  "View editing history of the toot at point in a popup buffer."
  (interactive)
  (let ((id (mastodon-tl--property 'base-item-id))
        (history (mastodon-tl--property 'edit-history)) ;; at byline
        (buf "*mastodon-toot-edits*"))
    (if (not history)
        (user-error "No editing history for this toot")
      (with-mastodon-buffer buf #'special-mode :other-window
        (cl-loop for count from 1
                 for x in history
                 do (insert (propertize (if (= count 1)
                                            (format "%s [original]:\n" count)
                                          (format "%s:\n" count))
                                        'face 'mastodon-toot-docs-face)
                            (mastodon-toot--insert-toot-iter x)
                            "\n"))
        (goto-char (point-min))
        (setq-local header-line-format
                    (propertize
                     (format "Edits to toot by %s:"
                             (alist-get 'username
                                        (alist-get 'account (car history))))
                     'face 'mastodon-toot-docs-face))
        (mastodon-tl--set-buffer-spec (buffer-name (current-buffer))
                                      (format "statuses/%s/history" id)
                                      nil)))))

(defun mastodon-toot--insert-toot-iter (it)
  "Insert iteration IT of toot."
  (let ((content (alist-get 'content it)))
    ;; TODO: handle polls, media
    (mastodon-tl--render-text content)))

(defun mastodon-toot--restore-previous-window-config (config)
  "Restore the window CONFIG after killing the toot compose buffer.
Buffer-local variable `mastodon-toot-previous-window-config' holds the config."
  (set-window-configuration (car config))
  (goto-char (cadr config)))

(defun mastodon-toot--mentions-to-string (mentions)
  "Apply `mastodon-toot--process-local' function to each mention in MENTIONS.
Remove empty string (self) from result and joins the sequence with whitespace."
  (let ((mentions (remove ""
                          (mapcar #'mastodon-toot--process-local mentions))))
    (mapconcat #'identity mentions " ")))

(defun mastodon-toot--process-local (acct)
  "Add domain to local ACCT and replace the curent user name with \"\".
Mastodon requires the full @user@domain, even in the case of local accts.
eg. \"user\" -> \"@user@local.social\" (when local.social is the domain of the
`mastodon-instance-url').
eg. \"yourusername\" -> \"\"
eg. \"feduser@fed.social\" -> \"@feduser@fed.social\"."
  (cond ((string-match-p "@" acct) (concat "@" acct)) ; federated acct
        ((string= (mastodon-auth--user-acct) acct) "") ; your acct
        (t
         (concat "@" acct "@" ; local acct
                 (cadr
                  (split-string mastodon-instance-url "/" :omit-nulls))))))


;;; COMPLETION (TAGS, MENTIONS)

(defun mastodon-toot--mentions (status)
  "Extract mentions (not the reply-to author or booster) from STATUS.
The mentioned users look like this:
Local user (including the logged in): `username`.
Federated user: `username@host.co`."
  (let* ((mentions (mastodon-tl--field 'mentions status)))
    ;; reverse does not work on vectors in 24.5
    (mastodon-tl--map-alist 'acct (reverse mentions))))

(defun mastodon-toot--get-bounds (regex)
  "Get bounds of tag or handle before point using REGEX."
  ;; # and @ are not part of any existing thing at point
  (save-match-data
    (save-excursion
      ;; match full handle inc. domain, or tag including #
      ;; (see the regexes for subexp 2)
      (when (re-search-backward regex
                                (save-excursion (forward-whitespace -1)
                                                (point))
                                :no-error)
        (cons (match-beginning 2)
              (match-end 2))))))

(defun mastodon-toot--fetch-emojify-candidates ()
  "Get the candidates to be used for emojis completion.
The candidates are calculated according to currently active
`emojify-emoji-styles'. Hacked off
`emojify--get-completing-read-candidates'."
  (let ((styles (mapcar #'symbol-name emojify-emoji-styles)))
    (let ((emojis '()))
      (emojify-emojis-each
       (lambda (key value)
         (when (seq-position styles (ht-get value "style"))
           (push (cons key
                       (format "%s (%s)"
                               (ht-get value "name")
                               (ht-get value "style")))
                 emojis))))
      emojis)))

(defun mastodon-toot--fetch-candidates (start end &optional type)
  "Search for a completion prefix from buffer positions START to END.
Return a list of candidates.
TYPE is the candidate type, it may be :tags, :handles, or :emoji."
  ;; we can't save the first two-letter search then only filter the
  ;; resulting list, as max results returned is 40.
  (setq mastodon-toot-completions
        (cond ((eq type :tags)
               (let ((tags-list (mastodon-search--search-tags-query
                                 (buffer-substring-no-properties start end))))
                 (cl-loop for tag in tags-list
                          collect (cons (concat "#" (car tag))
                                        (cdr tag)))))
              ((eq type :emoji)
               (when (bound-and-true-p emojify-mode)
                 (mastodon-toot--fetch-emojify-candidates)))
              (t
               (mastodon-search--search-accounts-query
                (buffer-substring-no-properties start end))))))

(defun mastodon-toot--make-capf (regex annot-fun type)
  "Build a completion backend for `completion-at-point-functions'.
REGEX is the regex to match preceding text.
TYPE is a keyword symbol for `mastodon-toot--fetch-candidates'.
ANNOT-FUN is a function returning an annotatation from a single
arg, a candidate."
  (let* ((bounds (mastodon-toot--get-bounds regex))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list start
            end
            (completion-table-dynamic ; only search when necessary
             (lambda (_)
               ;; Interruptible candidate computation, from minad/d mendler, thanks!
               (let ((result
                      (while-no-input
                        (mastodon-toot--fetch-candidates
                         start end type))))
                 (and (consp result) result))))
            :exclusive 'no
            :annotation-function
            (lambda (cand)
              (concat " " (funcall annot-fun cand)))))))

(defun mastodon-toot--mentions-capf ()
  "Build a mentions completion backend for `completion-at-point-functions'."
  (mastodon-toot--make-capf mastodon-toot-handle-regex
                            #'mastodon-toot--mentions-annotation-fun
                            :handles))

(defun mastodon-toot--tags-capf ()
  "Build a tags completion backend for `completion-at-point-functions'."
  (mastodon-toot--make-capf mastodon-toot-tag-regex
                            #'mastodon-toot--tags-annotation-fun
                            :tags))

(defun mastodon-toot--emoji-capf ()
  "Build an emoji completion backend for `completion-at-point-functions'."
  (mastodon-toot--make-capf mastodon-toot-emoji-regex
                            #'mastodon-toot--emoji-annotation-fun
                            :emoji))

(defun mastodon-toot--mentions-annotation-fun (candidate)
  "Given a handle completion CANDIDATE, return its annotation string, a username."
  (caddr (assoc candidate mastodon-toot-completions)))

(defun mastodon-toot--tags-annotation-fun (candidate)
  "Given a tag string CANDIDATE, return an annotation, the tag's URL."
  ;; TODO: check the list returned here? should be cadr
  ;; or make it an alist and use cdr
  (cadr (assoc candidate mastodon-toot-completions)))

(defun mastodon-toot--emoji-annotation-fun (candidate)
  "CANDIDATE."
  ;; TODO: emoji image as annot
  (cdr (assoc candidate mastodon-toot-completions)))


;;; REPLY

(defun mastodon-toot-reply ()
  "Reply to toot at `point'.
Customize `mastodon-toot-display-orig-in-reply-buffer' to display
text of the toot being replied to in the compose buffer.
If the region is active, inject it into the reply buffer,
prefixed by >."
  (interactive)
  (mastodon-toot--with-toot-item
   (let* ((quote (when (region-active-p)
                   (buffer-substring (region-beginning)
                                     (region-end))))
          (toot (mastodon-toot--base-toot-or-item-json))
          (account (mastodon-tl--field 'account toot))
          (user (alist-get 'acct account))
          (mentions (mastodon-toot--mentions toot))
          (boosted (mastodon-tl--field 'reblog toot))
          (booster (when boosted
                     (alist-get 'acct
                                (alist-get 'account toot))))
          (mentions
           (cond ((and booster ;; different booster, user and mentions:
                       (and (not (string= user booster))
                            (not (member booster mentions))))
	          (mastodon-toot--mentions-to-string
                   (append (list user booster) mentions nil)))
                 ((not (member user mentions)) ;; user not in mentions:
	          (mastodon-toot--mentions-to-string
                   (append (list user) mentions nil)))
                 (t ;; user already in mentions:
                  (mastodon-toot--mentions-to-string
                   (copy-sequence mentions))))))
     (mastodon-toot--compose-buffer mentions id toot quote))))


;;; COMPOSE TOOT SETTINGS

(defun mastodon-toot-set-content-warning ()
  "Set a content warning for the current toot."
  (interactive)
  (setq mastodon-toot--content-warning
        (read-string "Warning: " mastodon-toot--content-warning))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot-toggle-nsfw ()
  "Toggle `mastodon-toot--content-nsfw'."
  (interactive)
  (setq mastodon-toot--content-nsfw
        (not mastodon-toot--content-nsfw))
  (message "NSFW flag is now %s" (if mastodon-toot--content-nsfw "on" "off"))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot-change-visibility (&optional arg)
  "Change the current visibility to the next valid value.
With prefix ARG, read a visibility type in the minibuffer."
  (interactive "P")
  (if (mastodon-tl--buffer-type-eq 'edit-toot)
      (user-error "You can't change visibility when editing toots")
    (setq mastodon-toot--visibility
          (if arg
              (completing-read "Visibility: "
                               mastodon-toot-visibility-list)
            (cond ((string= mastodon-toot--visibility "public")
                   "unlisted")
                  ((string= mastodon-toot--visibility "unlisted")
                   "private")
                  ((string= mastodon-toot--visibility "private")
                   "direct")
                  (t
                   "public"))))
    (mastodon-toot--update-status-fields)))

(defun mastodon-toot-set-toot-language ()
  "Prompt for a language and set `mastodon-toot--language'.
Return its two letter ISO 639 1 code."
  (interactive)
  (let* ((choice (completing-read "Language for this toot: "
                                  mastodon-iso-639-1)))
    (setq mastodon-toot--language
          (alist-get choice mastodon-iso-639-1 nil nil #'string=))
    (message "Language set to %s" choice)
    (mastodon-toot--update-status-fields)))


;;; ATTACHMENTS

(defun mastodon-toot-clear-all-attachments ()
  "Remove all attachments from a toot draft."
  (interactive)
  (setq mastodon-toot--media-attachments nil)
  (setq mastodon-toot--media-attachment-ids nil)
  (mastodon-toot--refresh-attachments-display)
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--get-instance-max-attachments ()
  "Return the maximum attachments from `mastodon-active-user's instance.
If that fails, return 4 as a fallback"
  ;; FIXME: this likely various for other server types:
  ;; pleroma doesn't advertise this on "api/v1/instance" (checked
  ;; fe.disroot.org)
  (or
   (let ((config (alist-get 'statuses
                            (alist-get 'configuration
                                       (mastodon-views--get-own-instance)))))
     (alist-get 'max_media_attachments config))
   4)) ; mastodon default as fallback

(defun mastodon-toot-attach-media (file description)
  "Prompt for an attachment FILE with DESCRIPTION.
A preview is displayed in the new toot buffer, and the file
is uploaded asynchronously using `mastodon-toot--upload-attached-media'.
File is actually attached to the toot upon posting."
  (interactive "fFilename: \nsDescription: ")
  (let ((max-attachments (mastodon-toot--get-instance-max-attachments)))
    (when (>= (length mastodon-toot--media-attachments)
              max-attachments)
      ;; warn + pop the oldest one:
      (when (y-or-n-p
             (format "Maximum attachments (%s) reached: remove first one?"
                     max-attachments))
        (pop mastodon-toot--media-attachments)))
    (if (file-directory-p file)
        (user-error "Looks like you chose a directory not a file")
      (setq mastodon-toot--media-attachments
            (nconc mastodon-toot--media-attachments
                   `(((:contents . ,(mastodon-http--read-file-as-string file))
                      (:description . ,description)
                      (:filename . ,file)))))
      (mastodon-toot--refresh-attachments-display)
      ;; upload only most recent attachment:
      (mastodon-toot--upload-attached-media
       (car (last mastodon-toot--media-attachments))))))

(defun mastodon-toot--attachment-descriptions ()
  "Return a list of image descriptions for current attachments."
  (mastodon-tl--map-alist :description mastodon-toot--media-attachments))

(defun mastodon-toot--attachment-from-desc (desc)
  "Return an attachment based on its description DESC."
  (car
   (cl-member-if (lambda (x)
                   (rassoc desc x))
                 mastodon-toot--media-attachments)))

(defun mastodon-toot-edit-media-description ()
  "Prompt for an attachment, and update its description."
  (interactive)
  (let* ((descs (mastodon-toot--attachment-descriptions))
         (choice (completing-read "Attachment: " descs nil :match))
         (attachment (mastodon-toot--attachment-from-desc choice))
         (desc-new (read-string "Description: " choice)))
    (setf (alist-get :description attachment)
          desc-new)
    (mastodon-toot--refresh-attachments-display)))

(defun mastodon-toot--upload-attached-media (attachment)
  "Upload a single ATTACHMENT using `mastodon-http--post-media-attachment'.
The item's id is added to `mastodon-toot--media-attachment-ids',
which is used to attach it to a toot when posting."
  (let* ((filename (expand-file-name (alist-get :filename attachment)))
         (caption (alist-get :description attachment))
         (url (concat mastodon-instance-url "/api/v2/media")))
    (message "Uploading %s... (please wait before starting further uploads)"
             (file-name-nondirectory filename))
    (mastodon-http--post-media-attachment url filename caption)))

(defun mastodon-toot--refresh-attachments-display ()
  "Update the display attachment previews in toot draft buffer."
  (let ((inhibit-read-only t)
        (attachments-region (mastodon-tl--find-property-range
                             'toot-attachments (point-min)))
        (display-specs (mastodon-toot--format-attachments)))
    (dotimes (i (- (cdr attachments-region) (car attachments-region)))
      (add-text-properties (+ i (car attachments-region))
                           (+ i 1 (car attachments-region))
                           (list 'display (or (nth i display-specs) ""))))))

(defun mastodon-toot--format-attachments ()
  "Format the attachment previews for display in toot draft buffer."
  (or
   (let ((image-options (when (mastodon-tl--image-trans-check)
                          `(:height ,mastodon-toot--attachment-height))))
     (cl-loop for count from 1
              for att in mastodon-toot--media-attachments
              nconc
              (let* ((data (alist-get :contents att))
                     (image (apply #'create-image data
                                   (if (version< emacs-version "27.1")
                                       (when image-options 'imagemagick)
                                     nil) ; inbuilt scaling in 27.1
                                   t image-options))
                     (desc (alist-get :description att)))
                (list (format "\n    %d: " count)
                      image
                      (format " \"%s\"" desc)))))
   (list "None")))


;;; POLL

(defun mastodon-toot--fetch-max-poll-options (instance)
  "Return the maximum number of poll options from JSON data INSTANCE."
  (mastodon-toot--fetch-poll-field 'max_options instance))

(defun mastodon-toot--fetch-max-poll-option-chars (instance)
  "Return the maximum number of characters a poll option may have.
INSTANCE is JSON."
  (if (alist-get 'pleroma instance)
      (mastodon-toot--fetch-poll-field 'max_option_chars instance)
    (or (mastodon-toot--fetch-poll-field 'max_characters_per_option instance)
        50))) ; masto default

(defun mastodon-toot--fetch-poll-field (field instance)
  "Return FIELD from the poll settings from JSON data INSTANCE."
  (let* ((polls (if (alist-get 'pleroma instance)
                    (alist-get 'poll_limits instance)
                  (alist-get 'polls
                             (alist-get 'configuration instance)))))
    (alist-get field polls)))

(defun mastodon-toot--read-poll-options-count (max)
  "Read the user's choice of the number of options the poll should have.
MAX is the maximum number set by their instance."
  (let ((number (read-number (format "Number of options [2-%s]: " max) 2)))
    (if (> number max)
        (user-error "You need to choose a number between 2 and %s" max)
      number)))

(defun mastodon-toot-create-poll ()
  "Prompt for new poll options and return as a list."
  (interactive)
  (if mastodon-toot-poll-use-transient
      (call-interactively #'mastodon-create-poll)
    (mastodon-toot--read-poll)))

(defun mastodon-toot--read-poll ()
  "Read poll options."
  (let* ((instance (mastodon-instance-data))
         (max-options (mastodon-toot--fetch-max-poll-options instance))
         (count (mastodon-toot--read-poll-options-count max-options))
         (length (mastodon-toot--fetch-max-poll-option-chars instance))
         (multiple-p (y-or-n-p "Multiple choice? "))
         (options (mastodon-toot--read-poll-options count length))
         (hide-totals (y-or-n-p "Hide votes until poll ends? "))
         (expiry (mastodon-toot--read-poll-expiry))
         (expiry-str (cdr expiry))
         (expiry-human (car expiry)))
    (setq mastodon-toot-poll
          `( :options ,options :length ,length :expiry-readable ,expiry-human
             :expiry ,expiry-str :multi ,multiple-p :hide ,hide-totals))
    (message "poll created!")
    (mastodon-toot--update-status-fields)))

(defun mastodon-toot--read-poll-options (count length)
  "Read a list of options for poll with COUNT options.
LENGTH is the maximum character length allowed for a poll option."
  (let* ((choices
          (cl-loop for x from 1 to count
                   collect (read-string
                            (format "Poll option [%s/%s] [max %s chars]: "
                                    x count length))))
         (longest (apply #'max (mapcar #'length choices))))
    (if (not (> longest length))
        choices
      (user-error "Looks like you went over the max length. Try again")
      (sleep-for 2)
      (mastodon-toot--read-poll-options count length))))

(defun mastodon-toot--read-poll-expiry ()
  "Prompt for a poll expiry time.
Return a cons of a human readable string, and a seconds-from-now string."
  ;; API requires this in seconds
  (let* ((options (mastodon-toot--poll-expiry-options-alist))
         (response (completing-read "poll ends in [or enter seconds]: "
                                    options nil 'confirm)))
    (or (assoc response options #'string=)
        (if (< (string-to-number response) 600)
            (car options))))) ;; min 5 mins

(defun mastodon-toot--poll-expiry-options-alist ()
  "Return an alist of expiry options options in seconds."
  `(("5 minutes" . ,(number-to-string (* 60 5)))
    ("30 minutes" . ,(number-to-string (* 60 30)))
    ("1 hour" . ,(number-to-string (* 60 60)))
    ("6 hours" . ,(number-to-string (* 60 60 6)))
    ("1 day" . ,(number-to-string (* 60 60 24)))
    ("3 days" . ,(number-to-string (* 60 60 24 3)))
    ("7 days" . ,(number-to-string (* 60 60 24 7)))
    ("14 days" . ,(number-to-string (* 60 60 24 14)))
    ("30 days" . ,(number-to-string (* 60 60 24 30)))))

(defun mastodon-toot-clear-poll (&optional transient)
  "Remove poll from toot compose buffer.
Sets `mastodon-toot-poll' to nil.
If TRANSIENT, we are called from a transient, so nil
`tp-transient-settings' too."
  (interactive)
  (let ((var (if transient
                 'tp-transient-settings
               'mastodon-toot-poll)))
    (if (not (symbol-value var))
        (user-error "No poll?")
      (set var nil)
      (when transient
        (setq mastodon-toot-poll nil))
      (mastodon-toot--update-status-fields))))

(defun mastodon-toot--server-poll-to-local (json)
  "Convert server poll data JSON to a `mastodon-toot-poll' plist."
  (let-alist json
    (let* ((expiry-seconds-rel
            (time-to-seconds
             (time-subtract
              (encode-time
               (parse-time-string .expires_at))
              (current-time))))
           (expiry-str (format-time-string "%s" expiry-seconds-rel))
           (expiry-human (car
                          (mastodon-tl--human-duration expiry-seconds-rel)))
           (options (mastodon-tl--map-alist 'title .options))
           (multiple (if (eq :json-false .multiple) nil t)))
      (if mastodon-toot-poll-use-transient
          (setq mastodon-toot-poll
                `((multi . ,multiple)
                  (expiry . ,expiry-str)
                  ;; (hide . ,hide)
                  (one . ,(nth 0 options))
                  (two . ,(nth 1 options))
                  (three . ,(nth 2 options))
                  (four . ,(nth 3 options))))
        (setq mastodon-toot-poll
              `( :options ,options :expiry-readable ,expiry-human
                 :expiry ,expiry-str :multi ,multiple))))))


;;; SCHEDULE

(defun mastodon-toot-schedule-toot (&optional reschedule)
  "Read a date (+ time) in the minibuffer and schedule the current toot.
With RESCHEDULE, reschedule the scheduled toot at point without editing."
  ;; original idea by christian tietze, thanks!
  ;; https://codeberg.org/martianh/mastodon.el/issues/285
  (interactive)
  (cond ((mastodon-tl--buffer-type-eq 'edit-toot)
         (user-error "You can't schedule toots you're editing"))
        ((not (or (mastodon-tl--buffer-type-eq 'new-toot)
                  (mastodon-tl--buffer-type-eq 'scheduled-statuses)))
         (user-error "You can only schedule toots from the compose buffer or scheduled toots view"))
        (t
         (let* ((id (when reschedule (mastodon-tl--property 'id :no-move)))
                (ts (when reschedule
                      (alist-get 'scheduled_at
                                 (mastodon-tl--property 'scheduled-json :no-move))))
                (time-value
                 (org-read-date t t nil "Schedule toot:"
                                ;; default to scheduled timestamp if already set:
                                (mastodon-toot--iso-to-org
                                 ;; we are rescheduling without editing:
                                 (or ts
                                     ;; we are maybe editing the scheduled toot:
                                     mastodon-toot--scheduled-for))))
                (iso8601-str (format-time-string "%FT%T%z" time-value))
                (msg-str (format-time-string "%d-%m-%y at %H:%M[%z]" time-value)))
           (if (not reschedule)
               (progn
                 (setq-local mastodon-toot--scheduled-for iso8601-str)
                 (message "Toot scheduled for %s." msg-str))
             (let* ((args `(("scheduled_at" . ,iso8601-str)))
                    (url (mastodon-http--api (format "scheduled_statuses/%s" id)))
                    (response (mastodon-http--put url args)))
               (mastodon-http--triage
                response
                (lambda (_)
                  ;; reschedule means we are in scheduled toots view:
                  (mastodon-views-view-scheduled-toots)
                  (message "Toot rescheduled for %s." msg-str)))))))))

(defun mastodon-toot--iso-to-human (ts)
  "Format an ISO8601 timestamp TS to be more human-readable."
  (let* ((decoded (iso8601-parse ts))
         (encoded (encode-time decoded)))
    (format-time-string "%d-%m-%y, %H:%M[%z]" encoded)))

(defun mastodon-toot--iso-to-org (ts)
  "Convert ISO8601 timestamp TS to something `org-read-date' can handle."
  (when ts
    (let* ((decoded (iso8601-parse ts)))
      (encode-time decoded))))


;;; DISPLAY KEYBINDINGS

(defun mastodon-toot--get-kbinds ()
  "Get a list of the keybindings in the `mastodon-toot-mode'."
  (let* ((binds (copy-tree mastodon-toot-mode-map))
         (prefix (car (cadr binds)))
         (bindings (remove nil
                           (mapcar (lambda (i)
                                     (when (listp i) i))
                                   (cadr binds)))))
    (mapcar (lambda (b)
              (setf (car b) (vector prefix (car b)))
              b)
            bindings)))

(defun mastodon-toot--format-kbind-command (cmd)
  "Format CMD to be more readable.
e.g. `mastodon-toot-send' -> Send."
  (let* ((str (symbol-name cmd))
         (re "mastodon-toot-\\(.*\\)$")
         (str2 (save-match-data
                 (string-match re str)
                 (match-string 1 str))))
    (capitalize (replace-regexp-in-string "-" " " str2))))

(defun mastodon-toot--format-kbind (kbind)
  "Format a single keybinding, KBIND, for display in documentation."
  (let ((key (concat "\\`"
                     (help-key-description (car kbind) nil)
                     "'"))
        (command (mastodon-toot--format-kbind-command (cdr kbind))))
    (substitute-command-keys
     (format
      (concat (mastodon-toot--comment "    ")
              "%-10s"
              (mastodon-toot--comment " - %s"))
      key command))))

(defun mastodon-toot--comment (str)
  "Propertize STR with `mastodon-toot-docs-face'."
  (propertize str
              'face 'mastodon-toot-docs-face))

(defun mastodon-toot--format-kbinds (kbinds)
  "Format a list of keybindings, KBINDS, for display in documentation."
  (mapcar #'mastodon-toot--format-kbind kbinds))

(defvar-local mastodon-toot--kbinds-pairs nil
  "Contains a list of paired toot compose buffer keybindings for inserting.")

(defun mastodon-toot--formatted-kbinds-pairs (kbinds-list longest)
  "Return a list of strings each containing two formatted kbinds.
KBINDS-LIST is the list of formatted bindings to pair.
LONGEST is the length of the longest binding."
  (when kbinds-list
    (push (concat "\n"
                  (car kbinds-list)
                  (make-string (- (1+ longest) (length (car kbinds-list)))
                               ?\ )
                  (cadr kbinds-list))
          mastodon-toot--kbinds-pairs)
    (mastodon-toot--formatted-kbinds-pairs (cddr kbinds-list) longest))
  (reverse mastodon-toot--kbinds-pairs))

(defun mastodon-toot--kbinds-longest (kbinds-list)
  "Return the length of the longest item in KBINDS-LIST."
  (let ((lengths (mapcar #'length kbinds-list)))
    (car (sort lengths #'>))))


;;; DISPLAY DOCS

(defun mastodon-toot--make-mode-docs ()
  "Create formatted documentation text for `mastodon-toot-mode'."
  (let* ((kbinds (mastodon-toot--get-kbinds))
         (formatted (mastodon-toot--format-kbinds kbinds))
         (longest-kbind (mastodon-toot--kbinds-longest
                         formatted)))
    (concat
     (mastodon-toot--comment
      " Compose a new toot here. The following keybindings are available:")
     (mapconcat
      #'identity
      (mastodon-toot--formatted-kbinds-pairs formatted longest-kbind)
      nil))))

(defun mastodon-toot--format-reply-in-compose (reply-text)
  "Format a REPLY-TEXT for display in compose buffer docs."
  (let* ((rendered (mastodon-tl--render-text reply-text))
         (no-props (substring-no-properties rendered))
         ;; FIXME: this replaces \n at end of every post, so we have to trim:
         (no-newlines (string-trim
                       (replace-regexp-in-string "[\n]+" " " no-props)))
         (reply-to (concat " Reply to: \"" no-newlines "\""))
         (crop (truncate-string-to-width reply-to
                                         mastodon-toot-orig-in-reply-length)))
    (if (> (length no-newlines)
           (length crop)) ; we cropped:
        (concat crop "\n")
      (concat reply-to "\n"))))

(defun mastodon-toot--display-docs-and-status-fields (&optional reply-text)
  "Insert propertized text with documentation about `mastodon-toot-mode'.
Also includes and the status fields which will get updated based
on the status of NSFW, content warning flags, media attachments, etc.
REPLY-TEXT is the text of the toot being replied to."
  (let ((divider
         "|=================================================================|"))
    (insert
     (concat
      (mastodon-toot--make-mode-docs) "\n"
      (mastodon-toot--comment divider) "\n"
      (propertize
       (concat
        " "
        (propertize "Count"
                    'toot-post-counter t)
        " ⋅ "
        (propertize "Visibility"
                    'toot-post-visibility t)
        " ⋅ "
        (propertize "Language"
                    'toot-post-language t)
        " "
        (propertize "Scheduled"
                    'toot-post-scheduled t)
        " "
        (propertize "CW"
                    'toot-post-cw-flag t)
        " "
        (propertize "POLL"
                    'toot-post-poll-flag t)
        " "
        (propertize "NSFW"
                    'toot-post-nsfw-flag t)
        "\n"
        " Attachments: "
        (propertize "None                  "
                    'toot-attachments t)
        "\n"
        (when reply-text
          (propertize
           (mastodon-toot--format-reply-in-compose reply-text)
           'toot-reply t))
        divider)
       'face 'mastodon-toot-docs-face
       'read-only "Edit your message below."
       'toot-post-header t))
     ;; allow us to enter text after read-only header:
     (propertize "\n\n"
                 'rear-nonsticky t))))

(defun mastodon-toot--most-restrictive-visibility (reply-visibility)
  "Return REPLY-VISIBILITY or default visibility, whichever is more restrictive.
The default is given by `mastodon-toot--default-reply-visibility'."
  (unless (null reply-visibility)
    (let ((less-restrictive (member (intern mastodon-toot--default-reply-visibility)
				    mastodon-toot-visibility-list)))
      (if (member (intern reply-visibility) less-restrictive)
	  reply-visibility
        mastodon-toot--default-reply-visibility))))

(defun mastodon-toot--render-reply-region-str (str)
  "Refill STR and prefix all lines with >, as reply-quote text."
  (with-temp-buffer
    (insert str)
    ;; unfill first:
    (let ((fill-column (point-max)))
      (fill-region (point-min) (point-max)))
    ;; then fill:
    (fill-region (point-min) (point-max))
    ;; add our own prefix, pauschal:
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward "^" nil t)
        (replace-match " > ")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mastodon-toot--setup-as-reply (reply-to-user reply-to-id
                                                    reply-json reply-region)
  "If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set `mastodon-toot--reply-to-id'.
REPLY-JSON is the full JSON of the toot being replied to.
REPLY-REGION is a string to be injected into the buffer."
  (let ((reply-visibility (mastodon-toot--most-restrictive-visibility
	                   (alist-get 'visibility reply-json)))
        (reply-cw (alist-get 'spoiler_text reply-json)))
    (when reply-to-user
      (when (> (length reply-to-user) 0) ; self is "" unforch
        (insert (format "%s " reply-to-user)))
      (when reply-region
        (insert "\n"
                (mastodon-toot--render-reply-region-str reply-region)
                "\n"))
      (setq mastodon-toot--reply-to-id reply-to-id)
      (unless (string= mastodon-toot--visibility reply-visibility)
        (setq mastodon-toot--visibility reply-visibility))
      (mastodon-toot--set-cw reply-cw))))

(defun mastodon-toot--update-status-fields (&rest _args)
  "Update the status fields in the header based on the current state."
  (ignore-errors  ;; called from after-change-functions so let's not leak errors
    (let* ((inhibit-read-only t)
           (header-region (mastodon-tl--find-property-range 'toot-post-header
                                                            (point-min)))
           (count-region (mastodon-tl--find-property-range 'toot-post-counter
                                                           (point-min)))
           (vis-region (mastodon-tl--find-property-range
                        'toot-post-visibility (point-min)))
           (nsfw-region (mastodon-tl--find-property-range 'toot-post-nsfw-flag
                                                          (point-min)))
           (cw-region (mastodon-tl--find-property-range 'toot-post-cw-flag
                                                        (point-min)))
           (lang-region (mastodon-tl--find-property-range 'toot-post-language
                                                          (point-min)))
           (sched-region (mastodon-tl--find-property-range 'toot-post-scheduled
                                                           (point-min)))
           (poll-region (mastodon-tl--find-property-range 'toot-post-poll-flag
                                                          (point-min)))
           (toot-string (buffer-substring-no-properties (cdr header-region)
                                                        (point-max))))
      (mastodon-toot--apply-fields-props
       count-region
       (format "%s/%s chars"
               (mastodon-toot--count-toot-chars toot-string)
               (number-to-string mastodon-toot--max-toot-chars)))
      (mastodon-toot--apply-fields-props
       vis-region
       (format "%s"
               (if (string= "private" mastodon-toot--visibility)
                   "followers-only"
                 mastodon-toot--visibility)))
      ;; WHEN clauses don't work here, we need "" as display arg:
      (mastodon-toot--apply-fields-props
       lang-region
       (if mastodon-toot--language
           (format "Lang: %s ⋅" mastodon-toot--language)
         ""))
      (mastodon-toot--apply-fields-props
       sched-region
       (if mastodon-toot--scheduled-for
           (format "Scheduled: %s ⋅"
                   (mastodon-toot--iso-to-human
                    mastodon-toot--scheduled-for))
         ""))
      (mastodon-toot--apply-fields-props
       nsfw-region
       (if mastodon-toot--content-nsfw
           (if mastodon-toot--media-attachments
               "NSFW" "NSFW (attachments only)")
         "")
       'mastodon-cw-face)
      (mastodon-toot--apply-fields-props
       poll-region
       (if mastodon-toot-poll
           "POLL"
         "")
       'mastodon-cw-face
       (prin1-to-string mastodon-toot-poll))
      (mastodon-toot--apply-fields-props
       cw-region
       (if (and mastodon-toot--content-warning
                (not (string= "" mastodon-toot--content-warning)))
           (format "CW: %s" mastodon-toot--content-warning)
         "  ") ;; hold the blank space
       'mastodon-cw-face))))

(defun mastodon-toot--apply-fields-props (region display &optional face help-echo)
  "Apply DISPLAY props FACE and HELP-ECHO to REGION, a cons of beg and end."
  (add-text-properties (car region) (cdr region)
                       `(display
                         ,display
                         ,@(when face `(face ,face))
                         ,@(when help-echo `(help-echo ,help-echo)))))

(defun mastodon-toot--count-toot-chars (toot-string &optional cw)
  "Count the characters in TOOT-STRING.
URLs always = 23, and domain names of handles are not counted.
This is how mastodon does it.
CW is the content warning, which contributes to the character count."
  ;; FIXME: URL chars is avail at /api/v1/instance
  ;; for masto, it's .statuses.characters_reserved_per_url
  (let* ((url-replacement (make-string 23 ?x))
         (count-str (replace-regexp-in-string ; handle @handles
                     mastodon-toot-handle-regex "\2"
                     (replace-regexp-in-string ; handle URLs
                      mastodon-toot-url-regex url-replacement toot-string))))
    (+ (length cw)
       (length count-str))))


;;; DRAFTS

(defun mastodon-toot--save-toot-text (&rest _args)
  "Save the current toot text in `mastodon-toot-current-toot-text'.
Added to `after-change-functions' in new toot buffers."
  (let ((text (mastodon-toot--remove-docs)))
    (unless (string-empty-p text)
      (setq mastodon-toot-current-toot-text text))))

(defun mastodon-toot-open-draft-toot ()
  "Prompt for a draft and compose a toot with it."
  (interactive)
  (if mastodon-toot-draft-toots-list
      (let ((text (completing-read "Select draft toot: "
                                   mastodon-toot-draft-toots-list
                                   nil t)))
        (if (not (mastodon-toot--compose-buffer-p))
            (mastodon-toot--compose-buffer nil nil nil text)
          (when (and (not (mastodon-toot--empty-p :text-only))
                     (y-or-n-p "Replace current text with draft?"))
            (cl-pushnew mastodon-toot-current-toot-text
                        mastodon-toot-draft-toots-list)
            (goto-char
             (cdr (mastodon-tl--find-property-range 'toot-post-header
                                                    (point-min))))
            (kill-region (point) (point-max))
            ;; to not save to kill-ring:
            ;; (delete-region (point) (point-max))
            (insert text))))
    (unless (mastodon-toot--compose-buffer-p)
      (mastodon-toot--compose-buffer))
    (message "No drafts available.")))

(defun mastodon-toot-delete-draft-toot ()
  "Prompt for a draft toot and delete it."
  (interactive)
  (if (not mastodon-toot-draft-toots-list)
      (user-error "No drafts to delete")
    (let ((draft (completing-read "Select draft to delete: "
                                  mastodon-toot-draft-toots-list
                                  nil t)))
      (setq mastodon-toot-draft-toots-list
            (cl-delete draft mastodon-toot-draft-toots-list :test #'equal))
      (message "Draft deleted!"))))

(defun mastodon-toot-delete-all-drafts ()
  "Delete all drafts."
  (interactive)
  (setq mastodon-toot-draft-toots-list nil)
  (message "All drafts deleted!"))


;;; PROPERTIZE TAGS AND HANDLES

(defun mastodon-toot--propertize-tags-and-handles (&rest _args)
  "Propertize tags and handles in toot compose buffer.
Added to `after-change-functions'."
  (when (mastodon-toot--compose-buffer-p)
    (let ((header-region (mastodon-tl--find-property-range 'toot-post-header
                                                           (point-min)))
          (face (when mastodon-toot--proportional-fonts-compose
                  'variable-pitch)))
      ;; cull any prev props:
      ;; stops all text after a handle or mention being propertized:
      (set-text-properties (cdr header-region) (point-max) `(face ,face))
      (mastodon-toot--propertize-item mastodon-toot-tag-regex
                                      'success
                                      (cdr header-region))
      (mastodon-toot--propertize-item mastodon-toot-handle-regex
                                      'mastodon-display-name-face
                                      (cdr header-region))
      (mastodon-toot--propertize-item mastodon-toot-url-regex
                                      'link
                                      (cdr header-region)))))

(defun mastodon-toot--propertize-item (regex face start)
  "Propertize item matching REGEX with FACE starting from START."
  (save-excursion
    (goto-char start)
    (cl-loop while (search-forward-regexp regex nil :noerror)
             do (add-text-properties (match-beginning 2)
                                     (match-end 2)
                                     `(face ,face)))))

(defun mastodon-toot--compose-buffer-p ()
  "Return t if compose buffer is current."
  (or (mastodon-tl--buffer-type-eq 'edit-toot)
      (mastodon-tl--buffer-type-eq 'new-toot)))

(defun mastodon-toot--fill-reply-in-compose ()
  "Fill reply text in compose buffer to the width of the divider."
  (save-excursion
    (save-match-data
      (let* ((fill-column 67))
        (goto-char (point-min))
        (when-let* ((prop (text-property-search-forward 'toot-reply)))
          (fill-region (prop-match-beginning prop)
                       (point)))))))


;;; COMPOSE BUFFER FUNCTION

(defun mastodon-toot--compose-buffer
    (&optional reply-to-user reply-to-id reply-json initial-text edit)
  "Create a new buffer to capture text for a new toot.
If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the `mastodon-toot--reply-to-id' var.
REPLY-JSON is the full JSON of the toot being replied to.
INITIAL-TEXT is used by `mastodon-toot-insert-draft-toot' to add
a draft into the buffer.
EDIT means we are editing an existing toot, not composing a new one."
  (let* ((buffer-name (if edit "*edit toot*" "*new toot*"))
         (buffer-exists (get-buffer buffer-name))
         (buffer (if (not buffer-exists)
                     (get-buffer-create buffer-name)
                   ;; if a user hits reply while a compose buffer is already
                   ;; open, we really ought to wipe it all and start over.
                   (switch-to-buffer-other-window buffer-exists)
                   (if (not (y-or-n-p "Overwrite existing compose buffer?"))
                       (user-error "Aborting")
                     (kill-buffer-and-window)
                     (get-buffer-create buffer-name))))
         (inhibit-read-only t)
         (reply-text (alist-get 'content
                                (or (alist-get 'reblog reply-json)
                                    reply-json)))
         (previous-window-config (list (current-window-configuration)
                                       (point-marker))))
    (switch-to-buffer-other-window buffer)
    (text-mode)
    (mastodon-toot-mode t)
    ;; set visibility:
    (setq mastodon-toot--visibility
          (or (plist-get mastodon-profile-account-settings 'privacy)
              ;; use toot visibility setting from the server:
              (mastodon-profile--get-source-value 'privacy)
              "public")) ; fallback
    ;; default language:
    ;; NB: this is not necessarily set in
    ;; `mastodon-profile-credential-account' nor in
    ;; `mastodon-profile-account-settings'!
    (setq mastodon-toot--language
          (mastodon-profile--get-preferences-pref 'posting:default:language))
    ;; display original toot:
    (if mastodon-toot-display-orig-in-reply-buffer
        (progn
          (mastodon-toot--display-docs-and-status-fields reply-text)
          (mastodon-toot--fill-reply-in-compose))
      (mastodon-toot--display-docs-and-status-fields))
    ;; `reply-to-user' (alone) is also used by `mastodon-tl-dm-user', so
    ;; perhaps we should not always call --setup-as-reply, or make its
    ;; workings conditional on reply-to-id. currently it only checks for
    ;; reply-to-user.
    (mastodon-toot--setup-as-reply reply-to-user reply-to-id reply-json
                                   ;; only initial-text if reply (not edit):
                                   (when reply-json initial-text))
    (unless mastodon-toot--max-toot-chars
      ;; no need to fetch from `mastodon-profile-account-settings' as
      ;; `mastodon-toot--max-toot-chars' is set when we set it
      (mastodon-toot--get-max-toot-chars))
    ;; set up completion:
    (setq-local completion-ignore-case t)
    (when mastodon-toot--enable-completion
      (set (make-local-variable 'completion-at-point-functions)
           (add-to-list 'completion-at-point-functions
                        #'mastodon-toot--mentions-capf))
      (add-to-list 'completion-at-point-functions
                   #'mastodon-toot--tags-capf)
      (add-to-list 'completion-at-point-functions
                   #'mastodon-toot--emoji-capf)
      ;; company
      (when (and mastodon-toot--use-company-for-completion
                 (require 'company nil :no-error))
        (declare-function company-mode-on "company")
        (set (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-capf))
        (company-mode-on)))
    ;; after-change:
    (make-local-variable 'after-change-functions)
    (cl-pushnew #'mastodon-toot--save-toot-text after-change-functions)
    (cl-pushnew #'mastodon-toot--update-status-fields after-change-functions)
    (mastodon-toot--update-status-fields)
    (cl-pushnew #'mastodon-toot--propertize-tags-and-handles after-change-functions)
    (mastodon-toot--propertize-tags-and-handles)
    (mastodon-toot--refresh-attachments-display)
    ;; draft toot text saving:
    (setq mastodon-toot-current-toot-text nil)
    ;; if we set this before changing modes, it gets nuked:
    (setq mastodon-toot-previous-window-config previous-window-config)
    (when mastodon-toot--proportional-fonts-compose
      (facemenu-set-face 'variable-pitch))
    (when (and mastodon-use-emojify
               ;; emojify loaded but poss not enabled in our buffer:
               (boundp 'emojify-mode))
      (emojify-mode))
    (when (and initial-text
               (not reply-json))
      (insert initial-text))))

;; flyspell ignore masto toot regexes:
(defvar flyspell-generic-check-word-predicate)

(defun mastodon-toot-mode-flyspell-verify ()
  "A predicate function for `flyspell'.
Only text that is not one of these faces will be spell-checked."
  (let ((faces '(mastodon-display-name-face
                 mastodon-toot-docs-face font-lock-comment-face
                 success link)))
    (unless (eql (point) (point-min))
      ;; (point) is next char after the word. Must check one char before.
      (let ((f (get-text-property (1- (point)) 'face)))
        (not (memq f faces))))))

(defun mastodon-toot-mode-hook-fun ()
  "Function for code to run in `mastodon-toot-mode-hook'."
  ;; disable auto-fill-mode:
  (auto-fill-mode -1)
  ;; add flyspell predicate function:
  (setq flyspell-generic-check-word-predicate
        #'mastodon-toot-mode-flyspell-verify))

(add-hook 'mastodon-toot-mode-hook #'mastodon-toot-mode-hook-fun)

;;;###autoload
(add-hook 'mastodon-toot-mode-hook
          #'mastodon-profile--fetch-server-account-settings-maybe)


(define-minor-mode mastodon-toot-mode
  "Minor mode for composing toots."
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
