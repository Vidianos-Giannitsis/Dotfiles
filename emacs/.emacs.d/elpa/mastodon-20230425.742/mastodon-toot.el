;;; mastodon-toot.el --- Minor mode for sending Mastodon toots  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 1.0.0
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

(require 'emojify nil :noerror)
(declare-function emojify-insert-emoji "emojify")
(declare-function emojify-set-emoji-data "emojify")
(defvar emojify-emojis-dir)
(defvar emojify-user-emojis)

(require 'cl-lib)
(require 'persist)
(require 'mastodon-iso)

(defvar mastodon-instance-url)
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--enable-proportional-fonts)
(defvar mastodon-profile-account-settings)

(autoload 'iso8601-parse "iso8601")
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
(autoload 'mastodon-profile--fetch-server-account-settings "mastodon-profile")
(autoload 'mastodon-profile--fetch-server-account-settings-maybe "mastodon-profile")
(autoload 'mastodon-profile--get-source-pref "mastodon-profile")
(autoload 'mastodon-profile--show-user "mastodon-profile")
(autoload 'mastodon-profile--update-preference "mastodon-profile")
(autoload 'mastodon-search--search-accounts-query "mastodon-search")
(autoload 'mastodon-search--search-tags-query "mastodon-search")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-tl--buffer-type-eq "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--do-if-toot-strict "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--goto-next-toot "mastodon-tl")
(autoload 'mastodon-tl--map-alist "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-tl--set-buffer-spec "mastodon-tl")
(autoload 'mastodon-tl--symbol "mastodon-tl")
(autoload 'mastodon-tl--toot-id "mastodon-tl")
(autoload 'mastodon-toot "mastodon")
(autoload 'mastodon-views--cancel-scheduled-toot "mastodon-views")
(autoload 'mastodon-views--view-scheduled-toots "mastodon-views")
(autoload 'org-read-date "org")

;; for mastodon-toot--translate-toot-text
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

(defcustom mastodon-toot-orig-in-reply-length 160
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

(defvar-local mastodon-toot--content-warning nil
  "A flag whether the toot should be marked with a content warning.")

(defvar-local mastodon-toot--content-warning-from-reply-or-redraft nil
  "The content warning of the toot being replied to.")

(defvar-local mastodon-toot--content-nsfw nil
  "A flag indicating whether the toot should be marked as NSFW.")

(defvar mastodon-toot-visibility-list
  '(direct private unlisted public)
  "A list of the available toot visibility settings.")

(defvar-local mastodon-toot--visibility nil
  "A string indicating the visibility of the toot being composed.
Valid values are \"direct\", \"private\" (followers-only),
\"unlisted\", and \"public\".

This is determined by the account setting on the server. To
change the setting on the server, see
`mastodon-toot--set-default-visibility'.")

(defvar-local mastodon-toot--media-attachments nil
  "A list of the media attachments of the toot being composed.")

(defvar-local mastodon-toot--media-attachment-ids nil
  "A list of any media attachment ids of the toot being composed.")

(defvar-local mastodon-toot-poll nil
  "A list of poll options for the toot being composed.")

(defvar-local mastodon-toot--language nil
  "The language of the toot being composed, in ISO 639 (two-letter).")

(defvar-local mastodon-toot--scheduled-for nil
  "An ISO 8601 timestamp that specifying when the post should be published.
Should be at least 5 minutes into the future.")

(defvar-local mastodon-toot--scheduled-id nil
  "The id of the scheduled post that we are now editing.")

(defvar-local mastodon-toot--reply-to-id nil
  "Buffer-local variable to hold the id of the toot being replied to.")

(defvar-local mastodon-toot--edit-toot-id nil
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
to also capture toots that are 'sent' but that don't successfully
send.")

(defvar mastodon-toot-handle-regex
  (concat
   ;; preceding bracket, space or bol [boundary doesn't work with @]
   "\\([(\n\t ]\\|^\\)"
   "\\(?2:@[0-9a-zA-Z._-]+" ; a handle
   "\\(@[^ \n\t]*\\)?\\)" ; with poss domain, * = allow only @
   "\\b"))

(defvar mastodon-toot-tag-regex
  (concat
   ;; preceding bracket, space or bol [boundary doesn't work with #]
   "\\([(\n\t ]\\|^\\)"
   "\\(?2:#[0-9a-zA-Z_]+\\)" ; tag
   "\\b")) ; boundary

(defvar mastodon-toot-url-regex
  ;; adapted from ffap-url-regexp
  (concat
   "\\(?2:\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)" ; uri prefix
   "[^ \n\t]*\\)" ; any old thing that's, i.e. we allow invalid/unwise chars
   "\\b")) ; boundary

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot--send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot--cancel)
    (define-key map (kbd "C-c C-w") #'mastodon-toot--toggle-warning)
    (define-key map (kbd "C-c C-n") #'mastodon-toot--toggle-nsfw)
    (define-key map (kbd "C-c C-v") #'mastodon-toot--change-visibility)
    (when (require 'emojify nil :noerror)
      (define-key map (kbd "C-c C-e") #'mastodon-toot--insert-emoji))
    (define-key map (kbd "C-c C-a") #'mastodon-toot--attach-media)
    (define-key map (kbd "C-c !") #'mastodon-toot--clear-all-attachments)
    (define-key map (kbd "C-c C-p") #'mastodon-toot--create-poll)
    (define-key map (kbd "C-c C-l") #'mastodon-toot--set-toot-language)
    (define-key map (kbd "C-c C-s") #'mastodon-toot--schedule-toot)
    map)
  "Keymap for `mastodon-toot'.")

(defun mastodon-toot--set-default-visibility ()
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
         (or
          (alist-get 'max_toot_chars json-response)
          ;; some servers have this instead:
          (alist-get 'max_characters
                     (alist-get 'statuses
                                (alist-get 'configuration
                                           json-response))))))
    (setq mastodon-toot--max-toot-chars max-chars)
    (unless no-toot
      (with-current-buffer "*new toot*"
        (mastodon-toot--update-status-fields)))))

(defun mastodon-toot--action-success (marker byline-region remove)
  "Insert/remove the text MARKER with `success' face in byline.
BYLINE-REGION is a cons of start and end pos of the byline to be
modified.
Remove MARKER if REMOVE is non-nil, otherwise add it."
  (let ((inhibit-read-only t)
        (bol (car byline-region))
        (eol (cdr byline-region))
        (at-byline-p (eq (mastodon-tl--property 'byline :no-move) t)))
    (save-excursion
      (when remove
        (goto-char bol)
        (beginning-of-line) ;; The marker is not part of the byline
        (if (search-forward (format "(%s) " marker) eol t)
            (replace-match "")
          (message "Oops: could not find marker '(%s)'" marker)))
      (unless remove
        (goto-char bol)
        (insert (format "(%s) "
                        (propertize marker 'face 'success)))))
    (when at-byline-p
      ;; leave point after the marker:
      (unless remove
        ;; if point is inside the byline, back up first so
        ;; we don't move to the following toot:
        (beginning-of-line)
        (forward-line -1)
        (mastodon-tl--goto-next-toot)))))

(defun mastodon-toot--action (action callback)
  "Take ACTION on toot at point, then execute CALLBACK.
Makes a POST request to the server. Used for favouriting,
boosting, or bookmarking toots."
  (let* ((id (mastodon-tl--property 'base-toot-id))
         (url (mastodon-http--api (concat "statuses/"
                                          (mastodon-tl--as-string id)
                                          "/"
                                          action))))
    (let ((response (mastodon-http--post url)))
      (mastodon-http--triage response callback))))

(defun mastodon-toot--toggle-boost-or-favourite (type)
  "Toggle boost or favourite of toot at `point'.
TYPE is a symbol, either `favourite' or `boost.'"
  (interactive)
  (mastodon-tl--do-if-toot-strict
   (let* ((boost-p (equal type 'boost))
          (has-id (mastodon-tl--property 'base-toot-id))
          (byline-region (when has-id
                           (mastodon-tl--find-property-range 'byline (point))))
          (id (when byline-region
                (mastodon-tl--as-string (mastodon-tl--property 'base-toot-id))))
          (boosted (when byline-region
                     (get-text-property (car byline-region) 'boosted-p)))
          (faved (when byline-region
                   (get-text-property (car byline-region) 'favourited-p)))
          (action (if boost-p
                      (if boosted "unreblog" "reblog")
                    (if faved "unfavourite" "favourite")))
          (msg (if boosted "unboosted" "boosted"))
          (action-string (if boost-p "boost" "favourite"))
          (remove (if boost-p (when boosted t) (when faved t)))
          (toot-type (alist-get 'type (mastodon-tl--property 'toot-json)))
          (visibility (mastodon-tl--field 'visibility
                                          (mastodon-tl--property 'toot-json))))
     (if byline-region
         (cond ;; actually there's nothing wrong with faving/boosting own toots!
          ;;((mastodon-toot--own-toot-p (mastodon-tl--property 'toot-json))
          ;;(error "You can't %s your own toots" action-string))
          ;; & nothing wrong with faving/boosting own toots from notifs:
          ;; this boosts/faves the base toot, not the notif status
          ((and (equal "reblog" toot-type)
                (not (mastodon-tl--buffer-type-eq 'notifications)))
           (error "You can't %s boosts" action-string))
          ((and (equal "favourite" toot-type)
                (not (mastodon-tl--buffer-type-eq 'notifications)))
           (error "You can't %s favourites" action-string))
          ((and (equal "private" visibility)
                (equal type 'boost))
           (error "You can't boost private toots"))
          (t
           (mastodon-toot--action
            action
            (lambda ()
              (let ((inhibit-read-only t))
                (add-text-properties (car byline-region)
                                     (cdr byline-region)
                                     (if boost-p
                                         (list 'boosted-p (not boosted))
                                       (list 'favourited-p (not faved))))
                (mastodon-toot--update-stats-on-action type remove)
                (mastodon-toot--action-success
                 (if boost-p
                     (mastodon-tl--symbol 'boost)
                   (mastodon-tl--symbol 'favourite))
                 byline-region remove))
              (message (format "%s #%s" (if boost-p msg action) id))))))
       (message (format "Nothing to %s here?!?" action-string))))))

(defun mastodon-toot--inc-or-dec (count subtract)
  "If SUBTRACT, decrement COUNT, else increment."
  (if subtract
      (1- count)
    (1+ count)))

(defun mastodon-toot--update-stats-on-action (action &optional subtract)
  "Increment the toot stats display upon ACTION.
ACTION is a symbol, either `favourite' or `boost'.
SUBTRACT means we are un-favouriting or unboosting, so we decrement."
  (let* ((count-prop (if (eq action 'favourite)
                         'favourites-count
                       'boosts-count))
         (count-prop-range (mastodon-tl--find-property-range count-prop (point)))
         (count (get-text-property (car count-prop-range) count-prop))
         (inhibit-read-only 1))
    ;; TODO another way to implement this would be to async fetch counts again
    ;;  and re-display from count-properties
    (add-text-properties
     (car count-prop-range)
     (cdr count-prop-range)
     (list 'display ; update the display prop:
           (number-to-string
            (mastodon-toot--inc-or-dec count subtract))
           ;; update the count prop
           ;; we rely on this for any subsequent actions:
           count-prop
           (mastodon-toot--inc-or-dec count subtract)))))

(defun mastodon-toot--toggle-boost ()
  "Boost/unboost toot at `point'."
  (interactive)
  (mastodon-toot--toggle-boost-or-favourite 'boost))

(defun mastodon-toot--toggle-favourite ()
  "Favourite/unfavourite toot at `point'."
  (interactive)
  (mastodon-toot--toggle-boost-or-favourite 'favourite))

;; TODO maybe refactor into boost/fave fun
(defun mastodon-toot--toggle-bookmark ()
  "Bookmark or unbookmark toot at point."
  (interactive)
  (mastodon-tl--do-if-toot-strict
   (let* ( ;(toot (mastodon-tl--property 'toot-json))
          (id (mastodon-tl--property 'base-toot-id))
          ;; (mastodon-tl--as-string (mastodon-tl--toot-id toot)))
          (bookmarked-p (mastodon-tl--property 'bookmarked-p))
          (prompt (if bookmarked-p
                      (format "Toot already bookmarked. Remove? ")
                    (format "Bookmark this toot? ")))
          (byline-region
           (when id
             (mastodon-tl--find-property-range 'byline (point))))
          (action (if bookmarked-p "unbookmark" "bookmark"))
          (bookmark-str (mastodon-tl--symbol 'bookmark))
          (message (if bookmarked-p
                       "Bookmark removed!"
                     "Toot bookmarked!"))
          (remove (when bookmarked-p t)))
     (if byline-region
         (when (y-or-n-p prompt)
           (mastodon-toot--action
            action
            (lambda ()
              (let ((inhibit-read-only t))
                (add-text-properties (car byline-region)
                                     (cdr byline-region)
                                     (list 'bookmarked-p (not bookmarked-p))))
              (mastodon-toot--action-success
               bookmark-str
               byline-region remove)
              (message (format "%s #%s" message id)))))
       (message (format "Nothing to %s here?!?" action))))))

(defun mastodon-toot--list-toot-boosters ()
  "List the boosters of toot at point."
  (interactive)
  (mastodon-toot--list-toot-boosters-or-favers))

(defun mastodon-toot--list-toot-favouriters ()
  "List the favouriters of toot at point."
  (interactive)
  (mastodon-toot--list-toot-boosters-or-favers :favourite))

(defun mastodon-toot--list-toot-boosters-or-favers (&optional favourite)
  "List the favouriters or boosters of toot at point.
With FAVOURITE, list favouriters, else list boosters."
  (mastodon-tl--do-if-toot-strict
   (let* ((base-toot (mastodon-tl--property 'base-toot-id))
          (endpoint (if favourite "favourited_by" "reblogged_by"))
          (url (mastodon-http--api
                (format "statuses/%s/%s" base-toot endpoint)))
          (params '(("limit" . "80")))
          (json (mastodon-http--get-json url params)))
     (if (eq (caar json) 'error)
         (error "%s (Status does not exist or is private)"
                (alist-get 'error json))
       (let ((handles (mastodon-tl--map-alist 'acct json))
             (type-string (if favourite "Favouriters" "Boosters")))
         (if (not handles)
             (error "Looks like this toot has no %s" type-string)
           (let ((choice
                  (completing-read
                   (format "%s (enter to view profile): " type-string)
                   handles
                   nil
                   t)))
             (mastodon-profile--show-user choice))))))))

(defun mastodon-toot--copy-toot-url ()
  "Copy URL of toot at point.
If the toot is a fave/boost notification, copy the URLof the
base toot."
  (interactive)
  (let* ((toot (or (mastodon-tl--property 'base-toot)
                   (mastodon-tl--property 'toot-json)))
         (url (if (mastodon-tl--field 'reblog toot)
                  (alist-get 'url (alist-get 'reblog toot))
                (alist-get 'url toot))))
    (kill-new url)
    (message "Toot URL copied to the clipboard.")))

(defun mastodon-toot--copy-toot-text ()
  "Copy text of toot at point.
If the toot is a fave/boost notification, copy the text of the
base toot."
  (interactive)
  (let* ((toot (or (mastodon-tl--property 'base-toot)
                   (mastodon-tl--property 'toot-json))))
    (kill-new (mastodon-tl--content toot))
    (message "Toot content copied to the clipboard.")))

;; (when (require 'lingva nil :no-error)
(defun mastodon-toot--translate-toot-text ()
  "Translate text of toot at point.
Uses `lingva.el'."
  (interactive)
  (if (not (require 'lingva nil :no-error))
      (message "Looks like you need to install lingva.el first.")
    (if mastodon-tl--buffer-spec
        (let ((toot (mastodon-tl--property 'toot-json)))
          (if toot
              (lingva-translate nil
                                (mastodon-tl--content toot)
                                (when mastodon-tl--enable-proportional-fonts
                                  t))
            (message "No toot to translate?")))
      (message "No mastodon buffer?"))))

(defun mastodon-toot--own-toot-p (toot)
  "Check if TOOT is user's own, e.g. for deleting it."
  (and (not (alist-get 'reblog toot))
       (equal (alist-get 'acct (alist-get 'account toot))
              (mastodon-auth--user-acct))))

(defun mastodon-toot--pin-toot-toggle ()
  "Pin or unpin user's toot at point."
  (interactive)
  (let* ((toot (or (mastodon-tl--property 'base-toot) ;fave/boost notifs
                   (mastodon-tl--property 'toot-json)))
         (pinnable-p (mastodon-toot--own-toot-p toot))
         (pinned-p (equal (alist-get 'pinned toot) t))
         (action (if pinned-p "unpin" "pin"))
         (msg (if pinned-p "unpinned" "pinned"))
         (msg-y-or-n (if pinned-p "Unpin" "Pin")))
    (if (not pinnable-p)
        (message "You can only pin your own toots.")
      (when (y-or-n-p (format "%s this toot? " msg-y-or-n))
        (mastodon-toot--action action
                               (lambda ()
                                 (when mastodon-tl--buffer-spec
                                   (mastodon-tl--reload-timeline-or-profile))
                                 (message "Toot %s!" msg)))))))

(defun mastodon-toot--delete-toot ()
  "Delete user's toot at point synchronously."
  (interactive)
  (mastodon-toot--delete-and-redraft-toot t))

;; TODO: handle media/poll for redrafting toots
(defun mastodon-toot--delete-and-redraft-toot (&optional no-redraft)
  "Delete and redraft user's toot at point synchronously.
NO-REDRAFT means delete toot only."
  (interactive)
  (let* ((toot (or (mastodon-tl--property 'base-toot) ;fave/boost notifs
                   (mastodon-tl--property 'toot-json)))
         (id (mastodon-tl--as-string (mastodon-tl--toot-id toot)))
         (url (mastodon-http--api (format "statuses/%s" id)))
         (toot-cw (alist-get 'spoiler_text toot))
         (toot-visibility (alist-get 'visibility toot))
         (reply-id (alist-get 'in_reply_to_id toot))
         (pos (point)))
    (if (not (mastodon-toot--own-toot-p toot))
        (message "You can only delete (and redraft) your own toots.")
      (when (y-or-n-p (if no-redraft
                          (format "Delete this toot? ")
                        (format "Delete and redraft this toot? ")))
        (let* ((response (mastodon-http--delete url)))
          (mastodon-http--triage
           response
           (lambda ()
             (if no-redraft
                 (progn
                   (when mastodon-tl--buffer-spec
                     (mastodon-tl--reload-timeline-or-profile pos))
                   (message "Toot deleted!"))
               (mastodon-toot--redraft response
                                       reply-id
                                       toot-visibility
                                       toot-cw)))))))))

(defun mastodon-toot--set-cw (&optional cw)
  "Set content warning to CW if it is non-nil."
  (unless (or (null cw) ; cw is nil for `mastodon-tl--dm-user'
              (string-empty-p cw))
    (setq mastodon-toot--content-warning t)
    (setq mastodon-toot--content-warning-from-reply-or-redraft cw)))

(defun mastodon-toot--redraft (response &optional reply-id toot-visibility toot-cw)
  "Opens a new toot compose buffer using values from RESPONSE buffer.
REPLY-ID, TOOT-VISIBILITY, and TOOT-CW of deleted toot are preseved."
  (with-current-buffer response
    (let* ((json-response (mastodon-http--process-json))
           (content (alist-get 'text json-response)))
      (mastodon-toot--compose-buffer)
      (goto-char (point-max))
      (insert content)
      ;; adopt reply-to-id, visibility and CW from deleted toot:
      (mastodon-toot--set-toot-properties
       reply-id toot-visibility toot-cw
       ;; TODO set new lang/scheduled props here
       nil))))

(defun mastodon-toot--set-toot-properties (reply-id visibility cw lang
                                                    &optional scheduled
                                                    scheduled-id)
  "Set the toot properties for the current redrafted or edited toot.
REPLY-ID, VISIBILITY, CW, SCHEDULED, and LANG are the properties to set."
  (when reply-id
    (setq mastodon-toot--reply-to-id reply-id))
  (setq mastodon-toot--visibility visibility)
  (setq mastodon-toot--scheduled-for scheduled)
  (setq mastodon-toot--scheduled-id scheduled-id)
  (when (not (string-empty-p lang))
    (setq mastodon-toot--language lang))
  (mastodon-toot--set-cw cw)
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--kill (&optional cancel)
  "Kill `mastodon-toot-mode' buffer and window.
CANCEL means the toot was not sent, so we save the toot text as a draft."
  (let ((prev-window-config mastodon-toot-previous-window-config))
    (unless (eq mastodon-toot-current-toot-text nil)
      (when cancel
        (cl-pushnew mastodon-toot-current-toot-text
                    mastodon-toot-draft-toots-list :test 'equal)))
    ;; prevent some weird bug when cancelling a non-empty toot:
    (delete #'mastodon-toot--save-toot-text after-change-functions)
    (kill-buffer-and-window)
    (mastodon-toot--restore-previous-window-config prev-window-config)))

(defun mastodon-toot--cancel ()
  "Kill new-toot buffer/window. Does not POST content to Mastodon.
If toot is not empty, prompt to save text as a draft."
  (interactive)
  (if (mastodon-toot--empty-p)
      (mastodon-toot--kill)
    (when (y-or-n-p "Save draft toot?")
      (mastodon-toot--save-draft))
    (mastodon-toot--kill)))

(defun mastodon-toot--save-draft ()
  "Save the current compose toot text as a draft.
Pushes `mastodon-toot-current-toot-text' to
`mastodon-toot-draft-toots-list'."
  (interactive)
  (unless (eq mastodon-toot-current-toot-text nil)
    (cl-pushnew mastodon-toot-current-toot-text
                mastodon-toot-draft-toots-list :test 'equal)
    (message "Draft saved!")))

(defun mastodon-toot--empty-p (&optional text-only)
  "Return t if toot has no text, attachments, or polls.
TEXT-ONLY means don't check for attachments or polls."
  (and (if text-only
           t
         (not mastodon-toot--media-attachments)
         (not mastodon-toot-poll))
       (string-empty-p (mastodon-tl--clean-tabs-and-nl
                        (mastodon-toot--remove-docs)))))

(defalias 'mastodon-toot--insert-emoji
  'emojify-insert-emoji
  "Prompt to insert an emoji.")

(defun mastodon-toot--download-custom-emoji ()
  "Download `mastodon-instance-url's custom emoji.
Emoji images are stored in a subdir of `emojify-emojis-dir'.
To use the downloaded emoji, run `mastodon-toot--enable-custom-emoji'."
  (interactive)
  (let ((custom-emoji (mastodon-http--get-json
                       (mastodon-http--api "custom_emojis")))
        (mastodon-custom-emoji-dir (file-name-as-directory
                                    (concat (file-name-as-directory
                                             (expand-file-name
                                              emojify-emojis-dir))
                                            "mastodon-custom-emojis"))))
    (if (not (file-directory-p emojify-emojis-dir))
        (message "Looks like you need to set up emojify first.")
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
                                 (concat
                                  mastodon-custom-emoji-dir
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
  (let* ((mastodon-custom-emojis-dir (concat (expand-file-name
                                              emojify-emojis-dir)
                                             "/mastodon-custom-emojis/"))
         (custom-emoji-files (directory-files mastodon-custom-emojis-dir
                                              nil ; not full path
                                              "^[^.]")) ; no dot files
         (mastodon-emojify-user-emojis))
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

(defun mastodon-toot--enable-custom-emoji ()
  "Add `mastodon-instance-url's custom emoji to `emojify'.
Custom emoji must first be downloaded with
`mastodon-toot--download-custom-emoji'. Custom emoji are appended
to `emojify-user-emojis', and the emoji data is updated."
  (interactive)
  (unless (file-exists-p (concat (expand-file-name
                                  emojify-emojis-dir)
                                 "/mastodon-custom-emojis/"))
    (when (y-or-n-p "Looks like you haven't downloaded your
    instance's custom emoji yet. Download now? ")
      (mastodon-toot--download-custom-emoji)))
  (setq emojify-user-emojis
        (append (mastodon-toot--collect-custom-emoji)
                emojify-user-emojis))
  ;; if already loaded, reload
  (when (featurep 'emojify)
    (emojify-set-emoji-data)))

(defun mastodon-toot--remove-docs ()
  "Get the body of a toot from the current compose buffer."
  (let ((header-region (mastodon-tl--find-property-range 'toot-post-header
                                                         (point-min))))
    (buffer-substring (cdr header-region) (point-max))))

(defun mastodon-toot--build-poll-params ()
  "Return an alist of parameters for POSTing a poll status."
  (append
   (mastodon-http--build-array-params-alist
    "poll[options][]"
    (plist-get mastodon-toot-poll :options))
   `(("poll[expires_in]" .  ,(plist-get mastodon-toot-poll :expiry)))
   `(("poll[multiple]" . ,(symbol-name (plist-get mastodon-toot-poll :multi))))
   `(("poll[hide_totals]" . ,(symbol-name (plist-get mastodon-toot-poll :hide))))))

(defun mastodon-toot--read-cw-string ()
  "Read a content warning from the minibuffer."
  (when (and (not (mastodon-toot--empty-p))
             mastodon-toot--content-warning)
    (read-string "Warning: "
                 mastodon-toot--content-warning-from-reply-or-redraft)))

(defun mastodon-toot--send ()
  "POST contents of new-toot buffer to Mastodon instance and kill buffer.
If media items have been attached and uploaded with
`mastodon-toot--attach-media', they are attached to the toot.
If `mastodon-toot--edit-toot-id' is non-nil, PUT contents to
instance to edit a toot."
  (interactive)
  (let* ((toot (mastodon-toot--remove-docs))
         (scheduled mastodon-toot--scheduled-for)
         (scheduled-id mastodon-toot--scheduled-id)
         (edit-id mastodon-toot--edit-toot-id)
         (endpoint
          (if edit-id
              ;; we are sending an edit:
              (mastodon-http--api (format "statuses/%s"
                                          edit-id))
            (mastodon-http--api "statuses")))
         (cw (mastodon-toot--read-cw-string))
         (args-no-media (append `(("status" . ,toot)
                                  ("in_reply_to_id" . ,mastodon-toot--reply-to-id)
                                  ("visibility" . ,mastodon-toot--visibility)
                                  ("sensitive" . ,(when mastodon-toot--content-nsfw
                                                    (symbol-name t)))
                                  ("spoiler_text" . ,cw)
                                  ("language" . ,mastodon-toot--language))
                                ;; Pleroma instances can't handle null-valued
                                ;; scheduled_at args, so only add if non-nil
                                (when scheduled `(("scheduled_at" . ,scheduled)))))
         (args-media (when mastodon-toot--media-attachments
                       (mastodon-http--build-array-params-alist
                        "media_ids[]"
                        mastodon-toot--media-attachment-ids)))
         (args-poll (when mastodon-toot-poll
                      (mastodon-toot--build-poll-params)))
         ;; media || polls:
         (args (if mastodon-toot--media-attachments
                   (append args-media args-no-media)
                 (if mastodon-toot-poll
                     (append args-no-media args-poll)
                   args-no-media)))
         (prev-window-config mastodon-toot-previous-window-config))
    (cond ((and mastodon-toot--media-attachments
                ;; make sure we have media args
                ;; and the same num of ids as attachments
                (or (not args-media)
                    (not (= (length mastodon-toot--media-attachments)
                            (length mastodon-toot--media-attachment-ids)))))
           (message "Something is wrong with your uploads. Wait for them to complete or try again."))
          ((and mastodon-toot--max-toot-chars
                (> (mastodon-toot--count-toot-chars toot cw) mastodon-toot--max-toot-chars))
           (message "Looks like your toot (inc. CW) is longer than that maximum allowed length."))
          ((mastodon-toot--empty-p)
           (message "Empty toot. Cowardly refusing to post this."))
          (t
           (let ((response (if edit-id
                               ;; we are sending an edit:
                               (mastodon-http--put endpoint args)
                             (mastodon-http--post endpoint args))))
             (mastodon-http--triage
              response
              (lambda ()
                (setq masto-poll-toot-response response)
                (mastodon-toot--kill)
                (if scheduled
                    (message "Toot scheduled!")
                  (message "Toot toot!"))
                ;; cancel scheduled toot if we were editing it:
                (when scheduled-id
                  (mastodon-views--cancel-scheduled-toot
                   scheduled-id :no-confirm))
                (mastodon-toot--restore-previous-window-config prev-window-config)
                (when edit-id
                  (let ((pos (marker-position (cadr prev-window-config))))
                    (mastodon-tl--reload-timeline-or-profile pos))))))))))

;; EDITING TOOTS:

(defun mastodon-toot--edit-toot-at-point ()
  "Edit the user's toot at point."
  (interactive)
  (let ((toot (or (mastodon-tl--property 'base-toot); fave/boost notifs
                  (mastodon-tl--property 'toot-json))))
    (if (not (mastodon-toot--own-toot-p toot))
        (message "You can only edit your own toots.")
      (let* ((id (mastodon-tl--as-string (mastodon-tl--toot-id toot)))
             (source (mastodon-toot--get-toot-source id))
             (content (alist-get 'text source))
             (source-cw (alist-get 'spoiler_text source))
             (toot-visibility (alist-get 'visibility toot))
             (toot-language (alist-get 'language toot))
             (reply-id (alist-get 'in_reply_to_id toot)))
        (when (y-or-n-p "Edit this toot? ")
          (mastodon-toot--compose-buffer nil reply-id nil content :edit)
          (goto-char (point-max))
          ;; (insert content)
          ;; adopt reply-to-id, visibility, CW, and language:
          (mastodon-toot--set-toot-properties reply-id toot-visibility
                                              source-cw toot-language)
          (mastodon-toot--update-status-fields)
          (setq mastodon-toot--edit-toot-id id))))))

(defun mastodon-toot--get-toot-source (id)
  "Fetch the source JSON of toot with ID."
  (let ((url (mastodon-http--api (format "/statuses/%s/source" id))))
    (mastodon-http--get-json url nil :silent)))

(defun mastodon-toot--get-toot-edits (id)
  "Return the edit history of toot with ID."
  (let* ((url (mastodon-http--api (format "statuses/%s/history" id))))
    (mastodon-http--get-json url)))

(defun mastodon-toot--view-toot-edits ()
  "View editing history of the toot at point in a popup buffer."
  (interactive)
  (let ((id (mastodon-tl--property 'base-toot-id))
        (history (mastodon-tl--property 'edit-history)))
    (with-current-buffer (get-buffer-create "*mastodon-toot-edits*")
      (let ((inhibit-read-only t))
        (special-mode)
        (erase-buffer)
        (let ((count 1))
          (mapc (lambda (x)
                  (insert (propertize (if (= count 1)
                                          (format "%s [original]:\n" count)
                                        (format "%s:\n" count))
                                      'face font-lock-comment-face)
                          (mastodon-toot--insert-toot-iter x)
                          "\n")
                  (cl-incf count))
                history))
        (switch-to-buffer-other-window (current-buffer))
        (setq-local header-line-format
                    (propertize
                     (format "Edits to toot by %s:"
                             (alist-get 'username
                                        (alist-get 'account (car history))))
                     'face font-lock-comment-face))
        (mastodon-tl--set-buffer-spec (buffer-name (current-buffer))
                                      (format "statuses/%s/history" id)
                                      nil)))))

(defun mastodon-toot--insert-toot-iter (it)
  "Insert iteration IT of toot."
  (let ((content (alist-get 'content it)))
    ;; (account (alist-get 'account it))
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
  (mapconcat (lambda (mention) mention)
	     (remove "" (mapcar #'mastodon-toot--process-local mentions))
             " "))

(defun mastodon-toot--process-local (acct)
  "Add domain to local ACCT and replace the curent user name with \"\".
Mastodon requires the full @user@domain, even in the case of local accts.
eg. \"user\" -> \"@user@local.social\" (when local.social is the domain of the
mastodon-instance-url).
eg. \"yourusername\" -> \"\"
eg. \"feduser@fed.social\" -> \"@feduser@fed.social\"."
  (cond ((string-match-p "@" acct) (concat "@" acct)) ; federated acct
        ((string= (mastodon-auth--user-acct) acct) "") ; your acct
        (t (concat "@" acct "@" ; local acct
                   (cadr (split-string mastodon-instance-url "/" t))))))

(defun mastodon-toot--mentions (status)
  "Extract mentions (not the reply-to author or booster) from STATUS.
The mentioned users look like this:
Local user (including the logged in): `username`.
Federated user: `username@host.co`."
  (interactive)
  (let* ((boosted (mastodon-tl--field 'reblog status))
         (mentions
          (if boosted
	      (alist-get 'mentions (alist-get 'reblog status))
	    (alist-get 'mentions status))))
    ;; reverse does not work on vectors in 24.5
    (mastodon-tl--map-alist 'acct (reverse mentions))))

(defun mastodon-toot--get-bounds (regex)
  "Get bounds of tag or handle before point using REGEX."
  ;; needed because # and @ are not part of any existing thing at point
  (save-match-data
    (save-excursion
      ;; match full handle inc. domain, or tag including #
      ;; (see the regexes for subexp 2)
      (when (re-search-backward regex
                                (save-excursion
                                  (forward-whitespace -1)
                                  (point))
                                :no-error)
        (cons (match-beginning 2)
              (match-end 2))))))

(defun mastodon-toot--fetch-completion-candidates (start end &optional tags)
  "Search for a completion prefix from buffer positions START to END.
Return a list of candidates.
If TAGS, we search for tags, else we search for handles."
  ;; we can't save the first two-letter search then only filter the
  ;; resulting list, as max results returned is 40.
  (setq mastodon-toot-completions
        (if tags
            (let ((tags-list (mastodon-search--search-tags-query
                              (buffer-substring-no-properties start end))))
              (cl-loop for tag in tags-list
                       collect (cons (concat "#" (car tag))
                                     (cdr tag))))
          (mastodon-search--search-accounts-query
           (buffer-substring-no-properties start end)))))

(defun mastodon-toot--mentions-capf ()
  "Build a mentions completion backend for `completion-at-point-functions'."
  (let* ((bounds
          (mastodon-toot--get-bounds mastodon-toot-handle-regex))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list start
            end
            ;; only search when necessary:
            (completion-table-dynamic
             (lambda (_)
               ;; Interruptible candidate computation
               ;; suggestion from minad (d mendler), thanks!
               (let ((result
                      (while-no-input
                        (mastodon-toot--fetch-completion-candidates start end))))
                 (and (consp result) result))))
            :exclusive 'no
            :annotation-function
            (lambda (candidate)
              (concat " "
                      (mastodon-toot--mentions-annotation-fun candidate)))))))

(defun mastodon-toot--tags-capf ()
  "Build a tags completion backend for `completion-at-point-functions'."
  (let* ((bounds
          (mastodon-toot--get-bounds mastodon-toot-tag-regex))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list start
            end
            ;; only search when necessary:
            (completion-table-dynamic
             (lambda (_)
               ;; Interruptible candidate computation
               ;; suggestion from minad (d mendler), thanks!
               (let ((result
                      (while-no-input
                        (mastodon-toot--fetch-completion-candidates start end :tags))))
                 (and (consp result) result))))
            :exclusive 'no
            :annotation-function
            (lambda (candidate)
              (concat " "
                      (mastodon-toot--tags-annotation-fun candidate)))))))

(defun mastodon-toot--mentions-annotation-fun (candidate)
  "Given a handle completion CANDIDATE, return its annotation string, a username."
  (caddr (assoc candidate mastodon-toot-completions)))

(defun mastodon-toot--tags-annotation-fun (candidate)
  "Given a tag string CANDIDATE, return an annotation, the tag's URL."
  ;; FIXME check the list returned here? should be cadr
  ;;or make it an alist and use cdr
  (cadr (assoc candidate mastodon-toot-completions)))

(defun mastodon-toot--reply ()
  "Reply to toot at `point'.
Customize `mastodon-toot-display-orig-in-reply-buffer' to display
text of the toot being replied to in the compose buffer."
  (interactive)
  (mastodon-tl--do-if-toot-strict
   (let* ((toot (mastodon-tl--property 'toot-json))
          ;; no-move arg for base toot, because if it doesn't have one, it is
          ;; fetched from next toot!
          (base-toot (mastodon-tl--property 'base-toot :no-move)) ; for new notifs handling
          (id (mastodon-tl--as-string (mastodon-tl--field 'id (or base-toot toot))))
          (account (mastodon-tl--field 'account toot))
          (user (alist-get 'acct account))
          (mentions (mastodon-toot--mentions (or base-toot toot)))
          (boosted (mastodon-tl--field 'reblog (or base-toot toot)))
          (booster (when boosted
                     (alist-get 'acct
                                (alist-get 'account toot)))))
     (mastodon-toot
      (when user
        (if booster
            (if (and (not (equal user booster))
                     (not (member booster mentions)))
                ;; different booster, user and mentions:
		(mastodon-toot--mentions-to-string (append (list user booster) mentions nil))
              ;; booster is either user or in mentions:
              (if (not (member user mentions))
                  ;; user not already in mentions:
		  (mastodon-toot--mentions-to-string (append (list user) mentions nil))
                ;; user already in mentions:
                (mastodon-toot--mentions-to-string (copy-sequence mentions))))
          ;; ELSE no booster:
          (if (not (member user mentions))
              ;; user not in mentions:
	      (mastodon-toot--mentions-to-string (append (list user) mentions nil))
            ;; user in mentions already:
            (mastodon-toot--mentions-to-string (copy-sequence mentions)))))
      id
      (or base-toot toot)))))

(defun mastodon-toot--toggle-warning ()
  "Toggle `mastodon-toot--content-warning'."
  (interactive)
  (setq mastodon-toot--content-warning
        (not mastodon-toot--content-warning))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--toggle-nsfw ()
  "Toggle `mastodon-toot--content-nsfw'."
  (interactive)
  (setq mastodon-toot--content-nsfw
        (not mastodon-toot--content-nsfw))
  (message "NSFW flag is now %s" (if mastodon-toot--content-nsfw "on" "off"))
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--change-visibility ()
  "Change the current visibility to the next valid value."
  (interactive)
  (if (mastodon-tl--buffer-type-eq 'edit-toot)
      (message "You can't change visibility when editing toots.")
    (setq mastodon-toot--visibility
          (cond ((string= mastodon-toot--visibility "public")
                 "unlisted")
                ((string= mastodon-toot--visibility "unlisted")
                 "private")
                ((string= mastodon-toot--visibility "private")
                 "direct")
                (t
                 "public")))
    (mastodon-toot--update-status-fields)))

(defun mastodon-toot--clear-all-attachments ()
  "Remove all attachments from a toot draft."
  (interactive)
  (setq mastodon-toot--media-attachments nil)
  (setq mastodon-toot--media-attachment-ids nil)
  (mastodon-toot--refresh-attachments-display)
  (mastodon-toot--update-status-fields))

(defun mastodon-toot--attach-media (file content-type description)
  "Prompt for an attachment FILE of CONTENT-TYPE with DESCRIPTION.
A preview is displayed in the new toot buffer, and the file
is uploaded asynchronously using `mastodon-toot--upload-attached-media'.
File is actually attached to the toot upon posting."
  (interactive "fFilename: \nsContent type: \nsDescription: ")
  (when (>= (length mastodon-toot--media-attachments) 4)
    ;; Only a max. of 4 attachments are allowed, so pop the oldest one.
    (pop mastodon-toot--media-attachments))
  (if (file-directory-p file)
      (message "Looks like you chose a directory not a file.")
    (setq mastodon-toot--media-attachments
          (nconc mastodon-toot--media-attachments
                 `(((:contents . ,(mastodon-http--read-file-as-string file))
                    (:content-type . ,content-type)
                    (:description . ,description)
                    (:filename . ,file)))))
    (mastodon-toot--refresh-attachments-display)
    ;; upload only most recent attachment:
    (mastodon-toot--upload-attached-media (car (last mastodon-toot--media-attachments)))))

(defun mastodon-toot--upload-attached-media (attachment)
  "Upload a single ATTACHMENT using `mastodon-http--post-media-attachment'.
The item's id is added to `mastodon-toot--media-attachment-ids',
which is used to attach it to a toot when posting."
  (let* ((filename (expand-file-name
                    (alist-get :filename attachment)))
         (caption (alist-get :description attachment))
         (url (concat mastodon-instance-url "/api/v2/media")))
    (message "Uploading %s..." (file-name-nondirectory filename))
    (mastodon-http--post-media-attachment url filename caption)))

(defun mastodon-toot--refresh-attachments-display ()
  "Update the display attachment previews in toot draft buffer."
  (let ((inhibit-read-only t)
        (attachments-region (mastodon-tl--find-property-range
                             'toot-attachments (point-min)))
        (display-specs (mastodon-toot--format-attachments)))
    (dotimes (i (- (cdr attachments-region) (car attachments-region)))
      (add-text-properties (+ (car attachments-region) i)
                           (+ (car attachments-region) i 1)
                           (list 'display (or (nth i display-specs) ""))))))

(defun mastodon-toot--format-attachments ()
  "Format the attachment previews for display in toot draft buffer."
  (or (let ((counter 0)
            (image-options (when (or (image-type-available-p 'imagemagick)
                                     (image-transforms-p))
                             `(:height ,mastodon-toot--attachment-height))))
        (mapcan (lambda (attachment)
                  (let* ((data (alist-get :contents attachment))
                         (image (apply #'create-image data
                                       (if (version< emacs-version "27.1")
                                           (when image-options 'imagemagick)
                                         nil) ; inbuilt scaling in 27.1
                                       t image-options))
                         (type (alist-get :content-type attachment))
                         (description (alist-get :description attachment)))
                    (setq counter (1+ counter))
                    (list (format "\n    %d: " counter)
                          image
                          (format " \"%s\" (%s)" description type))))
                mastodon-toot--media-attachments))
      (list "None")))

(defun mastodon-toot--fetch-max-poll-options (instance)
  "Return the maximum number of poll options from INSTANCE, which is json."
  (mastodon-toot--fetch-poll-field 'max_options instance))

(defun mastodon-toot--fetch-max-poll-option-chars (instance)
  "Return the maximum number of characters a poll option may have.
INSTANCE is JSON."
  (if (alist-get 'pleroma instance)
      (mastodon-toot--fetch-poll-field 'max_option_chars instance)
    (or (mastodon-toot--fetch-poll-field 'max_characters_per_option instance)
        50))) ; masto default

(defun mastodon-toot--fetch-poll-field (field instance)
  "Return FIELD from the poll settings from INSTANCE, which is json."
  (let* ((polls (if (alist-get 'pleroma instance)
                    (alist-get 'poll_limits instance)
                  (alist-get 'polls
                             (alist-get 'configuration instance)))))
    (alist-get field polls)))

(defun mastodon-toot--read-poll-options-count (max)
  "Read the user's choice of the number of options the poll should have.
MAX is the maximum number set by their instance."
  (let ((number (read-number
                 (format "Number of options [2-%s]: " max) 2)))
    (if (> number max)
        (error "You need to choose a number between 2 and %s" max)
      number)))

(defun mastodon-toot--create-poll ()
  "Prompt for new poll options and return as a list."
  (interactive)
  ;; re length, API docs show a poll 9 options.
  (let* ((instance (mastodon-http--get-json (mastodon-http--api "instance")))
         (max-options (mastodon-toot--fetch-max-poll-options instance))
         (count (mastodon-toot--read-poll-options-count max-options))
         (length (mastodon-toot--fetch-max-poll-option-chars instance))
         (multiple-p (y-or-n-p "Multiple choice? "))
         (options (mastodon-toot--read-poll-options count length))
         (hide-totals (y-or-n-p "Hide votes until poll ends? "))
         (expiry (mastodon-toot--read-poll-expiry)))
    (setq mastodon-toot-poll
          `(:options ,options :length ,length :multi ,multiple-p
                     :hide ,hide-totals :expiry ,expiry))
    (message "poll created!")))

(defun mastodon-toot--read-poll-options (count length)
  "Read a list of options for poll with COUNT options.
LENGTH is the maximum character length allowed for a poll option."
  (let* ((choices
          (cl-loop for x from 1 to count
                   collect (read-string
                            (format "Poll option [%s/%s] [max %s chars]: "
                                    x count length))))
         (longest (cl-reduce #'max (mapcar #'length choices))))
    (if (> longest length)
        (progn
          (message "looks like you went over the max length. Try again.")
          (sleep-for 2)
          (mastodon-toot--read-poll-options count length))
      choices)))

(defun mastodon-toot--read-poll-expiry ()
  "Prompt for a poll expiry time."
  ;; API requires this in seconds
  (let* ((options (mastodon-toot--poll-expiry-options-alist))
         (response (completing-read "poll ends in [or enter seconds]: "
                                    options nil 'confirm)))
    (or (alist-get response options nil nil #'equal)
        (if (< (string-to-number response) 600)
            "600" ;; min 5 mins
          response))))

(defun mastodon-toot--poll-expiry-options-alist ()
  "Return an alist of seconds options."
  `(("5 minutes" . ,(number-to-string (* 60 5)))
    ("30 minutes" . ,(number-to-string (* 60 30)))
    ("1 hour" . ,(number-to-string (* 60 60)))
    ("6 hours" . ,(number-to-string (* 60 60 6)))
    ("1 day" . ,(number-to-string (* 60 60 24)))
    ("3 days" . ,(number-to-string (* 60 60 24 3)))
    ("7 days" . ,(number-to-string (* 60 60 24 7)))
    ("14 days" . ,(number-to-string (* 60 60 24 14)))
    ("30 days" . ,(number-to-string (* 60 60 24 30)))))

(defun mastodon-toot--set-toot-language ()
  "Prompt for a language and set `mastodon-toot--language'.
Return its two letter ISO 639 1 code."
  (interactive)
  (let* ((choice (completing-read "Language for this toot: "
                                  mastodon-iso-639-1)))
    (setq mastodon-toot--language
          (alist-get choice mastodon-iso-639-1 nil nil 'equal))
    (message "Language set to %s" choice)
    (mastodon-toot--update-status-fields)))

(defun mastodon-toot--schedule-toot (&optional reschedule)
  "Read a date (+ time) in the minibuffer and schedule the current toot.
With RESCHEDULE, reschedule the scheduled toot at point without editing."
  ;; original idea by christian tietze, thanks!
  ;; https://codeberg.org/martianh/mastodon.el/issues/285
  (interactive)
  (cond ((mastodon-tl--buffer-type-eq 'edit-toot)
         (message "You can't schedule toots you're editing."))
        ((not (or (mastodon-tl--buffer-type-eq 'new-toot)
                  (mastodon-tl--buffer-type-eq 'scheduled-statuses)))
         (message "You can only schedule toots from the compose toot buffer or the scheduled toots view."))
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
                 (message (format "Toot scheduled for %s." msg-str)))
             (let* ((args (when reschedule `(("scheduled_at" . ,iso8601-str))))
                    (url (when reschedule (mastodon-http--api
                                           (format "scheduled_statuses/%s" id))))
                    (response (mastodon-http--put url args)))
               (mastodon-http--triage response
                                      (lambda ()
                                        ;; reschedule means we are in scheduled toots view:
                                        (mastodon-views--view-scheduled-toots)
                                        (message
                                         (format "Toot rescheduled for %s." msg-str))))))))))

(defun mastodon-toot--iso-to-human (ts)
  "Format an ISO8601 timestamp TS to be more human-readable."
  (let* ((decoded (iso8601-parse ts))
         (encoded (encode-time decoded)))
    (format-time-string "%d-%m-%y, %H:%M[%z]" encoded)))

(defun mastodon-toot--iso-to-org (ts)
  "Convert ISO8601 timestamp TS to something `org-read-date' can handle."
  (when ts (let* ((decoded (iso8601-parse ts)))
             (encode-time decoded))))

;; we'll need to revisit this if the binds get
;; more diverse than two-chord bindings
(defun mastodon-toot--get-mode-kbinds ()
  "Get a list of the keybindings in the mastodon-toot-mode."
  (let* ((binds (copy-tree mastodon-toot-mode-map))
         (prefix (car (cadr binds)))
         (bindings (remove nil (mapcar (lambda (i) (if (listp i) i))
                                       (cadr binds)))))
    (mapcar (lambda (b)
              (setf (car b) (vector prefix (car b)))
              b)
            bindings)))

(defun mastodon-toot--format-kbind-command (cmd)
  "Format CMD to be more readable.
e.g. mastodon-toot--send -> Send."
  (let* ((str (symbol-name cmd))
         (re "--\\(.*\\)$")
         (str2 (save-match-data
                 (string-match re str)
                 (match-string 1 str))))
    (capitalize (replace-regexp-in-string "-" " " str2))))

(defun mastodon-toot--format-kbind (kbind)
  "Format a single keybinding, KBIND, for display in documentation."
  (let ((key (help-key-description (car kbind) nil))
        (command (mastodon-toot--format-kbind-command (cdr kbind))))
    (format "    %s - %s" key command)))

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

(defun mastodon-toot--formatted-kbinds-longest (kbinds-list)
  "Return the length of the longest item in KBINDS-LIST."
  (let ((lengths (mapcar (lambda (x)
                           (length x))
                         kbinds-list)))
    (car (sort lengths #'>))))

(defun mastodon-toot--make-mode-docs ()
  "Create formatted documentation text for the mastodon-toot-mode."
  (let* ((kbinds (mastodon-toot--get-mode-kbinds))
         (longest-kbind
          (mastodon-toot--formatted-kbinds-longest
           (mastodon-toot--format-kbinds kbinds))))
    (concat
     " Compose a new toot here. The following keybindings are available:"
     (mapconcat #'identity
                (mastodon-toot--formatted-kbinds-pairs
                 (mastodon-toot--format-kbinds kbinds)
                 longest-kbind)
                nil))))

(defun mastodon-toot--display-docs-and-status-fields (&optional reply-text)
  "Insert propertized text with documentation about `mastodon-toot-mode'.
Also includes and the status fields which will get updated based
on the status of NSFW, content warning flags, media attachments, etc.
REPLY-TEXT is the text of the toot being replied to."
  (let ((divider
         "|=================================================================|"))
    (insert
     (propertize
      (concat
       (mastodon-toot--make-mode-docs) "\n"
       divider "\n"
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
       (propertize "NSFW"
                   'toot-post-nsfw-flag t)
       "\n"
       " Attachments: "
       (propertize "None                  "
                   'toot-attachments t)
       "\n")
      'face font-lock-comment-face
      'read-only "Edit your message below."
      'toot-post-header t)
     (if reply-text
         (propertize (truncate-string-to-width
                      (mastodon-tl--render-text reply-text)
                      mastodon-toot-orig-in-reply-length)
                     'read-only "Edit your message below."
                     'toot-post-header t
                     'face '(variable-pitch :foreground "#7c6f64"))
       "")
     (propertize
      (concat divider "\n")
      'rear-nonsticky t
      'face font-lock-comment-face
      'read-only "Edit your message below."
      'toot-post-header t))))

(defun mastodon-toot--most-restrictive-visibility (reply-visibility)
  "Return REPLY-VISIBILITY or default visibility, whichever is more restrictive.
The default is given by `mastodon-toot--default-reply-visibility'."
  (unless (null reply-visibility)
    (let ((less-restrictive (member (intern mastodon-toot--default-reply-visibility)
				    mastodon-toot-visibility-list)))
      (if (member (intern reply-visibility) less-restrictive)
	  mastodon-toot--default-reply-visibility reply-visibility))))

(defun mastodon-toot--setup-as-reply (reply-to-user reply-to-id reply-json)
  "If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set `mastodon-toot--reply-to-id'.
REPLY-JSON is the full JSON of the toot being replied to."
  (let ((reply-visibility
	 (mastodon-toot--most-restrictive-visibility
	  (alist-get 'visibility reply-json)))
        (reply-cw (alist-get 'spoiler_text reply-json)))
    (when reply-to-user
      (when (> (length reply-to-user) 0) ; self is "" unforch
        (insert (format "%s " reply-to-user)))
      (setq mastodon-toot--reply-to-id reply-to-id)
      (unless (equal mastodon-toot--visibility reply-visibility)
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
           (visibility-region (mastodon-tl--find-property-range
                               'toot-post-visibility (point-min)))
           (nsfw-region (mastodon-tl--find-property-range 'toot-post-nsfw-flag
                                                          (point-min)))
           (cw-region (mastodon-tl--find-property-range 'toot-post-cw-flag
                                                        (point-min)))
           (lang-region (mastodon-tl--find-property-range 'toot-post-language
                                                          (point-min)))
           (scheduled-region (mastodon-tl--find-property-range 'toot-post-scheduled
                                                               (point-min)))
           (toot-string (buffer-substring-no-properties (cdr header-region)
                                                        (point-max))))
      (add-text-properties (car count-region) (cdr count-region)
                           (list 'display
                                 (format "%s/%s chars"
                                         (mastodon-toot--count-toot-chars toot-string)
                                         (number-to-string mastodon-toot--max-toot-chars))))
      (add-text-properties (car visibility-region) (cdr visibility-region)
                           (list 'display
                                 (format "%s"
                                         (if (equal
                                              mastodon-toot--visibility
                                              "private")
                                             "followers-only"
                                           mastodon-toot--visibility))))
      (add-text-properties (car lang-region) (cdr lang-region)
                           (list 'display
                                 (if mastodon-toot--language
                                     (format "Lang: %s ⋅"
                                             mastodon-toot--language)
                                   "")))
      (add-text-properties (car scheduled-region) (cdr scheduled-region)
                           (list 'display
                                 (if mastodon-toot--scheduled-for
                                     (format "Scheduled: %s ⋅"
                                             (mastodon-toot--iso-to-human
                                              mastodon-toot--scheduled-for))
                                   "")))
      (add-text-properties (car nsfw-region) (cdr nsfw-region)
                           (list 'display (if mastodon-toot--content-nsfw
                                              (if mastodon-toot--media-attachments
                                                  "NSFW" "NSFW (for attachments only)")
                                            "")
                                 'face 'mastodon-cw-face))
      (add-text-properties (car cw-region) (cdr cw-region)
                           (list 'invisible (not mastodon-toot--content-warning)
                                 'face 'mastodon-cw-face)))))

(defun mastodon-toot--count-toot-chars (toot-string &optional cw)
  "Count the characters in TOOT-STRING.
URLs always = 23, and domain names of handles are not counted.
This is how mastodon does it."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert toot-string)
    (goto-char (point-min))
    ;; handle URLs
    (while (search-forward-regexp "\\w+://[^ \n]*" nil t) ; URL
      (replace-match "xxxxxxxxxxxxxxxxxxxxxxx")) ; 23 x's
    ;; handle @handles
    (goto-char (point-min))
    (while (search-forward-regexp (concat "\\(?2:@[^ @\n]+\\)" ; a handle only
                                          "\\(@[^ \n]+\\)?" ; with poss domain
                                          "\\b")
                                  nil t)
      (replace-match (match-string 2))) ; replace with handle only
    (+ (length cw)
       (length (buffer-substring (point-min) (point-max))))))

(defun mastodon-toot--save-toot-text (&rest _args)
  "Save the current toot text in `mastodon-toot-current-toot-text'.
Added to `after-change-functions' in new toot buffers."
  (let ((text (mastodon-toot--remove-docs)))
    (unless (string-empty-p text)
      (setq mastodon-toot-current-toot-text text))))

(defun mastodon-toot--open-draft-toot ()
  "Prompt for a draft and compose a toot with it."
  (interactive)
  (if mastodon-toot-draft-toots-list
      (let ((text (completing-read "Select draft toot: "
                                   mastodon-toot-draft-toots-list
                                   nil t)))
        (if (mastodon-toot--compose-buffer-p)
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
              (insert text))
          (mastodon-toot--compose-buffer nil nil nil text)))
    (unless (mastodon-toot--compose-buffer-p)
      (mastodon-toot--compose-buffer))
    (message "No drafts available.")))

(defun mastodon-toot--delete-draft-toot ()
  "Prompt for a draft toot and delete it."
  (interactive)
  (if mastodon-toot-draft-toots-list
      (let ((draft (completing-read "Select draft to delete: "
                                    mastodon-toot-draft-toots-list
                                    nil t)))
        (setq mastodon-toot-draft-toots-list
              (cl-delete draft mastodon-toot-draft-toots-list
                         :test 'equal))
        (message "Draft deleted!"))
    (message "No drafts to delete.")))

(defun mastodon-toot--delete-all-drafts ()
  "Delete all drafts."
  (interactive)
  (setq mastodon-toot-draft-toots-list nil)
  (message "All drafts deleted!"))

(defun mastodon-toot--propertize-tags-and-handles (&rest _args)
  "Propertize tags and handles in toot compose buffer.
Added to `after-change-functions'."
  (when (mastodon-toot--compose-buffer-p)
    (let ((header-region
           (mastodon-tl--find-property-range 'toot-post-header
                                             (point-min))))
      ;; cull any prev props:
      ;; stops all text after a handle or mention being propertized:
      (set-text-properties (cdr header-region) (point-max) nil)
      ;; TODO: confirm allowed hashtag/handle characters:
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

;; NB: now that we have toot drafts, to ensure offline composing remains
;; possible, avoid any direct requests here:
(defun mastodon-toot--compose-buffer (&optional reply-to-user
                                                reply-to-id reply-json initial-text
                                                edit)
  "Create a new buffer to capture text for a new toot.
If REPLY-TO-USER is provided, inject their handle into the message.
If REPLY-TO-ID is provided, set the `mastodon-toot--reply-to-id' var.
REPLY-JSON is the full JSON of the toot being replied to.
INITIAL-TEXT is used by `mastodon-toot-insert-draft-toot' to add
a draft into the buffer.
EDIT means we are editing an existing toot, not composing a new one."
  (let* ((buffer-name (if edit "*edit toot*" "*new toot*"))
         (buffer-exists (get-buffer buffer-name))
         (buffer (or buffer-exists (get-buffer-create buffer-name)))
         (inhibit-read-only t)
         (reply-text (alist-get 'content
                                (or (alist-get 'reblog reply-json)
                                    reply-json)))
         (previous-window-config (list (current-window-configuration)
                                       (point-marker))))
    (switch-to-buffer-other-window buffer)
    (text-mode)
    (mastodon-toot-mode t)
    (setq mastodon-toot--visibility
          (or (plist-get mastodon-profile-account-settings 'privacy)
              ;; use toot visibility setting from the server:
              (mastodon-profile--get-source-pref 'privacy)
              "public")) ; fallback
    (unless buffer-exists
      (mastodon-toot--display-docs-and-status-fields
       (when mastodon-toot-display-orig-in-reply-buffer
         reply-text))
      ;; `reply-to-user' (alone) is also used by `mastodon-tl--dm-user', so
      ;; perhaps we should not always call --setup-as-reply, or make its
      ;; workings conditional on reply-to-id. currently it only checks for
      ;; reply-to-user.
      (mastodon-toot--setup-as-reply reply-to-user reply-to-id reply-json))
    (unless mastodon-toot--max-toot-chars
      ;; no need to fetch from `mastodon-profile-account-settings' as
      ;; `mastodon-toot--max-toot-chars' is set when we set it
      (mastodon-toot--get-max-toot-chars))
    ;; set up completion:
    (when mastodon-toot--enable-completion
      (set ; (setq-local
       (make-local-variable 'completion-at-point-functions)
       (add-to-list
        'completion-at-point-functions
        #'mastodon-toot--mentions-capf))
      (add-to-list
       'completion-at-point-functions
       #'mastodon-toot--tags-capf)
      ;; company
      (when (and mastodon-toot--use-company-for-completion
                 (require 'company nil :no-error))
        (declare-function company-mode-on "company")
        (set (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-capf))
        (company-mode-on)))
    ;; after-change:
    (make-local-variable 'after-change-functions)
    (push #'mastodon-toot--update-status-fields after-change-functions)
    (mastodon-toot--refresh-attachments-display)
    (mastodon-toot--update-status-fields)
    ;; draft toot text saving:
    (setq mastodon-toot-current-toot-text nil)
    (push #'mastodon-toot--save-toot-text after-change-functions)
    (push #'mastodon-toot--propertize-tags-and-handles after-change-functions)
    ;; if we set this before changing modes, it gets nuked:
    (setq mastodon-toot-previous-window-config previous-window-config)
    (when initial-text
      (insert initial-text))))

;;;###autoload
(add-hook 'mastodon-toot-mode-hook #'mastodon-profile--fetch-server-account-settings-maybe)

;; disable auto-fill-mode:
(add-hook 'mastodon-toot-mode-hook
          (lambda ()
            (auto-fill-mode -1)))

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
