;;; mastodon-tl.el --- Timeline functions for mastodon.el  -*- lexical-binding: t -*-

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

;; mastodon-tl.el provides timeline functions.

;;; Code:

(require 'shr)
(require 'ts)
(require 'thingatpt) ; for word-at-point
(require 'time-date)
(require 'cl-lib)
(require 'mastodon-iso)
(require 'mpv nil :no-error)

(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-notifications-get "mastodon")
(autoload 'mastodon-url-lookup "mastodon")
(autoload 'mastodon-auth--get-account-id "mastodon-auth")
(autoload 'mastodon-auth--get-account-name "mastodon-auth")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--build-array-params-alist "mastodon-http")
(autoload 'mastodon-http--build-params-string "mastodon-http")
(autoload 'mastodon-http--delete "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--get-json-async "mastodon-http")
(autoload 'mastodon-http--get-response-async "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--process-json "mastodon-http")
(autoload 'mastodon-http--put "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-media--get-avatar-rendering "mastodon-media")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-notifications--filter-types-list "mastodon-notifications")
(autoload 'mastodon-notifications--get-mentions "mastodon-notifications")
(autoload 'mastodon-profile--account-field "mastodon-profile")
(autoload 'mastodon-profile--account-from-id "mastodon-profile")
(autoload 'mastodon-profile--extract-users-handles "mastodon-profile")
(autoload 'mastodon-profile--get-preferences-pref "mastodon-profile")
(autoload 'mastodon-profile--get-toot-author "mastodon-profile")
(autoload 'mastodon-profile--lookup-account-in-status "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile--my-profile "mastodon-profile")
(autoload 'mastodon-profile--open-statuses-no-reblogs "mastodon-profile")
(autoload 'mastodon-profile--profile-json "mastodon-profile")
(autoload 'mastodon-profile--search-account-by-handle "mastodon-profile")
(autoload 'mastodon-profile--toot-json "mastodon-profile")
(autoload 'mastodon-profile--view-author-profile "mastodon-profile")
(autoload 'mastodon-profile-mode "mastodon-profile")
(autoload 'mastodon-search--get-user-info "mastodon-search")
(autoload 'mastodon-search--insert-users-propertized "mastodon-search")
(autoload 'mastodon-search--propertize-user "mastodon-search")
(autoload 'mastodon-toot--compose-buffer "mastodon-toot")
(autoload 'mastodon-toot--delete-toot "mastodon-toot")
(autoload 'mastodon-toot--get-toot-edits "mastodon-toot")
(autoload 'mastodon-toot--iso-to-human "mastodon-toot")
(autoload 'mastodon-toot--schedule-toot "mastodon-toot")
(autoload 'mastodon-toot--set-toot-properties "mastodon-toot")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")

(defvar mastodon-toot--visibility)
(defvar mastodon-toot-mode)
(defvar mastodon-active-user)

(when (require 'mpv nil :no-error)
  (declare-function mpv-start "mpv"))
(defvar mastodon-instance-url)
(defvar mastodon-toot-timestamp-format)
(defvar shr-use-fonts)  ;; declare it since Emacs24 didn't have this
(defvar mastodon-mode-map)


;;; CUSTOMIZES

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defcustom mastodon-tl--enable-relative-timestamps t
  "Whether to show relative (to the current time) timestamps.
This will require periodic updates of a timeline buffer to
keep the timestamps current as time progresses."
  :type '(boolean :tag "Enable relative timestamps and background updater task"))

(defcustom mastodon-tl--enable-proportional-fonts nil
  "Nonnil to enable using proportional fonts when rendering HTML.
By default fixed width fonts are used."
  :type '(boolean :tag "Enable using proportional rather than fixed \
width fonts when rendering HTML text"))

(defcustom mastodon-tl--display-caption-not-url-when-no-media t
  "Display an image's caption rather than URL.
Only has an effect when `mastodon-tl--display-media-p' is set to
nil."
  :type 'boolean)

(defcustom mastodon-tl--show-avatars nil
  "Whether to enable display of user avatars in timelines."
  :type '(boolean :tag "Whether to display user avatars in timelines"))

(defcustom mastodon-tl--show-stats t
  "Whether to show toot stats (faves, boosts, replies counts)."
  :type 'bool)

(defcustom mastodon-tl--symbols
  '((reply     . ("💬" . "R"))
    (boost     . ("🔁" . "B"))
    (favourite . ("⭐" . "F"))
    (bookmark  . ("🔖" . "K"))
    (media     . ("📹" . "[media]"))
    (verified  . ("" . "V"))
    (locked    . ("🔒" . "[locked]"))
    (private   . ("🔒" . "[followers]"))
    (direct    . ("✉" . "[direct]"))
    (edited    . ("✍" . "[edited]")))
  "A set of symbols (and fallback strings) to be used in timeline.
If a symbol does not look right (tofu), it means your
font settings do not support it."
  :type '(alist :key-type symbol :value-type string))

(defcustom mastodon-tl-position-after-update nil
  "Defines where `point' should be located after a timeline update.
Valid values are:
- nil            Top/bottom depending on timeline type
- keep-point     Keep original position of point
- last-old-toot  The last toot before the new ones"
  :type '(choice (const :tag "Top/bottom depending on timeline type" nil)
                 (const :tag "Keep original position of point" keep-point)
                 (const :tag "The last toot before the new ones" last-old-toot)))

(defcustom mastodon-tl--timeline-posts-count "20"
  "Number of posts to display when loading a timeline.
Must be an integer between 20 and 40 inclusive."
  :type '(string))

(defcustom mastodon-tl--hide-replies nil
  "Whether to hide replies from the timelines.
Note that you can hide replies on a one-off basis by loading a
timeline with a simple prefix argument, `C-u'."
  :type '(boolean :tag "Whether to hide replies from the timelines."))


;;; VARIABLES

(defvar-local mastodon-tl--buffer-spec nil
  "A unique identifier and functions for each Mastodon buffer.")

(defvar-local mastodon-tl--update-point nil
  "When updating a mastodon buffer this is where new toots will be inserted.
If nil `(point-min)' is used instead.")

(defvar-local mastodon-tl--after-update-marker nil
  "Marker defining the position of point after the update is done.")

(defvar mastodon-tl--display-media-p t
  "A boolean value stating whether to show media in timelines.")

(defvar-local mastodon-tl--timestamp-next-update nil
  "The timestamp when the buffer should next be scanned to update the timestamps.")

(defvar-local mastodon-tl--timestamp-update-timer nil
  "The timer that, when set will scan the buffer to update the timestamps.")

(defvar mastodon-tl--horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

;;; KEYMAPS

(defvar mastodon-tl--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'mastodon-tl--do-link-action-at-point)
    (define-key map [mouse-2] 'mastodon-tl--do-link-action)
    (define-key map [follow-link] 'mouse-face)
    map)
  "The keymap for link-like things in buffer (except for shr.el generate links).
This will make the region of text act like like a link with mouse
highlighting, mouse click action tabbing to next/previous link
etc.")

(defvar mastodon-tl--shr-map-replacement
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map shr-map)
    ;; Replace the move to next/previous link bindings with our
    ;; version that knows about more types of links.
    (define-key map [remap shr-next-link] #'mastodon-tl--next-tab-item)
    (define-key map [remap shr-previous-link] #'mastodon-tl--previous-tab-item)
    ;; keep new my-profile binding; shr 'O' doesn't work here anyway
    (define-key map (kbd "O") #'mastodon-profile--my-profile)
    (define-key map [remap shr-browse-url] #'mastodon-url-lookup)
    map)
  "The keymap to be set for shr.el generated links that are not images.
We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defvar mastodon-tl--shr-image-map-replacement
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (if (boundp 'shr-image-map)
                               shr-image-map
                             shr-map))
    ;; Replace the move to next/previous link bindings with our
    ;; version that knows about more types of links.
    (define-key map [remap shr-next-link] #'mastodon-tl--next-tab-item)
    (define-key map [remap shr-previous-link] #'mastodon-tl--previous-tab-item)
    ;; browse-url loads the preview only, we want browse-image
    ;; on RET to browse full sized image URL
    (define-key map [remap shr-browse-url] #'shr-browse-image)
    ;; remove shr's u binding, as it the maybe-probe-and-copy-url
    ;; is already bound to w also
    (define-key map (kbd "u") #'mastodon-tl--update)
    ;; keep new my-profile binding; shr 'O' doesn't work here anyway
    (define-key map (kbd "O") #'mastodon-profile--my-profile)
    (define-key map (kbd "<C-return>") #'mastodon-tl--mpv-play-video-at-point)
    map)
  "The keymap to be set for shr.el generated image links.
We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defvar mastodon-tl--byline-link-keymap
  (when (require 'mpv nil :no-error)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-return>") #'mastodon-tl--mpv-play-video-from-byline)
      (define-key map (kbd "RET") #'mastodon-profile--get-toot-author)
      map))
  "The keymap to be set for the author byline.
It is active where point is placed by `mastodon-tl--goto-next-toot.'")


;;; NAV

(defun mastodon-tl--next-tab-item ()
  "Move to the next interesting item.
This could be the next toot, link, or image; whichever comes first.
Don't move if nothing else to move to is found, i.e. near the end of the buffer.
This also skips tab items in invisible text, i.e. hidden spoiler text."
  (interactive)
  (let (next-range
        (search-pos (point)))
    (while (and (setq next-range (mastodon-tl--find-next-or-previous-property-range
                                  'mastodon-tab-stop search-pos nil))
                (get-text-property (car next-range) 'invisible)
                (setq search-pos (1+ (cdr next-range))))
      ;; do nothing, all the action in in the while condition
      )
    (if (null next-range)
        (message "Nothing else here.")
      (goto-char (car next-range))
      (message "%s" (mastodon-tl--property 'help-echo :no-move)))))

(defun mastodon-tl--previous-tab-item ()
  "Move to the previous interesting item.
This could be the previous toot, link, or image; whichever comes
first. Don't move if nothing else to move to is found, i.e. near
the start of the buffer. This also skips tab items in invisible
text, i.e. hidden spoiler text."
  (interactive)
  (let (next-range
        (search-pos (point)))
    (while (and (setq next-range (mastodon-tl--find-next-or-previous-property-range
                                  'mastodon-tab-stop search-pos t))
                (get-text-property (car next-range) 'invisible)
                (setq search-pos (1- (car next-range))))
      ;; do nothing, all the action in in the while condition
      )
    (if (null next-range)
        (message "Nothing else before this.")
      (goto-char (car next-range))
      (message "%s" (mastodon-tl--property 'help-echo :no-move)))))

(defun mastodon-tl--goto-toot-pos (find-pos refresh &optional pos)
  "Search for toot with FIND-POS.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (let* ((npos (funcall find-pos
                        (or pos (point))
                        'byline
                        (current-buffer))))
    (if npos
        (if (not (get-text-property npos 'toot-id))
            (mastodon-tl--goto-toot-pos find-pos refresh npos)
          (goto-char npos)
          ;; force display of help-echo on moving to a toot byline:
          (mastodon-tl--message-help-echo))
      (funcall refresh))))

(defun mastodon-tl--goto-next-toot ()
  "Jump to next toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'next-single-property-change
                              'mastodon-tl--more))

(defun mastodon-tl--goto-prev-toot ()
  "Jump to last toot header."
  (interactive)
  (mastodon-tl--goto-toot-pos 'previous-single-property-change
                              'mastodon-tl--update))

(defun mastodon-tl--goto-first-item ()
  "Jump to first toot or item in buffer.
Used on initializing a timeline or thread."
  ;; goto-next-toot assumes we already have toots, and is therefore
  ;; incompatible with any view where it is possible to have no items.
  ;; when that is the case the call to goto-toot-pos loops infinitely
  (goto-char (point-min))
  (mastodon-tl--goto-next-item))

(defun mastodon-tl--goto-next-item ()
  "Jump to next item, e.g. filter or follow request."
  (interactive)
  (mastodon-tl--goto-toot-pos 'next-single-property-change
                              'next-line))

(defun mastodon-tl--goto-prev-item ()
  "Jump to previous item, e.g. filter or follow request."
  (interactive)
  (mastodon-tl--goto-toot-pos 'previous-single-property-change
                              'previous-line))


;;; TIMELINES

(defun mastodon-tl--get-federated-timeline (&optional prefix local)
  "Open federated timeline.
If LOCAL, get only local timeline.
With a single PREFIX arg, hide-replies.
With a double PREFIX arg, only show posts with media."
  (interactive "p")
  (let ((params
         `(("limit" . ,mastodon-tl--timeline-posts-count))))
    ;; avoid adding 'nil' to our params alist:
    (when (eq prefix 16)
      (push '("only_media" . "true") params))
    (when local
      (push '("local" . "true") params))
    (message "Loading federated timeline...")
    (mastodon-tl--init
     (if local "local" "federated")
     "timelines/public" 'mastodon-tl--timeline nil
     params
     (when (eq prefix 4) t))))

(defun mastodon-tl--get-home-timeline (&optional arg)
  "Open home timeline.
With a single prefix ARG, hide replies."
  (interactive "p")
  (message "Loading home timeline...")
  (mastodon-tl--init
   "home" "timelines/home" 'mastodon-tl--timeline nil
   `(("limit" . ,mastodon-tl--timeline-posts-count))
   (when (eq arg 4) t)))

(defun mastodon-tl--get-local-timeline (&optional prefix)
  "Open local timeline.
With a single PREFIX arg, hide-replies.
With a double PREFIX arg, only show posts with media."
  (interactive "p")
  (message "Loading local timeline...")
  (mastodon-tl--get-federated-timeline prefix :local))

(defun mastodon-tl--get-tag-timeline (&optional prefix tag)
  "Prompt for tag and opens its timeline.
Optionally load TAG timeline directly.
With a single PREFIX arg, only show posts with media.
With a double PREFIX arg, limit results to your own instance."
  (interactive "p")
  (let* ((word (or (word-at-point) ""))
         (input (or tag (read-string (format "Load timeline for tag (%s): " word))))
         (tag (or tag (if (string-empty-p input) word input))))
    (message "Loading timeline for #%s..." tag)
    (mastodon-tl--show-tag-timeline prefix tag)))

(defun mastodon-tl--show-tag-timeline (&optional prefix tag)
  "Opens a new buffer showing the timeline of posts with hastag TAG.
If TAG is a list, show a timeline for all tags.
With a single PREFIX arg, only show posts with media.
With a double PREFIX arg, limit results to your own instance."
  (let ((params
         `(("limit" . ,mastodon-tl--timeline-posts-count))))
    ;; avoid adding 'nil' to our params alist:
    (when (eq prefix 4)
      (push '("only_media" . "true") params))
    (when (eq prefix 16)
      (push '("local" . "true") params))
    (when (listp tag)
      (let ((list (mastodon-http--build-array-params-alist "any[]" (cdr tag))))
        (while list
          (push (pop list) params))))
    (mastodon-tl--init (if (listp tag)
                           "tags-multiple"
                         (concat "tag-" tag))
                       (concat "timelines/tag/" (if (listp tag)
                                                    ;; endpoint needs to be /tag/:sometag
                                                    (car tag) tag))
                       'mastodon-tl--timeline
                       nil
                       params)))


;;; BYLINES, etc.

(defun mastodon-tl--message-help-echo ()
  "Call message on `help-echo' property at point.
Do so if type of status at poins is not follow_request/follow."
  (let ((type (alist-get
               'type
               (mastodon-tl--property 'toot-json :no-move)))
        (echo (mastodon-tl--property 'help-echo :no-move)))
    (when echo ; not for followers/following in profile
      (unless (or (string= type "follow_request")
                  (string= type "follow")) ; no counts for these
        (message "%s" (mastodon-tl--property 'help-echo :no-move))))))

(defun mastodon-tl--byline-author (toot &optional avatar)
  "Propertize author of TOOT.
With arg AVATAR, include the account's avatar image."
  (let* ((account (alist-get 'account toot))
         (handle (alist-get 'acct account))
         (name (if (not (string-empty-p (alist-get 'display_name account)))
                   (alist-get 'display_name account)
                 (alist-get 'username account)))
         (profile-url (alist-get 'url account))
         (avatar-url (alist-get 'avatar account)))
    (concat
     ;; avatar insertion moved up to `mastodon-tl--byline' by default in order
     ;; to be outside of text prop 'byline t. arg avatar is used by
     ;; `mastodon-profile--add-author-bylines'
     (when (and avatar
                mastodon-tl--show-avatars
                mastodon-tl--display-media-p
                (if (version< emacs-version "27.1")
                    (image-type-available-p 'imagemagick)
                  (image-transforms-p)))
       (mastodon-media--get-avatar-rendering avatar-url))
     (propertize name
                 'face 'mastodon-display-name-face
                 ;; enable playing of videos when point is on byline:
                 'attachments (mastodon-tl--get-attachments-for-byline toot)
                 'keymap mastodon-tl--byline-link-keymap
                 ;; echo faves count when point on post author name:
                 ;; which is where --goto-next-toot puts point.
                 'help-echo
                 ;; but don't add it to "following"/"follows" on profile views:
                 ;; we don't have a tl--buffer-spec yet:
                 (unless (or (string-suffix-p "-followers*" (buffer-name))
                             (string-suffix-p "-following*" (buffer-name)))
                   (mastodon-tl--format-byline-help-echo toot)))
     " ("
     (propertize (concat "@" handle)
                 'face 'mastodon-handle-face
                 'mouse-face 'highlight
	         'mastodon-tab-stop 'user-handle
                 'account account
	         'shr-url profile-url
	         'keymap mastodon-tl--link-keymap
                 'mastodon-handle (concat "@" handle)
	         'help-echo (concat "Browse user profile of @" handle))
     ")")))

(defun mastodon-tl--format-byline-help-echo (toot)
  "Format a help-echo for byline of TOOT.
Displays a toot's media types and optionally the binding to play
moving image media from the byline.
Used when point is at the start of a byline, i.e. where
`mastodon-tl--goto-next-toot' leaves point."
  (let* ((toot-to-count
          (or
           ;; simply praying this order works
           (alist-get 'status toot) ; notifications timeline
           ;; fol-req notif, has 'type
           ;; placed before boosts coz fol-reqs have a (useless) reblog entry:
           (when (and (or (mastodon-tl--buffer-type-eq 'notifications)
                          (mastodon-tl--buffer-type-eq 'mentions))
                      (alist-get 'type toot))
             toot)
           (alist-get 'reblog toot) ; boosts
           toot)) ; everything else
         (fol-req-p (or (string= (alist-get 'type toot-to-count) "follow")
                        (string= (alist-get 'type toot-to-count) "follow_request"))))
    (unless fol-req-p
      (let* ((media-types (mastodon-tl--get-media-types toot))
             (format-media (when media-types
                             (format "media: %s"
                                     (mapconcat #'identity media-types " "))))
             (format-media-binding (when (and (or
                                               (member "video" media-types)
                                               (member "gifv" media-types))
                                              (require 'mpv nil :no-error))
                                     (format " | C-RET to view with mpv"))))
        (format "%s" (concat format-media format-media-binding))))))

(defun mastodon-tl--get-media-types (toot)
  "Return a list of the media attachment types of the TOOT at point."
  (let* ((attachments (mastodon-tl--field 'media_attachments toot)))
    (mastodon-tl--map-alist 'type attachments)))

(defun mastodon-tl--get-attachments-for-byline (toot)
  "Return a list of attachment URLs and types for TOOT.
The result is added as an attachments property to author-byline."
  (let ((media-attachments (mastodon-tl--field 'media_attachments toot)))
    (mapcar
     (lambda (attachement)
       (let ((remote-url
              (or (alist-get 'remote_url attachement)
                  ;; fallback b/c notifications don't have remote_url
                  (alist-get 'url attachement)))
             (type (alist-get 'type attachement)))
         `(:url ,remote-url :type ,type)))
     media-attachments)))

(defun mastodon-tl--byline-boosted (toot)
  "Add byline for boosted data from TOOT."
  (let ((reblog (alist-get 'reblog toot)))
    (when reblog
      (concat
       "\n  "
       (propertize "Boosted" 'face 'mastodon-boosted-face)
       " "
       (mastodon-tl--byline-author reblog)))))

(defun mastodon-tl--format-faved-or-boosted-byline (letter)
  "Format the byline marker for a boosted or favourited status.
LETTER is a string, F for favourited, B for boosted, or K for bookmarked."
  (let ((help-string (cond ((equal letter "F")
                            "favourited")
                           ((equal letter "B")
                            "boosted")
                           ((equal letter (or "🔖" "K"))
                            "bookmarked"))))
    (format "(%s) "
            (propertize letter 'face 'mastodon-boost-fave-face
                        ;; emojify breaks this for 🔖:
                        'help-echo (format "You have %s this status."
                                           help-string)))))

(defun mastodon-tl--byline (toot author-byline action-byline &optional detailed-p)
  "Generate byline for TOOT.
AUTHOR-BYLINE is a function for adding the author portion of
the byline that takes one variable.
ACTION-BYLINE is a function for adding an action, such as boosting,
favouriting and following to the byline. It also takes a single function.
By default it is `mastodon-tl--byline-boosted'.
DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let* ((created-time
          ;; bosts and faves in notifs view
          ;; (makes timestamps be for the original toot
          ;; not the boost/fave):
          (or (mastodon-tl--field 'created_at
                                  (mastodon-tl--field 'status toot))
              ;; all other toots, inc. boosts/faves in timelines:
              ;; (mastodon-tl--field auto fetches from reblogs if needed):
              (mastodon-tl--field 'created_at toot)))
         (parsed-time (date-to-time created-time))
         (faved (equal 't (mastodon-tl--field 'favourited toot)))
         (boosted (equal 't (mastodon-tl--field 'reblogged toot)))
         (bookmarked (equal 't (mastodon-tl--field 'bookmarked toot)))
         (visibility (mastodon-tl--field 'visibility toot))
         (account (alist-get 'account toot))
         (avatar-url (alist-get 'avatar account))
         (edited-time (alist-get 'edited_at toot))
         (edited-parsed (when edited-time (date-to-time edited-time))))
    (concat
     ;; Boosted/favourited markers are not technically part of the byline, so
     ;; we don't propertize them with 'byline t', as per the rest. This
     ;; ensures that `mastodon-tl--goto-next-toot' puts point on
     ;; author-byline, not before the (F) or (B) marker. Not propertizing like
     ;; this makes the behaviour of these markers consistent whether they are
     ;; displayed for an already boosted/favourited toot or as the result of
     ;; the toot having just been favourited/boosted.
     (concat (when boosted
               (mastodon-tl--format-faved-or-boosted-byline
                (mastodon-tl--symbol 'boost)))
             (when faved
               (mastodon-tl--format-faved-or-boosted-byline
                (mastodon-tl--symbol 'favourite)))
             (when bookmarked
               (mastodon-tl--format-faved-or-boosted-byline
                (mastodon-tl--symbol 'bookmark))))
     ;; we remove avatars from the byline also, so that they also do not mess
     ;; with `mastodon-tl--goto-next-toot':
     (when (and mastodon-tl--show-avatars
                mastodon-tl--display-media-p
                (if (version< emacs-version "27.1")
                    (image-type-available-p 'imagemagick)
                  (image-transforms-p)))
       (mastodon-media--get-avatar-rendering avatar-url))
     (propertize
      (concat
       ;; we propertize help-echo format faves for author name
       ;; in `mastodon-tl--byline-author'
       (funcall author-byline toot)
       ;; visibility:
       (cond ((equal visibility "direct")
              (concat " " (mastodon-tl--symbol 'direct)))
             ((equal visibility "private")
              (concat " " (mastodon-tl--symbol 'private))))
       (funcall action-byline toot)
       " "
       (propertize
        (format-time-string mastodon-toot-timestamp-format parsed-time)
        'timestamp parsed-time
        'display (if mastodon-tl--enable-relative-timestamps
                     (mastodon-tl--relative-time-description parsed-time)
                   parsed-time))
       (when detailed-p
         (let* ((app (alist-get 'application toot))
                (app-name (alist-get 'name app))
                (app-url (alist-get 'website app)))
           (when app
             (concat
              (propertize " via " 'face 'default)
              (propertize app-name
                          'face 'mastodon-display-name-face
                          'follow-link t
                          'mouse-face 'highlight
		          'mastodon-tab-stop 'shr-url
		          'shr-url app-url
                          'help-echo app-url
		          'keymap mastodon-tl--shr-map-replacement)))))
       (if edited-time
           (concat
            " "
            (mastodon-tl--symbol 'edited)
            " "
            (propertize
             (format-time-string mastodon-toot-timestamp-format
                                 edited-parsed)
             'face font-lock-comment-face
             'timestamp edited-parsed
             'display (if mastodon-tl--enable-relative-timestamps
                          (mastodon-tl--relative-time-description edited-parsed)
                        edited-parsed)))
         "")
       (propertize (concat "\n  " mastodon-tl--horiz-bar)
                   'face 'default)
       (if mastodon-tl--show-stats
           (mastodon-tl--toot-stats toot)
         "")
       "\n")
      'favourited-p faved
      'boosted-p    boosted
      'bookmarked-p bookmarked
      'edited edited-time
      'edit-history (when edited-time
                      (mastodon-toot--get-toot-edits (alist-get 'id toot)))
      'byline       t))))


;;; TIMESTAMPS

(defun mastodon-tl--relative-time-details (timestamp &optional current-time)
  "Return cons of (descriptive string . next change) for the TIMESTAMP.
Use the optional CURRENT-TIME as the current time (only used for
reliable testing).
The descriptive string is a human readable version relative to
the current time while the next change timestamp give the first
time that this description will change in the future.
TIMESTAMP is assumed to be in the past."
  (let* ((now (or current-time (current-time)))
         (time-difference (time-subtract now timestamp))
         (seconds-difference (float-time time-difference))
         (regular-response
          (lambda (seconds-difference multiplier unit-name)
            (let ((n (floor (+ 0.5 (/ seconds-difference multiplier)))))
              (cons (format "%d %ss ago" n unit-name)
                    (* (+ 0.5 n) multiplier)))))
         (relative-result
          (cond
           ((< seconds-difference 60)
            (cons "just now"
                  60))
           ((< seconds-difference (* 1.5 60))
            (cons "1 minute ago"
                  90)) ;; at 90 secs
           ((< seconds-difference (* 60 59.5))
            (funcall regular-response seconds-difference 60 "minute"))
           ((< seconds-difference (* 1.5 60 60))
            (cons "1 hour ago"
                  (* 60 90))) ;; at 90 minutes
           ((< seconds-difference (* 60 60 23.5))
            (funcall regular-response seconds-difference (* 60 60) "hour"))
           ((< seconds-difference (* 1.5 60 60 24))
            (cons "1 day ago"
                  (* 1.5 60 60 24))) ;; at a day and a half
           ((< seconds-difference (* 60 60 24 6.5))
            (funcall regular-response seconds-difference (* 60 60 24) "day"))
           ((< seconds-difference (* 1.5 60 60 24 7))
            (cons "1 week ago"
                  (* 1.5 60 60 24 7))) ;; a week and a half
           ((< seconds-difference (* 60 60 24 7 52))
            (if (= 52 (floor (+ 0.5 (/ seconds-difference 60 60 24 7))))
                (cons "52 weeks ago"
                      (* 60 60 24 7 52))
              (funcall regular-response seconds-difference (* 60 60 24 7) "week")))
           ((< seconds-difference (* 1.5 60 60 24 365))
            (cons "1 year ago"
                  (* 60 60 24 365 1.5))) ;; a year and a half
           (t
            (funcall regular-response seconds-difference (* 60 60 24 365.25) "year")))))
    (cons (car relative-result)
          (time-add timestamp (seconds-to-time (cdr relative-result))))))

(defun mastodon-tl--relative-time-description (timestamp &optional current-time)
  "Return a string with a human readable TIMESTAMP relative to the current time.
Use the optional CURRENT-TIME as the current time (only used for
reliable testing).
E.g. this could return something like \"1 min ago\", \"yesterday\", etc.
TIME-STAMP is assumed to be in the past."
  (car (mastodon-tl--relative-time-details timestamp current-time)))


;;; RENDERING HTML, LINKS, HASHTAGS, HANDLES

(defun mastodon-tl--render-text (string &optional toot)
  "Return a propertized text rendering the given HTML string STRING.
The contents comes from the given TOOT which is used in parsing
links in the text. If TOOT is nil no parsing occurs."
  (when string ; handle rare empty notif server bug
    (with-temp-buffer
      (insert string)
      (let ((shr-use-fonts mastodon-tl--enable-proportional-fonts)
            (shr-width (when mastodon-tl--enable-proportional-fonts
                         (- (window-width) 1))))
        (shr-render-region (point-min) (point-max)))
      ;; Make all links a tab stop recognized by our own logic, make things point
      ;; to our own logic (e.g. hashtags), and update keymaps where needed:
      (when toot
        (let (region)
          (while (setq region (mastodon-tl--find-property-range
                               'shr-url (or (cdr region) (point-min))))
            (mastodon-tl--process-link toot
                                       (car region) (cdr region)
                                       (get-text-property (car region) 'shr-url)))))
      (buffer-string))))

(defun mastodon-tl--process-link (toot start end url)
  "Process link URL in TOOT as hashtag, userhandle, or normal link.
START and END are the boundaries of the link in the toot."
  (let* (mastodon-tab-stop-type
         keymap
         (help-echo (get-text-property start 'help-echo))
         extra-properties
         ;; handle calling this on non-toots, e.g. for profiles:
         (toot-url (when (proper-list-p toot)
                     (mastodon-tl--field 'url toot)))
         (toot-url (when toot-url (url-generic-parse-url toot-url)))
         (toot-instance-url (if toot-url
                                (concat (url-type toot-url) "://"
                                        (url-host toot-url))
                              mastodon-instance-url))
         (link-str (buffer-substring-no-properties start end))
         (maybe-hashtag (mastodon-tl--extract-hashtag-from-url
                         url toot-instance-url))
         (maybe-userhandle
          (if (proper-list-p toot) ; fails for profile buffers?
              (or (mastodon-tl--userhandle-from-mentions toot
                                                         link-str)
                  ;; FIXME: if prev always works, cut this:
                  (mastodon-tl--extract-userhandle-from-url
                   url link-str))
            (mastodon-tl--extract-userhandle-from-url
             url link-str))))
    (cond (;; Hashtags:
           maybe-hashtag
           (setq mastodon-tab-stop-type 'hashtag
                 keymap mastodon-tl--link-keymap
                 help-echo (concat "Browse tag #" maybe-hashtag)
                 extra-properties (list 'mastodon-tag maybe-hashtag)))
          (;; User handles:
           maybe-userhandle
           ;; this fails on mentions in profile notes:
           (let ((maybe-userid (when (proper-list-p toot)
                                 (mastodon-tl--extract-userid-toot
                                  toot link-str))))
             (setq mastodon-tab-stop-type 'user-handle
                   keymap mastodon-tl--link-keymap
                   help-echo (concat "Browse user profile of " maybe-userhandle)
                   extra-properties (append
                                     (list 'mastodon-handle maybe-userhandle)
                                     (when maybe-userid
                                       (list 'account-id maybe-userid))))))
          ;; Anything else:
          (t
           ;; Leave it as a url handled by shr.el.
           ;; (We still have to replace the keymap so that tabbing works.)
           (setq keymap (if (eq shr-map (get-text-property start 'keymap))
                            mastodon-tl--shr-map-replacement
                          mastodon-tl--shr-image-map-replacement)
                 mastodon-tab-stop-type 'shr-url)))
    (add-text-properties start end
                         (append
                          (list 'mastodon-tab-stop mastodon-tab-stop-type
                                'keymap keymap
                                'help-echo help-echo)
                          extra-properties))))

(defun mastodon-tl--userhandle-from-mentions (toot link)
  "Extract a user handle from mentions in json TOOT.
LINK is maybe the '@handle' to search for."
  (mastodon-tl--extract-el-from-mentions 'acct toot link))

(defun mastodon-tl--extract-userid-toot (toot link)
  "Extract a user id for an ACCT from mentions in a TOOT.
LINK is maybe the '@handle' to search for."
  (mastodon-tl--extract-el-from-mentions 'id toot link))

(defun mastodon-tl--extract-el-from-mentions (el toot link)
  "Extract element EL from TOOT mentions that matches LINK.
LINK should be a simple handle string with no domain, i.e. @user.
Return nil if no matching element"
  ;; Must return nil if nothing found!
  ;; TODO: we should break the while loop as soon as we get sth
  (let ((mentions (append (alist-get 'mentions toot) nil)))
    (when mentions
      (let* ((mention (pop mentions))
             (name (substring-no-properties link 1 (length link))) ; cull @
             return)
        (while mention
          (when (string= (alist-get 'username mention)
                         name)
            (setq return (alist-get el mention)))
          (setq mention (pop mentions)))
        return))))

(defun mastodon-tl--extract-userhandle-from-url (url buffer-text)
  "Return the user hande the URL points to or nil if it is not a profile link.
BUFFER-TEXT is the text covered by the link with URL, for a user profile
this should be of the form <at-sign><user id>, e.g. \"@Gargon\"."
  (let* ((parsed-url (url-generic-parse-url url))
         (local-p (string=
                   (url-host (url-generic-parse-url mastodon-instance-url))
                   (url-host parsed-url))))
    (when (and (string= "@" (substring buffer-text 0 1))
               (string= (downcase buffer-text)
                        (downcase (substring (url-filename parsed-url) 1))))
      (if local-p
          buffer-text ; no instance suffic for local mention
        (concat buffer-text "@" (url-host parsed-url))))))

(defun mastodon-tl--extract-hashtag-from-url (url instance-url)
  "Return the hashtag that URL points to or nil if URL is not a tag link.
INSTANCE-URL is the url of the instance for the toot that the link
came from (tag links always point to a page on the instance publishing
the toot)."
  (cond
   ;; Mastodon type tag link:
   ((string-prefix-p (concat instance-url "/tags/") url)
    (substring url (length (concat instance-url "/tags/"))))
   ;; Link from some other ostatus site we've encountered:
   ((string-prefix-p (concat instance-url "/tag/") url)
    (substring url (length (concat instance-url "/tag/"))))
   ;; If nothing matches we assume it is not a hashtag link:
   (t nil)))


;;; HYPERLINKS

(defun mastodon-tl--make-link (string link-type)
  "Return a propertized version of STRING that will act like link.
LINK-TYPE is the type of link to produce."
  (let ((help-text (cond
                    ((eq link-type 'content-warning)
                     "Toggle hidden text")
                    (t
                     (error "Unknown link type %s" link-type)))))
    (propertize
     string
     'mastodon-tab-stop link-type
     'mouse-face 'highlight
     'keymap mastodon-tl--link-keymap
     'help-echo help-text)))

(defun mastodon-tl--do-link-action-at-point (position)
  "Do the action of the link at POSITION.
Used for hitting RET on a given link."
  (interactive "d")
  (let ((link-type (get-text-property position 'mastodon-tab-stop)))
    (cond ((eq link-type 'content-warning)
           (mastodon-tl--toggle-spoiler-text position))
          ((eq link-type 'hashtag)
           (mastodon-tl--show-tag-timeline nil (get-text-property position 'mastodon-tag)))
          ;; 'account / 'account-id is not set for mentions, only bylines
          ((eq link-type 'user-handle)
           (let ((account-json (get-text-property position 'account))
                 (account-id (get-text-property position 'account-id)))
             (cond
              (account-json
               (mastodon-profile--make-author-buffer
                account-json))
              (account-id
               (mastodon-profile--make-author-buffer
                (mastodon-profile--account-from-id account-id)))
              (t
               (let ((account
                      (mastodon-profile--search-account-by-handle
                       (get-text-property position 'mastodon-handle))))
                 ;; never call make-author-buffer on nil account:
                 (if account
                     (mastodon-profile--make-author-buffer account)
                   ;; optional webfinger lookup:
                   (if (y-or-n-p
                        "Search for account returned nothing. Perform URL lookup?")
                       (mastodon-url-lookup (get-text-property position 'shr-url))
                     (message "Unable to find account."))))))))
          (t
           (error "Unknown link type %s" link-type)))))

(defun mastodon-tl--do-link-action (event)
  "Do the action of the link at point.
Used for a mouse-click EVENT on a link."
  (interactive "e")
  (mastodon-tl--do-link-action-at-point (posn-point (event-end event))))


;;; CONTENT WARNINGS

(defun mastodon-tl--has-spoiler (toot)
  "Check if the given TOOT has a spoiler text.
Spoiler text should initially be shown only while the main
content should be hidden."
  (let ((spoiler (mastodon-tl--field 'spoiler_text toot)))
    (and spoiler (> (length spoiler) 0))))

(defun mastodon-tl--toggle-spoiler-text (position)
  "Toggle the visibility of the spoiler text at/after POSITION."
  (let ((inhibit-read-only t)
        (spoiler-text-region (mastodon-tl--find-property-range
                              'mastodon-content-warning-body position nil)))
    (if (not spoiler-text-region)
        (message "No spoiler text here")
      (add-text-properties (car spoiler-text-region) (cdr spoiler-text-region)
                           (list 'invisible
                                 (not (get-text-property (car spoiler-text-region)
                                                         'invisible)))))))

(defun mastodon-tl--toggle-spoiler-text-in-toot ()
  "Toggle the visibility of the spoiler text in the current toot."
  (interactive)
  (let* ((toot-range (or (mastodon-tl--find-property-range
                          'toot-json (point))
                         (mastodon-tl--find-property-range
                          'toot-json (point) t)))
         (spoiler-range (when toot-range
                          (mastodon-tl--find-property-range
                           'mastodon-content-warning-body
                           (car toot-range)))))
    (cond ((null toot-range)
           (message "No toot here"))
          ((or (null spoiler-range)
               (> (car spoiler-range) (cdr toot-range)))
           (message "No content warning text here"))
          (t
           (mastodon-tl--toggle-spoiler-text (car spoiler-range))))))

(defun mastodon-tl--clean-tabs-and-nl (string)
  "Remove tabs and newlines from STRING."
  (replace-regexp-in-string
   "[\t\n ]*\\'" "" string))

(defun mastodon-tl--spoiler (toot)
  "Render TOOT with spoiler message.
This assumes TOOT is a toot with a spoiler message.
The main body gets hidden and only the spoiler text and the
content warning message are displayed. The content warning
message is a link which unhides/hides the main body."
  (let* ((spoiler (mastodon-tl--field 'spoiler_text toot))
         (string (mastodon-tl--set-face
                  ;; remove trailing whitespace
                  (mastodon-tl--clean-tabs-and-nl
                   (mastodon-tl--render-text spoiler toot))
                  'default))
         (message (concat
                   " " mastodon-tl--horiz-bar "\n "
                   (mastodon-tl--make-link
                    (concat "CW: " string)
                    'content-warning)
                   "\n "
                   mastodon-tl--horiz-bar "\n"))
         (cw (mastodon-tl--set-face message 'mastodon-cw-face)))
    (concat
     cw
     (propertize (mastodon-tl--content toot)
                 'invisible
                 ;; check server setting to expand all spoilers:
                 (unless (eq t
                             ;; If something goes wrong reading prefs,
                             ;; just return nil so CWs show by default.
                             (condition-case nil
                                 (mastodon-profile--get-preferences-pref
                                  'reading:expand:spoilers)
                               (error nil)))
                   t)
                 'mastodon-content-warning-body t))))


;;; MEDIA

(defun mastodon-tl--media (toot)
  "Retrieve a media attachment link for TOOT if one exists."
  (let* ((media-attachments (mastodon-tl--field 'media_attachments toot))
         (media-string (mapconcat #'mastodon-tl--media-attachment
                                  media-attachments "")))
    (if (not (and mastodon-tl--display-media-p
                  (string-empty-p media-string)))
        (concat "\n" media-string)
      "")))

(defun mastodon-tl--media-attachment (media-attachment)
  "Return a propertized string for MEDIA-ATTACHMENT."
  (let* ((preview-url
          (alist-get 'preview_url media-attachment))
         (remote-url
          (or (alist-get 'remote_url media-attachment)
              ;; fallback b/c notifications don't have remote_url
              (alist-get 'url media-attachment)))
         (type (alist-get 'type media-attachment))
         (caption (alist-get 'description media-attachment))
         (display-str
          (if (and mastodon-tl--display-caption-not-url-when-no-media
                   caption)
              (concat "Media:: " caption)
            (concat "Media:: " preview-url))))
    (if mastodon-tl--display-media-p
        ;; return placeholder [img]:
        (mastodon-media--get-media-link-rendering
         preview-url remote-url type caption) ; 2nd arg for shr-browse-url
      ;; return URL/caption:
      (concat
       (mastodon-tl--propertize-img-str-or-url
        (concat "Media:: " preview-url) ;; string
        preview-url remote-url type caption
        display-str ;; display
        ;; FIXME: shr-link underlining is awful for captions with
        ;; newlines, as the underlining runs to the edge of the
        ;; frame even if the text doesn'
        'shr-link)
       "\n"))))

(defun mastodon-tl--propertize-img-str-or-url (str media-url full-remote-url type
                                                   help-echo &optional display face)
  "Propertize an media placeholder string \"[img]\" or media URL.
STR is the string to propertize, MEDIA-URL is the preview link,
FULL-REMOTE-URL is the link to the full resolution image on the
server, TYPE is the media type.
HELP-ECHO, DISPLAY, and FACE are the text properties to add."
  (propertize str
              'media-url media-url
              'media-state (when (string= str "[img]") 'needs-loading)
              'media-type 'media-link
              'mastodon-media-type type
              'display display
              'face face
              'mouse-face 'highlight
              'mastodon-tab-stop 'image ; for do-link-action-at-point
              'image-url full-remote-url ; for shr-browse-image
              'keymap mastodon-tl--shr-image-map-replacement
              'help-echo (if (or (string= type "image")
                                 (string= type nil)
                                 (string= type "unknown")) ;handle borked images
                             help-echo
                           (concat help-echo "\nC-RET: play " type " with mpv"))))


;;; INSERT TOOTS

(defun mastodon-tl--content (toot)
  "Retrieve text content from TOOT.
Runs `mastodon-tl--render-text' and fetches poll or media."
  (let* ((content (mastodon-tl--field 'content toot))
         (reblog (alist-get 'reblog toot))
         (poll-p (if reblog
                     (alist-get 'poll reblog)
                   (alist-get 'poll toot))))
    (concat
     (mastodon-tl--render-text content toot)
     (when poll-p
       (mastodon-tl--get-poll toot))
     (mastodon-tl--media toot))))

(defun mastodon-tl--insert-status (toot body author-byline action-byline
                                        &optional id base-toot detailed-p)
  "Display the content and byline of timeline element TOOT.
BODY will form the section of the toot above the byline.
AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.
ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.
ID is that of the status if it is a notification, which is
attached as a `toot-id' property if provided. If the
status is a favourite or boost notification, BASE-TOOT is the
JSON of the toot responded to.
DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let ((start-pos (point)))
    (insert
     (propertize
      (concat "\n"
              body
              " \n"
              (mastodon-tl--byline toot author-byline action-byline detailed-p))
      'toot-id      (or id ; notification's own id
                        (alist-get 'id toot)) ; toot id
      'base-toot-id (mastodon-tl--toot-id
                     ;; if status is a notif, get id from base-toot
                     ;; (-tl--toot-id toot) will not work here:
                     (or base-toot
                         ;; else normal toot with reblog check:
                         toot))
      'toot-json    toot
      'base-toot    base-toot)
     "\n")
    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images start-pos (point)))))

;; from mastodon-alt.el:
(defun mastodon-tl--toot-for-stats (&optional toot)
  "Return the TOOT on which we want to extract stats.
If no TOOT is given, the one at point is considered."
  (let* ((original-toot (or toot (get-text-property (point) 'toot-json)))
         (toot (or (alist-get 'status original-toot)
                   (when (alist-get 'type original-toot)
                     original-toot)
                   (alist-get 'reblog original-toot)
                   original-toot))
         (type (alist-get 'type (or toot))))
    (unless (member type '("follow" "follow_request"))
      toot)))

(defun mastodon-tl--toot-stats (toot)
  "Return a right aligned string (using display align-to).
String is filled with TOOT statistics (boosts, favs, replies).
When the TOOT is a reblog (boost), statistics from reblogged
toots are returned.
To disable showing the stats, customize
`mastodon-tl--show-stats'."
  (when-let ((toot (mastodon-tl--toot-for-stats toot)))
    (let* ((favourites-count (alist-get 'favourites_count toot))
           (favourited (equal 't (alist-get 'favourited toot)))
           (faves-prop (propertize (format "%s" favourites-count)
                                   'favourites-count favourites-count))
           (boosts-count (alist-get 'reblogs_count toot))
           (boosted (equal 't (alist-get 'reblogged toot)))
           (boosts-prop (propertize (format "%s" boosts-count)
                                    'boosts-count boosts-count))
           (replies-count (alist-get 'replies_count toot))
           (favourites (format "%s %s" faves-prop ;favourites-count
                               (mastodon-tl--symbol 'favourite)))
           (boosts (format "%s %s" boosts-prop ;boosts-count
                           (mastodon-tl--symbol 'boost)))
           (replies (format "%s %s" replies-count (mastodon-tl--symbol 'reply)))
           (status (concat
                    (propertize favourites
                                'favourited-p favourited
                                'favourites-field t
                                'face font-lock-comment-face)
                    (propertize " | " 'face font-lock-comment-face)
                    (propertize boosts
                                'boosted-p boosted
                                'boosts-field t
                                'face font-lock-comment-face)
                    (propertize " | " 'face font-lock-comment-face)
                    (propertize replies
                                'replies-field t
                                'replies-count replies-count
                                'face font-lock-comment-face)))
           (status (concat
                    (propertize " " 'display `(space :align-to (- right ,(+ (length status) 7))))
                    status)))
      status)))


;; POLLS

(defun mastodon-tl--get-poll (toot)
  "If TOOT includes a poll, return it as a formatted string."
  (let* ((poll (mastodon-tl--field 'poll toot))
         (expiry (mastodon-tl--field 'expires_at poll))
         (expired-p (if (eq (mastodon-tl--field 'expired poll) :json-false) nil t))
         ;; (multi (mastodon-tl--field 'multiple poll))
         (voters-count (mastodon-tl--field 'voters_count poll))
         (vote-count (mastodon-tl--field 'votes_count poll))
         (options (mastodon-tl--field 'options poll))
         (option-titles (mastodon-tl--map-alist 'title options))
         (longest-option (car (sort option-titles
                                    (lambda (x y)
                                      (> (length x)
                                         (length y))))))
         (option-counter 0))
    (concat "\nPoll: \n\n"
            (mapconcat (lambda (option)
                         (progn
                           (format "%s: %s%s%s\n"
                                   (setq option-counter (1+ option-counter))
                                   (propertize (alist-get 'title option)
                                               'face 'success)
                                   (make-string
                                    (1+
                                     (- (length longest-option)
                                        (length (alist-get 'title
                                                           option))))
                                    ?\ )
                                   ;; TODO: disambiguate no votes from hidden votes
                                   (format "[%s votes]" (or (alist-get 'votes_count option)
                                                            "0")))))
                       options
                       "\n")
            "\n"
            (propertize
             (cond (voters-count ; sometimes it is nil
                    (if (= voters-count 1)
                        (format "%s person | " voters-count)
                      (format "%s people | " voters-count)))
                   (vote-count
                    (format "%s votes | " vote-count))
                   (t
                    ""))
             'face 'font-lock-comment-face)
            (let ((str (if expired-p
                           "Poll expired."
                         (mastodon-tl--format-poll-expiry expiry))))
              (propertize str 'face 'font-lock-comment-face))
            "\n")))

(defun mastodon-tl--format-poll-expiry (timestamp)
  "Convert poll expiry TIMESTAMP into a descriptive string."
  (let ((parsed (ts-human-duration
                 (ts-diff (ts-parse timestamp) (ts-now)))))
    (cond ((> (plist-get parsed :days) 0)
           (format "%s days, %s hours left" (plist-get parsed :days) (plist-get parsed :hours)))
          ((> (plist-get parsed :hours) 0)
           (format "%s hours, %s minutes left" (plist-get parsed :hours) (plist-get parsed :minutes)))
          ((> (plist-get parsed :minutes) 0)
           (format "%s minutes left" (plist-get parsed :minutes)))
          (t ;; we failed to guess:
           (format "%s days, %s hours, %s minutes left"
                   (plist-get parsed :days)
                   (plist-get parsed :hours)
                   (plist-get parsed :minutes))))))

(defun mastodon-tl--poll-vote (option)
  "If there is a poll at point, prompt user for OPTION to vote on it."
  (interactive
   (list
    (let* ((toot (mastodon-tl--property 'toot-json))
           (reblog (alist-get 'reblog toot))
           (poll (or (alist-get 'poll reblog)
                     (mastodon-tl--field 'poll toot)))
           (options (mastodon-tl--field 'options poll))
           (options-titles (mastodon-tl--map-alist 'title options))
           (options-number-seq (number-sequence 1 (length options)))
           (options-numbers (mapcar #'number-to-string options-number-seq))
           (options-alist (cl-mapcar #'cons options-numbers options-titles))
           ;; we display both option number and the option title
           ;; but also store both as cons cell as cdr, as we need it below
           (candidates (mapcar (lambda (cell)
                                 (cons (format "%s | %s" (car cell) (cdr cell))
                                       cell))
                               options-alist)))
      (if (null (mastodon-tl--field 'poll (mastodon-tl--property 'toot-json)))
          (message "No poll here.")
        ;; var "option" = just the cdr, a cons of option number and desc
        (cdr (assoc
              (completing-read "Poll option to vote for: "
                               candidates
                               nil ; (predicate)
                               t) ; require match
              candidates))))))
  (if (null (mastodon-tl--field 'poll (mastodon-tl--property 'toot-json)))
      (message "No poll here.")
    (let* ((toot (mastodon-tl--property 'toot-json))
           (poll (mastodon-tl--field 'poll toot))
           (poll-id (alist-get 'id poll))
           (url (mastodon-http--api (format "polls/%s/votes" poll-id)))
           ;; need to zero-index our option:
           (option-as-arg (number-to-string (1- (string-to-number (car option)))))
           (arg `(("choices[]" . ,option-as-arg)))
           (response (mastodon-http--post url arg)))
      (mastodon-http--triage response
                             (lambda ()
                               (message "You voted for option %s: %s!"
                                        (car option) (cdr option)))))))


;; VIDEOS / MPV

(defun mastodon-tl--find-first-video-in-attachments ()
  "Return the first media attachment that is a moving image."
  (let ((attachments (mastodon-tl--property 'attachments))
        vids)
    (mapc (lambda (x)
            (let ((att-type (plist-get x :type)))
              (when (or (string= "video" att-type)
                        (string= "gifv" att-type))
                (push x vids))))
          attachments)
    (car vids)))

(defun mastodon-tl--mpv-play-video-from-byline ()
  "Run `mastodon-tl--mpv-play-video-at-point' on first moving image in post."
  (interactive)
  (let* ((video (mastodon-tl--find-first-video-in-attachments))
         (url (plist-get video :url))
         (type (plist-get video :type)))
    (mastodon-tl--mpv-play-video-at-point url type)))

(defun mastodon-tl--mpv-play-video-at-point (&optional url type)
  "Play the video or gif at point with an mpv process.
URL and TYPE are provided when called while point is on byline,
in which case play first video or gif from current toot."
  (interactive)
  (let ((url (or
              ;; point in byline:
              url
              ;; point in toot:
              (mastodon-tl--property 'image-url :no-move)))
        (type (or ;; in byline:
               type
               ;; point in toot:
               (mastodon-tl--property 'mastodon-media-type :no-move))))
    (if url
        (if (or (equal type "gifv")
                (equal type "video"))
            (progn
              (message "'q' to kill mpv.")
              (mpv-start "--loop" url))
          (message "no moving image here?"))
      (message "no moving image here?"))))


;; INSERT TOOTS

(defun mastodon-tl--is-reply (toot)
  "Check if the TOOT is a reply to another one (and not boosted)."
  (and (null (mastodon-tl--field 'in_reply_to_id toot))
       (not (mastodon-tl--field 'rebloged toot))))

(defun mastodon-tl--toot (toot &optional detailed-p)
  "Format TOOT and insert it into the buffer.
DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (mastodon-tl--insert-status
   toot
   (mastodon-tl--clean-tabs-and-nl
    (if (mastodon-tl--has-spoiler toot)
        (mastodon-tl--spoiler toot)
      (mastodon-tl--content toot)))
   'mastodon-tl--byline-author
   'mastodon-tl--byline-boosted
   nil
   nil
   detailed-p))

(defun mastodon-tl--timeline (toots)
  "Display each toot in TOOTS.
This function removes replies if user required."
  (mapc #'mastodon-tl--toot
        ;; hack to *not* filter replies on profiles:
        (if (eq (mastodon-tl--get-buffer-type) 'profile-statuses)
            toots
          (if (or ; we were called via --more*:
               (mastodon-tl--get-buffer-property 'hide-replies nil :no-error)
               ;; loading a tl with a prefix arg:
               (mastodon-tl--hide-replies-p current-prefix-arg))
	      (cl-remove-if-not #'mastodon-tl--is-reply toots)
	    toots)))
  (goto-char (point-min)))


;;; BUFFER SPEC

(defun mastodon-tl--get-update-function (&optional buffer)
  "Get the UPDATE-FUNCTION stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'update-function buffer))

(defun mastodon-tl--get-endpoint (&optional buffer no-error)
  "Get the ENDPOINT stored in `mastodon-tl--buffer-spec'.
Optionally set it for BUFFER.
NO-ERROR means to fail silently."
  (mastodon-tl--get-buffer-property 'endpoint buffer no-error))

(defun mastodon-tl--buffer-name (&optional buffer no-error)
  "Get the BUFFER-NAME stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER.
NO-ERROR means to fail silently."
  (mastodon-tl--get-buffer-property 'buffer-name buffer no-error))

(defun mastodon-tl--link-header (&optional buffer)
  "Get the LINK HEADER stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'link-header buffer :no-error))

(defun mastodon-tl--update-params (&optional buffer)
  "Get the UPDATE PARAMS stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'update-params buffer :no-error))

(defun mastodon-tl--get-buffer-property (property &optional buffer no-error)
  "Get PROPERTY from `mastodon-tl--buffer-spec' in BUFFER or `current-buffer'.
If NO-ERROR is non-nil, do not error when property is empty."
  (with-current-buffer  (or buffer (current-buffer))
    (if no-error
        (plist-get mastodon-tl--buffer-spec property)
      (or (plist-get mastodon-tl--buffer-spec property)
          (error "Mastodon-tl--buffer-spec is not defined for buffer %s"
                 (or buffer (current-buffer)))))))

(defun mastodon-tl--set-buffer-spec (buffer endpoint update-function
                                            &optional link-header update-params
                                            hide-replies)
  "Set `mastodon-tl--buffer-spec' for the current buffer.
BUFFER is buffer name, ENDPOINT is buffer's enpoint,
UPDATE-FUNCTION is its update function.
LINK-HEADER is the http Link header if present.
UPDATE-PARAMS is any http parameters needed for the update function.
HIDE-REPLIES is a flag indicating if replies are hidden in the current buffer."
  (setq mastodon-tl--buffer-spec
        `(account ,(cons mastodon-active-user
                         mastodon-instance-url)
                  buffer-name ,buffer
                  endpoint ,endpoint
                  update-function ,update-function
                  link-header ,link-header
                  update-params ,update-params
                  hide-replies ,hide-replies)))


;;; BUFFERS

(defun mastodon-tl--get-buffer-type ()
  "Return a symbol descriptive of current mastodon buffer type.
Should work in all mastodon buffers.
Note that for many buffers, this requires `mastodon-tl--buffer-spec'
to be set. It is set for almost all buffers, but you still have to
call this function after it is set or use something else."
  (let ((endpoint-fun (mastodon-tl--get-endpoint nil :no-error))
        (buffer-name (mastodon-tl--buffer-name nil :no-error)))
    (cond (mastodon-toot-mode
           ;; composing/editing:
           (if (string= "*edit toot*" (buffer-name))
               'edit-toot
             'new-toot))
          ;; main timelines:
          ((string= "timelines/home" endpoint-fun)
           'home)
          ((string= "*mastodon-local*" buffer-name)
           'local)
          ((string= "timelines/public" endpoint-fun)
           'federated)
          ((string-prefix-p "timelines/tag/" endpoint-fun)
           'tag-timeline)
          ((string-prefix-p "timelines/list/" endpoint-fun)
           'list-timeline)
          ;; notifs:
          ((string-suffix-p "mentions*" buffer-name)
           'mentions)
          ((string= "notifications" endpoint-fun)
           'notifications)
          ;; threads:
          ((string-suffix-p "context" endpoint-fun)
           'thread)
          ((string-prefix-p "statuses" endpoint-fun)
           'single-status)
          ;; profiles:
          ((mastodon-tl--profile-buffer-p)
           (cond
            ;; own profile:
            ;; perhaps not needed, and needlessly confusing,
            ;; e.g. for `mastodon-profile--account-view-cycle':
            ;; ((equal (mastodon-tl--buffer-name)
            ;; (concat "*mastodon-" (mastodon-auth--get-account-name) "-statuses*"))
            ;; 'own-profile-statuses)
            ;; profile note:
            ((string-suffix-p "update-profile*" buffer-name)
             'update-profile-note)
            ;; posts inc. boosts:
            ((string-suffix-p "no-boosts*" buffer-name)
             'profile-statuses-no-boosts)
            ((string-suffix-p "statuses" endpoint-fun)
             'profile-statuses)
            ;; profile followers
            ((string-suffix-p "followers" endpoint-fun)
             'profile-followers)
            ;; profile following
            ((string-suffix-p "following" endpoint-fun)
             'profile-following)))
          ((string= "preferences" endpoint-fun)
           'preferences)
          ;; search
          ((string-suffix-p "search" endpoint-fun)
           'search)
          ;; trends
          ((equal "api/v1/trends/statuses" endpoint-fun)
           'trending-statuses)
          ((equal "api/v1/trends/tags" endpoint-fun)
           'trending-tags)
          ((equal "api/v1/trends/links" endpoint-fun)
           'trending-links)
          ;; User's views:
          ((string= "filters" endpoint-fun)
           'filters)
          ((string= "lists" endpoint-fun)
           'lists)
          ((string= "suggestions" endpoint-fun)
           'follow-suggestions)
          ((string= "favourites" endpoint-fun)
           'favourites)
          ((string= "bookmarks" endpoint-fun)
           'bookmarks)
          ((string= "follow_requests" endpoint-fun)
           'follow-requests)
          ((string= "scheduled_statuses" endpoint-fun)
           'scheduled-statuses)
          ;; instance description
          ((string= "instance" endpoint-fun)
           'instance-description)
          ((string= "*mastodon-toot-edits*" buffer-name)
           'toot-edits))))

(defun mastodon-tl--buffer-type-eq (type)
  "Return t if current buffer type is equal to symbol TYPE."
  (eq (mastodon-tl--get-buffer-type) type))

(defun mastodon-tl--profile-buffer-p ()
  "Return t if current buffer is a profile buffer of any kind.
This includes the update profile note buffer, but not the preferences one."
  (string-prefix-p "accounts" (mastodon-tl--get-endpoint nil :no-error)))

(defun mastodon-tl--timeline-proper-p ()
  "Return non-nil if the current buffer is a 'proper' timeline.
A proper timeline excludes notifications, threads, and other toot
buffers that aren't strictly mastodon timelines."
  (let ((timeline-buffers '(home federated local tag-timeline list-timeline profile-statuses)))
    (member (mastodon-tl--get-buffer-type) timeline-buffers)))

(defun mastodon-tl--hide-replies-p (&optional prefix)
  "Return non-nil if replies should be hidden in the timeline.
We hide replies if user explictly set the
`mastodon-tl--hide-replies' or used PREFIX combination to open a
timeline."
  (and
   ;; Only hide replies if we are in a proper timeline
   (mastodon-tl--timeline-proper-p)
   (or
    ;; User configured to hide replies
    mastodon-tl--hide-replies
    ;; Timeline called with C-u prefix
    (equal '(4) prefix))))


;;; UTILITIES

(defun mastodon-tl--map-alist (key alist)
  "Return a list of values extracted from ALIST with KEY.
Key is a symbol, as with `alist-get'."
  (mapcar (lambda (x)
            (alist-get key x))
          alist))

(defun mastodon-tl--map-alist-vals-to-alist (key1 key2 alist)
  "From ALIST, return an alist consisting of (val1 . val2) elements.
Values are accessed by `alist-get', using KEY1 and KEY2."
  (mapcar (lambda (x)
            (cons (alist-get key1 x)
                  (alist-get key2 x)))
          alist))

(defun mastodon-tl--symbol (name)
  "Return the unicode symbol (as a string) corresponding to NAME.
If symbol is not displayable, an ASCII equivalent is returned. If
NAME is not part of the symbol table, '?' is returned."
  (if-let* ((symbol (alist-get name mastodon-tl--symbols)))
      (if (char-displayable-p (string-to-char (car symbol)))
          (car symbol)
        (cdr symbol))
    "?"))

(defun mastodon-tl--set-face (string face)
  "Return the propertized STRING with the face property set to FACE."
  (propertize string 'face face))

(defun mastodon-tl--field (field toot)
  "Return FIELD from TOOT.
Return value from boosted content if available."
  (or (alist-get field (alist-get 'reblog toot))
      (alist-get field toot)))

(defun mastodon-tl--remove-html (toot)
  "Remove unrendered tags from TOOT."
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1)))
    (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))

(defun mastodon-tl--property (prop &optional no-move backward)
  "Get property PROP for toot at point.
Move forward (down) the timeline unless NO-MOVE is non-nil.
BACKWARD means move backward (up) the timeline."
  (if no-move
      (get-text-property (point) prop)
    (or (get-text-property (point) prop)
        (save-excursion
          (if backward
              (mastodon-tl--goto-prev-toot)
            (mastodon-tl--goto-next-toot))
          (get-text-property (point) prop)))))

(defun mastodon-tl--newest-id ()
  "Return toot-id from the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (mastodon-tl--property 'toot-id)))

(defun mastodon-tl--oldest-id ()
  "Return toot-id from the bottom of the buffer."
  (save-excursion
    (goto-char (point-max))
    (mastodon-tl--property 'toot-id nil :backward)))

(defun mastodon-tl--as-string (numeric)
  "Convert NUMERIC to string."
  (cond ((numberp numeric)
         (number-to-string numeric))
        ((stringp numeric) numeric)
        (t (error
            "Numeric:%s must be either a string or a number"
            numeric))))

(defun mastodon-tl--toot-id (json)
  "Find approproiate toot id in JSON.
If the toot has been boosted use the id found in the
reblog portion of the toot.  Otherwise, use the body of
the toot.  This is the same behaviour as the mastodon.social
webapp"
  (let ((id (alist-get 'id json))
        (reblog (alist-get 'reblog json)))
    (if reblog (alist-get 'id reblog) id)))

(defun mastodon-tl--toot-or-base (json)
  "Return the base toot or just the toot from toot JSON."
  (or (alist-get 'reblog json) json))


;;; THREADS

(defun mastodon-tl--single-toot (id)
  "View toot at point in separate buffer.
ID is that of the toot to view."
  (interactive)
  (let* ((buffer (format "*mastodon-toot-%s*" id))
         (toot (mastodon-http--get-json
                (mastodon-http--api (concat "statuses/" id)))))
    (if (equal (caar toot) 'error)
        (message "Error: %s" (cdar toot))
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (switch-to-buffer buffer)
          (mastodon-mode)
          (mastodon-tl--set-buffer-spec buffer
                                        (format "statuses/%s" id)
                                        nil)
          (let ((inhibit-read-only t))
            (mastodon-tl--toot toot :detailed-p)))))))

(defun mastodon-tl--view-whole-thread ()
  "From a thread view, view entire thread.
If you load a thread from a toot, only the branches containing
are displayed by default. Call this if you subsequently want to
view all branches of a thread."
  (interactive)
  (if (not (eq (mastodon-tl--get-buffer-type) 'thread))
      (error "You need to be viewing a thread to call this")
    (goto-char (point-min))
    (let ((id (mastodon-tl--property 'base-toot-id)))
      (mastodon-tl--thread id))))

(defun mastodon-tl--thread (&optional id)
  "Open thread buffer for toot at point or with ID."
  (interactive)
  (let* ((id (or id (mastodon-tl--property 'base-toot-id :no-move)))
         (type (mastodon-tl--field 'type (mastodon-tl--property 'toot-json :no-move))))
    (if (or (string= type "follow_request")
            (string= type "follow")) ; no can thread these
        (error "No thread")
      (let* ((endpoint (format "statuses/%s/context" id))
             (url (mastodon-http--api endpoint))
             (buffer (format "*mastodon-thread-%s*" id))
             (toot
              ;; refetch current toot in case we just faved/boosted:
              (mastodon-http--get-json
               (mastodon-http--api (concat "statuses/" id))
               nil
               :silent))
             (context (mastodon-http--get-json url nil :silent))
             (marker (make-marker)))
        (if (equal (caar toot) 'error)
            (message "Error: %s" (cdar toot))
          (when (member (alist-get 'type toot) '("reblog" "favourite"))
            (setq toot (alist-get 'status toot)))
          (if (> (+ (length (alist-get 'ancestors context))
                    (length (alist-get 'descendants context)))
                 0)
              ;; if we have a thread:
              (progn
                (with-current-buffer (get-buffer-create buffer)
                  (let ((inhibit-read-only t))
                    (switch-to-buffer buffer)
                    (erase-buffer)
                    (mastodon-mode)
                    (mastodon-tl--set-buffer-spec buffer
                                                  endpoint
                                                  #'mastodon-tl--thread)
                    (mastodon-tl--timeline (alist-get 'ancestors context))
                    (goto-char (point-max))
                    (move-marker marker (point))
                    ;; print re-fetched toot:
                    (mastodon-tl--toot toot :detailed-p)
                    (mastodon-tl--timeline (alist-get 'descendants context))))
                ;; put point at the toot:
                (goto-char (marker-position marker))
                (mastodon-tl--goto-next-toot))
            ;; else just print the lone toot:
            (mastodon-tl--single-toot id)))))))

(defun mastodon-tl--mute-thread ()
  "Mute the thread displayed in the current buffer.
Note that you can only (un)mute threads you have posted in."
  (interactive)
  (mastodon-tl--mute-or-unmute-thread))

(defun mastodon-tl--unmute-thread ()
  "Mute the thread displayed in the current buffer.
Note that you can only (un)mute threads you have posted in."
  (interactive)
  (mastodon-tl--mute-or-unmute-thread :unmute))

(defun mastodon-tl--mute-or-unmute-thread  (&optional unmute)
  "Mute a thread.
If UNMUTE, unmute it."
  (let ((endpoint (mastodon-tl--get-endpoint)))
    (if (mastodon-tl--buffer-type-eq 'thread)
        (let* ((id
                (save-match-data
                  (string-match "statuses/\\(?2:[[:digit:]]+\\)/context"
                                endpoint)
                  (match-string 2 endpoint)))
               (we-posted-p (mastodon-tl--user-in-thread-p id))
               (url (mastodon-http--api
                     (if unmute
                         (format "statuses/%s/unmute" id)
                       (format "statuses/%s/mute" id)))))
          (if (not we-posted-p)
              (message "You can only (un)mute a thread you have posted in.")
            (when (if unmute
                      (y-or-n-p "Unnute this thread? ")
                    (y-or-n-p "Mute this thread? "))
              (let ((response (mastodon-http--post url)))
                (mastodon-http--triage response
                                       (lambda ()
                                         (if unmute
                                             (message "Thread unmuted!")
                                           (message "Thread muted!")))))))))))

(defun mastodon-tl--map-account-id-from-toot (statuses)
  "Return a list of the account IDs of the author of each toot in STATUSES."
  (mapcar (lambda (status)
            (alist-get 'id
                       (alist-get 'account status)))
          statuses))

(defun mastodon-tl--user-in-thread-p (id)
  "Return non-nil if the logged-in user has posted to the current thread.
ID is that of the post the context is currently displayed for."
  (let* ((context-json (mastodon-http--get-json
                        (mastodon-http--api (format "statuses/%s/context" id))
                        nil :silent))
         (ancestors (alist-get 'ancestors context-json))
         (descendants (alist-get 'descendants context-json))
         (a-ids (mastodon-tl--map-account-id-from-toot ancestors))
         (d-ids (mastodon-tl--map-account-id-from-toot descendants)))
    (or (member (mastodon-auth--get-account-id) a-ids)
        (member (mastodon-auth--get-account-id) d-ids))))


;;; FOLLOW/BLOCK/MUTE, ETC

(defmacro mastodon-tl--do-if-toot (&rest body)
  "Execute BODY if we have a toot or user at point."
  (declare (debug t))
  `(if (and (not (mastodon-tl--profile-buffer-p))
            (not (mastodon-tl--property 'toot-json))) ; includes user listings
       (message "Looks like there's no toot or user at point?")
     ,@body))

(defmacro mastodon-tl--do-if-toot-strict (&rest body)
  "Execute BODY if we have a toot, and only a toot, at point."
  (declare (debug t))
  `(if (not (mastodon-tl--property 'toot-id :no-move))
       (message "Looks like there's no toot at point?")
     ,@body))

(defun mastodon-tl--follow-user (user-handle &optional notify langs)
  "Query for USER-HANDLE from current status and follow that user.
If NOTIFY is \"true\", enable notifications when that user posts.
If NOTIFY is \"false\", disable notifications when that user posts.
Can be called to toggle NOTIFY on users already being followed.
LANGS is an array parameters alist of languages to filer user's posts by."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "follow")))
  (mastodon-tl--do-if-toot
   (mastodon-tl--do-user-action-and-response
    user-handle "follow" nil notify langs)))

(defun mastodon-tl--enable-notify-user-posts (user-handle)
  "Query for USER-HANDLE and enable notifications when they post."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "enable")))
  (mastodon-tl--do-if-toot
   (mastodon-tl--follow-user user-handle "true")))

(defun mastodon-tl--disable-notify-user-posts (user-handle)
  "Query for USER-HANDLE and disable notifications when they post."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "disable")))
  (mastodon-tl--follow-user user-handle "false"))

(defun mastodon-tl--filter-user-user-posts-by-language (user-handle)
  "Query for USER-HANDLE and enable notifications when they post.
This feature is experimental and for now not easily varified by
the instance API."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "filter by language")))
  (let ((langs (mastodon-tl--read-filter-langs)))
    (mastodon-tl--do-if-toot
     (mastodon-tl--follow-user user-handle nil langs))))

(defun mastodon-tl--read-filter-langs (&optional langs)
  "Read language choices and return an alist array parameter.
LANGS is the accumulated array param alist if we re-run recursively."
  (let* ((langs-alist langs)
         (choice (completing-read "Filter user's posts by language: "
                                  mastodon-iso-639-1)))
    (when choice
      (setq langs-alist
            (push `("languages[]" . ,(alist-get choice mastodon-iso-639-1
                                                nil nil
                                                #'string=))
                  langs-alist))
      (if (y-or-n-p "Filter by another language? ")
          (mastodon-tl--read-filter-langs langs-alist)
        langs-alist))))

(defun mastodon-tl--unfollow-user (user-handle)
  "Query for USER-HANDLE from current status and unfollow that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "unfollow")))
  (mastodon-tl--do-if-toot
   (mastodon-tl--do-user-action-and-response user-handle "unfollow" t)))

(defun mastodon-tl--block-user (user-handle)
  "Query for USER-HANDLE from current status and block that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "block")))
  (mastodon-tl--do-if-toot
   (mastodon-tl--do-user-action-and-response user-handle "block")))

(defun mastodon-tl--unblock-user (user-handle)
  "Query for USER-HANDLE from list of blocked users and unblock that user."
  (interactive
   (list
    (mastodon-tl--interactive-blocks-or-mutes-list-get "unblock")))
  (if (not user-handle)
      (message "Looks like you have no blocks to unblock!")
    (mastodon-tl--do-user-action-and-response user-handle "unblock" t)))

(defun mastodon-tl--mute-user (user-handle)
  "Query for USER-HANDLE from current status and mute that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "mute")))
  (mastodon-tl--do-if-toot
   (mastodon-tl--do-user-action-and-response user-handle "mute")))

(defun mastodon-tl--unmute-user (user-handle)
  "Query for USER-HANDLE from list of muted users and unmute that user."
  (interactive
   (list
    (mastodon-tl--interactive-blocks-or-mutes-list-get "unmute")))
  (if (not user-handle)
      (message "Looks like you have no mutes to unmute!")
    (mastodon-tl--do-user-action-and-response user-handle "unmute" t)))

(defun mastodon-tl--dm-user (user-handle)
  "Query for USER-HANDLE from current status and compose a message to that user."
  (interactive
   (list
    (mastodon-tl--interactive-user-handles-get "message")))
  (mastodon-tl--do-if-toot
   (mastodon-toot--compose-buffer (concat "@" user-handle))
   (setq mastodon-toot--visibility "direct")
   (mastodon-toot--update-status-fields)))

(defun mastodon-tl--interactive-user-handles-get (action)
  "Get the list of user-handles for ACTION from the current toot."
  (mastodon-tl--do-if-toot
   (let ((user-handles
          (cond ((or (mastodon-tl--buffer-type-eq 'follow-suggestions)
                     ;; follow suggests / search / foll requests compat:
                     (mastodon-tl--buffer-type-eq 'search)
                     (mastodon-tl--buffer-type-eq 'follow-requests)
                     ;; profile view follows/followers compat:
                     ;; but not for profile statuses:
                     ;; fetch 'toot-json:
                     (mastodon-tl--buffer-type-eq 'profile-followers)
                     (mastodon-tl--buffer-type-eq 'profile-following))
                 (list (alist-get 'acct
                                  (mastodon-tl--property 'toot-json :no-move))))
                ;; profile view, no toots
                ;; needed for e.g. gup.pe groups which show no toots publically:
                ((mastodon-tl--profile-buffer-p)
                 (list (alist-get 'acct
                                  (mastodon-profile--profile-json))))
                (t
                 (mastodon-profile--extract-users-handles
                  (mastodon-profile--toot-json))))))
     ;; return immediately if only 1 handle:
     (if (eq 1 (length user-handles))
         (car user-handles)
       (completing-read (if (or (equal action "disable")
                                (equal action "enable"))
                            (format "%s notifications when user posts: " action)
                          (format "Handle of user to %s: " action))
                        user-handles
                        nil ; predicate
                        'confirm)))))

(defun mastodon-tl--interactive-blocks-or-mutes-list-get (action)
  "Fetch the list of accounts for ACTION from the server.
Action must be either \"unblock\" or \"unmute\"."
  (let* ((endpoint (cond ((equal action "unblock")
                          "blocks")
                         ((equal action "unmute")
                          "mutes")))
         (url (mastodon-http--api endpoint))
         (json (mastodon-http--get-json url))
         (accts (mastodon-tl--map-alist 'acct json)))
    (when accts
      (completing-read (format "Handle of user to %s: " action)
                       accts
                       nil ; predicate
                       t))))

(defun mastodon-tl--do-user-action-and-response (user-handle action &optional negp notify langs)
  "Do ACTION on user USER-HANDLE.
NEGP is whether the action involves un-doing something.
If NOTIFY is \"true\", enable notifications when that user posts.
If NOTIFY is \"false\", disable notifications when that user posts.
NOTIFY is only non-nil when called by `mastodon-tl--follow-user'.
LANGS is an array parameters alist of languages to filer user's posts by."
  (let* ((account (if negp
                      ;; if unmuting/unblocking, we got handle from mute/block list
                      (mastodon-profile--search-account-by-handle
                       user-handle)
                    ;; if profile view, use 'profile-json as status:
                    (if (mastodon-tl--profile-buffer-p)
                        (mastodon-profile--lookup-account-in-status
                         user-handle (mastodon-profile--profile-json))
                      ;; if muting/blocking, we select from handles in current status
                      (mastodon-profile--lookup-account-in-status
                       user-handle (mastodon-profile--toot-json)))))
         (user-id (mastodon-profile--account-field account 'id))
         (name (if (not (string-empty-p (mastodon-profile--account-field account 'display_name)))
                   (mastodon-profile--account-field account 'display_name)
                 (mastodon-profile--account-field account 'username)))
         (args (cond (notify
                      `(("notify" . ,notify)))
                     (langs langs)
                     (t nil)))
         (url (mastodon-http--api (format "accounts/%s/%s" user-id action))))
    (if account
        (if (equal action "follow") ; y-or-n for all but follow
            (mastodon-tl--do-user-action-function url name user-handle action notify args)
          (when (y-or-n-p (format "%s user %s? " action name))
            (mastodon-tl--do-user-action-function url name user-handle action args)))
      (message "Cannot find a user with handle %S" user-handle))))

(defun mastodon-tl--do-user-action-function (url name user-handle action &optional notify args)
  "Post ACTION on user NAME/USER-HANDLE to URL.
NOTIFY is either \"true\" or \"false\", and used when we have been called
by `mastodon-tl--follow-user' to enable or disable notifications.
ARGS is an alist of any parameters to send with the request."
  (let ((response (mastodon-http--post url args)))
    (mastodon-http--triage
     response
     (lambda ()
       (cond ((string-equal notify "true")
              (message "Receiving notifications for user %s (@%s)!"
                       name user-handle))
             ((string-equal notify "false")
              (message "Not receiving notifications for user %s (@%s)!"
                       name user-handle))
             ((or (string-equal action "mute")
                  (string-equal action "unmute"))
              (message "User %s (@%s) %sd!" name user-handle action))
             ((assoc "languages[]" args #'equal)
              (message "User %s filtered by language(s): %s" name
                       (mapconcat #'cdr args " ")))
             ((eq notify nil)
              (message "User %s (@%s) %sed!" name user-handle action)))))))


;; FOLLOW TAGS

(defun mastodon-tl--follow-tag (&optional tag)
  "Prompt for a tag and follow it.
If TAG provided, follow it."
  (interactive)
  (let* ((tag (or tag (read-string "Tag to follow: ")))
         (url (mastodon-http--api (format "tags/%s/follow" tag)))
         (response (mastodon-http--post url)))
    (mastodon-http--triage response
                           (lambda ()
                             (message "tag #%s followed!" tag)))))

(defun mastodon-tl--followed-tags ()
  "Return JSON of tags followed."
  (let ((url (mastodon-http--api (format "followed_tags"))))
    (mastodon-http--get-json url)))

(defun mastodon-tl--unfollow-tag (&optional tag)
  "Prompt for a followed tag, and unfollow it.
If TAG is provided, unfollow it."
  (interactive)
  (let* ((followed-tags-json (unless tag (mastodon-tl--followed-tags)))
         (tags (unless tag
                 (mastodon-tl--map-alist 'name followed-tags-json)))
         (tag (or tag (completing-read "Unfollow tag: "
                                       tags)))
         (url (mastodon-http--api (format "tags/%s/unfollow" tag)))
         (response (mastodon-http--post url)))
    (mastodon-http--triage response
                           (lambda ()
                             (message "tag #%s unfollowed!" tag)))))

(defun mastodon-tl--list-followed-tags (&optional prefix)
  "List followed tags. View timeline of tag user choses.
PREFIX is sent to `mastodon-tl--get-tag-timeline', which see."
  (interactive "p")
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (mastodon-tl--map-alist 'name followed-tags-json))
         (tag (completing-read "Tag: " tags nil)))
    (if (null tag)
        (message "You have to follow some tags first.")
      (mastodon-tl--get-tag-timeline prefix tag))))

(defun mastodon-tl--followed-tags-timeline (&optional prefix)
  "Open a timeline of all your followed tags.
PREFIX is sent to `mastodon-tl--show-tag-timeline', which see."
  (interactive "p")
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (mastodon-tl--map-alist 'name followed-tags-json)))
    (mastodon-tl--show-tag-timeline prefix tags)))

(defun mastodon-tl--some-followed-tags-timeline (&optional prefix)
  "Prompt for some tags, and open a timeline for them.
The suggestions are from followed tags, but any other tags are also allowed."
  (interactive "p")
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (mastodon-tl--map-alist 'name followed-tags-json))
         (selection (completing-read-multiple
                     "Tags' timelines to view [TAB to view, comma to separate]: "
                     tags)))
    (mastodon-tl--show-tag-timeline prefix selection)))


;;; REPORT TO MODERATORS

(defun mastodon-tl--instance-rules ()
  "Return the rules of the user's instance."
  (let ((url (mastodon-http--api "instance/rules")))
    (mastodon-http--get-json url nil :silent)))

(defun mastodon-tl--report-params (account toot)
  "Query user and return report params alist.
ACCOUNT and TOOT are the data to use."
  (let* ((account-id (mastodon-profile--account-field account 'id))
         (comment (read-string "Add comment [optional]: "))
         (toot-id (when (y-or-n-p "Also report status at point? ")
                    (mastodon-tl--toot-id toot))) ; base toot if poss
         (forward-p (when (y-or-n-p "Forward to remote admin? ") "true"))
         (rules (when (y-or-n-p "Cite a rule broken? ")
                  (mastodon-tl--read-rules-ids)))
         (cat (unless rules (if (y-or-n-p "Spam? ") "spam" "other"))))
    (mastodon-tl--report-build-params account-id comment toot-id
                                      forward-p cat rules)))

(defun mastodon-tl--report-build-params
    (account-id comment toot-id forward-p cat &optional rules)
  "Build the parameters alist based on user responses.
ACCOUNT-ID, COMMENT, TOOT-ID, FORWARD-P, CAT, and RULES are all from
`mastodon-tl--report-params', which see."
  (let ((params `(("account_id" . ,account-id)
                  ,(when comment
                     `("comment" . ,comment))
                  ,(when toot-id
                     `("status_ids[]" . ,toot-id))
                  ,(when forward-p
                     `("forward" . ,forward-p))
                  ,(when cat
                     `("category" . ,cat)))))
    (when rules
      (let ((alist
             (mastodon-http--build-array-params-alist "rule_ids[]" rules)))
        (mapc (lambda (x)
                (push x params))
              alist)))
    ;; FIXME: the above approach adds nils to your params.
    (setq params (delete nil params))
    params))

(defun mastodon-tl--report-to-mods ()
  "Report the author of the toot at point to your instance moderators.
Optionally report the toot at point, add a comment, cite rules
that have been broken, forward the report to the remove admin,
report the account for spam."
  (interactive)
  (mastodon-tl--do-if-toot
   (when (y-or-n-p "Report author of toot at point?")
     (let* ((url (mastodon-http--api "reports"))
            (toot (mastodon-tl--toot-or-base
                   (mastodon-tl--property 'toot-json :no-move)))
            (account (alist-get 'account toot))
            (handle (alist-get 'acct account))
            (params (mastodon-tl--report-params account toot))
            (response (mastodon-http--post url params)))
       ;; (setq masto-report-response response)
       (mastodon-http--triage response
                              (lambda ()
                                (message "User %s reported!" handle)))))))

(defvar crm-separator)

(defun mastodon-tl--map-rules-alist (rules)
  (mapcar (lambda (x)
            (let-alist x
              `(,.text . ,.id)))
          rules))

(defun mastodon-tl--read-rules-ids ()
  "Prompt for a list of instance rules and return a list of selected ids."
  (let* ((rules (mastodon-tl--instance-rules))
         (alist (mastodon-tl--map-rules-alist rules))
         (crm-separator (replace-regexp-in-string "," "|" crm-separator))
         (choices (completing-read-multiple
                   "rules [TAB for options, | to separate]: "
                   alist nil :match)))
    (mapcar (lambda (x)
              (alist-get x alist nil nil 'equal))
            choices)))


;;; UPDATING, etc.

(defun mastodon-tl--more-json (endpoint id)
  "Return JSON for timeline ENDPOINT before ID."
  (let* ((args `(("max_id" . ,(mastodon-tl--as-string id))))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url args)))

(defun mastodon-tl--more-json-async (endpoint id &optional params callback &rest cbargs)
  "Return JSON for timeline ENDPOINT before ID.
Then run CALLBACK with arguments CBARGS.
PARAMS is used to send any parameters needed to correctly update
the current view."
  (let* ((args `(("max_id" . ,(mastodon-tl--as-string id))))
         (args (if params (push (car args) params) args))
         (url (mastodon-http--api endpoint)))
    (apply #'mastodon-http--get-json-async url args callback cbargs)))

(defun mastodon-tl--updated-json (endpoint id &optional params)
  "Return JSON for timeline ENDPOINT since ID.
PARAMS is used to send any parameters needed to correctly update
the current view."
  (let* ((args `(("since_id" . ,(mastodon-tl--as-string id))))
         (args (if params (push (car args) params) args))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url args)))

;; TODO: add this to new posts in some cases, e.g. in thread view.
(defun mastodon-tl--reload-timeline-or-profile (&optional pos)
  "Reload the current timeline or profile page.
For use after e.g. deleting a toot.
POS is a number, where point will be placed."
  (let ((type (mastodon-tl--get-buffer-type)))
    (cond ((eq type 'home)
           (mastodon-tl--get-home-timeline))
          ((eq type 'federated)
           (mastodon-tl--get-federated-timeline))
          ((eq type 'local)
           (mastodon-tl--get-local-timeline))
          ((eq type 'mentions)
           (mastodon-notifications--get-mentions))
          ((eq type 'notifications)
           (mastodon-notifications-get nil nil :force))
          ((eq type 'profile-statuses-no-boosts)
           (mastodon-profile--open-statuses-no-reblogs))
          ((eq type 'profile-statuses)
           (mastodon-profile--my-profile))
          ((eq type 'thread)
           (save-match-data
             (let ((endpoint (mastodon-tl--get-endpoint)))
               (string-match
                "statuses/\\(?2:[[:digit:]]+\\)/context"
                endpoint)
               (mastodon-tl--thread
                (match-string 2 endpoint))))))
    ;; TODO: sends point to POS, which was where point was in buffer before reload. This is very rough; we may have removed an item (deleted a toot, cleared a notif), so the buffer will be smaller, point will end up past where we were, etc.
    (when pos
      (goto-char pos)
      (mastodon-tl--goto-prev-item))))

(defun mastodon-tl--build-link-header-url (str)
  "Return a URL from STR, an http Link header."
  (let* ((split (split-string str "; "))
         (url-base (string-trim (car split) "<" ">"))
         (param (cadr split)))
    (concat url-base "&" param)))

(defun mastodon-tl--use-link-header-p ()
  "Return t if we are in a view needing Link header pagination.
Currently this includes favourites, bookmarks, and profile pages
when showing followers or accounts followed."
  (or (mastodon-tl--buffer-type-eq 'favourites)
      (mastodon-tl--buffer-type-eq 'bookmarks)
      (mastodon-tl--buffer-type-eq 'profile-followers)
      (mastodon-tl--buffer-type-eq 'profile-following)))

(defun mastodon-tl--get-link-header-from-response (headers)
  "Get http Link header from list of http HEADERS."
  ;; pleroma uses "link", so case-insensitive match required:
  (when-let ((link-headers (alist-get "Link" headers nil nil 'cl-equalp)))
    (split-string link-headers ", ")))

(defun mastodon-tl--more ()
  "Append older toots to timeline, asynchronously."
  (interactive)
  (message "Loading older toots...")
  (if (mastodon-tl--use-link-header-p)
      ;; link-header: can't build a URL with --more-json-async, endpoint/id:
      ;; ensure we have a "next" type here, otherwise the CAR will be the
      ;; "prev" type!
      (let ((link-header (mastodon-tl--link-header)))
        (if (> 2 (length link-header))
            (error "No next page")
          (let* ((next (car link-header))
                 ;;(prev (cadr (mastodon-tl--link-header)))
                 (url (mastodon-tl--build-link-header-url next)))
            (mastodon-http--get-response-async url nil 'mastodon-tl--more* (current-buffer)
                                               (point) :headers))))
    (mastodon-tl--more-json-async
     (mastodon-tl--get-endpoint)
     (mastodon-tl--oldest-id)
     (mastodon-tl--update-params)
     'mastodon-tl--more* (current-buffer) (point))))

(defun mastodon-tl--more* (response buffer point-before &optional headers)
  "Append older toots to timeline, asynchronously.
Runs the timeline's update function on RESPONSE, in BUFFER.
When done, places point at POINT-BEFORE.
HEADERS is the http headers returned in the response, if any."
  (with-current-buffer buffer
    (when response
      (let* ((inhibit-read-only t)
             (json (if headers (car response) response))
             (headers (if headers (cdr response) nil))
             (link-header (mastodon-tl--get-link-header-from-response headers)))
        (goto-char (point-max))
        (if (eq (mastodon-tl--get-buffer-type) 'thread)
            ;; if thread view, call --thread with parent ID
            (progn (goto-char (point-min))
                   (mastodon-tl--goto-next-toot)
                   (funcall (mastodon-tl--get-update-function)))
          (funcall (mastodon-tl--get-update-function) json))
        (goto-char point-before)
        ;; update buffer spec to new link-header:
        ;; (other values should just remain as they were)
        (when headers
          (mastodon-tl--set-buffer-spec (mastodon-tl--buffer-name)
                                        (mastodon-tl--get-endpoint)
                                        (mastodon-tl--get-update-function)
                                        link-header))
        (message "Loading older toots... done.")))))

(defun mastodon-tl--find-property-range (property start-point &optional search-backwards)
  "Return `nil` if no such range is found.
If PROPERTY is set at START-POINT returns a range around
START-POINT otherwise before/after START-POINT.
SEARCH-BACKWARDS determines whether we pick point
before (non-nil) or after (nil)"
  (if (get-text-property start-point property)
      ;; We are within a range, so look backwards for the start:
      (cons (previous-single-property-change
             (if (equal start-point (point-max)) start-point (1+ start-point))
             property nil (point-min))
            (next-single-property-change start-point property nil (point-max)))
    (if search-backwards
        (let* ((end (or (previous-single-property-change
                         (if (equal start-point (point-max))
                             start-point (1+ start-point))
                         property)
                        ;; we may either be just before the range or there
                        ;; is nothing at all
                        (and (not (equal start-point (point-min)))
                             (get-text-property (1- start-point) property)
                             start-point)))
               (start (and
                       end
                       (previous-single-property-change end property nil (point-min)))))
          (when end
            (cons start end)))
      (let* ((start (next-single-property-change start-point property))
             (end (and start
                       (next-single-property-change start property nil (point-max)))))
        (when start
          (cons start end))))))

(defun mastodon-tl--find-next-or-previous-property-range
    (property start-point search-backwards)
  "Find (start . end) property range after/before START-POINT.
Does so while PROPERTY is set to a consistent value (different
from the value at START-POINT if that is set).
Return nil if no such range exists.
If SEARCH-BACKWARDS is non-nil it find a region before
START-POINT otherwise after START-POINT."
  (if (get-text-property start-point property)
      ;; We are within a range, we need to start the search from
      ;; before/after this range:
      (let ((current-range (mastodon-tl--find-property-range property start-point)))
        (if search-backwards
            (unless (equal (car current-range) (point-min))
              (mastodon-tl--find-property-range
               property (1- (car current-range)) search-backwards))
          (unless (equal (cdr current-range) (point-max))
            (mastodon-tl--find-property-range
             property (1+ (cdr current-range)) search-backwards))))
    ;; If we are not within a range, we can just defer to
    ;; mastodon-tl--find-property-range directly.
    (mastodon-tl--find-property-range property start-point search-backwards)))

(defun mastodon-tl--consider-timestamp-for-updates (timestamp)
  "Take note that TIMESTAMP is used in buffer and ajust timers as needed.
This calculates the next time the text for TIMESTAMP will change
and may adjust existing or future timer runs should that time
before current plans to run the update function.
The adjustment is only made if it is significantly (a few
seconds) before the currently scheduled time. This helps reduce
the number of occasions where we schedule an update only to
schedule the next one on completion to be within a few seconds.
If relative timestamps are disabled (i.e. if
`mastodon-tl--enable-relative-timestamps' is nil), this is a
no-op."
  (when mastodon-tl--enable-relative-timestamps
    (let ((this-update (cdr (mastodon-tl--relative-time-details timestamp))))
      (when (time-less-p this-update
                         (time-subtract mastodon-tl--timestamp-next-update
                                        (seconds-to-time 10)))
        (setq mastodon-tl--timestamp-next-update this-update)
        (when mastodon-tl--timestamp-update-timer
          ;; We need to re-schedule for an earlier time
          (cancel-timer mastodon-tl--timestamp-update-timer)
          (setq mastodon-tl--timestamp-update-timer
                (run-at-time (time-to-seconds (time-subtract this-update (current-time)))
                             nil ;; don't repeat
                             #'mastodon-tl--update-timestamps-callback
                             (current-buffer) nil)))))))

(defun mastodon-tl--update-timestamps-callback (buffer previous-marker)
  "Update the next few timestamp displays in BUFFER.
Start searching for more timestamps from PREVIOUS-MARKER or
from the start if it is nil."
  ;; only do things if the buffer hasn't been killed in the meantime
  (when (and mastodon-tl--enable-relative-timestamps ;; should be true but just in case...
             (buffer-live-p buffer))
    (save-excursion
      (with-current-buffer buffer
        (let ((previous-timestamp (if previous-marker
                                      (marker-position previous-marker)
                                    (point-min)))
              (iteration 0)
              next-timestamp-range)
          (if previous-marker
              ;; This is a follow-up call to process the next batch of
              ;; timestamps.
              ;; Release the marker to not slow things down.
              (set-marker previous-marker nil)
            ;; Otherwise this is a rew run, so let's initialize the next-run time.
            (setq mastodon-tl--timestamp-next-update (time-add (current-time)
                                                               (seconds-to-time 300))
                  mastodon-tl--timestamp-update-timer nil))
          (while (and (< iteration 5)
                      (setq next-timestamp-range
                            (mastodon-tl--find-property-range 'timestamp
                                                              previous-timestamp)))
            (let* ((start (car next-timestamp-range))
                   (end (cdr next-timestamp-range))
                   (timestamp (get-text-property start 'timestamp))
                   (current-display (get-text-property start 'display))
                   (new-display (mastodon-tl--relative-time-description timestamp)))
              (unless (string= current-display new-display)
                (let ((inhibit-read-only t))
                  (add-text-properties
                   start end (list 'display
                                   (mastodon-tl--relative-time-description timestamp)))))
              (mastodon-tl--consider-timestamp-for-updates timestamp)
              (setq iteration (1+ iteration)
                    previous-timestamp (1+ (cdr next-timestamp-range)))))
          (if next-timestamp-range
              ;; schedule the next batch from the previous location to
              ;; start very soon in the future:
              (run-at-time 0.1 nil #'mastodon-tl--update-timestamps-callback buffer
                           (copy-marker previous-timestamp))
            ;; otherwise we are done for now; schedule a new run for when needed
            (setq mastodon-tl--timestamp-update-timer
                  (run-at-time (time-to-seconds
                                (time-subtract mastodon-tl--timestamp-next-update
                                               (current-time)))
                               nil ;; don't repeat
                               #'mastodon-tl--update-timestamps-callback
                               buffer nil))))))))

(defun mastodon-tl--set-after-update-marker ()
  "Set `mastodon-tl--after-update-marker' to the after-update location.
This location is defined by a non-nil value of
`mastodon-tl-position-after-update'."
  (if mastodon-tl-position-after-update
      (let ((marker (make-marker)))
        (set-marker marker
                    (cond
                     ((eq 'keep-point mastodon-tl-position-after-update)
                      (point))
                     ((eq 'last-old-toot mastodon-tl-position-after-update)
                      (next-single-property-change
                       (or mastodon-tl--update-point (point-min))
                       'byline))
                     (t
                      (error "Unknown mastodon-tl-position-after-update value %S"
                             mastodon-tl-position-after-update))))
        ;; Make the marker advance if text gets inserted there.
        (set-marker-insertion-type marker t)
        (setq mastodon-tl--after-update-marker marker))
    (setq mastodon-tl--after-update-marker nil)))

(defun mastodon-tl--update ()
  "Update timeline with new toots."
  (interactive)
  (let* ((endpoint (mastodon-tl--get-endpoint))
         (update-function (mastodon-tl--get-update-function))
         (thread-id (mastodon-tl--property 'toot-id)))
    ;; update a thread, without calling `mastodon-tl--updated-json':
    (if (mastodon-tl--buffer-type-eq 'thread)
        (funcall update-function thread-id)
      ;; update other timelines:
      (let* ((id (mastodon-tl--newest-id))
             (params (mastodon-tl--update-params))
             (json (mastodon-tl--updated-json endpoint id params)))
        (if json
            (let ((inhibit-read-only t))
              (mastodon-tl--set-after-update-marker)
              (goto-char (or mastodon-tl--update-point (point-min)))
              (funcall update-function json)
              (when mastodon-tl--after-update-marker
                (goto-char mastodon-tl--after-update-marker)))
          (message "nothing to update"))))))


;;; LOADING TIMELINES

(defun mastodon-tl--init (buffer-name endpoint update-function
                                      &optional headers params hide-replies)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT asynchronously.
UPDATE-FUNCTION is used to recieve more toots.
HEADERS means to also collect the response headers. Used for paginating
favourites and bookmarks.
PARAMS is any parameters to send with the request.
HIDE-REPLIES is a flag indicating if replies are hidden in the current buffer."
  (let ((url (mastodon-http--api endpoint))
        (buffer (concat "*mastodon-" buffer-name "*")))
    (if headers
        (mastodon-http--get-response-async url params
                                           'mastodon-tl--init* buffer endpoint update-function
                                           headers params hide-replies)
      (mastodon-http--get-json-async url params
                                     'mastodon-tl--init* buffer endpoint update-function nil
                                     params hide-replies))))

(defun mastodon-tl--init* (response buffer endpoint update-function
                                    &optional headers update-params hide-replies)
  "Initialize BUFFER with timeline targeted by ENDPOINT.
UPDATE-FUNCTION is used to recieve more toots.
RESPONSE is the data returned from the server by
`mastodon-http--process-json', with arg HEADERS a cons cell of
JSON and http headers, without it just the JSON."
  (let ((json (if headers (car response) response)))
    (if (not json) ; praying this is right here, else try "\n[]"
	(message "Looks like nothing returned from endpoint: %s" endpoint)
      (let* ((headers (if headers (cdr response) nil))
             (link-header (mastodon-tl--get-link-header-from-response headers)))
        (with-current-buffer (get-buffer-create buffer)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (switch-to-buffer buffer)
            ;; mastodon-mode wipes buffer-spec, so order must unforch be:
            ;; 1 run update-function, 2 enable masto-mode, 3 set buffer spec.
            ;; which means we cannot use buffer-spec for update-function
            ;; unless we set it both before and after the others
            (mastodon-tl--set-buffer-spec buffer
                                          endpoint
                                          update-function
                                          link-header
                                          update-params
                                          hide-replies)
            (setq
             ;; Initialize with a minimal interval; we re-scan at least once
             ;; every 5 minutes to catch any timestamps we may have missed
             mastodon-tl--timestamp-next-update (time-add (current-time)
                                                          (seconds-to-time 300)))
            (funcall update-function json)
            (mastodon-mode)
            (mastodon-tl--set-buffer-spec buffer
                                          endpoint
                                          update-function
                                          link-header
                                          update-params
                                          hide-replies)
            (setq mastodon-tl--timestamp-update-timer
                  (when mastodon-tl--enable-relative-timestamps
                    (run-at-time (time-to-seconds
                                  (time-subtract mastodon-tl--timestamp-next-update
                                                 (current-time)))
                                 nil ;; don't repeat
                                 #'mastodon-tl--update-timestamps-callback
                                 (current-buffer)
                                 nil)))
            (unless (mastodon-tl--profile-buffer-p)
              (mastodon-tl--goto-first-item))))))))

(defun mastodon-tl--init-sync (buffer-name endpoint update-function &optional note-type)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT.
UPDATE-FUNCTION is used to receive more toots.
Runs synchronously.
Optional arg NOTE-TYPE means only get that type of note."
  (let* ((exclude-types (when note-type
                          (mastodon-notifications--filter-types-list note-type)))
         (args (when note-type (mastodon-http--build-array-params-alist
                                "exclude_types[]" exclude-types)))
         ;; NB: we now store 'update-params separately in `mastodon-tl--buffer-spec'
         ;; and -http.el handles all conversion of params alists into query strings.
         (url (mastodon-http--api endpoint))
         (buffer (concat "*mastodon-" buffer-name "*"))
         (json (mastodon-http--get-json url args)))
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (switch-to-buffer buffer)
        ;; mastodon-mode wipes buffer-spec, so order must unforch be:
        ;; 1 run update-function, 2 enable masto-mode, 3 set buffer spec.
        ;; which means we cannot use buffer-spec for update-function
        ;; unless we set it both before and after the others
        (mastodon-tl--set-buffer-spec buffer endpoint update-function)
        (setq
         ;; Initialize with a minimal interval; we re-scan at least once
         ;; every 5 minutes to catch any timestamps we may have missed
         mastodon-tl--timestamp-next-update (time-add (current-time)
                                                      (seconds-to-time 300)))
        (funcall update-function json)
        (mastodon-mode)
        (mastodon-tl--set-buffer-spec buffer endpoint update-function nil args)
        (setq mastodon-tl--timestamp-update-timer
              (when mastodon-tl--enable-relative-timestamps
                (run-at-time (time-to-seconds
                              (time-subtract mastodon-tl--timestamp-next-update
                                             (current-time)))
                             nil ;; don't repeat
                             #'mastodon-tl--update-timestamps-callback
                             (current-buffer)
                             nil)))
        (unless (mastodon-tl--profile-buffer-p)
          ;; FIXME: this breaks test (because test has empty buffer)
          (mastodon-tl--goto-first-item)))
      buffer)))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
