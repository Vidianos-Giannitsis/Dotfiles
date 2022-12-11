;;; mastodon-tl.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (ts "0.3"))
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

(autoload 'mastodon-auth--get-account-name "mastodon-auth")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-media--get-avatar-rendering "mastodon-media")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-profile--account-from-id "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile--search-account-by-handle "mastodon-profile")
;; mousebot adds
(autoload 'mastodon-profile--toot-json "mastodon-profile")
(autoload 'mastodon-profile--account-field "mastodon-profile")
(autoload 'mastodon-profile--extract-users-handles "mastodon-profile")
(autoload 'mastodon-profile--my-profile "mastodon-profile")
(autoload 'mastodon-toot--delete-toot "mastodon-toot")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-http--get-json-async "mastodon-http")
(autoload 'mastodon-profile--lookup-account-in-status "mastodon-profile")
(autoload 'mastodon-profile-mode "mastodon-profile")
;; make notifications--get available via M-x and outside our keymap:
(autoload 'mastodon-notifications-get "mastodon-notifications"
  "Display NOTIFICATIONS in buffer." t) ; interactive
(autoload 'mastodon-search--propertize-user "mastodon-search")
(autoload 'mastodon-search--insert-users-propertized "mastodon-search")
(autoload 'mastodon-search--get-user-info "mastodon-search")
(autoload 'mastodon-http--delete "mastodon-http")
(autoload 'mastodon-profile--view-author-profile "mastodon-profile")
(autoload 'mastodon-profile--get-preferences-pref "mastodon-profile")
(autoload 'mastodon-http--get-response-async "mastodon-http")
(autoload 'mastodon-url-lookup "mastodon")
(autoload 'mastodon-auth--get-account-id "mastodon-auth")
(autoload 'mastodon-http--put "mastodon-http")
(autoload 'mastodon-http--process-json "mastodon-http")
(autoload 'mastodon-http--build-array-params-alist "mastodon-http")
(autoload 'mastodon-http--build-params-string "mastodon-http")
(autoload 'mastodon-notifications--filter-types-list "mastodon-notifications")
(autoload 'mastodon-toot--get-toot-edits "mastodon-toot")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")
(autoload 'mastodon-toot--compose-buffer "mastodon-toot")
(autoload 'mastodon-toot--set-toot-properties "mastodon-toot")
(autoload 'mastodon-toot--schedule-toot "mastodon-toot")
(autoload 'mastodon-toot--iso-to-human "mastodon-toot")

(defvar mastodon-toot--visibility)
(defvar mastodon-active-user)

(when (require 'mpv nil :no-error)
  (declare-function mpv-start "mpv"))
(defvar mastodon-instance-url)
(defvar mastodon-toot-timestamp-format)
(defvar shr-use-fonts)  ;; declare it since Emacs24 didn't have this
(defvar mastodon-mode-map)

(defgroup mastodon-tl nil
  "Timelines in Mastodon."
  :prefix "mastodon-tl-"
  :group 'mastodon)

(defcustom mastodon-tl--enable-relative-timestamps t
  "Whether to show relative (to the current time) timestamps.

This will require periodic updates of a timeline buffer to
keep the timestamps current as time progresses."
  :group 'mastodon-tl
  :type '(boolean :tag "Enable relative timestamps and background updater task"))

(defcustom mastodon-tl--enable-proportional-fonts nil
  "Nonnil to enable using proportional fonts when rendering HTML.

By default fixed width fonts are used."
  :group 'mastodon-tl
  :type '(boolean :tag "Enable using proportional rather than fixed \
width fonts when rendering HTML text"))

(defcustom mastodon-tl--display-caption-not-url-when-no-media t
  "Display an image's caption rather than URL.
Only has an effect when `mastodon-tl--display-media-p' is set to
nil."
  :group 'mastodon-tl
  :type 'boolean)

(defvar-local mastodon-tl--buffer-spec nil
  "A unique identifier and functions for each Mastodon buffer.")

(defcustom mastodon-tl--show-avatars nil
  "Whether to enable display of user avatars in timelines."
  :group 'mastodon-tl
  :type '(boolean :tag "Whether to display user avatars in timelines"))

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
  :type '(alist :key-type symbol :value-type string)
  :group 'mastodon-tl)

(defvar-local mastodon-tl--update-point nil
  "When updating a mastodon buffer this is where new toots will be inserted.

If nil `(point-min)' is used instead.")

(defvar mastodon-tl--display-media-p t
  "A boolean value stating whether to show media in timelines.")

(defvar-local mastodon-tl--timestamp-next-update nil
  "The timestamp when the buffer should next be scanned to update the timestamps.")

(defvar-local mastodon-tl--timestamp-update-timer nil
  "The timer that, when set will scan the buffer to update the timestamps.")

;; KEYMAPS

(defvar mastodon-tl--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'mastodon-tl--do-link-action-at-point)
    (define-key map [mouse-2] 'mastodon-tl--do-link-action)
    (define-key map [follow-link] 'mouse-face)
    (keymap-canonicalize map))
  "The keymap for link-like things in buffer (except for shr.el generate links).

This will make the region of text act like like a link with mouse
highlighting, mouse click action tabbing to next/previous link
etc.")

(defvar mastodon-tl--shr-map-replacement
  (let ((map (copy-keymap shr-map)))
    ;; Replace the move to next/previous link bindings with our
    ;; version that knows about more types of links.
    (define-key map [remap shr-next-link] 'mastodon-tl--next-tab-item)
    (define-key map [remap shr-previous-link] 'mastodon-tl--previous-tab-item)
    ;; keep new my-profile binding; shr 'O' doesn't work here anyway
    (define-key map (kbd "O") 'mastodon-profile--my-profile)
    (define-key map [remap shr-browse-url] 'mastodon-url-lookup)
    (keymap-canonicalize map))
  "The keymap to be set for shr.el generated links that are not images.

We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defvar mastodon-tl--shr-image-map-replacement
  (let ((map (copy-keymap (if (boundp 'shr-image-map)
                              shr-image-map
                            shr-map))))
    ;; Replace the move to next/previous link bindings with our
    ;; version that knows about more types of links.
    (define-key map [remap shr-next-link] 'mastodon-tl--next-tab-item)
    (define-key map [remap shr-previous-link] 'mastodon-tl--previous-tab-item)
    ;; browse-url loads the preview only, we want browse-image
    ;; on RET to browse full sized image URL
    (define-key map [remap shr-browse-url] 'shr-browse-image)
    ;; remove shr's u binding, as it the maybe-probe-and-copy-url
    ;; is already bound to w also
    (define-key map (kbd "u") 'mastodon-tl--update)
    ;; keep new my-profile binding; shr 'O' doesn't work here anyway
    (define-key map (kbd "O") 'mastodon-profile--my-profile)
    (define-key map (kbd "<C-return>") 'mastodon-tl--mpv-play-video-at-point)
    (keymap-canonicalize map))
  "The keymap to be set for shr.el generated image links.

We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defvar mastodon-tl--view-filters-keymap
  (let ((map
         (copy-keymap mastodon-mode-map)))
    (define-key map (kbd "d") 'mastodon-tl--delete-filter)
    (define-key map (kbd "c") 'mastodon-tl--create-filter)
    (define-key map (kbd "n") 'mastodon-tl--goto-next-item)
    (define-key map (kbd "p") 'mastodon-tl--goto-prev-item)
    (define-key map (kbd "TAB") 'mastodon-tl--goto-next-item)
    (define-key map (kbd "g") 'mastodon-tl--view-filters)
    (keymap-canonicalize map))
  "Keymap for viewing filters.")

(defvar mastodon-tl--follow-suggestions-map
  (let ((map
         (copy-keymap mastodon-mode-map)))
    (define-key map (kbd "n") 'mastodon-tl--goto-next-item)
    (define-key map (kbd "p") 'mastodon-tl--goto-prev-item)
    (define-key map (kbd "g") 'mastodon-tl--get-follow-suggestions)
    (keymap-canonicalize map))
  "Keymap for viewing follow suggestions.")

(defvar mastodon-tl--view-lists-keymap
  (let ((map ;(make-sparse-keymap)))
         (copy-keymap mastodon-mode-map)))
    (define-key map (kbd "D") 'mastodon-tl--delete-list)
    (define-key map (kbd "C") 'mastodon-tl--create-list)
    (define-key map (kbd "A") 'mastodon-tl--add-account-to-list)
    (define-key map (kbd "R") 'mastodon-tl--remove-account-from-list)
    (define-key map (kbd "E") 'mastodon-tl--edit-list)
    (define-key map (kbd "n") 'mastodon-tl--goto-next-item)
    (define-key map (kbd "p") 'mastodon-tl--goto-prev-item)
    (define-key map (kbd "g") 'mastodon-tl--view-lists)
    (keymap-canonicalize map))
  "Keymap for viewing lists.")

(defvar mastodon-tl--list-name-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'mastodon-tl--view-timeline-list-at-point)
    (define-key map (kbd "d") 'mastodon-tl--delete-list-at-point)
    (define-key map (kbd "a") 'mastodon-tl--add-account-to-list-at-point)
    (define-key map (kbd "r") 'mastodon-tl--remove-account-from-list-at-point)
    (define-key map (kbd "e") 'mastodon-tl--edit-list-at-point)
    (keymap-canonicalize map))
  "Keymap for when point is on list name.")

(defvar mastodon-tl--scheduled-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'mastodon-tl--reschedule-toot)
    (define-key map (kbd "c") 'mastodon-tl--cancel-scheduled-toot)
    (define-key map (kbd "e") 'mastodon-tl--edit-scheduled-as-new)
    (define-key map (kbd "<return>") 'mastodon-tl--edit-scheduled-as-new)
    (keymap-canonicalize map))
  "Keymap for when point is on a scheduled toot.")

(defvar mastodon-tl--byline-link-keymap
  (when (require 'mpv nil :no-error)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-return>") 'mastodon-tl--mpv-play-video-from-byline)
      (define-key map (kbd "<return>") 'mastodon-profile--get-toot-author)
      (keymap-canonicalize map)))
  "The keymap to be set for the author byline.
It is active where point is placed by `mastodon-tl--goto-next-toot.'")

(defun mastodon-tl--symbol (name)
  "Return the unicode symbol (as a string) corresponding to NAME.
If symbol is not displayable, an ASCII equivalent is returned. If
NAME is not part of the symbol table, '?' is returned."
  (if-let* ((symbol (alist-get name mastodon-tl--symbols)))
      (if (char-displayable-p (string-to-char (car symbol)))
          (car symbol)
        (cdr symbol))
    "?"))

;; NAV

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
      (message "%s" (get-text-property (point) 'help-echo)))))

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
      (message "%s" (get-text-property (point) 'help-echo)))))


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

;; TIMELINES

(defun mastodon-tl--get-federated-timeline ()
  "Opens federated timeline."
  (interactive)
  (message "Loading federated timeline...")
  (mastodon-tl--init
   "federated" "timelines/public" 'mastodon-tl--timeline))

(defun mastodon-tl--get-home-timeline ()
  "Opens home timeline."
  (interactive)
  (message "Loading home timeline...")
  (mastodon-tl--init
   "home" "timelines/home" 'mastodon-tl--timeline))

(defun mastodon-tl--get-local-timeline ()
  "Opens local timeline."
  (interactive)
  (message "Loading local timeline...")
  (mastodon-tl--init
   "local" "timelines/public?local=true" 'mastodon-tl--timeline))

(defun mastodon-tl--get-tag-timeline ()
  "Prompt for tag and opens its timeline."
  (interactive)
  (let* ((word (or (word-at-point) ""))
         (input (read-string (format "Load timeline for tag (%s): " word)))
         (tag (if (string-empty-p input) word input)))
    (message "Loading timeline for #%s..." tag)
    (mastodon-tl--show-tag-timeline tag)))

(defun mastodon-tl--show-tag-timeline (tag)
  "Opens a new buffer showing the timeline of posts with hastag TAG."
  (mastodon-tl--init
   (concat "tag-" tag) (concat "timelines/tag/" tag) 'mastodon-tl--timeline))

(defun mastodon-tl--message-help-echo ()
  "Call message on 'help-echo property at point.
Do so if type of status at poins is not follow_request/follow."
  (let ((type (alist-get
               'type
               (get-text-property (point) 'toot-json)))
        (echo (get-text-property (point) 'help-echo)))
    (when echo ; not for followers/following in profile
      (unless (or (string= type "follow_request")
                  (string= type "follow")) ; no counts for these
        (message "%s" (get-text-property (point) 'help-echo))))))

(defun mastodon-tl--remove-html (toot)
  "Remove unrendered tags from TOOT."
  (let* ((t1 (replace-regexp-in-string "<\/p>" "\n\n" toot))
         (t2 (replace-regexp-in-string "<\/?span>" "" t1)))
    (replace-regexp-in-string "<span class=\"h-card\">" "" t2)))

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
    ;; TODO: Once we have a view for a user (e.g. their posts
    ;; timeline) make this a tab-stop and attach an action
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
                   ;; (mastodon-tl--get-endpoint)))
                   (mastodon-tl--format-faves-count toot)))
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

(defun mastodon-tl--format-faves-count (toot)
  "Format a favourites, boosts, replies count for a TOOT.
Used as a help-echo when point is at the start of a byline, i.e.
where `mastodon-tl--goto-next-toot' leaves point. Also displays a
toot's media types and optionally the binding to play moving
image media from the byline."
  (let* ((toot-to-count
          (or
           ;; simply praying this order works
           (alist-get 'status toot) ; notifications timeline
           ;; fol-req notif, has 'type
           ;; placed before boosts coz fol-reqs have a (useless) reblog entry:
           ;; TODO: cd also test for notifs buffer before we do this to be sure
           (when (alist-get 'type toot)
             toot)
           (alist-get 'reblog toot) ; boosts
           toot)) ; everything else
         (fol-req-p (or (string= (alist-get 'type toot-to-count) "follow")
                        (string= (alist-get 'type toot-to-count) "follow_request"))))
    (unless fol-req-p
      (let* ((media-types (mastodon-tl--get-media-types toot))
             (format-faves (format "%s faves | %s boosts | %s replies"
                                   (alist-get 'favourites_count toot-to-count)
                                   (alist-get 'reblogs_count toot-to-count)
                                   (alist-get 'replies_count toot-to-count)))
             (format-media (when media-types
                             (format " | media: %s"
                                     (mapconcat #'identity media-types " "))))
             (format-media-binding (when (and (or
                                               (member "video" media-types)
                                               (member "gifv" media-types))
                                              (require 'mpv nil :no-error))
                                     (format " | C-RET to view with mpv"))))
        (format "%s" (concat format-faves format-media format-media-binding))))))

(defun mastodon-tl--get-media-types (toot)
  "Return a list of the media attachment types of the TOOT at point."
  (let* ((attachments (mastodon-tl--field 'media_attachments toot)))
    (mapcar (lambda (x)
              (alist-get 'type x))
            attachments)))

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

(defun mastodon-tl--field (field toot)
  "Return FIELD from TOOT.

Return value from boosted content if available."
  (or (alist-get field (alist-get 'reblog toot))
      (alist-get field toot)))

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
       ;; TODO: Once we have a view for toot (responses etc.) make
       ;; this a tab stop and attach an action.
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
             'face 'font-lock-comment-face
             'timestamp edited-parsed
             'display (if mastodon-tl--enable-relative-timestamps
                          (mastodon-tl--relative-time-description edited-parsed)
                        edited-parsed)))
         "")
       (propertize "\n  ------------\n" 'face 'default))
      'favourited-p faved
      'boosted-p    boosted
      'bookmarked-p bookmarked
      'edited edited-time
      'edit-history (when edited-time
                      (mastodon-toot--get-toot-edits (alist-get 'id toot)))
      'byline       t))))

(defun mastodon-tl--format-edit-timestamp (timestamp)
  "Convert edit TIMESTAMP into a descriptive string."
  (let ((parsed (ts-human-duration
                 (ts-diff (ts-now) (ts-parse timestamp)))))
    (cond ((> (plist-get parsed :days) 0)
           (format "%s days ago" (plist-get parsed :days) (plist-get parsed :hours)))
          ((> (plist-get parsed :hours) 0)
           (format "%s hours ago" (plist-get parsed :hours) (plist-get parsed :minutes)))
          ((> (plist-get parsed :minutes) 0)
           (format "%s minutes ago" (plist-get parsed :minutes)))
          (t ;; we failed to guess:
           (format "%s days, %s hours, %s minutes ago"
                   (plist-get parsed :days)
                   (plist-get parsed :hours)
                   (plist-get parsed :minutes))))))

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

(defun mastodon-tl--set-face (string face)
  "Return the propertized STRING with the face property set to FACE."
  (propertize string 'face face))

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
Used for hitting <return> on a given link."
  (interactive "d")
  (let ((link-type (get-text-property position 'mastodon-tab-stop)))
    (cond ((eq link-type 'content-warning)
           (mastodon-tl--toggle-spoiler-text position))
          ((eq link-type 'hashtag)
           (mastodon-tl--show-tag-timeline (get-text-property position 'mastodon-tag)))
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

(defun mastodon-tl--has-spoiler (toot)
  "Check if the given TOOT has a spoiler text.

Spoiler text should initially be shown only while the main
content should be hidden."
  (let ((spoiler (mastodon-tl--field 'spoiler_text toot)))
    (and spoiler (> (length spoiler) 0))))

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
         (message (concat ;"\n"
                   " ---------------\n"
                   " " (mastodon-tl--make-link
                        (concat "CW: " string)
                        'content-warning)
                   "\n"
                   " ---------------\n"))
         (cw (mastodon-tl--set-face message 'mastodon-cw-face)))
    (concat
     cw
     (propertize (mastodon-tl--content toot)
                 'invisible
                 ;; check server setting to expand all spoilers:
                 (unless (eq t
                             (mastodon-profile--get-preferences-pref
                              'reading:expand:spoilers))
                   t)
                 'mastodon-content-warning-body t))))

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
`mastodon-tl--byline-author'
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

(defun mastodon-tl--get-poll (toot)
  "If TOOT includes a poll, return it as a formatted string."
  (let* ((poll (mastodon-tl--field 'poll toot))
         (expiry (mastodon-tl--field 'expires_at poll))
         (expired-p (if (eq (mastodon-tl--field 'expired poll) :json-false) nil t))
         ;; (multi (mastodon-tl--field 'multiple poll))
         (voters-count (mastodon-tl--field 'voters_count poll))
         (vote-count (mastodon-tl--field 'votes_count poll))
         (options (mastodon-tl--field 'options poll))
         (option-titles (mapcar (lambda (x)
                                  (alist-get 'title x))
                                options))
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
  ;; TODO: this bugged when a timestamp was in the past
  ;; despite the poll not being listed as expired
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
           (options-titles (mapcar (lambda (x)
                                     (alist-get 'title x))
                                   options))
           (options-number-seq (number-sequence 1 (length options)))
           (options-numbers (mapcar (lambda(x)
                                      (number-to-string x))
                                    options-number-seq))
           (options-alist (cl-mapcar 'cons options-numbers options-titles))
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
              (get-text-property (point) 'image-url)))
        (type (or ;; in byline:
               type
               ;; point in toot:
               (mastodon-tl--property 'mastodon-media-type))))
    (if url
        (if (or (equal type "gifv")
                (equal type "video"))
            (progn
              (message "'q' to kill mpv.")
              (mpv-start "--loop" url))
          (message "no moving image here?"))
      (message "no moving image here?"))))

(defun mastodon-tl--toot (toot &optional detailed-p)
  "Formats TOOT and insertes it into the buffer.

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
  "Display each toot in TOOTS."
  (mapc 'mastodon-tl--toot toots)
  (goto-char (point-min)))

(defun mastodon-tl--get-update-function (&optional buffer)
  "Get the UPDATE-FUNCTION stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'update-function buffer))

(defun mastodon-tl--get-endpoint (&optional buffer)
  "Get the ENDPOINT stored in `mastodon-tl--buffer-spec'.
Optionally set it for BUFFER."
  (mastodon-tl--get-buffer-property 'endpoint buffer))

(defun mastodon-tl--buffer-name (&optional buffer)
  "Get the BUFFER-NAME stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'buffer-name buffer))

(defun mastodon-tl--link-header (&optional buffer)
  "Get the LINK HEADER stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--get-buffer-property 'link-header buffer))

(defun mastodon-tl--get-buffer-property (property &optional buffer)
  "Get PROPERTY from `mastodon-tl--buffer-spec' in BUFFER or `current-buffer'."
  (with-current-buffer  (or buffer (current-buffer))
    (or (plist-get mastodon-tl--buffer-spec property)
        (error "Mastodon-tl--buffer-spec is not defined for buffer %s"
               (or buffer (current-buffer))))))

(defun mastodon-tl--set-buffer-spec (buffer endpoint update-function
                                            &optional link-header)
  "Set `mastodon-tl--buffer-spec' for the current buffer.

BUFFER is buffer name, ENDPOINT is buffer's enpoint,
UPDATE-FUNCTION is its update function.
LINK-HEADER is the http Link header if present."
  (setq mastodon-tl--buffer-spec
        `(account ,(cons mastodon-active-user
                         mastodon-instance-url)
                  buffer-name ,buffer
                  endpoint ,endpoint
                  update-function ,update-function
                  link-header ,link-header)))

(defun mastodon-tl--more-json (endpoint id)
  "Return JSON for timeline ENDPOINT before ID."
  (let* ((args `(("max_id" . ,(mastodon-tl--as-string id))))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url args)))

(defun mastodon-tl--more-json-async (endpoint id callback &rest cbargs)
  "Return JSON for timeline ENDPOINT before ID.
Then run CALLBACK with arguments CBARGS."
  (let* ((args `(("max_id" . ,(mastodon-tl--as-string id))))
         (url (mastodon-http--api endpoint)))
    (apply 'mastodon-http--get-json-async url args callback cbargs)))

;; TODO
;; Look into the JSON returned here by Local
(defun mastodon-tl--updated-json (endpoint id)
  "Return JSON for timeline ENDPOINT since ID."
  (let* ((args `(("since_id" . ,(mastodon-tl--as-string id))))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url args)))

(defun mastodon-tl--property (prop &optional backward)
  "Get property PROP for toot at point.

Move forward (down) the timeline unless BACKWARD is non-nil."
  (or (get-text-property (point) prop)
      (save-excursion
        (if backward
            (mastodon-tl--goto-prev-toot)
          (mastodon-tl--goto-next-toot))
        (get-text-property (point) prop))))

(defun mastodon-tl--newest-id ()
  "Return toot-id from the top of the buffer."
  (save-excursion
    (goto-char (point-min))
    (mastodon-tl--property 'toot-id)))

(defun mastodon-tl--oldest-id ()
  "Return toot-id from the bottom of the buffer."
  (save-excursion
    (goto-char (point-max))
    (mastodon-tl--property 'toot-id t)))

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
      (with-output-to-temp-buffer buffer
        (switch-to-buffer buffer)
        (mastodon-mode)
        (mastodon-tl--set-buffer-spec buffer
                                      (format "statuses/%s" id)
                                      nil)
        (let ((inhibit-read-only t))
          (mastodon-tl--toot toot :detailed-p))))))

(defun mastodon-tl--thread (&optional id)
  "Open thread buffer for toot at point or with ID."
  ;; NB: this is called by `mastodon-url-lookup', which means it must work
  ;; without `mastodon-tl--buffer-spec' being set!
  ;; so avoid calls to `mastodon-tl--property' and friends
  (interactive)
  (let* ((id (or id (get-text-property (point) 'base-toot-id)))
         (type (mastodon-tl--field 'type (get-text-property (point) 'toot-json))))
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
                (with-output-to-temp-buffer buffer
                  (switch-to-buffer buffer)
                  (mastodon-mode)
                  (mastodon-tl--set-buffer-spec buffer
                                                endpoint
                                                nil)
                  (let ((inhibit-read-only t))
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
    (if (string-suffix-p "context" endpoint) ; thread view
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

(defun mastodon-tl--user-in-thread-p (id)
  "Return non-nil if the logged-in user has posted to the current thread.
ID is that of the post the context is currently displayed for."
  (let* ((context-json (mastodon-http--get-json
                        (mastodon-http--api (format "statuses/%s/context" id))
                        nil :silent))
         (ancestors (alist-get 'ancestors context-json))
         (descendants (alist-get 'descendants context-json))
         (a-ids (mapcar (lambda (status)
                          (alist-get 'id
                                     (alist-get 'account status)))
                        ancestors))
         (d-ids (mapcar (lambda (status)
                          (alist-get 'id
                                     (alist-get 'account status)))
                        descendants)))
    (or (member (mastodon-auth--get-account-id) a-ids)
        (member (mastodon-auth--get-account-id) d-ids))))

;;; LISTS

(defun mastodon-tl--get-users-lists ()
  "Get the list of the user's lists from the server."
  (let ((url (mastodon-http--api "lists")))
    (mastodon-http--get-json url)))

(defun mastodon-tl--get-lists-names ()
  "Return a list of the user's lists' names."
  (let ((lists (mastodon-tl--get-users-lists)))
    (mapcar (lambda (x)
              (alist-get 'title x))
            lists)))

(defun mastodon-tl--get-list-by-name (name)
  "Return the list data for list with NAME."
  (let* ((lists (mastodon-tl--get-users-lists)))
    (cl-loop for list in lists
             if (string= (alist-get 'title list) name)
             return list)))

(defun mastodon-tl--get-list-id (name)
  "Return id for list with NAME."
  (let ((list (mastodon-tl--get-list-by-name name)))
    (alist-get 'id list)))

(defun mastodon-tl--get-list-name (id)
  "Return name of list with ID."
  (let* ((url (mastodon-http--api (format "lists/%s" id)))
         (response (mastodon-http--get-json url)))
    (alist-get 'title response)))

(defun mastodon-tl--edit-list-at-point ()
  "Edit list at point."
  (interactive)
  (let ((id (get-text-property (point) 'list-id)))
    (mastodon-tl--edit-list id)))

(defun mastodon-tl--edit-list (&optional id)
  "Prompt for a list and edit the name and replies policy.
If ID is provided, use that list."
  (interactive)
  (let* ((list-names (unless id (mastodon-tl--get-lists-names)))
         (name-old (if id
                       (get-text-property (point) 'list-name)
                     (completing-read "Edit list: "
                                      list-names)))
         (id (or id (mastodon-tl--get-list-id name-old)))
         (name-choice (read-string "List name: " name-old))
         (replies-policy (completing-read "Replies policy: " ; give this a proper name
                                          '("followed" "list" "none")
                                          nil t nil nil "list"))
         (url (mastodon-http--api (format "lists/%s" id)))
         (response (mastodon-http--put url
                                       `(("title" . ,name-choice)
                                         ("replies_policy" . ,replies-policy)))))
    (mastodon-http--triage response
                           (lambda ()
                             (with-current-buffer response
                               (let* ((json (mastodon-http--process-json))
                                      (name-new (alist-get 'title json)))
                                 (message "list %s edited to %s!" name-old name-new)))
                             (when (equal (buffer-name (current-buffer))
                                          "*mastodon-lists*")
                               (mastodon-tl--view-lists))))))

(defun mastodon-tl--view-timeline-list-at-point ()
  "View timeline of list at point."
  (interactive)
  (let ((list-id (get-text-property (point) 'list-id)))
    (mastodon-tl--view-list-timeline list-id)))

(defun mastodon-tl--view-list-timeline (&optional id)
  "Prompt for a list and view its timeline.
If ID is provided, use that list."
  (interactive)
  (let* ((list-names (unless id (mastodon-tl--get-lists-names)))
         (list-name (unless id (completing-read "View list: " list-names)))
         (id (or id (mastodon-tl--get-list-id list-name)))
         (endpoint (format "timelines/list/%s" id))
         (name (mastodon-tl--get-list-name id))
         (buffer-name (format "list-%s" name)))
    (mastodon-tl--init buffer-name endpoint 'mastodon-tl--timeline)))

(defun mastodon-tl--create-list ()
  "Create a new list.
Prompt for name and replies policy."
  (interactive)
  (let* ((title (read-string "New list name: "))
         (replies-policy (completing-read "Replies policy: " ; give this a proper name
                                          '("followed" "list" "none")
                                          nil t nil nil "list")) ; default
         (response (mastodon-http--post (mastodon-http--api "lists")
                                        `(("title" . ,title)
                                          ("replies_policy" . ,replies-policy))
                                        nil)))
    (mastodon-tl--list-action-triage response
                                     (message "list %s created!" title))))

(defun mastodon-tl--delete-list-at-point ()
  "Delete list at point."
  (interactive)
  (let ((id (get-text-property (point) 'list-id)))
    (mastodon-tl--delete-list id)))

(defun mastodon-tl--delete-list (&optional id)
  "Prompt for a list and delete it.
If ID is provided, delete that list."
  (interactive)
  (let* ((list-names (unless id (mastodon-tl--get-lists-names)))
         (name (if id
                   (mastodon-tl--get-list-name id)
                 (completing-read "Delete list: "
                                  list-names)))
         (id (or id (mastodon-tl--get-list-id name)))
         (url (mastodon-http--api (format "lists/%s" id))))
    (when (y-or-n-p (format "Delete list %s?" name))
      (let ((response (mastodon-http--delete url)))
        (mastodon-tl--list-action-triage response
                                         (message "list %s deleted!" name))))))

(defun mastodon-tl--view-lists ()
  "Show the user's lists in a new buffer."
  (interactive)
  (mastodon-tl--init-sync "lists"
                          "lists"
                          'mastodon-tl--insert-lists)
  (use-local-map mastodon-tl--view-lists-keymap))

(defun mastodon-tl--insert-lists (_json)
  "Insert the user's lists from JSON."
  ;; TODO: for now we don't use the JSON, we get it ourself again
  (let* ((lists-names (mastodon-tl--get-lists-names)))
    (erase-buffer)
    (insert (mastodon-tl--set-face
             (concat "\n ------------\n"
                     " YOUR LISTS\n"
                     " ------------\n\n")
             'success)
            (mastodon-tl--set-face
             "[C - create a list\n D - delete a list\
\n A/R - add/remove account from a list\
\n E - edit a list\n n/p - go to next/prev item]\n\n"
             'font-lock-comment-face))
    (mapc (lambda (x)
            (mastodon-tl--print-list-accounts x)
            (insert (propertize " ------------\n\n"
                                'face 'success)))
          lists-names)
    (goto-char (point-min))))
;; (mastodon-tl--goto-next-item))) ; causes another request!

(defun mastodon-tl--print-list-accounts (list-name)
  "Insert the accounts in list named LIST-NAME."
  (let* ((id (mastodon-tl--get-list-id list-name))
         (accounts (mastodon-tl--accounts-in-list id)))
    (insert
     (propertize list-name
                 'byline t ; so we nav here
                 'toot-id "0" ; so we nav here
                 'help-echo "RET: view list timeline, d: delete this list, \
a: add account to this list, r: remove account from this list"
                 'list t
                 'face 'link
                 'keymap mastodon-tl--list-name-keymap
                 'list-name list-name
                 'list-id id)
     (propertize
      "\n\n"
      'list t
      'keymap mastodon-tl--list-name-keymap
      'list-name list-name
      'list-id id)
     (propertize
      (mapconcat #'mastodon-search--propertize-user accounts
                 " ")
      ;; (mastodon-search--insert-users-propertized accounts)
      'list t
      'keymap mastodon-tl--list-name-keymap
      'list-name list-name
      'list-id id))))

(defun mastodon-tl--get-users-followings ()
  "Return the list of followers of the logged in account."
  (let* ((id (mastodon-auth--get-account-id))
         (url (mastodon-http--api (format "accounts/%s/following" id))))
    (mastodon-http--get-json url)))

(defun mastodon-tl--add-account-to-list-at-point ()
  "Prompt for account and add to list at point."
  (interactive)
  (let ((id (get-text-property (point) 'list-id)))
    (mastodon-tl--add-account-to-list id)))

(defun mastodon-tl--add-account-to-list (&optional id account-id handle)
  "Prompt for a list and for an account, add account to list.
If ID is provided, use that list.
If ACCOUNT-ID and HANDLE are provided use them rather than prompting."
  (interactive)
  (let* ((list-name (if id
                        (get-text-property (point) 'list-name)
                      (completing-read "Add account to list: "
                                       (mastodon-tl--get-lists-names) nil t)))
         (list-id (or id (mastodon-tl--get-list-id list-name)))
         (followings (mastodon-tl--get-users-followings))
         (handles (mapcar (lambda (x)
                            (cons (alist-get 'acct x)
                                  (alist-get 'id x)))
                          followings))
         (account (or handle (completing-read "Account to add: "
                                              handles nil t)))
         (account-id (or account-id (alist-get account handles nil nil 'equal)))
         (url (mastodon-http--api (format "lists/%s/accounts" list-id)))
         (response (mastodon-http--post url
                                        `(("account_ids[]" . ,account-id)))))
    (mastodon-tl--list-action-triage
     response
     (message "%s added to list %s!" account list-name))))

(defun mastodon-tl--remove-account-from-list-at-point ()
  "Prompt for account and remove from list at point."
  (interactive)
  (let ((id (get-text-property (point) 'list-id)))
    (mastodon-tl--remove-account-from-list id)))

(defun mastodon-tl--remove-account-from-list (&optional id)
  "Prompt for a list, select an account and remove from list.
If ID is provided, use that list."
  (interactive)
  (let* ((list-name (if id
                        (get-text-property (point) 'list-name)
                      (completing-read "Remove account from list: "
                                       (mastodon-tl--get-lists-names) nil t)))
         (list-id (or id (mastodon-tl--get-list-id list-name)))
         (accounts (mastodon-tl--accounts-in-list list-id))
         (handles (mapcar (lambda (x)
                            (cons (alist-get 'acct x)
                                  (alist-get 'id x)))
                          accounts))
         (account (completing-read "Account to remove: "
                                   handles nil t))
         (account-id (alist-get account handles nil nil 'equal))
         (url (mastodon-http--api (format "lists/%s/accounts" list-id)))
         (args (mastodon-http--build-array-params-alist "account_ids[]" `(,account-id)))
         (response (mastodon-http--delete url args)))
    (mastodon-tl--list-action-triage
     response
     (message "%s removed from list %s!" account list-name))))

(defun mastodon-tl--list-action-triage (response message)
  "Call `mastodon-http--triage' on RESPONSE and display MESSAGE."
  (mastodon-http--triage response
                         (lambda ()
                           (when (equal (buffer-name (current-buffer))
                                        "*mastodon-lists*")
                             (mastodon-tl--view-lists))
                           message)))

(defun mastodon-tl--accounts-in-list (list-id)
  "Return the JSON of the accounts in list with LIST-ID."
  (let* ((url (mastodon-http--api (format "lists/%s/accounts" list-id))))
    (mastodon-http--get-json url)))

;;; SCHEDULED TOOTS

(defun mastodon-tl--get-scheduled-toots (&optional id)
  "Get the user's currently scheduled toots.
If ID, just return that toot."
  (let* ((endpoint (if id
                       (format "scheduled_statuses/%s" id)
                     "scheduled_statuses"))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url)))

(defun mastodon-tl--reschedule-toot ()
  "Reschedule the scheduled toot at point."
  (interactive)
  (mastodon-toot--schedule-toot :reschedule))

(defun mastodon-tl--view-scheduled-toots ()
  "Show the user's scheduled toots in a new buffer."
  (interactive)
  (mastodon-tl--init-sync "scheduled-toots"
                          "scheduled_statuses"
                          'mastodon-tl--insert-scheduled-toots))

(defun mastodon-tl--insert-scheduled-toots (json)
  "Insert the user's scheduled toots, from JSON."
  (let ((scheduleds (mastodon-tl--get-scheduled-toots)))
    (erase-buffer)
    (insert (mastodon-tl--set-face
             (concat "\n ------------\n"
                     " YOUR SCHEDULED TOOTS\n"
                     " ------------\n\n")
             'success)
            (mastodon-tl--set-face
             "[n/p - prev/next\n r - reschedule\n e/RET - edit toot\n c - cancel]\n\n"
             'font-lock-comment-face))
    (mapc (lambda (x)
            (mastodon-tl--insert-scheduled-toot x))
          scheduleds)
    (goto-char (point-min))
    (when json
      (mastodon-tl--goto-next-toot))))

(defun mastodon-tl--insert-scheduled-toot (toot)
  "Insert scheduled TOOT into the buffer."
  (let* ((id (alist-get 'id toot))
         (scheduled (alist-get 'scheduled_at toot))
         (params (alist-get 'params toot))
         (text (alist-get 'text params)))
    (insert
     (propertize (concat text
                         " | "
                         (mastodon-toot--iso-to-human scheduled))
                 'byline t ; so we nav here
                 'toot-id "0" ; so we nav here
                 'face 'font-lock-comment-face
                 'keymap mastodon-tl--scheduled-map
                 'scheduled-json toot
                 'id id)
     "\n")))

(defun mastodon-tl--copy-scheduled-toot-text ()
  "Copy the text of the scheduled toot at point."
  (interactive)
  (let* ((toot (get-text-property (point) 'toot))
         (params (alist-get 'params toot))
         (text (alist-get 'text params)))
    (kill-new text)))

(defun mastodon-tl--cancel-scheduled-toot (&optional id no-confirm)
  "Cancel the scheduled toot at point.
ID is that of the scheduled toot to cancel.
NO-CONFIRM means there is no ask or message, there is only do."
  (interactive)
  (let* ((id (or id (get-text-property (point) 'id)))
         (url (mastodon-http--api (format "scheduled_statuses/%s" id))))
    (when (or no-confirm
              (y-or-n-p "Cancel scheduled toot?"))
      (let ((response (mastodon-http--delete url)))
        (mastodon-http--triage response
                               (lambda ()
                                 (mastodon-tl--view-scheduled-toots)
                                 (unless no-confirm
                                   (message "Toot cancelled!"))))))))

(defun mastodon-tl--edit-scheduled-as-new ()
  "Edit scheduled status as new toot."
  (interactive)
  (let* ((toot (get-text-property (point) 'scheduled-json))
         (id (alist-get 'id toot))
         (scheduled (alist-get 'scheduled_at toot))
         (params (alist-get 'params toot))
         (text (alist-get 'text params))
         (visibility (alist-get 'visibility params))
         (cw (alist-get 'spoiler_text params))
         (lang (alist-get 'language params))
         ;; (poll (alist-get 'poll params))
         (reply-id (alist-get 'in_reply_to_id params)))
    ;; (media (alist-get 'media_attachments toot)))
    (mastodon-toot--compose-buffer)
    (goto-char (point-max))
    (insert text)
    ;; adopt properties from scheduled toot:
    (mastodon-toot--set-toot-properties reply-id visibility cw
                                        lang scheduled id)))

;;; FILTERS

(defun mastodon-tl--create-filter ()
  "Create a filter for a word.
Prompt for a context, must be a list containting at least one of \"home\",
\"notifications\", \"public\", \"thread\"."
  (interactive)
  (let* ((url (mastodon-http--api "filters"))
         (word (read-string
                (format "Word(s) to filter (%s): " (or (current-word) ""))
                nil nil (or (current-word) "")))
         (contexts
          (if (string-empty-p word)
              (error "You must select at least one word for a filter")
            (completing-read-multiple
             "Contexts to filter [TAB for options]: "
             '("home" "notifications" "public" "thread")
             nil ; no predicate
             t))) ; require-match, as context is mandatory
         (contexts-processed
          (if (equal nil contexts)
              (error "You must select at least one context for a filter")
            (mapcar (lambda (x)
                      (cons "context[]" x))
                    contexts)))
         (response (mastodon-http--post url (push
                                             `("phrase" . ,word)
                                             contexts-processed))))
    (mastodon-http--triage response
                           (lambda ()
                             (message "Filter created for %s!" word)
                             ;; reload if we are in filters view:
                             (when (string= (mastodon-tl--get-endpoint)
                                            "filters")
                               (mastodon-tl--view-filters))))))

(defun mastodon-tl--view-filters ()
  "View the user's filters in a new buffer."
  (interactive)
  (mastodon-tl--init-sync "filters"
                          "filters"
                          'mastodon-tl--insert-filters)
  (use-local-map mastodon-tl--view-filters-keymap))

(defun mastodon-tl--insert-filters (json)
  "Insert the user's current filters.
JSON is what is returned by by the server."
  (insert (mastodon-tl--set-face
           (concat "\n ------------\n"
                   " CURRENT FILTERS\n"
                   " ------------\n\n")
           'success)
          (mastodon-tl--set-face
           "[c - create filter\n d - delete filter at point\n n/p - go to next/prev filter]\n\n"
           'font-lock-comment-face))
  (if (seq-empty-p json)
      (insert (propertize
               "Looks like you have no filters for now."
               'face font-lock-comment-face
               'byline t
               'toot-id "0")) ; so point can move here when no filters
    (mapc (lambda (x)
            (mastodon-tl--insert-filter-string x)
            (insert "\n\n"))
          json)))

(defun mastodon-tl--insert-filter-string (filter)
  "Insert a single FILTER."
  (let* ((phrase (alist-get 'phrase filter))
         (contexts (alist-get 'context filter))
         (id (alist-get 'id filter))
         (filter-string (concat "- \"" phrase "\" filtered in: "
                                (mapconcat #'identity contexts ", "))))
    (insert
     (propertize filter-string
                 'toot-id id ;for goto-next-filter compat
                 'phrase phrase
                 ;;'help-echo "n/p to go to next/prev filter, c to create new filter, d to delete filter at point."
                 ;;'keymap mastodon-tl--view-filters-keymap
                 'byline t)))) ;for goto-next-filter compat

(defun mastodon-tl--delete-filter ()
  "Delete filter at point."
  (interactive)
  (let* ((filter-id (get-text-property (point) 'toot-id))
         (phrase (get-text-property (point) 'phrase))
         (url (mastodon-http--api
               (format "filters/%s" filter-id))))
    (if (equal nil filter-id)
        (error "No filter at point?")
      (when (y-or-n-p (format "Delete this filter? ")))
      (let ((response (mastodon-http--delete url)))
        (mastodon-http--triage response (lambda ()
                                          (mastodon-tl--view-filters)
                                          (message "Filter for \"%s\" deleted!" phrase)))))))

;;; FOLLOW SUGGESTIONS

(defun mastodon-tl--get-follow-suggestions ()
  "Display a buffer of suggested accounts to follow."
  (interactive)
  (mastodon-tl--init-sync "follow-suggestions"
                          "suggestions"
                          'mastodon-tl--insert-follow-suggestions)
  (use-local-map mastodon-tl--follow-suggestions-map))

(defun mastodon-tl--insert-follow-suggestions (response)
  "Insert follow suggestions into buffer.
RESPONSE is the JSON returned by the server."
  (insert (mastodon-tl--set-face
           (concat "\n ------------\n"
                   " SUGGESTED ACCOUNTS\n"
                   " ------------\n\n")
           'success))
  (mastodon-search--insert-users-propertized response :note)
  (goto-char (point-min)))

(defmacro mastodon-tl--do-if-toot (&rest body)
  "Execute BODY if we have a toot or user at point."
  (declare (debug t))
  `(if (and (not (string-prefix-p "accounts" (mastodon-tl--get-endpoint))) ;profile view
            (not (mastodon-tl--property 'toot-json)))
       (message "Looks like there's no toot or user at point?")
     ,@body))

;;; INSTANCES

(defun mastodon-tl--view-own-instance (&optional brief)
  "View details of your own instance.
BRIEF means show fewer details."
  (interactive)
  (mastodon-tl--view-instance-description :user brief))

(defun mastodon-tl--view-own-instance-brief ()
  "View brief details of your own instance."
  (interactive)
  (mastodon-tl--view-instance-description :user :brief))

(defun mastodon-tl--view-instance-description-brief ()
  "View brief details of the instance the current post's author is on."
  (interactive)
  (mastodon-tl--view-instance-description nil :brief))

(defun mastodon-tl--view-instance-description (&optional user brief instance)
  "View the details of the instance the current post's author is on.
USER means to show the instance details for the logged in user.
BRIEF means to show fewer details.
INSTANCE is an instance domain name."
  (interactive)
  (mastodon-tl--do-if-toot
   (let* ((profile-p (get-text-property (point) 'profile-json))
          (toot (if profile-p
                    (mastodon-tl--property 'profile-json) ; profile may have 0 toots
                  (mastodon-tl--property 'toot-json)))
          (reblog (alist-get 'reblog toot))
          (account (or (alist-get 'account reblog)
                       (alist-get 'account toot)))
          (url (if profile-p
                   (alist-get 'url toot) ; profile
                 (alist-get 'url account)))
          (username (if profile-p
                        (alist-get 'username toot) ;; profile
                      (alist-get 'username account)))
          (instance (if instance
                        (concat "https://" instance)
                      ;; pleroma URL is https://instance.com/users/username
                      (if (string-suffix-p "users/" (url-basepath url))
                          (string-remove-suffix "/users/"
                                                (url-basepath url))
                        ;; mastodon:
                        (string-remove-suffix (concat "/@" username)
                                              url))))
          (response (mastodon-http--get-json
                     (if user
                         (mastodon-http--api "instance")
                       (concat instance "/api/v1/instance"))
                     nil ; params
                     nil ; silent
                     :vector)))
     (when response
       (let ((buf (get-buffer-create "*mastodon-instance*")))
         (with-current-buffer buf
           (switch-to-buffer-other-window buf)
           (mastodon-tl--set-buffer-spec (buffer-name buf)
                                         "instance"
                                         nil)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (special-mode)
             (when brief
               (setq response
                     (list (assoc 'uri response)
                           (assoc 'title response)
                           (assoc 'short_description response)
                           (assoc 'email response)
                           (cons 'contact_account
                                 (list
                                  (assoc 'username
                                         (assoc 'contact_account response))))
                           (assoc 'rules response)
                           (assoc 'stats response))))
             (mastodon-tl--print-json-keys response)
             (mastodon-mode)
             (goto-char (point-min)))))))))

(defun mastodon-tl--format-key (el pad)
  "Format a key of element EL, a cons, with PAD padding."
  (format (concat "%-"
                  (number-to-string pad)
                  "s: ")
          (propertize
           (prin1-to-string (car el))
           'face '(:underline t))))

(defun mastodon-tl--print-json-keys (response &optional ind)
  "Print the JSON keys and values in RESPONSE.
IND is the optional indentation level to print at."
  (let* ((cars (mapcar
                (lambda (x) (symbol-name (car x)))
                response))
         (pad (1+ (cl-reduce #'max (mapcar #'length cars)))))
    (while response
      (let ((el (pop response)))
        (cond
         ;; vector of alists (fields, instance rules):
         ((and (vectorp (cdr el))
               (not (seq-empty-p (cdr el)))
               (consp (seq-elt (cdr el) 0)))
          (insert
           (mastodon-tl--format-key el pad)
           "\n\n")
          (seq-do #'mastodon-tl--print-instance-rules-or-fields (cdr el))
          (insert "\n"))
         ;; vector of strings (media types):
         ((and (vectorp (cdr el))
               (not (seq-empty-p (cdr el)))
               (< 1 (seq-length (cdr el)))
               (stringp (seq-elt (cdr el) 0)))
          (when ind (indent-to ind))
          (insert
           (mastodon-tl--format-key el pad)
           "\n"
           (seq-mapcat
            (lambda (x) (concat x ", "))
            (cdr el) 'string)
           "\n\n"))
         ;; basic nesting:
         ((consp (cdr el))
          (when ind (indent-to ind))
          (insert
           (mastodon-tl--format-key el pad)
           "\n\n")
          (mastodon-tl--print-json-keys
           (cdr el) (if ind (+ ind 4) 4)))
         (t
          ;; basic handling of raw booleans:
          (let ((val (cond ((equal (cdr el) ':json-false)
                            "no")
                           ((equal (cdr el) 't)
                            "yes")
                           (t
                            (cdr el)))))
            (when ind (indent-to ind))
            (insert (mastodon-tl--format-key el pad)
                    " "
                    (mastodon-tl--newline-if-long (cdr el))
                    ;; only send strings straight to --render-text
                    ;; this makes hyperlinks work:
                    (if (not (stringp val))
                        (mastodon-tl--render-text
                         (prin1-to-string val))
                      (mastodon-tl--render-text val))
                    "\n"))))))))

(defun mastodon-tl--print-instance-rules-or-fields (alist)
  "Print ALIST of instance rules or contact account or emoji fields."
  (let ((key   (cond ((alist-get 'id alist)
                      'id)
                     ((alist-get 'name alist)
                      'name)
                     ((alist-get 'shortcode alist)
                      'shortcode)))
        (value (cond ((alist-get 'id alist)
                      'text)
                     ((alist-get 'value alist)
                      'value)
                     ((alist-get 'url alist)
                      'url))))
    (indent-to 4)
    (insert
     (format "%-5s: "
             (propertize (alist-get key alist)
                         'face '(:underline t)))
     (mastodon-tl--newline-if-long (alist-get value alist))
     (format "%s" (mastodon-tl--render-text
                   (alist-get value alist)))
     "\n")))

(defun mastodon-tl--newline-if-long (el)
  "Return a newline string if the cdr of EL is over 50 characters long."
  (let ((rend (if (stringp el) (mastodon-tl--render-text el) el)))
    (if (and (sequencep rend)
             (< 50 (length rend)))
        "\n"
      "")))

;;; FOLLOW/BLOCK/MUTE, ETC

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
          (cond ((or (equal (buffer-name) "*mastodon-follow-suggestions*")
                     ;; follow suggests / search / foll requests compat:
                     (string-prefix-p "*mastodon-search" (buffer-name))
                     (equal (buffer-name) "*mastodon-follow-requests*")
                     ;; profile view follows/followers compat:
                     ;; but not for profile statuses:
                     ;; fetch 'toot-json:
                     (and (string-prefix-p "accounts" (mastodon-tl--get-endpoint))
                          (not (string-suffix-p "statuses" (mastodon-tl--get-endpoint)))))
                 (list (alist-get 'acct (get-text-property (point) 'toot-json))))
                ;; profile view, no toots, point on profile note, ie. 'profile-json:
                ;; needed for e.g. gup.pe groups which show no toots publically:
                ((and (string-prefix-p "accounts" (mastodon-tl--get-endpoint))
                      (get-text-property (point) 'profile-json))
                 (list (alist-get 'acct (get-text-property (point) 'profile-json))))
                ;; avoid tl--property here because it calls next-toot
                ;; which breaks non-toot buffers like foll reqs etc.:
                (t
                 (mastodon-profile--extract-users-handles
                  (mastodon-profile--toot-json))))))
     (completing-read (if (or (equal action "disable")
                              (equal action "enable"))
                          (format "%s notifications when user posts: " action)
                        (format "Handle of user to %s: " action))
                      user-handles
                      nil ; predicate
                      'confirm))))

(defun mastodon-tl--interactive-blocks-or-mutes-list-get (action)
  "Fetch the list of accounts for ACTION from the server.
Action must be either \"unblock\" or \"unmute\"."
  (let* ((endpoint (cond ((equal action "unblock")
                          "blocks")
                         ((equal action "unmute")
                          "mutes")))
         (url (mastodon-http--api endpoint))
         (json (mastodon-http--get-json url))
         (accts (mapcar (lambda (user)
                          (alist-get 'acct user))
                        json)))
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
                    (if (string-prefix-p "accounts" (mastodon-tl--get-endpoint))
                        (mastodon-profile--lookup-account-in-status
                         user-handle (get-text-property (point) 'profile-json))
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

(defun mastodon-tl--get-tag-json (tag)
  "Return JSON data about TAG."
  (let ((url (mastodon-http--api (format "tags/%s" tag))))
    (mastodon-http--get-json url)))

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
If TAG if provided, unfollow it."
  (interactive)
  (let* ((followed-tags-json (unless tag (mastodon-tl--followed-tags)))
         (tags (unless tag (mapcar (lambda (x)
                                     (alist-get 'name x))
                                   followed-tags-json)))
         (tag (or tag (completing-read "Unfollow tag: "
                                       tags)))
         (url (mastodon-http--api (format "tags/%s/unfollow" tag)))
         (response (mastodon-http--post url)))
    (mastodon-http--triage response
                           (lambda ()
                             (message "tag #%s unfollowed!" tag)))))

(defun mastodon-tl--list-followed-tags ()
  "List tags followed. If user choses one, display its JSON."
  (interactive)
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (mapcar (lambda (x)
                         (alist-get 'name x))
                       followed-tags-json))
         (tag (completing-read "Tag: " tags)))
    (message (prin1-to-string
              (mastodon-tl--get-tag-json tag)))))

;; TODO: add this to new posts in some cases, e.g. in thread view.
(defun mastodon-tl--reload-timeline-or-profile ()
  "Reload the current timeline or profile page.
For use after e.g. deleting a toot."
  (cond ((equal (mastodon-tl--get-endpoint) "timelines/home")
         (mastodon-tl--get-home-timeline))
        ((equal (mastodon-tl--get-endpoint) "timelines/public")
         (mastodon-tl--get-federated-timeline))
        ((equal (mastodon-tl--get-endpoint) "timelines/public?local=true")
         (mastodon-tl--get-local-timeline))
        ((equal (mastodon-tl--get-endpoint) "notifications")
         (mastodon-notifications-get))
        ((equal (mastodon-tl--buffer-name)
                (concat "*mastodon-" (mastodon-auth--get-account-name) "-statuses*"))
         (mastodon-profile--my-profile))
        ((save-match-data
           (string-match
            "statuses/\\(?2:[[:digit:]]+\\)/context"
            (mastodon-tl--get-endpoint))
           (mastodon-tl--thread
            (match-string 2 (mastodon-tl--get-endpoint)))))))

(defun mastodon-tl--build-link-header-url (str)
  "Return a URL from STR, an http Link header."
  (let* ((split (split-string str "; "))
         (url-base (string-trim (car split) "<" ">"))
         (param (cadr split)))
    (concat url-base "&" param)))

(defun mastodon-tl--use-link-header-p ()
  "Return t if we are in a view that uses Link header pagination.
Currently this includes favourites, bookmarks, and profile pages
when showing followers or accounts followed."
  (let ((buf (buffer-name (current-buffer)))
        (endpoint (mastodon-tl--get-endpoint)))
    (or (member buf '("*mastodon-favourites*" "*mastodon-bookmarks*"))
        (and (string-prefix-p "accounts" endpoint)
             (or (string-suffix-p "followers" endpoint)
                 (string-suffix-p "following" endpoint))))))

(defun mastodon-tl--more ()
  "Append older toots to timeline, asynchronously."
  (interactive)
  (message "Loading older toots...")
  (if (mastodon-tl--use-link-header-p)
      ;; link-header: can't build a URL with --more-json-async, endpoint/id:
      (let* ((next (car (mastodon-tl--link-header)))
             ;;(prev (cadr (mastodon-tl--link-header)))
             (url (mastodon-tl--build-link-header-url next)))
        (mastodon-http--get-response-async url nil 'mastodon-tl--more* (current-buffer)
                                           (point) :headers))
    (mastodon-tl--more-json-async (mastodon-tl--get-endpoint) (mastodon-tl--oldest-id)
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
        (funcall (mastodon-tl--get-update-function) json)
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

If relative timestamps are
disabled (`mastodon-tl--enable-relative-timestamps` is nil) this
is a no-op."
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

(defun mastodon-tl--update ()
  "Update timeline with new toots."
  (interactive)
  (let* ((endpoint (mastodon-tl--get-endpoint))
         (update-function (mastodon-tl--get-update-function))
         (id (mastodon-tl--newest-id))
         (json (mastodon-tl--updated-json endpoint id)))
    (if json
        (let ((inhibit-read-only t))
          (goto-char (or mastodon-tl--update-point (point-min)))
          (funcall update-function json))
      (message "nothing to update"))))

(defun mastodon-tl--get-link-header-from-response (headers)
  "Get http Link header from list of http HEADERS."
  (when headers
    (split-string (alist-get "Link" headers nil nil 'equal) ", ")))

(defun mastodon-tl--init (buffer-name endpoint update-function &optional headers)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT asynchronously.
UPDATE-FUNCTION is used to recieve more toots.
HEADERS means to also collect the response headers. Used for paginating
favourites and bookmarks."
  (let ((url (mastodon-http--api endpoint))
        (buffer (concat "*mastodon-" buffer-name "*")))
    (if headers
        (mastodon-http--get-response-async
         url nil 'mastodon-tl--init* buffer endpoint update-function headers)
      (mastodon-http--get-json-async
       url nil 'mastodon-tl--init* buffer endpoint update-function))))

(defun mastodon-tl--init* (response buffer endpoint update-function &optional headers)
  "Initialize BUFFER with timeline targeted by ENDPOINT.
UPDATE-FUNCTION is used to recieve more toots.
RESPONSE is the data returned from the server by
`mastodon-http--process-json', with arg HEADERS a cons cell of
JSON and http headers, without it just the JSON."
  (let* ((json (if headers (car response) response))
         (headers (if headers (cdr response) nil))
         (link-header (mastodon-tl--get-link-header-from-response headers)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      ;; mastodon-mode wipes buffer-spec, so order must unforch be:
      ;; 1 run update-function, 2 enable masto-mode, 3 set buffer spec.
      ;; which means we cannot use buffer-spec for update-function
      ;; unless we set it both before and after the others
      (mastodon-tl--set-buffer-spec buffer
                                    endpoint
                                    update-function
                                    link-header)
      (setq
       ;; Initialize with a minimal interval; we re-scan at least once
       ;; every 5 minutes to catch any timestamps we may have missed
       mastodon-tl--timestamp-next-update (time-add (current-time)
                                                    (seconds-to-time 300)))
      (funcall update-function json))
    (mastodon-mode)
    (with-current-buffer buffer
      (mastodon-tl--set-buffer-spec buffer
                                    endpoint
                                    update-function
                                    link-header)
      (setq mastodon-tl--timestamp-update-timer
            (when mastodon-tl--enable-relative-timestamps
              (run-at-time (time-to-seconds
                            (time-subtract mastodon-tl--timestamp-next-update
                                           (current-time)))
                           nil ;; don't repeat
                           #'mastodon-tl--update-timestamps-callback
                           (current-buffer)
                           nil)))
      (unless (string-prefix-p "accounts" endpoint)
        ;; for everything save profiles
        (mastodon-tl--goto-first-item)))))

(defun mastodon-tl--init-sync (buffer-name endpoint update-function &optional note-type)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT.

UPDATE-FUNCTION is used to receive more toots.
Runs synchronously.
Optional arg NOTE-TYPE means only get that type of note."
  (let* ((exclude-types (when note-type
                          (mastodon-notifications--filter-types-list note-type)))
         (args (when note-type (mastodon-http--build-array-params-alist
                                "exclude_types[]" exclude-types)))
         ;; (query-string (when note-type
         ;; (mastodon-http--build-params-string args)))
         ;; add note-type exclusions to endpoint so it works in `mastodon-tl--buffer-spec'
         ;; that way `mastodon-tl--more' works seamlessly too:
         ;; (endpoint (if note-type (concat endpoint "?" query-string) endpoint))
         (url (mastodon-http--api endpoint))
         (buffer (concat "*mastodon-" buffer-name "*"))
         (json (mastodon-http--get-json url args)))
    (with-output-to-temp-buffer buffer
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
      (funcall update-function json))
    (mastodon-mode)
    (with-current-buffer buffer
      (mastodon-tl--set-buffer-spec buffer endpoint update-function)
      (setq mastodon-tl--timestamp-update-timer
            (when mastodon-tl--enable-relative-timestamps
              (run-at-time (time-to-seconds
                            (time-subtract mastodon-tl--timestamp-next-update
                                           (current-time)))
                           nil ;; don't repeat
                           #'mastodon-tl--update-timestamps-callback
                           (current-buffer)
                           nil)))
      (when ;(and (not (equal json '[]))
          ;; for everything save profiles:
          (not (string-prefix-p "accounts" endpoint))
        (mastodon-tl--goto-first-item)))
    buffer))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
