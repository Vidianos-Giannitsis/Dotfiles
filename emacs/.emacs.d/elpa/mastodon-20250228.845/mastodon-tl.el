;;; mastodon-tl.el --- Timeline functions for mastodon.el  -*- lexical-binding: t -*-

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

;; mastodon-tl.el provides timeline functions.

;;; Code:

(require 'shr)
(require 'thingatpt) ; for word-at-point
(require 'time-date)
(require 'cl-lib)
(require 'mastodon-iso)
(require 'mpv nil :no-error)
(require 'url-cache)

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
(autoload 'mastodon-notifications-get-mentions "mastodon-notifications")
(autoload 'mastodon-profile--account-from-id "mastodon-profile")
(autoload 'mastodon-profile--extract-users-handles "mastodon-profile")
(autoload 'mastodon-profile--get-preferences-pref "mastodon-profile")
(autoload 'mastodon-profile-get-toot-author "mastodon-profile")
(autoload 'mastodon-profile--lookup-account-in-status "mastodon-profile")
(autoload 'mastodon-profile--make-author-buffer "mastodon-profile")
(autoload 'mastodon-profile-my-profile "mastodon-profile")
(autoload 'mastodon-profile-open-statuses-no-reblogs "mastodon-profile")
(autoload 'mastodon-profile--profile-json "mastodon-profile")
(autoload 'mastodon-profile--search-account-by-handle "mastodon-profile")
(autoload 'mastodon-profile--item-json "mastodon-profile")
(autoload 'mastodon-profile--view-author-profile "mastodon-profile")
(autoload 'mastodon-profile-mode "mastodon-profile")
(autoload 'mastodon-search--get-user-info "mastodon-search")
(autoload 'mastodon-search--insert-users-propertized "mastodon-search")
(autoload 'mastodon-search--propertize-user "mastodon-search")
(autoload 'mastodon-toot--compose-buffer "mastodon-toot")
(autoload 'mastodon-toot-delete-toot "mastodon-toot")
(autoload 'mastodon-toot--get-toot-edits "mastodon-toot")
(autoload 'mastodon-toot--iso-to-human "mastodon-toot")
(autoload 'mastodon-toot-schedule-toot "mastodon-toot")
(autoload 'mastodon-toot--set-toot-properties "mastodon-toot")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")
(autoload 'mastodon-search--buf-type "mastodon-search")
(autoload 'mastodon-views--insert-users-propertized-note "mastodon-views") ; for search pagination
(autoload 'mastodon-http--get-response "mastodon-http")
(autoload 'mastodon-search--insert-heading "mastodon-search")
(autoload 'mastodon-media--process-full-sized-image-response "mastodon-media")
(autoload 'mastodon-search-trending-statuses "mastodon-search")
(autoload 'mastodon-search--format-heading "mastodon-search")
(autoload 'mastodon-media--image-or-cached "mastodon-media")
(autoload 'mastodon-toot--base-toot-or-item-json "mastodon-toot")
(autoload 'mastodon-search-load-link-posts "mastodon-search")
(autoload 'mastodon-notifications--current-type "mastodon-notifications")
(autoload 'mastodon-notifications--timeline "mastodon-notifications")

(defvar mastodon-toot--visibility)
(defvar mastodon-toot-mode)
(defvar mastodon-active-user)
(defvar mastodon-images-in-notifs)
(defvar mastodon-group-notifications)

(when (require 'mpv nil :no-error)
  (declare-function mpv-start "mpv"))
(defvar mastodon-mode-map)
(defvar mastodon-instance-url)
(defvar mastodon-toot-timestamp-format)
(defvar shr-use-fonts)  ;; declare it since Emacs24 didn't have this
(defvar mastodon-media--enable-image-caching)
(defvar mastodon-media--generic-broken-image-data)
(defvar mastodon-media--sensitive-image-data)


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

(defcustom mastodon-tl--no-fill-on-render nil
  "Non-nil to disable filling by shr.el while rendering toot body.
Use this if your setup isn't compatible with shr's window width filling."
  :type '(boolean))

(defcustom mastodon-tl--display-media-p t
  "A boolean value stating whether to show media in timelines."
  :type 'boolean)

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
  :type 'boolean)

(defcustom mastodon-tl--symbols
  '((reply                 . ("💬" . "R"))
    (boost                 . ("🔁" . "B"))
    (reblog                . ("🔁" . "B")) ;; server compat
    (favourite             . ("⭐" . "F"))
    (bookmark              . ("🔖" . "K"))
    (media                 . ("📹" . "[media]"))
    (verified              . ("✓" . "V"))
    (locked                . ("🔒" . "[locked]"))
    (private               . ("🔒" . "[followers]"))
    (mention               . ("@"  . "[mention]"))
    (direct                . ("✉" . "[direct]"))
    (edited                . ("✍" . "[edited]"))
    (update                . ("✍" . "[edited]")) ;; server compat
    (status                . ("✍" . "[posted]"))
    (replied               . ("⬇" . "↓"))
    (reply-bar             . ("┃" . "|"))
    (poll                  . ("📊" . ""))
    (follow                . ("👤" . "+"))
    (follow_request        . ("👤" . "+"))
    (severed_relationships . ("🔗" . "//"))
    (moderation_warning    . ("⚠" . "!!")))
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

(defcustom mastodon-tl--highlight-current-toot nil
  "Whether to highlight the toot at point. Uses `cursor-face' special property."
  :type '(boolean))

(defcustom mastodon-tl--expand-content-warnings 'server
  "Whether to expand content warnings by default.
The API returns data about this setting on the server, but no
means to set it, so we roll our own option here to override the
server setting if desired. If you change the server setting and
want it to be respected by mastodon.el, you'll likely need to
either unset `mastodon-profile-acccount-preferences-data' and
re-load mastodon.el, or restart Emacs."
  :type '(choice (const :tag "true" t)
                 (const :tag "false" nil)
                 (const :tag "follow server setting" server)))

(defcustom mastodon-tl--tag-timeline-tags nil
  "A list of up to four tags for use with `mastodon-tl-followed-tags-timeline'."
  :type '(repeat string))

(defcustom mastodon-tl--load-full-sized-images-in-emacs t
  "Whether to load full-sized images inside Emacs.
Full-sized images are loaded when you hit return on or click on
an image in a timeline.
If nil, mastodon.el will instead call `shr-browse-image', which
respects the user's `browse-url' settings."
  :type '(boolean))

(defcustom mastodon-tl--remote-local-domains nil
  "A list of domains to view the local timelines of.
See `mastodon-tl-get-remote-local-timeline' for view remote local domains."
  :type '(repeat string))

(defcustom mastodon-tl--fold-toots-at-length 1200
  "Length, in characters, to fold a toot at.
Longer toots will be folded and the remainder replaced by a
\"read more\" button. If the value is nil, don't fold at all."
  :type '(integer))


;;; VARIABLES

(defvar-local mastodon-tl--buffer-spec nil
  "A unique identifier and functions for each mastodon buffer.")

(defvar-local mastodon-tl--update-point nil
  "When updating a mastodon buffer this is where new toots will be inserted.
If nil `(point-min)' is used instead.")

(defvar-local mastodon-tl--after-update-marker nil
  "Marker defining the position of point after the update is done.")

(defvar-local mastodon-tl--timestamp-next-update nil
  "The timestamp when the buffer should next be scanned to update the timestamps.")

(defvar-local mastodon-tl--timestamp-update-timer nil
  "The timer that, when set will scan the buffer to update the timestamps.")

(defvar mastodon-tl--horiz-bar
  (make-string 12
               (if (char-displayable-p ?―) ?― ?-)))


;;; KEYMAPS

(defvar mastodon-tl--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'mastodon-tl-do-link-action-at-point)
    (define-key map [mouse-2] #'mastodon-tl-do-link-action)
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
    (define-key map [remap shr-next-link] #'mastodon-tl-next-tab-item)
    (define-key map [remap shr-previous-link] #'mastodon-tl-previous-tab-item)
    ;; keep new my-profile binding; shr 'O' doesn't work here anyway
    (define-key map (kbd "O") #'mastodon-profile-my-profile)
    ;; remove shr's u binding, as it the maybe-probe-and-copy-url
    ;; is already bound to w also
    (define-key map (kbd "u") #'mastodon-tl-update)
    (define-key map [remap shr-browse-url] #'mastodon-url-lookup)
    (define-key map (kbd "M-RET") #'mastodon-search-load-link-posts)
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
    (define-key map [remap shr-next-link] #'mastodon-tl-next-tab-item)
    (define-key map [remap shr-previous-link] #'mastodon-tl-previous-tab-item)
    ;; browse-url loads the preview only, we want browse-image
    ;; on RET to browse full sized image URL
    (define-key map [remap shr-browse-url] #'mastodon-tl-view-full-image-or-play-video)
    ;; remove shr's u binding, as it the maybe-probe-and-copy-url
    ;; is already bound to w also
    (define-key map (kbd "u") #'mastodon-tl-update)
    ;; keep new my-profile binding; shr 'O' doesn't work here anyway
    (define-key map (kbd "O") #'mastodon-profile-my-profile)
    (define-key map (kbd "C") #'mastodon-tl-copy-image-caption)
    (define-key map (kbd "S") #'mastodon-tl-toggle-sensitive-image)
    (define-key map (kbd "<C-return>") #'mastodon-tl-mpv-play-video-at-point)
    (define-key map (kbd "<mouse-2>") #'mastodon-tl-click-image-or-video)
    map)
  "The keymap to be set for shr.el generated image links.
We need to override the keymap so tabbing will navigate to all
types of mastodon links and not just shr.el-generated ones.")

(defvar mastodon-tl--byline-link-keymap
  (when (require 'mpv nil :no-error)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-return>") #'mastodon-tl-mpv-play-video-from-byline)
      (define-key map (kbd "RET") #'mastodon-profile-get-toot-author)
      (define-key map (kbd "S") #'mastodon-tl-toggle-sensitive-image)
      map))
  "The keymap to be set for the author byline.
It is active where point is placed by `mastodon-tl-goto-next-item.'")


;;; MACROS

(defmacro with-mastodon-buffer (buffer mode-fun other-window &rest body)
  "Evaluate BODY in a new or existing buffer called BUFFER.
MODE-FUN is called to set the major mode.
OTHER-WINDOW means call `switch-to-buffer-other-window' rather
than `pop-to-buffer'."
  (declare (debug t)
           (indent 3))
  `(with-current-buffer (get-buffer-create ,buffer)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (funcall ,mode-fun)
       (remove-overlays) ; video overlays
       ,@body
       ;; return result of switching buffer:
       (if ,other-window
           (switch-to-buffer-other-window ,buffer)
         (pop-to-buffer ,buffer '(display-buffer-same-window))))))

(defmacro mastodon-tl--do-if-item (&rest body)
  "Execute BODY if we have an item at point."
  (declare (debug t))
  `(if (and (not (mastodon-tl--profile-buffer-p))
            (not (mastodon-tl--property 'item-json))) ; includes users but not tags
       (user-error "Looks like there's no item at point?")
     ,@body))


;;; NAV

(defun mastodon-tl-scroll-up-command ()
  "Call `scroll-up-command', loading more toots if necessary.
If we hit `point-max', call `mastodon-tl--more' then `scroll-up-command'."
  (interactive)
  (if (not (eq (point) (point-max)))
      (scroll-up-command)
    (mastodon-tl--more)
    (scroll-up-command)))

(defun mastodon-tl-next-tab-item (&optional previous)
  "Move to the next interesting item.
This could be the next toot, link, or image; whichever comes first.
Don't move if nothing to move to is found, i.e. near the end of the buffer.
This also skips tab items in invisible text, i.e. hidden spoiler text.
PREVIOUS means move to previous item."
  (interactive)
  (let (next-range
        (search-pos (point)))
    (while (and (setq next-range
                      (mastodon-tl--find-next-or-previous-property-range
                       'mastodon-tab-stop search-pos previous))
                (get-text-property (car next-range) 'invisible)
                (setq search-pos (if previous
                                     (1- (car next-range))
                                   (1+ (cdr next-range)))))
      ;; do nothing, all the action is in the while condition
      )
    (if (null next-range)
        (user-error "Nothing else here")
      (goto-char (car next-range))
      (message "%s" (mastodon-tl--property 'help-echo :no-move)))))

(defun mastodon-tl-previous-tab-item ()
  "Move to the previous interesting item.
This could be the previous toot, link, or image; whichever comes
first. Don't move if nothing else to move to is found, i.e. near
the start of the buffer. This also skips tab items in invisible
text, i.e. hidden spoiler text."
  (interactive)
  (mastodon-tl-next-tab-item :previous))

(defun mastodon-tl--goto-item-pos (find-pos refresh &optional pos)
  "Search for item with function FIND-POS.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (let* ((npos ; toot/user items have byline:
          (funcall find-pos
                   (or pos (point))
                   ;; FIXME: we need to fix item-type?
                   ;; 'item-type ; breaks nav to last item in a view?
                   'byline
                   (current-buffer)))
         (max-lisp-eval-depth 4)) ;; clamp down on endless loops
    (if npos
        (if (not (get-text-property npos 'item-type)) ; generic
            ;; FIXME let's make refresh &optional and only call refresh/recur
            ;; if non-nil:
            (mastodon-tl--goto-item-pos find-pos refresh npos)
          (goto-char npos)
          ;; force display of help-echo on moving to a toot byline:
          (mastodon-tl--message-help-echo))
      (condition-case nil
          (funcall refresh)
        (error "No more items")))))

(defun mastodon-tl-goto-next-item (&optional no-refresh)
  "Jump to next item.
Load more items it no next item.
NO-REFRESH means do no not try to load more items if no next item
found."
  (interactive)
  (condition-case nil
      (mastodon-tl--goto-item-pos 'next-single-property-change
                                  (unless no-refresh 'mastodon-tl--more))
    (t (error "No more items"))))

(defun mastodon-tl-goto-prev-item (&optional no-refresh)
  "Jump to previous item.
Update if no previous items.
NO-REFRESH means do no not try to load more items if no next item
found."
  (interactive)
  (condition-case nil
      (mastodon-tl--goto-item-pos 'previous-single-property-change
                                  (unless no-refresh 'mastodon-tl-update))
    (t (error "No more items"))))

(defun mastodon-tl--goto-first-item ()
  "Jump to first toot or item in buffer.
Used on initializing a timeline or thread."
  (goto-char (point-min))
  (condition-case nil
      (mastodon-tl--goto-item-pos 'next-single-property-change
                                  'next-line)
    (t (error "No item"))))


;;; TIMELINES

(defun mastodon-tl-get-federated-timeline (&optional prefix local max-id)
  "Open federated timeline.
If LOCAL, get only local timeline.
With a single PREFIX arg, hide-replies.
With a double PREFIX arg, only show posts with media."
  (interactive "p")
  (let ((params
         `(("limit" . ,mastodon-tl--timeline-posts-count)
           ,@(when (eq prefix 16)
               '(("only_media" . "true")))
           ,@(when local
               '(("local" . "true")))
           ,@(when max-id
               `(("max_id" . ,(mastodon-tl--buffer-property 'max-id)))))))
    (message "Loading federated timeline...")
    (mastodon-tl--init (if local "local" "federated")
                       "timelines/public" 'mastodon-tl--timeline nil
                       params
                       (when (eq prefix 4) t))))

(defun mastodon-tl-get-home-timeline (&optional arg max-id)
  "Open home timeline.
With a single prefix ARG, hide replies.
MAX-ID is a flag to add the max_id pagination parameter."
  (interactive "p")
  (let* ((params
          `(("limit" . ,mastodon-tl--timeline-posts-count)
            ,(when max-id
               `("max_id" . ,(mastodon-tl--buffer-property 'max-id))))))
    (message "Loading home timeline...")
    (mastodon-tl--init "home" "timelines/home" 'mastodon-tl--timeline nil
                       params
                       (when (eq arg 4) t))))

(defun mastodon-tl-get-remote-local-timeline (&optional endpoint)
  "Prompt for an instance domain and try to display its local timeline.
You can enter any working instance domain. Domains that you want
to regularly load can be stored in
`mastodon-tl--remote-local-domains' for easy access with completion.
Note that some instances do not make their local timelines public, in
which case this will not work.
To interact with any item, you must view it from your own
instance, which you can do with
`mastodon-tl-view-item-on-own-instance'.
Optionally, provide API ENDPOINT."
  (interactive)
  (let* ((domain (completing-read "Domain for remote local tl: "
                                  mastodon-tl--remote-local-domains))
         (params `(("limit" . ,mastodon-tl--timeline-posts-count)
                   ("local" . "true")))
         (buf (concat "remote-local-" domain))
         (known (member domain
                        (mastodon-http--get-json
                         (mastodon-http--api "instance/peers")))))
    ;; condition-case doesn't work here, so i added basic error handling to
    ;; `mastodon-tl--init*' instead
    (when (or known
              (y-or-n-p
               "Domain appears unknown to your instance. Proceed?"))
      (mastodon-tl--init buf
                         (or endpoint "timelines/public")
                         'mastodon-tl--timeline nil
                         params nil domain))))

(defun mastodon-tl-remote-tag-timeline (&optional tag)
  "Call `mastodon-tl-get-remote-local-timeline' but for a TAG timeline."
  (interactive)
  (let* ((tag (or tag (read-string "Tag: ")))
         (endpoint (format "timelines/tag/%s" tag)))
    (mastodon-tl-get-remote-local-timeline endpoint)))

(defun mastodon-tl-view-item-on-own-instance ()
  "Load current toot on your own instance.
Use this to re-load remote-local items in order to interact with them."
  (interactive)
  (mastodon-tl--do-if-item
   (let* ((toot (mastodon-tl--property 'item-json))
          (uri (mastodon-tl--field 'uri toot)))
     (mastodon-url-lookup uri))))

(defun mastodon-tl-get-local-timeline (&optional prefix max-id)
  "Open local timeline.
With a single PREFIX arg, hide-replies.
With a double PREFIX arg, only show posts with media.
MAX-ID is a flag to add the max_id pagination parameter."
  (interactive "p")
  (message "Loading local timeline...")
  (mastodon-tl-get-federated-timeline prefix :local max-id))

(defun mastodon-tl-get-tag-timeline (&optional prefix tag)
  "Prompt for tag and opens its timeline.
Optionally load TAG timeline directly.
With a single PREFIX arg, only show posts with media.
With a double PREFIX arg, limit results to your own instance."
  (interactive "p")
  (let* ((word (or (word-at-point) ""))
         (input (or tag (read-string
                         (format "Load timeline for tag (%s): " word))))
         (tag (or tag (if (string-empty-p input) word input))))
    (message "Loading timeline for #%s..." tag)
    (mastodon-tl--show-tag-timeline prefix tag)))

(defun mastodon-tl--show-tag-timeline (&optional prefix tag)
  "Opens a new buffer showing the timeline of posts with hastag TAG.
If TAG is a list, show a timeline for all tags.
With a single PREFIX arg, only show posts with media.
With a double PREFIX arg, limit results to your own instance."
  (let ((params
         `(("limit" . ,mastodon-tl--timeline-posts-count)
           ,@(when (eq prefix 4)
               '(("only_media" . "true")))
           ,@(when (eq prefix 16)
               '(("local" . "true"))))))
    (when (listp tag)
      (let ((list (mastodon-http--build-array-params-alist "any[]" (cdr tag))))
        (while list
          (push (pop list) params))))
    (mastodon-tl--init
     (if (listp tag) "tags-multiple" (concat "tag-" tag))
     (concat "timelines/tag/" (if (listp tag) (car tag) tag)) ; must be /tag/:sth
     'mastodon-tl--timeline nil params)))

(defun mastodon-tl--link-timeline (url)
  "Load a link timeline, displaying posts containing URL."
  (let ((params `(("url" . ,url))))
    (mastodon-tl--init "links" "timelines/link"
                       'mastodon-tl--timeline nil
                       params)))

(defun mastodon-tl-announcements ()
  "Display announcements from your instance."
  (interactive)
  (mastodon-tl--init "announcements" "announcements"
                     'mastodon-tl--timeline nil nil nil nil :no-byline))


;;; BYLINES, etc.

(defun mastodon-tl--message-help-echo ()
  "Call message on `help-echo' property at point.
Do so if type of status at poins is not follow_request/follow."
  (let ((type (alist-get 'type
                         (mastodon-tl--property 'item-json :no-move)))
        (echo (mastodon-tl--property 'help-echo :no-move)))
    (when (not (string= "" echo)) ; not for followers/following in profile
      (unless (or (string= type "follow_request")
                  (string= type "follow")) ; no counts for these
        (message "%s" echo)))))

(defun mastodon-tl--byline-username (toot)
  "Format a byline username from account in TOOT.
TOOT may be account data, or toot data, in which case acount data
is extracted from it."
  (let ((data (or (alist-get 'account toot)
                  toot))) ;; grouped nofifs use account data directly
    (let-alist data
      (propertize (if (not (string-empty-p .display_name))
                      .display_name
                    .username)
                  'face 'mastodon-display-name-face
                  ;; enable playing of videos when point is on byline:
                  ;; 'attachments (mastodon-tl--get-attachments-for-byline toot)
                  'keymap mastodon-tl--byline-link-keymap
                  ;; echo faves count when point on post author name:
                  ;; which is where --goto-next-toot puts point.
                  'help-echo
                  ;; but don't add it to "following"/"follows" on
                  ;; profile views: we don't have a tl--buffer-spec
                  ;; yet:
                  (unless (or (string-suffix-p "-followers*" (buffer-name))
                              (string-suffix-p "-following*" (buffer-name)))
                    (mastodon-tl--format-byline-help-echo data))))))

(defun mastodon-tl--byline-handle (toot &optional domain string face)
  "Format a byline handle from account in TOOT.
DOMAIN is optionally added to the handle.
ACCOUNT is optionally acccount data to use.
STRING is optionally the string to propertize, it is used to make
username rather than handle buttons.
FACE is optionally the face to use.
The last two args allow for display a username as a clickable
handle."
  (let-alist (or (alist-get 'account toot)
                 toot) ;; grouped notifs
    (mastodon-tl--buttonify-link
     (or string
         (concat "@" .acct
                 (when domain
                   (concat "@"
                           (url-host
                            (url-generic-parse-url .url))))))
     'face (or face 'mastodon-handle-face)
     'mastodon-tab-stop 'user-handle
     'shr-url .url
     'mastodon-handle (concat "@" .acct)
     'help-echo (concat "Browse user profile of @" .acct))))

(defun mastodon-tl--byline-uname-+-handle (data &optional domain)
  "Concatenate a byline username and handle.
DATA is the (toot) data to use.
DOMAIN is optionally a domain for the handle.
ACCOUNT is optionally acccount data to use."
  (concat (mastodon-tl--byline-username data)
          " (" (mastodon-tl--byline-handle data domain) ")"))

(defun mastodon-tl--display-or-uname (account)
  "Return display name or username from ACCOUNT data."
  (if (not (string-empty-p (alist-get 'display_name account)))
      (alist-get 'display_name account)
    (alist-get 'username account)))

(defun mastodon-tl--byline-author (toot &optional avatar domain base)
  "Propertize author of TOOT.
With arg AVATAR, include the account's avatar image.
When DOMAIN, force inclusion of user's domain in their handle.
BASE means to use data from the base item (reblog slot) if possible.
If BASE is nil, we are a boosted byline, so show less info.
ACCOUNT is optionally acccount data to use."
  (let* ((data (if base
                   (mastodon-tl--toot-or-base toot)
                 toot))
         (account (alist-get 'account data))
         (uname (mastodon-tl--display-or-uname account)))
    (concat
     ;; avatar insertion moved up to `mastodon-tl--byline' by default to
     ;; be outside 'byline propt.
     (when (and avatar ; used by `mastodon-profile--format-user'
                mastodon-tl--show-avatars
                mastodon-tl--display-media-p
                (mastodon-tl--image-trans-check))
       (mastodon-media--get-avatar-rendering
        (map-nested-elt data '(account avatar))))
     (if (not base)
         ;; boost symbol:
         (concat (mastodon-tl--symbol 'boost)
                 " "
                 ;; username as button:
                 (mastodon-tl--byline-handle
                  data domain
                  ;; display uname not handle (for boosts):
                  uname 'mastodon-display-name-face))
       ;; normal combo author byline:
       (mastodon-tl--byline-uname-+-handle data domain)))))

(defun mastodon-tl--format-byline-help-echo (toot)
  "Format a help-echo for byline of TOOT.
Displays a toot's media types and optionally the binding to play
moving image media from the byline.
Used when point is at the start of a byline, i.e. where
`mastodon-tl-goto-next-item' leaves point."
  (let* ((toot-to-count
          (or ; simply praying this order works
           (alist-get 'status toot) ; notifications timeline
           ;; fol-req notif, has 'type placed before boosts coz fol-reqs have
           ;; a (useless) reblog entry:
           (when (and (or (mastodon-tl--buffer-type-eq 'notifications)
                          (mastodon-tl--buffer-type-eq 'mentions))
                      (alist-get 'type toot))
             toot)
           (alist-get 'reblog toot) ; boosts
           toot)) ; everything else
         (fol-req-p (let ((type (alist-get 'type toot-to-count)))
                      (or (string= type "follow")
                          (string= type "follow_request")))))
    (unless fol-req-p
      (let* ((media-types (mastodon-tl--get-media-types toot))
             (format-media (when media-types
                             (format "media: %s"
                                     (mapconcat #'identity media-types " "))))
             (format-media-binding (when (and (or (member "video" media-types)
                                                  (member "gifv" media-types))
                                              (require 'mpv nil :no-error))
                                     " | C-RET to view with mpv")))
        (concat format-media format-media-binding)))))

(defun mastodon-tl--get-media-types (toot)
  "Return a list of the media attachment types of the TOOT at point."
  (let* ((attachments (mastodon-tl--field 'media_attachments toot)))
    (mastodon-tl--map-alist 'type attachments)))

(defun mastodon-tl--get-attachments-for-byline (toot)
  "Return a list of attachment URLs and types for TOOT.
The result is added as an attachments property to author-byline."
  (let ((media (mastodon-tl--field 'media_attachments toot)))
    (mapcar (lambda (attachment)
              (let-alist attachment
                (list :url (or .remote_url .url) ; fallback for notifications
                      :type .type)))
            media)))

(defun mastodon-tl--byline-booster (toot)
  "Add author byline for booster from TOOT.
Only return something if TOOT contains a reblog."
  (let ((reblog (alist-get 'reblog toot)))
    (if reblog
        (mastodon-tl--byline-author toot)
      "")))

(defun mastodon-tl--byline-booster-str (toot)
  "Format boosted string for action byline.
Only return string if TOOT contains a reblog."
  (let ((reblog (alist-get 'reblog toot)))
    (if reblog
        (concat
         " " (propertize "boosted" 'face 'mastodon-boosted-face) "\n")
      "")))

(defun mastodon-tl--byline-boost (toot)
  "Format a boost action-byline element for TOOT."
  (concat (mastodon-tl--byline-booster toot)
          (mastodon-tl--byline-booster-str toot)))

(defun mastodon-tl--format-faved-or-boosted-byline (letter)
  "Format the byline marker for a boosted or favourited status.
LETTER is a string, F for favourited, B for boosted, or K for bookmarked."
  (let ((help-string (cond ((string= letter "F")
                            "favourited")
                           ((string= letter "B")
                            "boosted")
                           ((string= letter (or "🔖" "K"))
                            "bookmarked"))))
    (format "(%s) "
            (propertize letter 'face 'mastodon-boost-fave-face
                        ;; emojify breaks this for 🔖:
                        'help-echo (format "You have %s this status."
                                           help-string)))))

(defun mastodon-tl--image-trans-check ()
  "Call `image-transforms-p', or `image-type-available-p' imagemagick."
  (if (version< emacs-version "27.1")
      (image-type-available-p 'imagemagick)
    (image-transforms-p)))

(defun mastodon-tl--byline (toot &optional detailed-p
                                 domain base-toot group ts)
  "Generate (bottom) byline for TOOT.
AUTHOR-BYLINE is a function for adding the author portion of
the byline that takes one variable.
DETAILED-P means display more detailed info. For now
this just means displaying toot client.
When DOMAIN, force inclusion of user's domain in their handle.
BASE-TOOT is JSON for the base toot, if any.
GROUP is the notification group if any.
ACCOUNT is the notification account if any.
TS is a timestamp from the server, if any."
  (let* ((type (alist-get 'type (or group toot)))
         (created-time
          (or ts ;; mentions, statuses, folls/foll-reqs
              ;; bosts, faves, edits, polls in notifs view use base item
              ;; timestamp:
              (mastodon-tl--field 'created_at
                                  (mastodon-tl--field 'status toot))
              ;; all other toots, inc. boosts/faves in timelines:
              ;; (mastodon-tl--field auto fetches from reblogs if needed):
              (mastodon-tl--field 'created_at toot)))
         (parsed-time (when created-time (date-to-time created-time)))
         ;; non-grouped notifs now need to pull the following data from
         ;; base toot:
         (base-maybe (or base-toot ;; show edits for notifs
                         (mastodon-tl--toot-or-base toot))) ;; for boosts
         (faved (eq t (mastodon-tl--field 'favourited base-maybe)))
         (boosted (eq t (mastodon-tl--field 'reblogged base-maybe)))
         (bookmarked (eq t (mastodon-tl--field 'bookmarked base-maybe)))
         (visibility (mastodon-tl--field 'visibility base-maybe))
         (account (alist-get 'account base-maybe))
         (avatar-url (alist-get 'avatar account))
         (edited-time (alist-get 'edited_at base-maybe))
         (edited-parsed (when edited-time (date-to-time edited-time))))
    (concat
     ;; Boosted/favourited markers are not technically part of the byline, so
     ;; we don't propertize them with 'byline t', as per the rest. This
     ;; ensures that `mastodon-tl-goto-next-item' puts point on
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
     ;; we remove avatars from the byline also, so that they also do not
     ;; mess with `mastodon-tl-goto-next-item':
     (when (and mastodon-tl--show-avatars
                mastodon-tl--display-media-p
                (mastodon-tl--image-trans-check))
       (mastodon-media--get-avatar-rendering avatar-url))
     (propertize
      (concat
       ;; NB: action-byline (boost) is now added in insert-status, so no
       ;; longer part of the byline.
       ;; (base) author byline:
       ;; we use base-toot if poss for fave/boost notifs that need to show
       ;; base item in author byline
       (mastodon-tl--byline-author (or base-toot toot)
                                   nil domain :base)
       ;; visibility:
       (cond ((string= visibility "direct")
              (propertize (concat " " (mastodon-tl--symbol 'direct))
                          'help-echo visibility))
             ((string= visibility "private")
              (propertize (concat " " (mastodon-tl--symbol 'private))
                          'help-echo visibility)))
       " "
       ;; timestamp:
       (let ((ts (format-time-string
                  mastodon-toot-timestamp-format parsed-time)))
         (propertize ts
                     'timestamp parsed-time
                     'display
                     (if mastodon-tl--enable-relative-timestamps
                         (mastodon-tl--relative-time-description parsed-time)
                       parsed-time)
                     'help-echo ts))
       ;; detailed:
       (when detailed-p
         (let* ((app-name (map-nested-elt toot '(application name)))
                (app-url (map-nested-elt toot '(application website))))
           (when app-name
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
       ;; edited:
       (when edited-time
         (concat
          " "
          (mastodon-tl--symbol 'edited)
          " "
          (propertize
           (format-time-string mastodon-toot-timestamp-format
                               edited-parsed)
           'face 'mastodon-toot-docs-face
           'timestamp edited-parsed
           'display (if mastodon-tl--enable-relative-timestamps
                        (mastodon-tl--relative-time-description edited-parsed)
                      edited-parsed))))
       (propertize (concat "\n  " mastodon-tl--horiz-bar)
                   'face 'default)
       ;; stats:
       (when (and mastodon-tl--show-stats
                  (not (member type '("follow" "follow_request"))))
         (mastodon-tl--toot-stats toot))
       "\n")
      'favourited-p faved
      'boosted-p    boosted
      'bookmarked-p bookmarked
      ;; enable playing of videos when point is on byline:
      'attachments (mastodon-tl--get-attachments-for-byline toot)
      'edited edited-time
      'edit-history (when edited-time
                      (mastodon-toot--get-toot-edits
                       (alist-get 'id base-maybe)))
      'byline       t))))


;;; TIMESTAMPS

(defun mastodon-tl--relative-time-details (timestamp &optional current-time)
  "Return cons of (DESCRIPTIVE STRING . NEXT-CHANGE) for the TIMESTAMP.
Use the optional CURRENT-TIME as the current time (only used for
reliable testing).
The descriptive string is a human readable version relative to
the current time while the next change timestamp give the first
time that this description will change in the future.
TIMESTAMP is assumed to be in the past."
  (let* ((time-difference (time-subtract current-time timestamp))
         (seconds-difference (float-time time-difference))
         (tmp (mastodon-tl--human-duration (max 0 seconds-difference))))
    ;; revert to old just now style for < 1 min
    (cons (concat (car tmp) (if (string= "just now" (car tmp)) "" " ago"))
          (time-add current-time (cdr tmp)))))

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
                         (if mastodon-tl--no-fill-on-render
                             0
                           (- (window-width) 3))))
            (cat (get 'mastodon-tl-link 'button-category-symbol)))
        (shr-render-region (point-min) (point-max))
        (alter-text-property
         (point-min) (point-max) 'category
         (lambda (type) (when type cat))))
      ;; Make all links a tab stop recognized by our own logic, make
      ;; things point to our own logic (e.g. hashtags), and update keymaps
      ;; where needed:
      (when toot
        (let (region)
          (while (setq region (mastodon-tl--find-property-range
                               'shr-url (or (cdr region) (point-min))))
            (mastodon-tl--process-link toot
                                       (car region) (cdr region)
                                       (get-text-property (car region) 'shr-url))
            (when (proper-list-p toot) ;; not on profile fields cons cells
              ;; render card author maybe:
              (let* ((card-url (map-nested-elt toot '(card url)))
                     (authors (map-nested-elt toot '(card authors)))
                     (url (buffer-substring (car region) (cdr region)))
                     (url-no-query (car (split-string url "?"))))
                (when (and (string= url-no-query card-url)
                           ;; only if we have an account's data:
                           (alist-get 'account (car authors)))
                  (goto-char (point-max))
                  (mastodon-tl--insert-card-authors authors)))))))
      (buffer-string))))

(defun mastodon-tl--insert-card-authors (authors)
  "Insert a string of card AUTHORS."
  (let ((authors-str (format "Author%s: "
                             (if (< 1 (length authors)) "s" ""))))
    (insert
     (concat
      "\n(" authors-str
      (mapconcat #'mastodon-tl--format-card-author authors "\n")
      ")\n"))))

(defun mastodon-tl--format-card-author (data)
  "Render card author DATA."
  (when-let* ((account (alist-get 'account data))) ;.account
    (let-alist account ;.account
      ;; FIXME: replace with refactored handle render fun
      ;; in byline refactor branch:
      (concat
       (propertize (or .display_name .username)
                   'face 'mastodon-display-name-face
                   'item-type 'user
                   'item-id .id)
       " "
       (propertize (concat "@" .acct)
                   'face 'mastodon-handle-face
                   'mouse-face 'highlight
		           'mastodon-tab-stop 'user-handle
		           'keymap mastodon-tl--link-keymap
                   'mastodon-handle (concat "@" .acct)
		           'help-echo (concat "Browse user profile of @" .acct))))))

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
         (maybe-hashtag (mastodon-tl--hashtag-from-url
                         url toot-instance-url))
         (maybe-userhandle
          (if (proper-list-p toot) ; fails for profile buffers?
              (or (mastodon-tl--userhandle-from-mentions toot link-str)
                  (mastodon-tl--userhandle-from-url url link-str))
            (mastodon-tl--userhandle-from-url url link-str))))
    (cond (maybe-hashtag
           (setq mastodon-tab-stop-type 'hashtag
                 keymap mastodon-tl--link-keymap
                 help-echo (concat "Browse tag #" maybe-hashtag)
                 extra-properties (list 'mastodon-tag maybe-hashtag)))
          (maybe-userhandle ;; fails on mentions in profile notes:
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
          (t ;; Anything else (leave it as a url handled by shr.el):
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
LINK is maybe the `@handle' to search for."
  (mastodon-tl--el-from-mentions 'acct toot link))

(defun mastodon-tl--extract-userid-toot (toot link)
  "Extract a user id for an ACCT from mentions in a TOOT.
LINK is maybe the `@handle' to search for."
  (mastodon-tl--el-from-mentions 'id toot link))

(defun mastodon-tl--el-from-mentions (el toot link)
  "Extract element EL from TOOT mentions that matches LINK.
LINK should be a simple handle string with no domain, i.e. \"@user\".
Return nil if no matching element."
  (let ((mentions (alist-get 'mentions toot)))
    (when mentions
      (let* ((mention (pop mentions))
             (name (substring-no-properties link 1 (length link))) ; cull @
             return)
        (while mention
          (when (string= name (alist-get 'username mention))
            (setq return (alist-get el mention)))
          (setq mention (pop mentions)))
        return))))

(defun mastodon-tl--userhandle-from-url (url buffer-text)
  "Return the user hande the URL points to or nil if it is not a profile link.
BUFFER-TEXT is the text covered by the link with URL, for a user profile
this should be of the form <at-sign><user id>, e.g. \"@Gargon\"."
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (local-p (string=
                   (url-host (url-generic-parse-url mastodon-instance-url))
                   host))
         (path (url-filename parsed-url)))
    (when (and (string= "@" (substring buffer-text 0 1))
               ;; don't error on domain only url (rare):
               (not (string= "" path))
               (string= (downcase buffer-text)
                        (downcase (substring path 1))))
      (if local-p
          buffer-text ; no instance suffix for local mention
        (concat buffer-text "@" host)))))

(defun mastodon-tl--hashtag-from-url (url instance-url)
  "Return the hashtag that URL points to or nil if URL is not a tag link.
INSTANCE-URL is the url of the instance for the toot that the link
came from (tag links always point to a page on the instance publishing
the toot)."
  ;; TODO: do we rly need to check it against instance-url?
  ;; test suggests we might
  (let* ((instance-host (url-host
                         (url-generic-parse-url instance-url)))
         (parsed (url-generic-parse-url url))
         (path (url-filename parsed))
         (split (split-string path "/")))
    (when (and (string= instance-host (url-host parsed))
               (string-prefix-p "/tag" path)) ;; "/tag/" or "/tags/"
      (nth 2 split))))


;;; HYPERLINKS

(define-button-type 'mastodon-tl-link
  'action #'mastodon-tl--push-button
  'keymap mastodon-tl--link-keymap
  'mouse-face 'highlight)

(defun mastodon-tl--push-button (button)
  "Do the appropriate action for BUTTON."
  (mastodon-tl-do-link-action-at-point (button-start button)))

(defun mastodon-tl--buttonify-link (string &rest properties)
  "Make STRING a `mastodon-tl-link' type button.
PROPERTIES are additional properties to attach to string."
  (apply #'propertize string
         'button t
         'category (get 'mastodon-tl-link 'button-category-symbol)
         properties))

(defun mastodon-tl--make-link (string link-type)
  "Return a propertized version of STRING that will act like link.
LINK-TYPE is the type of link to produce."
  (let ((help-text (cond ((eq link-type 'content-warning)
                          "Toggle hidden text")
                         ((or (eq link-type 'read-more)
                              (eq link-type 'read-less))
                          "Toggle full post")
                         (t
                          (error "Unknown link type %s" link-type)))))
    (mastodon-tl--buttonify-link string
                                 'mastodon-tab-stop link-type
                                 'help-echo help-text)))

(defun mastodon-tl-do-link-action-at-point (pos)
  "Do the action of the link at POS.
Used for hitting RET on a given link."
  (interactive "d")
  (let ((link-type (get-text-property pos 'mastodon-tab-stop)))
    (cond ((eq link-type 'content-warning)
           (mastodon-tl--toggle-spoiler-text pos))
          ((eq link-type 'hashtag)
           (mastodon-tl--show-tag-timeline
            nil (get-text-property pos 'mastodon-tag)))
          ;; 'account / 'account-id is not set for mentions, only bylines
          ((eq link-type 'user-handle)
           (let ((account-json (get-text-property pos 'account))
                 (account-id (get-text-property pos 'account-id)))
             (cond
              (account-json
               (mastodon-profile--make-author-buffer account-json))
              (account-id
               (mastodon-profile--make-author-buffer
                (mastodon-profile--account-from-id account-id)))
              (t
               (let ((account (mastodon-profile--search-account-by-handle
                               (get-text-property pos 'mastodon-handle))))
                 ;; never call make-author-buffer on nil account:
                 (cond (account
                        (mastodon-profile--make-author-buffer account))
                       ;; optional webfinger lookup:
                       ((y-or-n-p
                         "Search for account returned nothing. Perform URL lookup?")
                        (mastodon-url-lookup (get-text-property pos 'shr-url)))
                       (t
                        (error "Unable to find account"))))))))
          ((eq link-type 'shr-url)
           (mastodon-url-lookup (get-text-property pos 'shr-url)))
          ((eq link-type 'read-more)
           (mastodon-tl-unfold-post))
          ((eq link-type 'read-less)
           (mastodon-tl-fold-post))
          (t
           (error "Unknown link type %s" link-type)))))

(defun mastodon-tl-do-link-action (event)
  "Do the action of the link at point.
Used for a mouse-click EVENT on a link."
  (interactive "@e")
  (mastodon-tl-do-link-action-at-point (posn-point (event-end event))))


;;; CONTENT WARNINGS

(defun mastodon-tl--has-spoiler (toot)
  "Check if the given TOOT has a spoiler text.
Spoiler text should initially be shown only while the main
content should be hidden."
  (let ((spoiler (mastodon-tl--field 'spoiler_text toot)))
    (and spoiler (> (length spoiler) 0))))

(defun mastodon-tl--toggle-spoiler-text (position)
  "Toggle the visibility of the spoiler text at/after POSITION."
  (let* ((inhibit-read-only t)
         (spoiler-region (mastodon-tl--find-property-range
                          'mastodon-content-warning-body position nil))
         (new-state (not (get-text-property (car spoiler-region)
                                            'invisible))))
    (if (not spoiler-region)
        (user-error "No spoiler text here")
      (add-text-properties (car spoiler-region) (cdr spoiler-region)
                           (list 'invisible new-state))
      new-state))) ;; return what we set invisibility to

(defun mastodon-tl-toggle-spoiler-text-in-toot ()
  "Toggle the visibility of the spoiler text in the current toot."
  (interactive)
  (let* ((toot-range (or (mastodon-tl--find-property-range
                          'item-json (point))
                         (mastodon-tl--find-property-range
                          'item-json (point) t)))
         (spoiler-range (when toot-range
                          (mastodon-tl--find-property-range
                           'mastodon-content-warning-body
                           (car toot-range)))))
    (cond ((null toot-range)
           (user-error "No toot here"))
          ((or (null spoiler-range)
               (> (car spoiler-range) (cdr toot-range)))
           (user-error "No content warning text here"))
          (t
           (mastodon-tl--toggle-spoiler-text (car spoiler-range))))))

(defun mastodon-tl-toggle-spoiler-in-thread ()
  "Toggler content warning for all posts in current thread."
  (interactive)
  (let ((thread-p (eq (mastodon-tl--buffer-property 'update-function)
                      'mastodon-tl--thread-do)))
    (if (not thread-p)
        (user-error "Not in a thread")
      (save-excursion
        (goto-char (point-min))
        (while (not (string= "No more items" ; improve this hack test!
                             (mastodon-tl-goto-next-item :no-refresh)))
          (let* ((json (mastodon-tl--property 'item-json :no-move))
                 (cw (alist-get 'spoiler_text json)))
            (when (not (string= "" cw))
              (let ((new-state
                     (pcase
                         (mastodon-tl-toggle-spoiler-text-in-toot)
                       ('t 'folded)
                       ('nil 'unfolded))))
                (plist-put mastodon-tl--buffer-spec
                           'thread-unfolded new-state)))))))))

(defun mastodon-tl--spoiler (toot &optional filter)
  "Render TOOT with spoiler message.
This assumes TOOT is a toot with a spoiler message.
The main body gets hidden and only the spoiler text and the
content warning message are displayed. The content warning
message is a link which unhides/hides the main body.
FILTER is a string to use as a filter warning spoiler instead."
  (let* ((spoiler (mastodon-tl--field 'spoiler_text toot))
         (string (mastodon-tl--set-face
                  (mastodon-tl--clean-tabs-and-nl
                   (mastodon-tl--render-text spoiler toot))
                  'default))
         (message (concat " " mastodon-tl--horiz-bar "\n "
                          (mastodon-tl--make-link
                           (if filter
                               (concat "Filtered: " filter)
                             (concat "CW: " string))
                           'content-warning)
                          "\n "
                          mastodon-tl--horiz-bar "\n"))
         (cw (mastodon-tl--set-face message 'mastodon-cw-face)))
    (concat
     cw
     (propertize
      (mastodon-tl--content toot)
      'invisible
      (or filter ;; filters = invis
          (let ((cust mastodon-tl--expand-content-warnings))
            (if (not (eq 'server cust))
                (not cust) ;; opp to setting
              ;; respect server setting:
              ;; If something goes wrong reading prefs,
              ;; just return t so CWs fold by default.
              (condition-case nil
                  (if (eq :json-false
                          (mastodon-profile--get-preferences-pref
                           'reading:expand:spoilers))
                      t
                    nil)
                (error t)))))
      'mastodon-content-warning-body t))))


;;; MEDIA

(defun mastodon-tl--media (toot)
  "Retrieve a media attachment link for TOOT if one exists.
Else return an empty string."
  (let* ((attachments (mastodon-tl--field 'media_attachments toot))
         (sensitive (mastodon-tl--field 'sensitive toot))
         (media-string (mapconcat
                        (lambda (x)
                          (mastodon-tl--media-attachment x sensitive))
                        attachments "")))
    (if (not (and mastodon-tl--display-media-p
                  (string-empty-p media-string)))
        (concat "\n" media-string)
      "")))

(defun mastodon-tl--media-attachment (attachment sensitive)
  "Return a propertized string for ATTACHMENT.
SENSITIVE is a flag from the item's JSON data."
  (let-alist attachment
    (let ((display-str
           (concat "Media:: "
                   (if (and mastodon-tl--display-caption-not-url-when-no-media
                            .description)
                       .description
                     .preview_url)))
          (remote-url (or .remote_url .url)))
      (if (and mastodon-tl--display-media-p
               ;; if in notifs, also check notifs images custom:
               (if (or (mastodon-tl--buffer-type-eq 'notifications)
                       (mastodon-tl--buffer-type-eq 'mentions))
                   mastodon-images-in-notifs
                 t))
          (mastodon-media--get-media-link-rendering ; placeholder: "[img]"
           .preview_url remote-url ; for shr-browse-url
           .type .description sensitive)
        ;; return URL/caption:
        (concat (mastodon-tl--propertize-img-str-or-url
                 (concat "Media:: " .preview_url) ; string
                 .preview_url remote-url .type .description
                 display-str 'shr-link .description sensitive)
                "\n")))))

(defun mastodon-tl--propertize-img-str-or-url
    (str media-url full-remote-url type help-echo
         &optional display face caption sensitive)
  "Propertize an media placeholder string \"[img]\" or media URL.
STR is the string to propertize, MEDIA-URL is the preview link,
FULL-REMOTE-URL is the link to the full resolution image on the
server, TYPE is the media type.
HELP-ECHO, DISPLAY, and FACE are the text properties to add.
CAPTION is the image caption, added as a text property.
SENSITIVE is a flag from the item's JSON data."
  (propertize str
              'media-url media-url
              'media-state (when (string= str "[img]") 'needs-loading)
              'media-type 'media-link
              'mastodon-media-type type
              'display display
              'face face
              'mouse-face 'highlight
              'mastodon-tab-stop 'image ; for do-link-action-at-point
              'image-url (or full-remote-url media-url) ; for shr-browse-image
              'keymap mastodon-tl--shr-image-map-replacement
              'image-description caption
              'sensitive sensitive
              'help-echo (if (or (string= type "image")
                                 (string= type nil)
                                 (string= type "unknown")) ; handle borked images
                             help-echo
                           (concat help-echo "\nC-RET: play " type " with mpv"))))

(defun mastodon-tl-view-full-image ()
  "Browse full-sized version of image at point in a new window."
  (interactive)
  (if (not (eq (mastodon-tl--property 'mastodon-tab-stop) 'image))
      (user-error "No image at point?")
    (let* ((url (mastodon-tl--property 'image-url)))
      (if (not mastodon-tl--load-full-sized-images-in-emacs)
          (shr-browse-image)
        (mastodon-media--image-or-cached
         url
         #'mastodon-media--process-full-sized-image-response
         `(nil ,url))))))

(defun mastodon-tl-toggle-sensitive-image ()
  "Toggle dislay of sensitive image at point."
  (interactive)
  (if (not (eq t (mastodon-tl--property 'sensitive)))
      (user-error "No sensitive media at point?")
    (let ((data (mastodon-tl--property 'image-data :no-move))
          (inhibit-read-only t)
          (end (next-single-property-change (point) 'sensitive-state)))
      (add-text-properties
       (point) end
       (if (eq 'hidden (mastodon-tl--property 'sensitive-state :no-move))
           ;; display:
           `( display ,data
              sensitive-state showing)
         ;; hide:
         `( sensitive-state hidden
            display
            ,(create-image
              mastodon-media--sensitive-image-data nil t)))))))


;; POLLS

(defun mastodon-tl--format-poll-option (option counter length)
  "Format poll OPTION. COUNTER is a counter.
LENGTH is of the longest option, for formatting."
  (format "%s: %s%s%s\n"
          counter
          (propertize (alist-get 'title option)
                      'face 'success)
          (make-string (1+ (- length
                              (length (alist-get 'title option))))
                       ?\ )
          ;; TODO: disambiguate no votes from hidden votes
          (format "[%s votes]" (or (alist-get 'votes_count option)
                                   "0"))))

(defun mastodon-tl--format-poll (poll)
  "From json poll data POLL, return a display string."
  (let-alist poll
    (let* ((options (mastodon-tl--map-alist 'title .options))
           (longest (car (sort (mapcar #'length options ) #'>)))
           (counter 0))
      (concat "\nPoll: \n\n"
              (mapconcat (lambda (option)
                           (setq counter (1+ counter))
                           (mastodon-tl--format-poll-option
                            option counter longest))
                         .options
                         "\n")
              "\n"
              (propertize
               (cond (.voters_count ; sometimes it is nil
                      (format "%s %s | " .voters_count
                              (if (= .voters_count 1) "person" "people")))
                     (.vote_count
                      (format "%s votes | " .vote_count))
                     (t ""))
               'face 'mastodon-toot-docs-face)
              (let ((str (if (eq .expired :json-false)
                             (if (eq .expires_at nil)
                                 ""
                               (mastodon-tl--format-poll-expiry .expires_at))
                           "Poll expired.")))
                (propertize str 'face 'mastodon-toot-docs-face))
              "\n"))))

(defconst mastodon-tl--time-units
  '("sec"   60.0 ;; Use a float to convert `n' to float.
    "min"   60
    "hour"  24
    "day"   7
    "week"  4.345
    "month" 12
    "year"))

(defun mastodon-tl--format-poll-expiry (timestamp)
  "Convert poll expiry TIMESTAMP into a descriptive string.
TIMESTAMP is from the expires_at field of a poll's JSON data, and
is in ISO 8601 Datetime format."
  (let* ((ts (encode-time (parse-time-string timestamp)))
         (seconds (time-to-seconds (time-subtract ts nil))))
    ;; FIXME: Use the `cdr' to update poll expiry times?
    (concat (car (mastodon-tl--human-duration (max 0 seconds))) " left")))

(defun mastodon-tl--human-duration (seconds &optional resolution)
  "Return a string describing SECONDS in a more human-friendly way.
The return format is (STRING . RES) where RES is the resolution of
this string, in seconds.
RESOLUTION is the finest resolution, in seconds, to use for the
second part of the output (defaults to 60, so that seconds are only
displayed when the duration is smaller than a minute)."
  (cl-assert (>= seconds 0))
  (unless resolution (setq resolution 60))
  (let* ((units mastodon-tl--time-units)
         (n1 seconds) (unit1 (pop units)) (res1 1)
         n2 unit2 res2
         next)
    (while (and units (> (truncate (setq next (/ n1 (car units)))) 0))
      (setq unit2 unit1)
      (setq res2 res1)
      (setq n2 (- n1 (* (car units) (truncate n1 (car units)))))
      (setq n1 next)
      (setq res1 (truncate (* res1 (car units))))
      (pop units)
      (setq unit1 (pop units)))
    (setq n1 (truncate n1))
    (if n2 (setq n2 (truncate n2)))
    (cond
     ((null n2)
      ;; revert to old just now style for < 1 min:
      (cons "just now" 60))
     ;; (cons (format "%d %s%s" n1 unit1 (if (> n1 1) "s" ""))
     ;;       (max resolution res1)))
     ((< (* res2 n2) resolution)
      (cons (format "%d %s%s" n1 unit1 (if (> n1 1) "s" ""))
            (max resolution res2)))
     ((< res2 resolution)
      (let ((n2 (/ (* resolution (/ (* n2 res2) resolution)) res2)))
        (cons (format "%d %s%s, %d %s%s"
                      n1 unit1 (if (> n1 1) "s" "")
                      n2 unit2 (if (> n2 1) "s" ""))
              resolution)))
     (t
      (cons (format "%d %s%s, %d %s%s"
                    n1 unit1 (if (> n1 1) "s" "")
                    n2 unit2 (if (> n2 1) "s" ""))
            (max res2 resolution))))))

(defun mastodon-tl--format-read-poll-option (options)
  "Format poll OPTIONS for `completing-read'.
OPTIONS is an alist."
  ;; we display option number and the option title
  ;; but also store both as a cons cell as the cdr, as we need it later
  (cl-loop for cell in options
           collect (cons (format "%s | %s" (car cell) (cdr cell))
                         cell)))

(defun mastodon-tl--read-poll-option ()
  "Read a poll option to vote on a poll."
  (let* ((toot (mastodon-tl--property 'item-json))
         (poll (mastodon-tl--field 'poll toot)))
    (if (null poll)
        (user-error "No poll here")
      (let* ((options (mastodon-tl--field 'options poll))
             (titles (mastodon-tl--map-alist 'title options))
             (number-seq (number-sequence 1 (length options)))
             (numbers (mapcar #'number-to-string number-seq))
             (options-alist (cl-mapcar #'cons numbers titles))
             (candidates (mastodon-tl--format-read-poll-option options-alist))
             (choice (completing-read "Poll option to vote for: "
                                      candidates nil :match)))
        (list (cdr (assoc choice candidates)))))))

(defun mastodon-tl-poll-vote (option)
  "If there is a poll at point, prompt user for OPTION to vote on it."
  (interactive (mastodon-tl--read-poll-option))
  (let ((toot (mastodon-tl--property 'item-json)))
    (if (null (mastodon-tl--field 'poll toot))
        (user-error "No poll here")
      (let* ((poll (mastodon-tl--field 'poll toot))
             (id (alist-get 'id poll))
             (url (mastodon-http--api (format "polls/%s/votes" id)))
             ;; zero-index our option:
             (option-arg (number-to-string
                          (1- (string-to-number (car option)))))
             (arg `(("choices[]" . ,option-arg)))
             (response (mastodon-http--post url arg)))
        (mastodon-http--triage response
                               (lambda (_)
                                 (message "You voted for option %s: %s!"
                                          (car option) (cdr option))))))))


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

(defun mastodon-tl-mpv-play-video-from-byline ()
  "Run `mastodon-tl-mpv-play-video-at-point' on first moving image in post."
  (interactive)
  (let* ((video (mastodon-tl--find-first-video-in-attachments))
         (url (plist-get video :url))
         (type (plist-get video :type)))
    (mastodon-tl-mpv-play-video-at-point url type)))

(defun mastodon-tl-view-full-image-or-play-video (_pos)
  "View full sized version of image at point, or try to play video."
  (interactive "d")
  (if (mastodon-tl--media-video-p)
      (mastodon-tl-mpv-play-video-at-point)
    (mastodon-tl-view-full-image)))

(defun mastodon-tl-click-image-or-video (event)
  "Click to play video with `mpv.el'.
EVENT is a mouse-click arg."
  (interactive "@e")
  (mastodon-tl-view-full-image-or-play-video
   (posn-point (event-end event))))

(defun mastodon-tl--media-video-p (&optional type)
  "T if mastodon-media-type prop is \"gifv\" or \"video\".
TYPE is a mastodon media type."
  (let ((type (or type (mastodon-tl--property 'mastodon-media-type :no-move))))
    (or (string= type "gifv")
        (string= type "video"))))

(defun mastodon-tl-mpv-play-video-at-point (&optional url type)
  "Play the video or gif at point with an mpv process.
URL and TYPE are provided when called while point is on byline,
in which case play first video or gif from current toot."
  (interactive)
  (let ((url (or url ; point in byline:
                 (mastodon-tl--property 'image-url :no-move)))) ; point in toot
    (if (or (not url)
            (not (mastodon-tl--media-video-p type)))
        (user-error "No moving image here?")
      (message "'q' to kill mpv.")
      (condition-case x
          (mpv-start "--loop" url)
        (void-function
         (message "Looks like mpv.el not installed. Error: %s"
                  (error-message-string x)))))))

(defun mastodon-tl-copy-image-caption ()
  "Copy the caption of the image at point."
  (interactive)
  (if-let* ((desc (get-text-property (point) 'image-description)))
      (progn
        (kill-new desc)
        (message "Image caption copied."))
    (message "No image caption.")))


;;; INSERT TOOTS

(defun mastodon-tl--content (toot)
  "Retrieve text content from TOOT.
Runs `mastodon-tl--render-text' and fetches poll or media."
  (let* ((content (mastodon-tl--field 'content toot))
         (poll-p (mastodon-tl--field 'poll toot))
         (media-p (mastodon-tl--field 'media_attachments toot)))
    (concat (mastodon-tl--render-text content toot)
            (when poll-p
              (mastodon-tl--format-poll
               (mastodon-tl--field 'poll toot))) ;; toot or reblog
            (when media-p
              (mastodon-tl--media toot)))))

(defun mastodon-tl--prev-item-id ()
  "Return the id of the last toot inserted into the buffer."
  (let* ((prev-change
          (save-excursion
            (previous-single-property-change (point) 'base-item-id)))
         (prev-pos (when prev-change (1- prev-change))))
    (when prev-pos
      (get-text-property prev-pos 'base-item-id))))

(defun mastodon-tl--after-reply-status (reply-to-id)
  "T if REPLY-TO-ID is equal to that of the last toot inserted in the bufer."
  (let ((prev-id (mastodon-tl--prev-item-id)))
    (string= reply-to-id prev-id)))

(defun mastodon-tl--insert-status
    (toot body &optional detailed-p thread domain unfolded no-byline
          cw-expanded)
  "Display the content and byline of timeline element TOOT.
BODY will form the section of the toot above the byline.
DETAILED-P means display more detailed info. For now
this just means displaying toot client.
THREAD means the status will be displayed in a thread view.
When DOMAIN, force inclusion of user's domain in their handle.
UNFOLDED is a boolean meaning whether to unfold or fold item if foldable.
NO-BYLINE means just insert toot body, used for folding.
CW-EXPANDED means treat content warnings as unfolded."
  (let* ((reply-to-id (alist-get 'in_reply_to_id toot))
         (after-reply-status-p
          (when (and thread reply-to-id)
            (mastodon-tl--after-reply-status reply-to-id)))
         ;; (type (alist-get 'type toot))
         (toot-foldable
          (and mastodon-tl--fold-toots-at-length
               (length> body mastodon-tl--fold-toots-at-length)))
         (cw-p (not
                (string-empty-p
                 (alist-get 'spoiler_text toot)))))
    (insert
     (propertize ;; body + byline:
      (concat
       (propertize ;; body only:
        (concat
         "\n"
         (mastodon-tl--byline-boost toot) ;; top byline (boost)
         ;; relpy symbol:
         (when (and after-reply-status-p thread)
           (concat (mastodon-tl--symbol 'replied)
                   "\n"))
         ;; actual body:
         (let ((bar (mastodon-tl--symbol 'reply-bar))
               (body (if (and toot-foldable (not unfolded))
                         (mastodon-tl--fold-body body)
                       body)))
           (if (and after-reply-status-p thread)
               (propertize body
                           'line-prefix bar
                           'wrap-prefix bar)
             body))
         (if (and toot-foldable unfolded cw-expanded)
             (mastodon-tl--read-more-or-less
              "LESS" cw-p (not cw-expanded))
           ""))
        'toot-body t) ;; includes newlines etc. for folding
       ;; byline:
       "\n"
       (if no-byline
           ""
         (mastodon-tl--byline toot detailed-p domain)))
      'item-type    'toot
      'item-id (alist-get 'id toot) ; toot id
      'base-item-id (mastodon-tl--item-id toot) ; with reblog check
      'item-json    toot
      'cursor-face 'mastodon-cursor-highlight-face
      'toot-foldable toot-foldable
      'toot-folded (and toot-foldable (not unfolded)))
     (if no-byline "" "\n"))))

(defun mastodon-tl--is-reply (toot)
  "Check if the TOOT is a reply to another one (and not boosted).
Used as a predicate in `mastodon-tl--timeline'."
  (and (mastodon-tl--field 'in_reply_to_id toot)
       (eq :json-false (mastodon-tl--field 'reblogged toot))))

(defun mastodon-tl--filters-alist (filters)
  "Parse filter data for FILTERS.
For each filter, return a list of action (warn or hide), filter
title, and context."
  (cl-loop for x in filters ;; includes non filter elts!
           for f = (alist-get 'filter x)
           collect (list (alist-get 'filter_action f)
                         (alist-get 'title f)
                         (alist-get 'context f))))

(defun mastodon-tl--filter-by-context (context filters)
  "Remove FILTERS that don't apply to the current CONTEXT."
  (cl-remove-if-not
   (lambda (x)
     (member context (nth 2 x)))
   filters))

(defun mastodon-tl--filters-context ()
  "Return a string of the current buffer's filter context.
Returns a member of `mastodon-views--filter-types'."
  (let ((buf (mastodon-tl--get-buffer-type)))
    (cond ((or (eq buf 'local) (eq buf 'federated))
           "public")
          ((mastodon-tl--profile-buffer-p)
           "profile")
          ((eq buf 'list-timeline)
           "home") ;; lists are "home" filter
          (t ;; thread, notifs, home:
           (symbol-name buf)))))

(defun mastodon-tl--current-filters (filters)
  "Return the filters from FILTERS data that apply in the current context.
For each filter, return a list of action (warn or hide), filter
title, and context."
  (let ((context (mastodon-tl--filters-context))
        (filters-no-context (mastodon-tl--filters-alist filters)))
    (mastodon-tl--filter-by-context context filters-no-context)))

(defun mastodon-tl--toot (toot &optional detailed-p thread domain
                               unfolded no-byline cw-expanded)
  "Format TOOT and insert it into the buffer.
DETAILED-P means display more detailed info. For now
this just means displaying toot client.
THREAD means the status will be displayed in a thread view.
When DOMAIN, force inclusion of user's domain in their handle.
UNFOLDED is a boolean meaning whether to unfold or fold item if foldable.
NO-BYLINE means just insert toot body, used for folding.
CW-EXPANDED means treat content warnings as unfolded."
  (let* ((mastodon-tl--expand-content-warnings
          (or cw-expanded mastodon-tl--expand-content-warnings))
         (filtered (mastodon-tl--field 'filtered toot))
         (filters (when filtered
                    (mastodon-tl--current-filters filtered)))
         (spoiler-or-content (if-let* ((match (assoc "warn" filters)))
                                 (mastodon-tl--spoiler toot (cadr match))
                               (if (mastodon-tl--has-spoiler toot)
                                   (mastodon-tl--spoiler toot)
                                 (mastodon-tl--content toot)))))
    ;; If any filters are "hide", then we hide,
    ;; even though item may also have a "warn" filter:
    (unless (and filtered (assoc "hide" filters)) ;; no insert
      (mastodon-tl--insert-status
       toot (mastodon-tl--clean-tabs-and-nl spoiler-or-content)
       detailed-p thread domain unfolded no-byline cw-expanded))))

(defun mastodon-tl--timeline (toots &optional thread domain no-byline)
  "Display each toot in TOOTS.
This function removes replies if user required.
THREAD means the status will be displayed in a thread view.
When DOMAIN, force inclusion of user's domain in their handle.
NO-BYLINE means just insert toot body, used for folding."
  (let ((start-pos (point))
        (toots ;; hack to *not* filter replies on profiles:
         (if (eq (mastodon-tl--get-buffer-type) 'profile-statuses)
             toots
           (if (or ; we were called via --more*:
                (mastodon-tl--buffer-property 'hide-replies nil :no-error)
                ;; loading a tl with a prefix arg:
                (mastodon-tl--hide-replies-p current-prefix-arg))
	           (cl-remove-if-not #'mastodon-tl--is-reply toots)
	         toots))))
    (mapc (lambda (toot)
            (mastodon-tl--toot toot nil thread domain nil no-byline))
          toots)
    ;; media:
    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images start-pos (point)))
    (goto-char (point-min))))

;;; FOLDING

(defun mastodon-tl--read-more-or-less (str cw invis)
  "Return a read more or read less heading.
STR is an uppercase string, either MORE or LESS.
The heading is a link to toggle the fold status of the toot.
CW and INVIS are boolean values for the properties invisible and
mastodon-content-warning-body."
  (let ((type (if (string= str "MORE") 'read-more 'read-less)))
    (propertize
     (mastodon-search--format-heading
      (mastodon-tl--make-link (format "READ %s" str) type)
      nil :no-newline)
     'mastodon-content-warning-body cw
     'invisible invis)))

(defun mastodon-tl--fold-body (body)
  "Fold toot BODY if it is very long.
Folding decided by `mastodon-tl--fold-toots-at-length'."
  (let* ((invis (get-text-property (1- (length body)) 'invisible body))
         (cw (get-text-property (1- (length body))
                                'mastodon-content-warning-body body))
         (heading (mastodon-tl--read-more-or-less "MORE" cw invis))
         (display (concat (substring body 0
                                     mastodon-tl--fold-toots-at-length)
                          heading)))
    (propertize display
                'read-more body)))

(defun mastodon-tl-unfold-post (&optional fold)
  "Unfold the toot at point if it is folded (read-more).
FOLD means to fold it instead."
  (interactive)
  (let ((at-byline (mastodon-tl--property 'byline :no-move)))
    (if (save-excursion
          (when (not at-byline)
            (mastodon-tl-goto-next-item))
          (not (mastodon-tl--property 'toot-foldable :no-move)))
        (user-error "No foldable item at point?")
      (let* ((inhibit-read-only t)
             (body-range (mastodon-tl--find-property-range 'toot-body
                                                           (point) :backward))
             (cw-range (mastodon-tl--find-property-range
                        'mastodon-content-warning-body
                        (point) :backward))
             (cw-invis (when cw-range
                         (get-text-property (car cw-range) 'invisible)))
             (toot (mastodon-tl--property 'item-json :no-move))
             ;; `replace-region-contents' is much too slow, our hack from
             ;; fedi.el is much simpler and much faster:
             (beg (car body-range))
             (end (cdr body-range))
             (last-point (point))
             (point-after-fold (> last-point
                                  (+ beg mastodon-tl--fold-toots-at-length))))
        ;; save-excursion here useless actually:
        ;; FIXME: because point goes to top of item, the screen gets scrolled
        ;; by insertion
        (goto-char beg)
        (delete-region beg end)
        (delete-char 1) ;; prevent newlines accumulating
        ;; insert toot body:
        (mastodon-tl--toot toot nil nil nil (not fold) :no-byline
                           (unless cw-invis :cw-expanded)) ;; respect CW state
        ;; set toot-folded prop on entire toot (not just body):
        (let ((toot-range ;; post fold action range:
               (mastodon-tl--find-property-range 'item-json
                                                 (point) :backward)))
          (add-text-properties (car toot-range)
                               (cdr toot-range)
                               `(toot-folded ,fold)))
        ;; try to leave point somewhere sane:
        (cond ((or at-byline
                   (and fold point-after-fold)) ;; point was in area now folded
               (ignore-errors (forward-line -1)) ;; in case we are between
               (mastodon-tl-goto-next-item)) ;; goto byline
              (t
               (goto-char last-point)
               (when point-after-fold ;; point was in READ MORE heading:
                 (beginning-of-line))))
        (message (format "%s toot" (if fold "Fold" "Unfold")))))))

(defun mastodon-tl-fold-post ()
  "Fold post at point, if it is too long."
  (interactive)
  (mastodon-tl-unfold-post :fold))

(defun mastodon-tl-fold-post-toggle ()
  "Toggle the folding status of the toot at point."
  (interactive)
  (let* ((folded (mastodon-tl--property 'toot-folded :no-move)))
    (mastodon-tl-unfold-post (not folded))))

;;; TOOT STATS

;; calqued off mastodon-alt.el:
(defun mastodon-tl--toot-for-stats (&optional toot)
  "Return the TOOT on which we want to extract stats.
If no TOOT is given, the one at point is considered."
  (let* ((original-toot (or toot (get-text-property (point) 'item-json)))
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
  (let-alist (mastodon-tl--toot-for-stats toot)
    (let* ((faves-prop (propertize (format "%s" .favourites_count)
                                   'favourites-count .favourites_count))
           (boosts-prop (propertize (format "%s" .reblogs_count)
                                    'boosts-count .reblogs_count))
           (faves (format "%s %s" faves-prop (mastodon-tl--symbol 'favourite)))
           (boosts (format "%s %s" boosts-prop (mastodon-tl--symbol 'boost)))
           (replies (format "%s %s" .replies_count (mastodon-tl--symbol 'reply)))
           (stats (concat
                   (propertize faves
                               'favourited-p (eq t .favourited)
                               'favourites-field t
                               'help-echo (format "%s favourites" .favourites_count)
                               'face 'mastodon-toot-docs-face)
                   (propertize " | " 'face 'mastodon-toot-docs-face)
                   (propertize boosts
                               'boosted-p (eq t .reblogged)
                               'boosts-field t
                               'help-echo (format "%s boosts" .reblogs_count)
                               'face 'mastodon-toot-docs-face)
                   (propertize " | " 'face 'mastodon-toot-docs-face)
                   (propertize replies
                               'replies-field t
                               'replies-count .replies_count
                               'help-echo (format "%s replies" .replies_count)
                               'face 'mastodon-toot-docs-face)))
           (right-spacing
            (propertize " "
                        'display
                        `(space :align-to (- right ,(+ (length stats) 7))))))
      (concat right-spacing stats))))


;;; BUFFER SPEC

(defun mastodon-tl--update-function (&optional buffer)
  "Get the UPDATE-FUNCTION stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--buffer-property 'update-function buffer))

(defun mastodon-tl--endpoint (&optional buffer no-error)
  "Get the ENDPOINT stored in `mastodon-tl--buffer-spec'.
Optionally set it for BUFFER.
NO-ERROR means to fail silently."
  (mastodon-tl--buffer-property 'endpoint buffer no-error))

(defun mastodon-tl--buffer-name (&optional buffer no-error)
  "Get the BUFFER-NAME stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER.
NO-ERROR means to fail silently."
  (mastodon-tl--buffer-property 'buffer-name buffer no-error))

(defun mastodon-tl--link-header (&optional buffer)
  "Get the LINK HEADER stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--buffer-property 'link-header buffer :no-error))

(defun mastodon-tl--update-params (&optional buffer)
  "Get the UPDATE PARAMS stored in `mastodon-tl--buffer-spec'.
Optionally get it for BUFFER."
  (mastodon-tl--buffer-property 'update-params buffer :no-error))

(defun mastodon-tl--buffer-property (property &optional buffer no-error)
  "Get PROPERTY from `mastodon-tl--buffer-spec' in BUFFER or `current-buffer'.
If NO-ERROR is non-nil, do not error when property is empty."
  (with-current-buffer (or buffer (current-buffer))
    (if no-error
        (plist-get mastodon-tl--buffer-spec property)
      (or (plist-get mastodon-tl--buffer-spec property)
          (error "Mastodon-tl--buffer-spec not defined for buffer %s, prop %s"
                 (or buffer (current-buffer))
                 property)))))

(defun mastodon-tl--set-buffer-spec
    (buffer endpoint update-fun
            &optional link-header update-params hide-replies max-id
            thread-item-id)
  "Set `mastodon-tl--buffer-spec' for the current buffer.
BUFFER is buffer name, ENDPOINT is buffer's enpoint,
UPDATE-FUN is its update function.
LINK-HEADER is the http Link header if present.
UPDATE-PARAMS is any http parameters needed for the update function.
HIDE-REPLIES is a flag indicating if replies are hidden in the current buffer.
MAX-ID is the pagination parameter.
THREAD-ITEM-ID is the ID of the item in thread that we opened the thread with."
  (setq mastodon-tl--buffer-spec
        `( account ,(cons mastodon-active-user mastodon-instance-url)
           buffer-name ,buffer
           endpoint ,endpoint
           update-function ,update-fun
           link-header ,link-header
           update-params ,update-params
           hide-replies ,hide-replies
           max-id ,max-id
           thread-item-id ,thread-item-id)))


;;; BUFFERS

(defun mastodon-tl--endpoint-str-= (str &optional type)
  "Return T if STR is equal to the current buffer's endpoint.
TYPE may be :prefix or :suffix, in which case, T if STR is a prefix or suffix."
  (let ((endpoint-fun (mastodon-tl--endpoint nil :no-error)))
    (cond ((eq type :prefix)
           (string-prefix-p str endpoint-fun))
          ((eq type :suffix)
           (string-suffix-p str endpoint-fun))
          (t
           (string= str endpoint-fun)))))

(defun mastodon-tl--get-buffer-type ()
  "Return a symbol descriptive of current mastodon buffer type.
Should work in all mastodon buffers.
Note that for many buffers, this requires `mastodon-tl--buffer-spec'
to be set. It is set for almost all buffers, but you still have to
call this function after it is set or use something else."
  (let ((buffer-name (mastodon-tl--buffer-name nil :no-error)))
    (cond (mastodon-toot-mode
           ;; composing/editing (no buffer spec):
           (if (string= "*edit toot*" (buffer-name))
               'edit-toot
             'new-toot))
          ;; main timelines:
          ((mastodon-tl--endpoint-str-= "timelines/home")
           'home)
          ((string= "*mastodon-local*" buffer-name)
           'local)
          ((mastodon-tl--endpoint-str-= "timelines/public")
           'federated)
          ((mastodon-tl--endpoint-str-= "timelines/tag/" :prefix)
           'tag-timeline)
          ((mastodon-tl--endpoint-str-= "timelines/list/" :prefix)
           'list-timeline)
          ;; notifs:
          ((string-suffix-p "mentions*" buffer-name)
           'mentions)
          ((mastodon-tl--endpoint-str-= "notifications")
           'notifications)
          ((mastodon-tl--endpoint-str-= "notifications/requests")
           'notification-requests)
          ;; threads:
          ((mastodon-tl--endpoint-str-= "context" :suffix)
           'thread)
          ((mastodon-tl--endpoint-str-= "statuses" :prefix)
           'single-status)
          ;; profiles:
          ((mastodon-tl--profile-buffer-p)
           (cond
            ;; an own profile option is needlessly confusing e.g. for
            ;; `mastodon-profile-account-view-cycle'
            ;; profile note:
            ((string-suffix-p "update-profile*" buffer-name)
             'update-profile-note)
            ;; posts inc. boosts:
            ((string-suffix-p "no-boosts*" buffer-name)
             'profile-statuses-no-boosts)
            ((string-suffix-p "no-replies*" buffer-name)
             'profile-statuses-no-replies)
            ((string-suffix-p "only-media*" buffer-name)
             'profile-statuses-only-media)
            ((string-match-p "-tagged-" buffer-name)
             'profile-statuses-tagged)
            ((mastodon-tl--endpoint-str-= "statuses" :suffix)
             'profile-statuses)
            ;; profile followers
            ((mastodon-tl--endpoint-str-= "followers" :suffix)
             'profile-followers)
            ;; profile following
            ((mastodon-tl--endpoint-str-= "following" :suffix)
             'profile-following)))
          ((mastodon-tl--endpoint-str-= "preferences")
           'preferences)
          ;; search
          ((mastodon-tl--search-buffer-p)
           (cond ((string= "accounts" (mastodon-search--buf-type))
                  'search-accounts)
                 ((string= "hashtags" (mastodon-search--buf-type))
                  'search-hashtags)
                 ((string= "statuses" (mastodon-search--buf-type))
                  'search-statuses)))
          ;; trends
          ((mastodon-tl--endpoint-str-= "trends/statuses")
           'trending-statuses)
          ((mastodon-tl--endpoint-str-= "trends/tags")
           'trending-tags)
          ((mastodon-tl--endpoint-str-= "trends/links")
           'trending-links)
          ;; User's views:
          ((mastodon-tl--endpoint-str-= "filters")
           'filters)
          ((mastodon-tl--endpoint-str-= "lists")
           'lists)
          ((mastodon-tl--endpoint-str-= "suggestions")
           'follow-suggestions)
          ((mastodon-tl--endpoint-str-= "favourites")
           'favourites)
          ((mastodon-tl--endpoint-str-= "bookmarks")
           'bookmarks)
          ((mastodon-tl--endpoint-str-= "follow_requests")
           'follow-requests)
          ((mastodon-tl--endpoint-str-= "scheduled_statuses")
           'scheduled-statuses)
          ;; instance description
          ((mastodon-tl--endpoint-str-= "instance")
           'instance-description)
          ((string= "*mastodon-toot-edits*" buffer-name)
           'toot-edits)
          ((string= "*masto-image*" (buffer-name))
           'mastodon-image)
          ((mastodon-tl--endpoint-str-= "timelines/link")
           'link-timeline)
          ((mastodon-tl--endpoint-str-= "announcements")
           'announcements))))

(defun mastodon-tl--buffer-type-eq (type)
  "Return t if current buffer type is equal to symbol TYPE."
  (eq (mastodon-tl--get-buffer-type) type))

(defun mastodon-tl--profile-buffer-p ()
  "Return t if current buffer is a profile buffer of any kind.
This includes the update profile note buffer, but not the preferences one."
  (string-prefix-p "accounts" (mastodon-tl--endpoint nil :no-error)))

(defun mastodon-tl--search-buffer-p ()
  "T if current buffer is a search buffer."
  (string-suffix-p "search" (mastodon-tl--endpoint nil :no-error)))

(defun mastodon-tl--timeline-proper-p ()
  "Return non-nil if the current buffer is a \"proper\" timeline.
A proper timeline excludes notifications, threads, profiles, and
other toot buffers that aren't strictly mastodon timelines."
  (let ((timeline-buffers
         '(home federated local tag-timeline list-timeline profile-statuses)))
    (member (mastodon-tl--get-buffer-type) timeline-buffers)))

(defun mastodon-tl--hide-replies-p (&optional prefix)
  "Return non-nil if replies should be hidden in the timeline.
We hide replies if user explictly set the
`mastodon-tl--hide-replies' or used PREFIX combination to open a
timeline."
  (and (mastodon-tl--timeline-proper-p) ; Only if we are in a proper timeline
       (or mastodon-tl--hide-replies ; User configured to hide replies
           (equal '(4) prefix)))) ; Timeline called with C-u prefix


;;; UTILITIES

(defun mastodon-tl--clean-tabs-and-nl (string)
  "Remove tabs and newlines from STRING."
  (replace-regexp-in-string "[\t\n ]*\\'" "" string))

(defun mastodon-tl--map-alist (key alists &optional testfn)
  "Return a list of values extracted from ALISTS with KEY.
Key is a symbol, as with `alist-get', or else compatible with TESTFN.
ALISTS is a list of alists."
  ;; this actually for a list of alists, right? so change the arg?
  (cl-loop for x in alists
           collect (alist-get key x nil nil testfn)))

(defun mastodon-tl--map-alist-vals-to-alist (key1 key2 alist)
  "From ALIST, return an alist consisting of (val1 . val2) elements.
Values are accessed by `alist-get', using KEY1 and KEY2."
  (cl-loop for x in alist
           collect (cons (alist-get key1 x)
                         (alist-get key2 x))))

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

(defun mastodon-tl--field-status (field toot)
  "Return FIELD from TOOT.
Return value from status field if available."
  (or (alist-get field (alist-get 'status toot))
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
    ;; NB: this doesn't differentiate absence of property from
    ;; property set to zero, making flag props fraught:
    (or (get-text-property (point) prop)
        (save-excursion
          (if backward
              (mastodon-tl-goto-prev-item)
            (mastodon-tl-goto-next-item))
          (get-text-property (point) prop)))))

(defun mastodon-tl--newest-id ()
  "Return item-id from the top of the buffer.
If we are in a notifications view, return `notifications-max-id'."
  (save-excursion
    (goto-char (point-min))
    (mastodon-tl--property
     (if (eq (mastodon-tl--get-buffer-type)
             (member (mastodon-tl--get-buffer-type)
                     '(mentions notifications)))
         'notifications-max-id
       'item-id))))

(defun mastodon-tl--oldest-id ()
  "Return item-id from the bottom of the buffer.
If we are in a notifications view, return `notifications-min-id'."
  (save-excursion
    (goto-char (point-max))
    (mastodon-tl--property
     (if (and mastodon-group-notifications
              (member (mastodon-tl--get-buffer-type)
                      '(mentions notifications)))
         'notifications-min-id
       'item-id)
     nil :backward)))

(defun mastodon-tl--as-string (numeric)
  "Convert NUMERIC to string."
  (cond ((numberp numeric)
         (number-to-string numeric))
        ((stringp numeric) numeric)
        (t (error "Numeric: %s must be either a string or a number"
                  numeric))))

(defun mastodon-tl--item-id (json)
  "Find approproiate toot id in JSON.
If the toot has been boosted use the id found in the
reblog portion of the toot.  Otherwise, use the body of
the toot.  This is the same behaviour as the mastodon.social
webapp"
  (let-alist json
    (if .reblog .reblog.id .id)))

(defun mastodon-tl--toot-or-base (json)
  "Return the base toot or just the toot from toot JSON."
  (or (alist-get 'reblog json) json))


;;; THREADS

(defun mastodon-tl-single-toot (id)
  "View toot at point in separate buffer.
ID is that of the toot to view."
  (interactive)
  (let* ((buffer (format "*mastodon-toot-%s*" id))
         (toot (mastodon-http--get-json
                (mastodon-http--api (concat "statuses/" id)))))
    (if (eq (caar toot) 'error)
        (user-error "Error: %s" (cdar toot))
      (with-mastodon-buffer buffer #'mastodon-mode nil
        (mastodon-tl--set-buffer-spec buffer (format "statuses/%s" id)
                                      #'mastodon-tl--update-toot
                                      ;; id for reload on reply:
                                      nil nil nil nil id)
        (mastodon-tl--toot toot :detailed-p)
        (goto-char (point-min))
        (when mastodon-tl--display-media-p
          (mastodon-media--inline-images (point-min)
                                         (point-max)))
        (mastodon-tl-goto-next-item :no-refresh)))))

(defun mastodon-tl--update-toot (json)
  "Call `mastodon-tl-single-toot' on id found in JSON."
  (let ((id (alist-get 'id json)))
    (mastodon-tl-single-toot id)))

(defun mastodon-tl-view-whole-thread ()
  "From a thread view, view entire thread.
If you load a thread from a toot, only the branches containing
are displayed by default. Call this if you subsequently want to
view all branches of a thread."
  (interactive)
  (if (not (eq (mastodon-tl--get-buffer-type) 'thread))
      (user-error "You need to be viewing a thread to call this")
    (goto-char (point-min))
    (let ((id (mastodon-tl--property 'base-item-id)))
      (mastodon-tl--thread-do id))))

(defun mastodon-tl-thread ()
  "Open thread buffer for toot at point."
  (interactive)
  (if (not (eq 'toot (mastodon-tl--property 'item-type :no-move)))
      (user-error "Looks like there's no toot at point?")
    (mastodon-tl--thread-do)))

(defun mastodon-tl--thread-do (&optional thread-id)
  "Open thread buffer for toot at point or with THREAD-ID.
This is the non-interactive version, so we can call it
programmatically and not crash into
`mastodon-toot--with-toot-item'."
  ;; this function's var must not be id as the above macro binds id and even
  ;; if we provide the arg (e.g. url-lookup), the macro definition overrides
  ;; it, making the optional arg unusable!
  (let* ((id (or thread-id (mastodon-tl--property 'base-item-id :no-move)))
         (type (mastodon-tl--field 'type
                                   (mastodon-tl--property 'item-json :no-move)))
         (unfolded-state (mastodon-tl--buffer-property 'thread-unfolded
                                                       (current-buffer) :noerror))
         (mastodon-tl--expand-content-warnings
          ;; if reloading and thread was explicitly (un)folded, respect it:
          (or (pcase unfolded-state
                ('folded nil)
                ('unfolded t)
                (_ mastodon-tl--expand-content-warnings)))))
    (if (or (string= type "follow_request")
            (string= type "follow")) ; no can thread these
        (user-error "No thread")
      (let* ((endpoint (format "statuses/%s/context" id))
             (url (mastodon-http--api endpoint))
             (buffer (format "*mastodon-thread-%s*" id))
             (toot (mastodon-http--get-json ; refetch in case we just faved/boosted:
                    (mastodon-http--api (concat "statuses/" id))
                    nil :silent))
             (context (mastodon-http--get-json url nil :silent)))
        (if (eq (caar toot) 'error)
            (user-error "Error: %s" (cdar toot))
          (when (member (alist-get 'type toot) '("reblog" "favourite"))
            (setq toot (alist-get 'status toot)))
          (if (not (< 0 (+ (length (alist-get 'ancestors context))
                           (length (alist-get 'descendants context)))))
              ;; just print the lone toot:
              (mastodon-tl-single-toot id)
            ;; we have a thread:
            (with-mastodon-buffer buffer #'mastodon-mode nil
              (let ((marker (make-marker)))
                (mastodon-tl--set-buffer-spec buffer endpoint
                                              #'mastodon-tl--thread-do
                                              nil nil nil nil id)
                (when unfolded-state
                  (plist-put mastodon-tl--buffer-spec
                             'thread-unfolded unfolded-state))
                (when-let* ((ancestors (alist-get 'ancestors context)))
                  (mastodon-tl--timeline ancestors :thread))
                (goto-char (point-max))
                (move-marker marker (point))
                ;; print re-fetched toot:
                (mastodon-tl--toot toot :detailed-p :thread)
                ;; inline images only for the toot
                ;; (`mastodon-tl--timeline' handles the rest):
                (when mastodon-tl--display-media-p
                  (mastodon-media--inline-images marker ;start-pos
                                                 (point)))
                (when-let* ((descendants (alist-get 'descendants context)))
                  (mastodon-tl--timeline descendants :thread))
                ;; put point at the toot:
                (goto-char (marker-position marker))
                (mastodon-tl-goto-next-item :no-refresh)))))))))

(defun mastodon-tl-mute-thread ()
  "Mute the thread displayed in the current buffer.
Note that you can only (un)mute threads you have posted in."
  (interactive)
  (mastodon-tl--mute-or-unmute-thread))

(defun mastodon-tl-unmute-thread ()
  "Unmute the thread displayed in the current buffer.
Note that you can only (un)mute threads you have posted in."
  (interactive)
  (mastodon-tl--mute-or-unmute-thread :unmute))

(defun mastodon-tl--thread-parent-id ()
  "Return the ID of the top item in a thread."
  (save-excursion
    (mastodon-tl--goto-first-item)
    (mastodon-tl--property 'base-item-id :no-move)))

(defun mastodon-tl--mute-or-unmute-thread  (&optional unmute)
  "Mute a thread.
If UNMUTE, unmute it."
  (let ((mute-str (if unmute "unmute" "mute")))
    (when (or (mastodon-tl--buffer-type-eq 'thread)
              (mastodon-tl--buffer-type-eq 'notifications))
      (let* ((id
              ;; the id for `mastodon-tl--user-in-thread-p' ought to be the
              ;; top-level item:
              (if (mastodon-tl--buffer-type-eq 'notifications)
                  (mastodon-tl--property 'base-item-id :no-move)
                (mastodon-tl--thread-parent-id)))
             (we-posted-p (mastodon-tl--user-in-thread-p id))
             (url (mastodon-http--api (format "statuses/%s/%s" id mute-str))))
        (if (not we-posted-p)
            (user-error "You can only (un)mute a thread you have posted in")
          (when (y-or-n-p (format "%s this thread? " (capitalize mute-str)))
            (let ((response (mastodon-http--post url)))
              (mastodon-http--triage
               response
               (lambda (_)
                 (message (format "Thread %sd!" mute-str)))))))))))

(defun mastodon-tl--map-account-id-from-toot (statuses)
  "Return a list of the account IDs of the author of each toot in STATUSES."
  (mapcar (lambda (status)
            (map-nested-elt status '(account id)))
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

(defun mastodon-tl-follow-user (user-handle
                                 &optional notify langs reblogs json)
  "Query for USER-HANDLE from current status and follow that user.
If NOTIFY is \"true\", enable notifications when that user posts.
If NOTIFY is \"false\", disable notifications when that user posts.
Can be called to toggle NOTIFY on users already being followed.
LANGS is an array parameters alist of languages to filer user's posts by.
REBLOGS is a boolean string like NOTIFY, enabling or disabling
display of the user's boosts in your timeline.
JSON is a flag arg for `mastodon-http--post'."
  (interactive (list (mastodon-tl--user-handles-get "follow")))
  (mastodon-tl--do-if-item
   (mastodon-tl--do-user-action-and-response
    user-handle "follow" nil notify langs reblogs json)))

;; TODO: make this action "enable/disable notifications"
(defun mastodon-tl-enable-notify-user-posts (user-handle)
  "Query for USER-HANDLE and enable notifications when they post."
  (interactive (list (mastodon-tl--user-handles-get "enable")))
  (mastodon-tl--do-if-item
   (mastodon-tl-follow-user user-handle "true")))

(defun mastodon-tl-disable-notify-user-posts (user-handle)
  "Query for USER-HANDLE and disable notifications when they post."
  (interactive (list (mastodon-tl--user-handles-get "disable")))
  (mastodon-tl-follow-user user-handle "false"))

(defun mastodon-tl-follow-user-disable-boosts (user-handle)
  "Prompt for a USER-HANDLE, and disable display of boosts in home timeline.
If they are also not yet followed, follow them."
  (interactive (list (mastodon-tl--user-handles-get "disable boosts")))
  (mastodon-tl-follow-user user-handle nil nil "false"))

(defun mastodon-tl-follow-user-enable-boosts (user-handle)
  "Prompt for a USER-HANDLE, and enable display of boosts in home timeline.
If they are also not yet followed, follow them.
You only need to call this if you have previously disabled
display of boosts."
  (interactive (list (mastodon-tl--user-handles-get "enable boosts")))
  (mastodon-tl-follow-user user-handle nil nil "true"))

(defun mastodon-tl-filter-user-user-posts-by-language (user-handle)
  "Query for USER-HANDLE and filter display of their posts by language.
If they are not already followed, they will be too.
To be filtered, a post has to be marked as in the language given.
This may mean that you will not see posts that are in your
desired language if they are not marked as such (or as anything)."
  (interactive (list (mastodon-tl--user-handles-get "filter by language")))
  (let ((langs (mastodon-tl--read-filter-langs)))
    (mastodon-tl--do-if-item
     (if (string= "" (cdar langs))
         (mastodon-tl-unfilter-user-languages user-handle)
       (mastodon-tl-follow-user user-handle nil langs)))))

(defun mastodon-tl-unfilter-user-languages (user-handle)
  "Remove any language filters for USER-HANDLE.
This means you will receive posts of theirs marked as being in
any or no language."
  (interactive (list (mastodon-tl--user-handles-get "filter by language")))
  (let ((langs "languages[]"))
    (mastodon-tl--do-if-item
     ;; we need "languages[]" as a param, with no "=" and not json-encoded as
     ;; a string
     (mastodon-tl-follow-user user-handle nil langs nil :raw))))

(defun mastodon-tl--read-filter-langs (&optional langs)
  "Read language choices and return an alist array parameter.
LANGS is the accumulated array param alist if we re-run recursively."
  (let* ((iso-const mastodon-iso-639-1)
         (iso (cons '("None (all)" . "") iso-const))
         (langs-alist langs)
         (choice (completing-read "Filter user's posts by language: "
                                  iso)))
    (when choice
      (setq langs-alist
            (push `("languages[]" . ,(alist-get choice iso
                                                nil nil #'string=))
                  langs-alist))
      (if (y-or-n-p "Filter by another language? ")
          (mastodon-tl--read-filter-langs langs-alist)
        langs-alist))))

(defun mastodon-tl-unfollow-user (user-handle)
  "Query for USER-HANDLE from current status and unfollow that user."
  (interactive (list (mastodon-tl--user-handles-get "unfollow")))
  (mastodon-tl--do-if-item
   (mastodon-tl--do-user-action-and-response user-handle "unfollow" t)))

(defun mastodon-tl-block-user (user-handle)
  "Query for USER-HANDLE from current status and block that user."
  (interactive (list (mastodon-tl--user-handles-get "block")))
  (mastodon-tl--do-if-item
   (mastodon-tl--do-user-action-and-response user-handle "block")))

(defun mastodon-tl-unblock-user (user-handle)
  "Query for USER-HANDLE from list of blocked users and unblock that user."
  (interactive (list (mastodon-tl--get-blocks-or-mutes-list "unblock")))
  (if (not user-handle)
      (user-error "Looks like you have no blocks to unblock!")
    (mastodon-tl--do-user-action-and-response user-handle "unblock" t)))

(defun mastodon-tl-mute-user (user-handle)
  "Query for USER-HANDLE from current status and mute that user."
  (interactive (list (mastodon-tl--user-handles-get "mute")))
  (mastodon-tl--do-if-item
   (mastodon-tl--do-user-action-and-response user-handle "mute")))

(defun mastodon-tl-unmute-user (user-handle)
  "Query for USER-HANDLE from list of muted users and unmute that user."
  (interactive (list (mastodon-tl--get-blocks-or-mutes-list "unmute")))
  (if (not user-handle)
      (user-error "Looks like you have no mutes to unmute!")
    (mastodon-tl--do-user-action-and-response user-handle "unmute" t)))

(defun mastodon-tl-dm-user (user-handle)
  "Query for USER-HANDLE from current status and compose a message to that user."
  (interactive (list (mastodon-tl--user-handles-get "message")))
  (mastodon-tl--do-if-item
   (mastodon-toot--compose-buffer (concat "@" user-handle))
   (setq mastodon-toot--visibility "direct")
   (mastodon-toot--update-status-fields)))

(defun mastodon-tl--user-handles-get (action)
  "Get the list of user-handles for ACTION from the current toot."
  (mastodon-tl--do-if-item
   (let ((user-handles
          (cond
           ((or ; follow suggests / search / foll requests compat:
             (member (mastodon-tl--get-buffer-type)
                     '( follow-suggestions search follow-requests
                        ;; profile follows/followers but not statuses:
                        profile-followers profile-following)))
            ;; fetch 'item-json:
            (list (alist-get 'acct
                             (mastodon-tl--property 'item-json :no-move))))
           ;; profile view, point in profile details, poss no toots
           ;; needed for e.g. gup.pe groups which show no toots publically:
           ((and (mastodon-tl--profile-buffer-p)
                 (get-text-property (point) 'profile-json))
            (list (alist-get 'acct
                             (mastodon-profile--profile-json))))
           ;; (grouped) notifications:
           ((member (mastodon-tl--get-buffer-type) '(mentions notifications))
            (append ;; those acting on item:
             (cl-remove-duplicates
              (cl-loop for a in (mastodon-tl--property
                                 'notification-accounts :no-move)
                       collect (alist-get 'acct a)))
             ;; mentions in item:
             (mastodon-profile--extract-users-handles
              (mastodon-profile--item-json))))
           (t
            (mastodon-profile--extract-users-handles
             (mastodon-profile--item-json))))))
     (completing-read
      (cond ((or ; TODO: make this "enable/disable notifications"
              (string= action "disable")
              (string= action "enable"))
             (format "%s notifications when user posts: " action))
            ((string-suffix-p "boosts" action)
             (format "%s by user: " action))
            (t (format "Handle of user to %s: " action)))
      user-handles nil ; predicate
      'confirm))))

(defun mastodon-tl--get-blocks-or-mutes-list (action)
  "Fetch the list of accounts for ACTION from the server.
Action must be either \"unblock\" or \"unmute\"."
  (let* ((endpoint (cond ((string= action "unblock")
                          "blocks")
                         ((string= action "unmute")
                          "mutes")))
         (url (mastodon-http--api endpoint))
         (json (mastodon-http--get-json url))
         (accts (mastodon-tl--map-alist 'acct json)))
    (when accts
      (completing-read (format "Handle of user to %s: " action)
                       accts nil :match))))

(defun mastodon-tl--do-user-action-and-response
    (user-handle action &optional negp notify langs reblogs json)
  "Do ACTION on user USER-HANDLE.
NEGP is whether the action involves un-doing something.
If NOTIFY is \"true\", enable notifications when that user posts.
If NOTIFY is \"false\", disable notifications when that user posts.
NOTIFY is only non-nil when called by `mastodon-tl-follow-user'.
LANGS is an array parameters alist of languages to filer user's posts by.
REBLOGS is a boolean string like NOTIFY, enabling or disabling
display of the user's boosts in your timeline."
  (let* ((account
          (cond
           (negp ;; unmuting/unblocking, use mute/block list
            (mastodon-profile--search-account-by-handle user-handle))
           ;; (grouped) notifications:
           ((member (mastodon-tl--get-buffer-type)
                    '(mentions notifications))
            (let ((accounts (mastodon-tl--property 'notification-accounts)))
              (or (cl-some (lambda (x)
                             (when (string= user-handle (alist-get 'acct x))
                               x))
                           accounts)
                  (mastodon-profile--lookup-account-in-status
                   user-handle
                   (mastodon-profile--item-json)))))
           (t
            (mastodon-profile--lookup-account-in-status
             user-handle
             (if (mastodon-tl--profile-buffer-p)
                 ;; profile view, use 'profile-json as status:
                 (mastodon-profile--profile-json)
               ;; muting/blocking, select from handles in current status
               (mastodon-profile--item-json))))))
         (user-id (alist-get 'id account))
         (name (mastodon-tl--display-or-uname account))
         (args (cond (notify `(("notify" . ,notify)))
                     (langs langs)
                     (reblogs `(("reblogs" . ,reblogs)))
                     (t nil)))
         (url (mastodon-http--api (format "accounts/%s/%s" user-id action))))
    (if (not account)
        (user-error "Cannot find a user with handle %S" user-handle)
      (when (or (string= action "follow") ;; y-or-n for all but follow
                (y-or-n-p (format "%s user %s? " action name)))
        (mastodon-tl--do-user-action-function
         url name user-handle action notify args reblogs json)))))

(defun mastodon-tl--do-user-action-function
    (url name user-handle action &optional notify args reblogs json)
  "Post ACTION on user NAME/USER-HANDLE to URL.
NOTIFY is either \"true\" or \"false\", and used when we have been called
by `mastodon-tl-follow-user' to enable or disable notifications.
ARGS is an alist of any parameters to send with the request."
  (let ((response (mastodon-http--post url args nil nil json)))
    (mastodon-http--triage
     response
     (lambda (response)
       (let ((json (with-current-buffer response
                     (mastodon-http--process-json))))
         ;; TODO: when > if, with failure msg
         (cond ((string= notify "true")
                (when (eq t (alist-get 'notifying json))
                  (message "Receiving notifications for user %s (@%s)!"
                           name user-handle)))
               ((string= notify "false")
                (when (eq :json-false (alist-get 'notifying json))
                  (message "Not receiving notifications for user %s (@%s)!"
                           name user-handle)))
               ((string= reblogs "true")
                (when (eq t (alist-get 'showing_reblogs json))
                  (message "Receiving boosts by user %s (@%s)!"
                           name user-handle)))
               ((string= reblogs "false")
                (when (eq :json-false (alist-get 'showing_reblogs json))
                  (message "Not receiving boosts by user %s (@%s)!"
                           name user-handle)))
               ((or (string= action "mute")
                    (string= action "unmute"))
                (message "User %s (@%s) %sd!" name user-handle action))
               ((string= args "languages[]")
                (message "User %s language filters removed!" name))
               ((assoc "languages[]" args #'string=)
                (message "User %s filtered by language(s): %s" name
                         (mapconcat #'cdr args " ")))
               ((not (or notify reblogs))
                (if (and (string= action "follow")
                         (eq t (alist-get 'requested json)))
                    (message "Follow requested for user %s (@%s)!" name user-handle)
                  (message "User %s (@%s) %sed!" name user-handle action)))))))))

(defun mastodon-tl--get-domain-blocks ()
  "Return a list of current domain blocks."
  (mastodon-http--get-json
   (mastodon-http--api "domain_blocks")))

(defun mastodon-tl-block-domain ()
  "Read a domain and block it."
  (interactive)
  (let* ((domain (read-string "Block domain: "))
         (params `(("domain" . ,domain)))
         (url (mastodon-http--api "domain_blocks"))
         (resp (mastodon-http--post url params)))
    (mastodon-http--triage
     resp
     (lambda (_)
       (message "Domain blocked!")))))

(defun mastodon-tl-unblock-domain ()
  "Read a blocked domain and unblock it."
  (interactive)
  (let ((blocks (mastodon-tl--get-domain-blocks)))
    (if (not blocks)
        (user-error "No blocked domains?")
      (let* ((domain (completing-read "Unblock domain: "
                                      blocks))
             (params `(("domain" . ,domain)))
             (url (mastodon-http--api "domain_blocks"))
             (resp (mastodon-http--delete url params)))
        (mastodon-http--triage
         resp
         (lambda (_)
           (message "Domain unblocked!")))))))


;; FOLLOW TAGS

(defun mastodon-tl--get-tags-list ()
  "Return the list of tags of the toot at point."
  (let* ((toot (mastodon-toot--base-toot-or-item-json))
         (tags (mastodon-tl--field 'tags toot)))
    (mastodon-tl--map-alist 'name tags)))

(defun mastodon-tl-follow-tag (&optional tag)
  "Prompt for a tag (from post at point) and follow it.
If TAG provided, follow it."
  (interactive)
  (let* ((tags (unless tag (mastodon-tl--get-tags-list)))
         (tag-at-point
          (unless tag
            (when (eq 'hashtag
                      (mastodon-tl--property 'mastodon-tab-stop :no-move))
              (mastodon-tl--property 'mastodon-tag :no-move))))
         (tag (or tag (completing-read
                       (format "Tag to follow [%s]: " tag-at-point)
                       tags nil nil nil nil tag-at-point)))
         (url (mastodon-http--api (format "tags/%s/follow" tag)))
         (response (mastodon-http--post url)))
    (mastodon-http--triage response
                           (lambda (_)
                             (message "tag #%s followed!" tag)))))

(defun mastodon-tl--followed-tags ()
  "Return JSON of tags followed."
  (let ((url (mastodon-http--api (format "followed_tags"))))
    (mastodon-http--get-json url)))

(defun mastodon-tl-unfollow-tag (&optional tag)
  "Prompt for a followed tag, and unfollow it.
If TAG is provided, unfollow it."
  (interactive)
  (let* ((followed-tags-json (unless tag (mastodon-tl--followed-tags)))
         (tags (unless tag (mastodon-tl--map-alist 'name followed-tags-json)))
         (tag (or tag (completing-read "Unfollow tag: " tags)))
         (url (mastodon-http--api (format "tags/%s/unfollow" tag)))
         (response (mastodon-http--post url)))
    (mastodon-http--triage response
                           (lambda (_)
                             (message "tag #%s unfollowed!" tag)))))

(defun mastodon-tl-list-followed-tags (&optional prefix)
  "List followed tags. View timeline of tag user choses.
PREFIX is sent to `mastodon-tl-get-tag-timeline', which see."
  (interactive "p")
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (mastodon-tl--map-alist 'name followed-tags-json))
         (tag (completing-read "Tag: " tags nil)))
    (if (null tag)
        (user-error "You have to follow some tags first")
      (mastodon-tl-get-tag-timeline prefix tag))))

(defun mastodon-tl-followed-tags-timeline (&optional prefix)
  "Open a timeline of multiple tags.
With a single PREFIX arg, only show posts with media.
With a double PREFIX arg, limit results to your own instance.
If `mastodon-tl--tag-timeline-tags' is set, use its tags, else
fetch followed tags and load the first four of them."
  (interactive "p")
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (or mastodon-tl--tag-timeline-tags
                   (mastodon-tl--map-alist 'name followed-tags-json))))
    (mastodon-tl--show-tag-timeline prefix tags)))

(defun mastodon-tl-some-followed-tags-timeline (&optional prefix)
  "Prompt for some tags, and open a timeline for them.
The suggestions are from followed tags, but any other tags are also allowed.
PREFIX is for `mastodon-tl--show-tag-timeline', which see."
  (interactive "p")
  (let* ((followed-tags-json (mastodon-tl--followed-tags))
         (tags (mastodon-tl--map-alist 'name followed-tags-json))
         (selection (completing-read-multiple
                     "Tags' timelines to view [TAB to view, comma to separate]: "
                     tags)))
    (mastodon-tl--show-tag-timeline prefix selection)))

(defcustom mastodon-tl--tags-groups nil
  "A list containing lists of up to four tags each.
You can load a tag timeline list with one of these by calling
`mastodon-tl-tag-group-timeline'."
  :group 'mastodon-tl
  :type '(repeat (list string string string string)))

(defun mastodon-tl-tag-group-timeline (&optional prefix)
  "Load a timeline of a tag group from `mastodon-tl--tags-groups'.
PREFIX is for `mastodon-tl--show-tag-timeline', which see."
  (interactive "P")
  (if (not mastodon-tl--tags-groups)
      (user-error
       "Set `mastodon-tl--tags-groups' to view tag group timelines")
    (let* ((list-strs (mapcar (lambda (x)
                                ;; cons of list-as-string and list:
                                (cons (prin1-to-string x) x))
                              mastodon-tl--tags-groups))
           (choice (completing-read "Tag group: " list-strs))
           (choice-list (cdr (assoc choice list-strs #'equal))))
      (mastodon-tl--show-tag-timeline prefix choice-list))))


;;; REPORT TO MODERATORS

(defun mastodon-tl--instance-rules ()
  "Return the rules of the user's instance."
  (let ((url (mastodon-http--api "instance/rules")))
    (mastodon-http--get-json url nil :silent)))

(defun mastodon-tl--report-params (account toot)
  "Query user and return report params alist.
ACCOUNT and TOOT are the data to use."
  (let* ((account-id (alist-get 'id account))
         (comment (read-string "Add comment [optional]: "))
         (item-id (when (y-or-n-p "Also report status at point? ")
                    (mastodon-tl--item-id toot))) ; base toot if poss
         (forward-p (when (y-or-n-p "Forward to remote admin? ") "true"))
         (rules (when (y-or-n-p "Cite a rule broken? ")
                  (mastodon-tl--read-rules-ids)))
         (cat (unless rules (if (y-or-n-p "Spam? ") "spam" "other"))))
    (mastodon-tl--report-build-params account-id comment item-id
                                      forward-p cat rules)))

(defun mastodon-tl--report-build-params
    (account-id comment item-id forward-p cat &optional rules)
  "Build the parameters alist based on user responses.
ACCOUNT-ID, COMMENT, ITEM-ID, FORWARD-P, CAT, and RULES are all from
`mastodon-tl--report-params', which see."
  (let ((params
         `(("account_id" . ,account-id)
           ,@(when comment `(("comment" . ,comment)))
           ,@(when item-id `(("status_ids[]" . ,item-id)))
           ,@(when forward-p `(("forward" . ,forward-p)))
           ,@(when cat `(("category" . ,cat))))))
    (if (not rules)
        params
      (let ((alist
             (mastodon-http--build-array-params-alist "rule_ids[]" rules)))
        (append alist params)))))

(defun mastodon-tl-report-to-mods ()
  "Report the author of the toot at point to your instance moderators.
Optionally report the toot at point, add a comment, cite rules
that have been broken, forward the report to the remove admin,
report the account for spam."
  (interactive)
  (mastodon-tl--do-if-item
   (when (y-or-n-p "Report author of toot at point?")
     (let* ((url (mastodon-http--api "reports"))
            (toot (mastodon-tl--toot-or-base
                   (mastodon-tl--property 'item-json :no-move)))
            (account (alist-get 'account toot))
            (handle (alist-get 'acct account))
            (params (mastodon-tl--report-params account toot))
            (response (mastodon-http--post url params)))
       (mastodon-http--triage response
                              (lambda (_)
                                (message "User %s reported!" handle)))))))

(defvar crm-separator)

(defun mastodon-tl--map-rules-alist (rules)
  "Convert RULES text and id fields into an alist."
  (mastodon-tl--map-alist-vals-to-alist 'text 'id rules))

(defun mastodon-tl--read-rules-ids ()
  "Prompt for a list of instance rules and return a list of selected ids."
  (let* ((rules (mastodon-tl--instance-rules))
         (alist (mastodon-tl--map-rules-alist rules))
         (crm-separator (replace-regexp-in-string "," "|" crm-separator))
         (choices (completing-read-multiple
                   "rules [TAB for options, | to separate]: "
                   alist nil t)))
    (mapcar (lambda (x)
              (alist-get x alist nil nil #'string=))
            choices)))


;;; UPDATING, etc.

(defun mastodon-tl--more-json (endpoint id)
  "Return JSON for timeline ENDPOINT before ID."
  (let* ((args `(("max_id" . ,(mastodon-tl--as-string id))))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url args)))

(defun mastodon-tl--more-json-async
    (endpoint id &optional params callback &rest cbargs)
  "Return JSON for timeline ENDPOINT before ID.
Then run CALLBACK with arguments CBARGS.
PARAMS is used to send any parameters needed to correctly update
the current view."
  (let* ((args `(("max_id" . ,(mastodon-tl--as-string id))))
         (args (append args params))
         (url (mastodon-http--api
               endpoint
               (when (or (and mastodon-group-notifications
                              (string= endpoint "notifications"))
                         (string-suffix-p "search" endpoint))
                 "v2"))))
    (apply #'mastodon-http--get-json-async url args callback cbargs)))

(defun mastodon-tl--more-json-async-offset (endpoint &optional params
                                                     callback &rest cbargs)
  "Return JSON for ENDPOINT, using the \"offset\" query param.
This is used for pagination with endpoints that implement the
\"offset\" parameter, rather than using link-headers or
\"max_id\".
PARAMS are the update parameters, see
`mastodon-tl--update-params'. These (\"limit\" and \"offset\")
must be set in `mastodon-tl--buffer-spec' for pagination to work.
Then run CALLBACK with arguments CBARGS."
  (let* ((params (or params (mastodon-tl--update-params)))
         (limit (string-to-number
                 (alist-get "limit" params nil nil #'string=)))
         (offset (number-to-string
                  (+ limit ; limit + old offset = new offset
                     (string-to-number
                      (alist-get "offset" params nil nil #'string=)))))
         (url (mastodon-http--api
               endpoint
               (when (string-suffix-p "search" endpoint)
                 "v2"))))
    ;; increment:
    (setf (alist-get "offset" params nil nil #'string=) offset)
    (apply #'mastodon-http--get-json-async url params callback cbargs)))

(defun mastodon-tl--updated-json (endpoint id &optional params)
  "Return JSON for timeline ENDPOINT since ID.
PARAMS is used to send any parameters needed to correctly update
the current view."
  (let* ((args `(("since_id" . ,(mastodon-tl--as-string id))))
         (args (append args params))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url args)))

;; TODO: add this to new posts in some cases, e.g. in thread view.
(defun mastodon-tl--reload-timeline-or-profile (&optional pos)
  "Reload the current timeline or profile page.
For use after e.g. deleting a toot.
POS is a number, where point will be placed.
Aims to respect any pagination in effect."
  (let ((type (mastodon-tl--get-buffer-type))
        (max-id (mastodon-tl--buffer-property 'max-id nil :no-error)))
    (cond ((eq type 'home)
           (mastodon-tl-get-home-timeline nil max-id))
          ((eq type 'federated)
           (mastodon-tl-get-federated-timeline nil nil max-id))
          ((eq type 'local)
           (mastodon-tl-get-local-timeline nil max-id))
          ((eq type 'mentions)
           (mastodon-notifications-get-mentions))
          ((eq type 'notifications)
           (mastodon-notifications-get nil nil max-id))
          ((eq type 'profile-statuses-no-boosts)
           ;; TODO: max-id arg needed here also
           (mastodon-profile-open-statuses-no-reblogs))
          ((eq type 'profile-statuses)
           (save-excursion
             (goto-char (point-min))
             (mastodon-profile--make-author-buffer
              ;; (mastodon-profile-get-toot-author max-id)))
              (mastodon-profile--profile-json))))
          ((or (eq type 'single-status)
               (eq type 'thread))
           (let ((id (mastodon-tl--buffer-property
                      'thread-item-id (current-buffer) :no-error)))
             (mastodon-tl--thread-do id))))
    ;; TODO: sends point to where point was in buffer. This is very rough; we
    ;; may have removed an item , so the buffer will be smaller, point will
    ;; end up past where we were, etc.
    (when pos
      (goto-char pos)
      (mastodon-tl-goto-prev-item :no-refresh))))

(defun mastodon-tl--build-link-header-url (str)
  "Return a URL from STR, an http Link header."
  (let* ((split (split-string str "; "))
         (url-base (string-trim (car split) "<" ">"))
         (param (cadr split)))
    (concat url-base "&" param)))

(defun mastodon-tl--use-link-header-p ()
  "Return t if we are in a view needing Link header pagination.
Currently this includes favourites, bookmarks, follow requests,
and profile pages when showing followers or accounts followed."
  (or (mastodon-tl--buffer-type-eq 'favourites)
      (mastodon-tl--buffer-type-eq 'bookmarks)
      (mastodon-tl--buffer-type-eq 'profile-followers)
      (mastodon-tl--buffer-type-eq 'profile-following)
      (mastodon-tl--buffer-type-eq 'follow-requests)))

(defun mastodon-tl--get-link-header-from-response (headers)
  "Get http Link header from list of http HEADERS."
  ;; pleroma uses "link", so case-insensitive match required:
  (when-let* ((link-headers (alist-get "Link" headers nil nil #'cl-equalp)))
    (split-string link-headers ", ")))

(defun mastodon-tl--more ()
  "Append older toots to timeline, asynchronously."
  (message "Loading...")
  (if (mastodon-tl--use-link-header-p)
      ;; link-header paginate:
      ;; can't build a URL with --more-json-async, endpoint/id:
      ;; ensure we have a "next" type here, otherwise the CAR will be the
      ;; "prev" type!
      (let ((link-header (mastodon-tl--link-header)))
        (if (> 2 (length link-header))
            (user-error "No next page")
          (let* ((next (car link-header))
                 ;;(prev (cadr (mastodon-tl--link-header)))
                 (url (mastodon-tl--build-link-header-url next)))
            (mastodon-http--get-response-async
             url nil 'mastodon-tl--more* (current-buffer) (point) :headers))))
    (cond (;; no paginate
           (or (mastodon-tl--buffer-type-eq 'follow-suggestions)
               (mastodon-tl--buffer-type-eq 'filters)
               (mastodon-tl--buffer-type-eq 'lists))
           (user-error "No more results"))
          ;; offset paginate (search, trending, user lists, ...?):
          ((or (string-prefix-p "*mastodon-trending-" (buffer-name))
               (mastodon-tl--search-buffer-p))
           (mastodon-tl--more-json-async-offset
            (mastodon-tl--endpoint)
            (mastodon-tl--update-params)
            'mastodon-tl--more* (current-buffer) (point)))
          (t ;; max_id paginate (timelines, items with ids/timestamps):
           (let ((max-id (mastodon-tl--oldest-id)))
             (mastodon-tl--more-json-async
              (mastodon-tl--endpoint)
              max-id
              (mastodon-tl--update-params)
              'mastodon-tl--more* (current-buffer) (point) nil max-id))))))

(defun mastodon-tl--more* (response buffer point-before
                                    &optional headers max-id)
  "Append older toots to timeline, asynchronously.
Runs the timeline's update function on RESPONSE, in BUFFER.
When done, places point at POINT-BEFORE.
HEADERS is the http headers returned in the response, if any.
MAX-ID is the pagination parameter, a string."
  (with-current-buffer buffer
    (if (not response)
        (user-error "No more results")
      (let* ((inhibit-read-only t)
             (json (if headers (car response) response))
             ;; FIXME: max-id pagination works for statuses only, not other
             ;; search results pages:
             (json (if (not (mastodon-tl--search-buffer-p))
                       json
                     (let ((type (mastodon-search--buf-type)))
                       (cond ((string= "statuses" type)
                              (cdr ; avoid repeat of last status
                               (alist-get 'statuses response)))
                             ((string= "hashtags" type)
                              (alist-get 'hashtags response))
                             ((string= "accounts" type)
                              (alist-get 'accounts response))))))
             (headers (when headers (cdr response)))
             (link-header
              (mastodon-tl--get-link-header-from-response headers))
             (buf-type (mastodon-tl--get-buffer-type))
             (notifs-p (or (eq buf-type 'notifications)
                           (eq buf-type 'mentions)))
             (notif-type (when notifs-p
                           (mastodon-notifications--current-type))))
        (goto-char (point-max))
        (if (eq 'thread buf-type)
            ;; if thread fully unfolded, respect it:
            ;; if thread view, call --thread-do with parent ID
            (progn (goto-char (point-min))
                   (mastodon-tl-goto-next-item)
                   (mastodon-tl--thread-do)
                   (goto-char point-before)
                   (message "Loaded full thread."))
          (if (not json)
              (user-error "No more results")
            (if notifs-p
                (mastodon-notifications--timeline json notif-type :update)
              (funcall (mastodon-tl--update-function) json))
            (goto-char point-before)
            ;; update buffer spec to new link-header or max-id:
            ;; (other values should just remain as they were)
            (mastodon-tl--set-buffer-spec (mastodon-tl--buffer-name)
                                          (mastodon-tl--endpoint)
                                          (mastodon-tl--update-function)
                                          link-header nil nil max-id)
            (message "Loading... done.")))))))

(defun mastodon-tl--find-property-range (property start-point
                                                  &optional search-backwards)
  "Return nil if no such range is found.
If PROPERTY is set at START-POINT returns a range around
START-POINT otherwise before/after START-POINT.
SEARCH-BACKWARDS determines whether we pick point
before (non-nil) or after (nil)"
  (if (get-text-property start-point property)
      ;; We are within a range, so look backwards for the start:
      (cons (previous-single-property-change
             (if (eq start-point (point-max)) start-point (1+ start-point))
             property nil (point-min))
            (next-single-property-change start-point property nil (point-max)))
    (if search-backwards
        (let* ((end (or (previous-single-property-change
                         (if (eq start-point (point-max))
                             start-point
                           (1+ start-point))
                         property)
                        ;; we may either be just before the range or there
                        ;; is nothing at all
                        (and (not (eq start-point (point-min)))
                             (get-text-property (1- start-point) property)
                             start-point)))
               (start (and end (previous-single-property-change
                                end property nil (point-min)))))
          (when end
            (cons start end)))
      (let* ((start (next-single-property-change start-point property))
             (end (and start (next-single-property-change
                              start property nil (point-max)))))
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
  (if (not (get-text-property start-point property))
      ;; If we are not within a range, we can just defer to
      ;; mastodon-tl--find-property-range directly.
      (mastodon-tl--find-property-range property start-point search-backwards)
    ;; We are within a range, we need to start the search from
    ;; before/after this range:
    (let ((current-range
           (mastodon-tl--find-property-range property start-point)))
      (if search-backwards
          (unless (eq (car current-range) (point-min))
            (mastodon-tl--find-property-range
             property (1- (car current-range)) search-backwards))
        (unless (eq (cdr current-range) (point-max))
          (mastodon-tl--find-property-range
           property (1+ (cdr current-range)) search-backwards))))))

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
                (run-at-time (time-to-seconds (time-subtract this-update
                                                             (current-time)))
                             nil ;; don't repeat
                             #'mastodon-tl--update-timestamps-callback
                             (current-buffer) nil)))))))

(defun mastodon-tl--update-timestamps-callback (buffer previous-marker)
  "Update the next few timestamp displays in BUFFER.
Start searching for more timestamps from PREVIOUS-MARKER or
from the start if it is nil."
  ;; only do things if the buffer hasn't been killed in the meantime
  (when (and mastodon-tl--enable-relative-timestamps ; just in case
             (buffer-live-p buffer))
    (save-excursion
      (with-current-buffer buffer
        (let ((previous-timestamp (if previous-marker
                                      (marker-position previous-marker)
                                    (point-min)))
              (iteration 0)
              next-timestamp-range)
          (if previous-marker
              ;; a follow-up call to process the next batch of timestamps.
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
                   start end
                   (list 'display
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
  (if (not mastodon-tl-position-after-update)
      (setq mastodon-tl--after-update-marker nil)
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
      (setq mastodon-tl--after-update-marker marker))))

(defun mastodon-tl-update ()
  "Update timeline with new toots."
  (interactive)
  ;; FIXME: actually these buffers should just reload by calling their own
  ;; load function (actually g is mostly mapped as such):
  (if (or (member (mastodon-tl--get-buffer-type)
                  '(trending-statuses
                    trending-tags
                    follow-suggestions
                    lists
                    filters
                    scheduled-statuses))
          (mastodon-tl--search-buffer-p))
      (user-error "Update not available in this view")
    ;; FIXME: handle update for search and trending buffers
    (let* ((endpoint (mastodon-tl--endpoint))
           (update-function (mastodon-tl--update-function))
           (id (mastodon-tl--newest-id)))
      ;; update a thread, without calling `mastodon-tl--updated-json':
      (if (mastodon-tl--buffer-type-eq 'thread)
          ;; load whole thread:
          (progn (mastodon-tl--thread-do id)
                 (message "Loaded full thread."))
        ;; update other timelines:
        (let* ((params (mastodon-tl--update-params))
               (json (mastodon-tl--updated-json endpoint id params)))
          (if (not json)
              (user-error "Nothing to update")
            (let ((inhibit-read-only t))
              (mastodon-tl--set-after-update-marker)
              (goto-char (or mastodon-tl--update-point (point-min)))
              (if (eq update-function 'mastodon-notifications--timeline)
                  (funcall update-function json nil :update)
                (funcall update-function json))
              (if mastodon-tl--after-update-marker
                  (goto-char mastodon-tl--after-update-marker)
                (mastodon-tl-goto-next-item)))))))))


;;; LOADING TIMELINES

(defun mastodon-tl--init
    (buffer-name endpoint update-function &optional headers params
                 hide-replies instance no-byline)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT asynchronously.
UPDATE-FUNCTION is used to recieve more toots.
HEADERS means to also collect the response headers. Used for paginating
favourites and bookmarks.
PARAMS is any parameters to send with the request.
HIDE-REPLIES is a flag indicating if replies are hidden in the current buffer.
INSTANCE is a string of another instance domain we are displaying
a timeline from.
NO-BYLINE means just insert toot body, used for announcements."
  (let ((url (if instance
                 (concat "https://" instance "/api/v1/" endpoint)
               (mastodon-http--api endpoint)))
        (buffer (concat "*mastodon-" buffer-name "*")))
    (funcall
     (if headers
         #'mastodon-http--get-response-async
       #'mastodon-http--get-json-async)
     url params 'mastodon-tl--init*
     buffer endpoint update-function headers params hide-replies
     instance no-byline)))

(defun mastodon-tl--init*
    (response buffer endpoint update-function &optional headers
              update-params hide-replies instance no-byline)
  "Initialize BUFFER with timeline targeted by ENDPOINT.
UPDATE-FUNCTION is used to recieve more toots.
RESPONSE is the data returned from the server by
`mastodon-http--process-json', with arg HEADERS a cons cell of
JSON and http headers, without it just the JSON.
NO-BYLINE means just insert toot body, used for announcements."
  (let ((json (if headers (car response) response)))
    (cond ((not json) ; praying this is right here, else try "\n[]"
           ;; this means that whatever tl was inited won't load, which is not
           ;; always wanted, as sometimes you still need the page to load so
           ;; you can be in eg mastodon-mode, have keymap, search etc.
           (message "Looks like nothing returned from endpoint: %s" endpoint)
           ;; if we are a new account, home tl may have nothing, but then
           ;; this clause means we can never load mastodon.el at all!
           ;; so as a fallback, load trending statuses:
           ;; FIXME: this could possibly be a fallback for all timelines not
           ;; just home?
           (when (string= endpoint "timelines/home")
             (mastodon-search-trending-statuses)))
          ((eq (caar json) 'error)
           (user-error "Looks like the server bugged out: \"%s\"" (cdar json)))
          (t
           (let* ((headers (when headers (cdr response)))
                  (link-header
                   (mastodon-tl--get-link-header-from-response headers)))
             (with-mastodon-buffer buffer #'mastodon-mode nil
               (mastodon-tl--set-buffer-spec
                buffer endpoint update-function
                link-header update-params hide-replies
                ;; awful hack to fix multiple reloads:
                (alist-get "max_id" update-params nil nil #'string=))
               (mastodon-tl--do-init json update-function instance no-byline)))))))

(defun mastodon-tl--init-sync
    (buffer-name endpoint update-function &optional note-type params
                 headers view-name binding-str endpoint-version)
  "Initialize BUFFER-NAME with timeline targeted by ENDPOINT.
UPDATE-FUNCTION is used to receive more toots.
Runs synchronously.
Optional arg NOTE-TYPE means only get that type of notification.
PARAMS is an alist of any params to include in the request.
HEADERS are any headers to send in the request.
VIEW-NAME is a string, to be used as a heading for the view.
BINDING-STR is a string explaining any bindins in the view, it can have
formatting for `substitute-command-keys'.
ENDPOINT-VERSION is a string, format Vx, e.g. V2."
  ;; Used by `mastodon-notifications-get' and in views.el
  (let* ((notes-params (when note-type
                         (mastodon-http--build-array-params-alist
                          "types[]" (list note-type))))
         (params (append notes-params params))
         (url (mastodon-http--api endpoint endpoint-version))
         (buffer (concat "*mastodon-" buffer-name "*"))
         (response (mastodon-http--get-response url params))
         (json (car response))
         (headers (when headers (cdr response)))
         (link-header (when headers
                        (mastodon-tl--get-link-header-from-response headers))))
    (with-mastodon-buffer buffer #'mastodon-mode nil
      ;; insert view-name/ heading-str
      (when view-name
        (mastodon-search--insert-heading view-name))
      (when binding-str
        (insert
         (substitute-command-keys
          (mastodon-tl--set-face (concat "[" binding-str "]\n\n")
                                 'mastodon-toot-docs-face))))
      (mastodon-tl--set-buffer-spec
       buffer endpoint update-function
       link-header params nil
       ;; awful hack to fix multiple reloads:
       (alist-get "max_id" params nil nil #'string=))
      (mastodon-tl--do-init json update-function nil nil note-type)
      buffer)))

(defun mastodon-tl--do-init (json update-fun &optional domain no-byline type)
  "Utility function for `mastodon-tl--init*' and `mastodon-tl--init-sync'.
JSON is the data to call UPDATE-FUN on.
When DOMAIN, force inclusion of user's domain in their handle.
NO-BYLINE means just insert toot body, used for announcements.
TYPE is a notification type."
  (remove-overlays) ; video overlays
  (cond (domain ;; maybe our update-fun doesn't always have 3 args...:
         (funcall update-fun json nil domain))
        (type (funcall update-fun json type)) ;; notif types
        (no-byline (funcall update-fun json nil nil no-byline))
        (t (funcall update-fun json)))
  (setq
   ;; Initialize with a minimal interval; we re-scan at least once
   ;; every 5 minutes to catch any timestamps we may have missed
   mastodon-tl--timestamp-next-update (time-add (current-time)
                                                (seconds-to-time 300)))
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
    (mastodon-tl--goto-first-item)))

;;; BOOKMARKS

(require 'bookmark)

(defun mastodon-tl--bookmark-handler (record)
  "Jump to a bookmarked location in mastodon.el.
RECORD is the bookmark record."
  (let ((id (bookmark-prop-get record 'id)))
    ;; we need to handle thread and single toot for starters
    (pop-to-buffer
     (mastodon-tl--thread-do id))))

(defun mastodon-tl--bookmark-make-record ()
  "Return a bookmark record for the current mastodon buffer."
  (let ((id (mastodon-tl--property 'item-id :no-move))
        (name (buffer-name)))
    `(,name
      (buf . ,name)
      (id . ,id)
      (handler . mastodon-tl--bookmark-handler))))

(add-hook 'mastodon-mode-hook
          (lambda ()
            (setq-local bookmark-make-record-function
                        #'mastodon-tl--bookmark-make-record)))

(provide 'mastodon-tl)
;;; mastodon-tl.el ends here
