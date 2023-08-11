;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

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

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-params-async-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-notifications-get "mastodon")
(autoload 'mastodon-tl--byline "mastodon-tl")
(autoload 'mastodon-tl--byline-author "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--content "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl")
(autoload 'mastodon-tl--init "mastodon-tl")
(autoload 'mastodon-tl--insert-status "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--spoiler "mastodon-tl")
(autoload 'mastodon-tl--toot-id "mastodon-tl")
(autoload 'mastodon-tl--update "mastodon-tl")
(autoload 'mastodon-views--view-follow-requests "mastodon-views")

(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--display-media-p)
(defvar mastodon-mode-map)

(defvar mastodon-notifications--types-alist
  '(("follow" . mastodon-notifications--follow)
    ("favourite" . mastodon-notifications--favourite)
    ("reblog" . mastodon-notifications--reblog)
    ("mention" . mastodon-notifications--mention)
    ("poll" . mastodon-notifications--poll)
    ("follow_request" . mastodon-notifications--follow-request)
    ("status" . mastodon-notifications--status)
    ("update" . mastodon-notifications--edit))
  "Alist of notification types and their corresponding function.")

(defvar mastodon-notifications--response-alist
  '(("Followed" . "you")
    ("Favourited" . "your status from")
    ("Boosted" . "your status from")
    ("Mentioned" . "you")
    ("Posted a poll" . "that has now ended")
    ("Requested to follow" . "you")
    ("Posted" . "a post")
    ("Edited" . "a post from"))
  "Alist of subjects for notification types.")

(defvar mastodon-notifications--map
  (let ((map
         (copy-keymap mastodon-mode-map)))
    (define-key map (kbd "a") #'mastodon-notifications--follow-request-accept)
    (define-key map (kbd "j") #'mastodon-notifications--follow-request-reject)
    (define-key map (kbd "C-k") #'mastodon-notifications--clear-current)
    (keymap-canonicalize map))
  "Keymap for viewing notifications.")

(defun mastodon-notifications--byline-concat (message)
  "Add byline for TOOT with MESSAGE."
  (concat
   " "
   (propertize message 'face 'highlight)
   " "
   (cdr (assoc message mastodon-notifications--response-alist))))

(defun mastodon-notifications--follow-request-process (&optional reject)
  "Process the follow request at point.
With no argument, the request is accepted. Argument REJECT means
reject the request. Can be called in notifications view or in
follow-requests view."
  (if (not (mastodon-tl--find-property-range 'toot-json (point)))
      (message "No follow request at point?")
    (let* ((toot-json (mastodon-tl--property 'toot-json))
           (f-reqs-view-p (string= "follow_requests"
                                   (plist-get mastodon-tl--buffer-spec 'endpoint)))
           (f-req-p (or (string= "follow_request" (alist-get 'type toot-json)) ;notifs
                        f-reqs-view-p)))
      (if f-req-p
          (let* ((account (or (alist-get 'account toot-json) ;notifs
                              toot-json)) ;f-reqs
                 (id (alist-get 'id account))
                 (handle (alist-get 'acct account))
                 (name (alist-get 'username account)))
            (if id
                (let ((response
                       (mastodon-http--post
                        (concat
                         (mastodon-http--api "follow_requests")
                         (format "/%s/%s" id (if reject
                                                 "reject"
                                               "authorize"))))))
                  (mastodon-http--triage response
                                         (lambda ()
                                           (if f-reqs-view-p
                                               (mastodon-views--view-follow-requests)
                                             (mastodon-tl--reload-timeline-or-profile))
                                           (message "Follow request of %s (@%s) %s!"
                                                    name handle (if reject
                                                                    "rejected"
                                                                  "accepted")))))
              (message "No account result at point?")))
        (message "No follow request at point?")))))

(defun mastodon-notifications--follow-request-accept ()
  "Accept a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process))

(defun mastodon-notifications--follow-request-reject ()
  "Reject a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process :reject))

(defun mastodon-notifications--mention (note)
  "Format for a `mention' NOTE."
  (mastodon-notifications--format-note note 'mention))

(defun mastodon-notifications--follow (note)
  "Format for a `follow' NOTE."
  (mastodon-notifications--format-note note 'follow))

(defun mastodon-notifications--follow-request (note)
  "Format for a `follow-request' NOTE."
  (mastodon-notifications--format-note note 'follow-request))

(defun mastodon-notifications--favourite (note)
  "Format for a `favourite' NOTE."
  (mastodon-notifications--format-note note 'favourite))

(defun mastodon-notifications--reblog (note)
  "Format for a `boost' NOTE."
  (mastodon-notifications--format-note note 'boost))

(defun mastodon-notifications--status (note)
  "Format for a `status' NOTE.
Status notifications are given when
`mastodon-tl--enable-notify-user-posts' has been set."
  (mastodon-notifications--format-note note 'status))

(defun mastodon-notifications--poll (note)
  "Format for a `poll' NOTE."
  (mastodon-notifications--format-note note 'poll))

(defun mastodon-notifications--edit (note)
  "Format for an `edit' NOTE."
  (mastodon-notifications--format-note note 'edit))

(defun mastodon-notifications--format-note (note type)
  "Format for a NOTE of TYPE."
  (let ((id (alist-get 'id note))
        (status (mastodon-tl--field 'status note))
        (follower (alist-get 'username (alist-get 'account note))))
    (mastodon-notifications--insert-status
     (cond ((or (equal type 'follow)
                (equal type 'follow-request))
            ;; Using reblog with an empty id will mark this as something
            ;; non-boostable/non-favable.
            (cons '(reblog (id . nil)) note))
           ;; reblogs/faves use 'note' to process their own json
           ;; not the toot's. this ensures following etc. work on such notifs
           ((or (equal type 'favourite)
                (equal type 'boost))
            note)
           (t
            status))
     (if (or (equal type 'follow)
             (equal type 'follow-request))
         (propertize (if (equal type 'follow)
                         "Congratulations, you have a new follower!"
                       (format "You have a follow request from... %s"
                               follower))
                     'face 'default)
       (mastodon-tl--clean-tabs-and-nl
        (if (mastodon-tl--has-spoiler status)
            (mastodon-tl--spoiler status)
          (mastodon-tl--content status))))
     (if (or (equal type 'follow)
             (equal type 'follow-request)
             (equal type 'mention))
         'mastodon-tl--byline-author
       (lambda (_status)
         (mastodon-tl--byline-author
          note)))
     (lambda (_status)
       (mastodon-notifications--byline-concat
        (cond ((equal type 'boost)
               "Boosted")
              ((equal type 'favourite)
               "Favourited")
              ((equal type 'follow-request)
               "Requested to follow")
              ((equal type 'follow)
               "Followed")
              ((equal type 'mention)
               "Mentioned")
              ((equal type 'status)
               "Posted")
              ((equal type 'poll)
               "Posted a poll")
              ((equal type 'edit)
               "Edited"))))
     id
     (when (or (equal type 'favourite)
               (equal type 'boost))
       status))))

(defun mastodon-notifications--insert-status (toot body
                                                   author-byline action-byline id
                                                   &optional base-toot)
  "Display the content and byline of timeline element TOOT.
BODY will form the section of the toot above the byline.

AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.

ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.

ID is the notification's own id, which is attached as a property.
If the status is a favourite or a boost, BASE-TOOT is the JSON
of the toot responded to."
  (when toot ; handle rare blank notif server bug
    (mastodon-tl--insert-status toot body author-byline action-byline id base-toot)))

(defun mastodon-notifications--by-type (note)
  "Filters NOTE for those listed in `mastodon-notifications--types-alist'."
  (let* ((type (mastodon-tl--field 'type note))
         (fun (cdr (assoc type mastodon-notifications--types-alist)))
         (start-pos (point)))
    (when fun
      (funcall fun note)
      (when mastodon-tl--display-media-p
        (mastodon-media--inline-images start-pos (point))))))

(defun mastodon-notifications--timeline (json)
  "Format JSON in Emacs buffer."
  (if (seq-empty-p json)
      (message "Looks like you have no (more) notifications for the moment.")
    (mapc #'mastodon-notifications--by-type json)
    (goto-char (point-min))))

(defun mastodon-notifications--get-mentions ()
  "Display mention notifications in buffer."
  (interactive)
  (mastodon-notifications-get "mention" "mentions"))

(defun mastodon-notifications--get-favourites ()
  "Display favourite notifications in buffer."
  (interactive)
  (mastodon-notifications-get "favourite" "favourites"))

(defun mastodon-notifications--get-boosts ()
  "Display boost notifications in buffer."
  (interactive)
  (mastodon-notifications-get "reblog" "boosts"))

(defun mastodon-notifications--get-polls ()
  "Display poll notifications in buffer."
  (interactive)
  (mastodon-notifications-get "poll" "polls"))

(defun mastodon-notifications--get-statuses ()
  "Display status notifications in buffer.
Status notifications are created when you call
`mastodon-tl--enable-notify-user-posts'."
  (interactive)
  (mastodon-notifications-get "status" "statuses"))

(defun mastodon-notifications--filter-types-list (type)
  "Return a list of notification types with TYPE removed."
  (let ((types
         (mapcar #'car mastodon-notifications--types-alist)))
    (remove type types)))

(defun mastodon-notifications--clear-all ()
  "Clear all notifications."
  (interactive)
  (when (y-or-n-p "Clear all notifications?")
    (let ((response
           (mastodon-http--post (mastodon-http--api "notifications/clear"))))
      (mastodon-http--triage
       response (lambda ()
                  (when mastodon-tl--buffer-spec
                    (mastodon-tl--reload-timeline-or-profile))
                  (message "All notifications cleared!"))))))

(defun mastodon-notifications--clear-current ()
  "Dismiss the notification at point."
  (interactive)
  (let* ((id (or (mastodon-tl--property 'toot-id)
                 (mastodon-tl--field 'id
                                     (mastodon-tl--property 'toot-json))))
         (response
          (mastodon-http--post (mastodon-http--api
                                (format "notifications/%s/dismiss" id)))))
    (mastodon-http--triage
     response (lambda ()
                (when mastodon-tl--buffer-spec
                  (mastodon-tl--reload-timeline-or-profile))
                (message "Notification dismissed!")))))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
