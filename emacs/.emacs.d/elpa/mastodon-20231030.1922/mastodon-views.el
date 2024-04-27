;;; mastodon-views.el --- Minor views functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Marty Hiatt <martianhiatus@riseup.net>
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

;; mastodon-views.el provides minor views functions.

;; These are currently lists, follow suggestions, filters, scheduled toots,
;; follow requests, and instance descriptions.

;; It doesn't include favourites, bookmarks, preferences, trending tags, followed tags, toot edits,

;;; Code:

(require 'cl-lib)
(require 'mastodon-http)
(eval-when-compile
  (require 'mastodon-tl))

(defvar mastodon-mode-map)
(defvar mastodon-tl--horiz-bar)
(defvar mastodon-tl--timeline-posts-count)

(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-tl--init "mastodon-tl")
(autoload 'mastodon-tl--init-sync "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--set-face "mastodon-tl")
(autoload 'mastodon-tl--buffer-type-eq "mastodon-tl")
(autoload 'mastodon-tl--profile-buffer-p "mastodon-tl")
(autoload 'mastodon-tl--goto-first-item "mastodon-tl")
(autoload 'mastodon-tl--do-if-item "mastodon-tl")
(autoload 'mastodon-tl--set-buffer-spec "mastodon-tl")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-notifications--follow-request-accept "mastodon-notifications")
(autoload 'mastodon-notifications--follow-request-reject "mastodon-notifications")
(autoload 'mastodon-auth--get-account-id "mastodon-auth")
(autoload 'mastodon-toot--iso-to-human "mastodon-toot")
(autoload 'mastodon-toot--schedule-toot "mastodon-toot")
(autoload 'mastodon-toot--compose-buffer "mastodon-toot")
(autoload 'mastodon-toot--set-toot-properties "mastodon-toot")
(autoload 'mastodon-search--propertize-user "mastodon-search")
(autoload 'mastodon-search--insert-users-propertized "mastodon-search")
(autoload 'mastodon-tl--map-alist "mastodon-tl")
(autoload 'mastodon-tl--map-alist-vals-to-alist "mastodon-tl")


;;; KEYMAPS

;; we copy `mastodon-mode-map', as then all timeline functions are
;; available. this is helpful because if a minor view is the only buffer left
;; open, calling `mastodon' will switch to it, but then we will be unable to
;; switch to timlines without closing the minor view.

;; copying the mode map however means we need to avoid/unbind/override any
;; functions that might cause interfere with the minor view.

;; this is not redundant, as while the buffer -init function calls
;; `mastodon-mode', it gets overridden in some but not all cases.

(defvar mastodon-views-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-mode-map)
    map)
  "Base keymap for minor mastodon views.")

(defvar mastodon-views--view-filters-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-views-map)
    (define-key map (kbd "d") #'mastodon-views--delete-filter)
    (define-key map (kbd "c") #'mastodon-views--create-filter)
    (define-key map (kbd "g") #'mastodon-views--view-filters)
    map)
  "Keymap for viewing filters.")

(defvar mastodon-views--follow-suggestions-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-views-map)
    (define-key map (kbd "g") #'mastodon-views--view-follow-suggestions)
    map)
  "Keymap for viewing follow suggestions.")

(defvar mastodon-views--view-lists-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-views-map)
    (define-key map (kbd "D") #'mastodon-views--delete-list)
    (define-key map (kbd "C") #'mastodon-views--create-list)
    (define-key map (kbd "A") #'mastodon-views--add-account-to-list)
    (define-key map (kbd "R") #'mastodon-views--remove-account-from-list)
    (define-key map (kbd "E") #'mastodon-views--edit-list)
    (define-key map (kbd "g") #'mastodon-views--view-lists)
    map)
  "Keymap for viewing lists.")

(defvar mastodon-views--list-name-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mastodon-views--view-timeline-list-at-point)
    (define-key map (kbd "d") #'mastodon-views--delete-list-at-point)
    (define-key map (kbd "a") #'mastodon-views--add-account-to-list-at-point)
    (define-key map (kbd "r") #'mastodon-views--remove-account-from-list-at-point)
    (define-key map (kbd "e") #'mastodon-views--edit-list-at-point)
    map)
  "Keymap for when point is on list name.")

(defvar mastodon-views--scheduled-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-views-map)
    (define-key map (kbd "r") #'mastodon-views--reschedule-toot)
    (define-key map (kbd "c") #'mastodon-views--cancel-scheduled-toot)
    (define-key map (kbd "e") #'mastodon-views--edit-scheduled-as-new)
    (define-key map (kbd "RET") #'mastodon-views--edit-scheduled-as-new)
    map)
  "Keymap for when point is on a scheduled toot.")

(defvar mastodon-views--view-follow-requests-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-views-map)
    ;; make reject binding match the binding in notifs view
    ;; 'r' is then reserved for replying, even tho it is not avail
    ;; in foll-reqs view
    (define-key map (kbd "j") #'mastodon-notifications--follow-request-reject)
    (define-key map (kbd "a") #'mastodon-notifications--follow-request-accept)
    (define-key map (kbd "g") #'mastodon-views--view-follow-requests)
    map)
  "Keymap for viewing follow requests.")


;;; GENERAL FUNCTION

(defun mastodon-views--minor-view (view-name insert-fun data)
  "Load a minor view named VIEW-NAME.
BINDINGS-STRING is a string explaining the view's local bindings.
INSERT-FUN is the function to call to insert the view's elements.
DATA is the argument to insert-fun, usually JSON returned in a
request.
This function is used as the update-function to
`mastodon-tl--init-sync', which initializes a buffer for us and
provides the JSON data."
  ;; FIXME: this is not an update function as it inserts a heading and
  ;; possible bindings string
  ;; either it should go in init-sync, or possibly in each view function
  ;; but either way, this function does almost nothing for us.
  ;; could we call init-sync in here pehaps?
  ;; (mastodon-search--insert-heading view-name)
  ;; (when bindings-string
  ;;   (insert (mastodon-tl--set-face (concat "[" bindings-string "]\n\n")
  ;;                                  'font-lock-comment-face)))
  (if (seq-empty-p data)
      (insert (propertize
               (format "Looks like you have no %s for now." view-name)
               'face 'font-lock-comment-face
               'byline t
               'item-type 'no-item ; for nav
               'item-id "0")) ; so point can move here when no item
    (funcall insert-fun data)
    (goto-char (point-min)))
  ;; (when data
  ;; FIXME: this seems to trigger a new request, but ideally would run.
  ;; (mastodon-tl--goto-next-item))
  )


;;; LISTS

(defun mastodon-views--view-lists ()
  "Show the user's lists in a new buffer."
  (interactive)
  (mastodon-tl--init-sync "lists" "lists"
                          'mastodon-views--insert-lists
                          nil nil nil
                          "your lists"
                          "C - create a list\n D - delete a list\
     \n A/R - add/remove account from a list\
     \n E - edit a list\n n/p - go to next/prev item")
  (with-current-buffer "*mastodon-lists*"
    (use-local-map mastodon-views--view-lists-keymap)))

(defun mastodon-views--insert-lists (json)
  "Insert the user's lists from JSON."
  (mastodon-views--minor-view
   "lists"
   #'mastodon-views--print-list-set
   json))

(defun mastodon-views--print-list-set (lists)
  "Print each account plus a separator for each list in LISTS."
  (mapc (lambda (x)
          (mastodon-views--print-list-accounts x)
          (insert (propertize (concat " " mastodon-tl--horiz-bar "\n\n")
                              'face 'success)))
        lists))

(defun mastodon-views--print-list-accounts (list)
  "Insert the accounts in list named LIST, an alist."
  (let-alist list
    (let* ((accounts (mastodon-views--accounts-in-list .id)))
      (insert
       (propertize .title
                   'byline t ; so we nav here
                   'item-id "0" ; so we nav here
                   'item-type 'list
                   'help-echo "RET: view list timeline, d: delete this list, \
a: add account to this list, r: remove account from this list"
                   'list t
                   'face 'link
                   'keymap mastodon-views--list-name-keymap
                   'list-name .title
                   'list-id .id)
       (propertize (format " [replies: %s, exclusive %s]"
                           .replies_policy
                           (when (eq t .exclusive) "true"))
                   'face 'font-lock-comment-face)
       (propertize "\n\n"
                   'list t
                   'keymap mastodon-views--list-name-keymap
                   'list-name .title
                   'list-id .id)
       (propertize
        (mapconcat #'mastodon-search--propertize-user accounts
                   " ")
        'list t
        'keymap mastodon-views--list-name-keymap
        'list-name .title
        'list-id .id)))))

(defun mastodon-views--get-users-lists ()
  "Get the list of the user's lists from the server."
  (let ((url (mastodon-http--api "lists")))
    (mastodon-http--get-json url)))

(defun mastodon-views--get-lists-names ()
  "Return a list of the user's lists' names."
  (let ((lists (mastodon-views--get-users-lists)))
    (mastodon-tl--map-alist 'title lists)))

(defun mastodon-views--get-list-by-name (name)
  "Return the list data for list with NAME."
  (let* ((lists (mastodon-views--get-users-lists)))
    (cl-loop for list in lists
             if (string= (alist-get 'title list) name)
             return list)))

(defun mastodon-views--get-list-id (name)
  "Return id for list with NAME."
  (let ((list (mastodon-views--get-list-by-name name)))
    (alist-get 'id list)))

(defun mastodon-views--get-list-name (id)
  "Return name of list with ID."
  (let* ((url (mastodon-http--api (format "lists/%s" id)))
         (response (mastodon-http--get-json url)))
    (alist-get 'title response)))

(defun mastodon-views--edit-list-at-point ()
  "Edit list at point."
  (interactive)
  (let ((id (mastodon-tl--property 'list-id :no-move)))
    (mastodon-views--edit-list id)))

(defun mastodon-views--edit-list (&optional id)
  "Prompt for a list and edit the name and replies policy.
If ID is provided, use that list."
  (interactive)
  (let* ((list-names (unless id (mastodon-views--get-lists-names)))
         (name-old (if id
                       (mastodon-tl--property 'list-name :no-move)
                     (completing-read "Edit list: " list-names)))
         (id (or id (mastodon-views--get-list-id name-old)))
         (name-choice (read-string "List name: " name-old))
         (replies-policy (completing-read "Replies policy: " ; give this a proper name
                                          '("followed" "list" "none")
                                          nil t nil nil "list"))
         (exclusive (if (y-or-n-p "Exclude items from home timeline? ")
                        "true"
                      "false"))
         (url (mastodon-http--api (format "lists/%s" id)))
         (response (mastodon-http--put url
                                       `(("title" . ,name-choice)
                                         ("replies_policy" . ,replies-policy)
                                         ("exclusive" . ,exclusive)))))
    (mastodon-http--triage response
                           (lambda (_)
                             (with-current-buffer response
                               (let* ((json (mastodon-http--process-json))
                                      (name-new (alist-get 'title json)))
                                 (message "list %s edited to %s!" name-old name-new)))
                             (when (mastodon-tl--buffer-type-eq 'lists)
                               (mastodon-views--view-lists))))))

(defun mastodon-views--view-timeline-list-at-point ()
  "View timeline of list at point."
  (interactive)
  (let ((list-id (mastodon-tl--property 'list-id :no-move)))
    (mastodon-views--view-list-timeline list-id)))

(defun mastodon-views--view-list-timeline (&optional id)
  "Prompt for a list and view its timeline.
If ID is provided, use that list."
  (interactive)
  (let* ((list-names (unless id (mastodon-views--get-lists-names)))
         (list-name (unless id (completing-read "View list: " list-names)))
         (id (or id (mastodon-views--get-list-id list-name)))
         (endpoint (format "timelines/list/%s" id))
         (name (mastodon-views--get-list-name id))
         (buffer-name (format "list-%s" name)))
    (mastodon-tl--init buffer-name endpoint
                       'mastodon-tl--timeline
                       nil
                       `(("limit" . ,mastodon-tl--timeline-posts-count)))))

(defun mastodon-views--create-list ()
  "Create a new list.
Prompt for name and replies policy."
  (interactive)
  (let* ((title (read-string "New list name: "))
         (replies-policy (completing-read "Replies policy: " ; give this a proper name
                                          '("followed" "list" "none")
                                          nil t nil nil "list")) ; default
         (exclusive (when (y-or-n-p "Exclude items from home timeline? ")
                      "true"))
         (response (mastodon-http--post (mastodon-http--api "lists")
                                        `(("title" . ,title)
                                          ("replies_policy" . ,replies-policy)
                                          ("exclusive" . ,exclusive)))))
    (mastodon-views--list-action-triage
     response "list %s created!" title)))

(defun mastodon-views--delete-list-at-point ()
  "Delete list at point."
  (interactive)
  (let ((id (mastodon-tl--property 'list-id :no-move)))
    (mastodon-views--delete-list id)))

(defun mastodon-views--delete-list (&optional id)
  "Prompt for a list and delete it.
If ID is provided, delete that list."
  (interactive)
  (let* ((list-names (unless id (mastodon-views--get-lists-names)))
         (name (if id
                   (mastodon-views--get-list-name id)
                 (completing-read "Delete list: " list-names)))
         (id (or id (mastodon-views--get-list-id name)))
         (url (mastodon-http--api (format "lists/%s" id))))
    (when (y-or-n-p (format "Delete list %s?" name))
      (let ((response (mastodon-http--delete url)))
        (mastodon-views--list-action-triage
         response "list %s deleted!" name)))))

(defun mastodon-views--get-users-followings ()
  "Return the list of followers of the logged in account."
  (let* ((id (mastodon-auth--get-account-id))
         (url (mastodon-http--api (format "accounts/%s/following" id))))
    (mastodon-http--get-json url '(("limit" . "80"))))) ; max 80 accounts

(defun mastodon-views--add-account-to-list-at-point ()
  "Prompt for account and add to list at point."
  (interactive)
  (let ((id (mastodon-tl--property 'list-id :no-move)))
    (mastodon-views--add-account-to-list id)))

(defun mastodon-views--add-account-to-list (&optional id account-id handle)
  "Prompt for a list and for an account, add account to list.
If ID is provided, use that list.
If ACCOUNT-ID and HANDLE are provided use them rather than prompting."
  (interactive)
  (let* ((list-prompt (if handle
                          (format "Add %s to list: " handle)
                        "Add account to list: "))
         (list-name (if id
                        (mastodon-tl--property 'list-name :no-move)
                      (completing-read list-prompt
                                       (mastodon-views--get-lists-names) nil t)))
         (list-id (or id (mastodon-views--get-list-id list-name)))
         (followings (mastodon-views--get-users-followings))
         (handles (mastodon-tl--map-alist-vals-to-alist 'acct 'id followings))
         (account (or handle (completing-read "Account to add: "
                                              handles nil t)))
         (account-id (or account-id (alist-get account handles)))
         (url (mastodon-http--api (format "lists/%s/accounts" list-id)))
         (response (mastodon-http--post url `(("account_ids[]" . ,account-id)))))
    (mastodon-views--list-action-triage
     response "%s added to list %s!" account list-name)))

(defun mastodon-views--add-toot-account-at-point-to-list ()
  "Prompt for a list, and add the account of the toot at point to it."
  (interactive)
  (let* ((toot (mastodon-tl--property 'item-json))
         (account (mastodon-tl--field 'account toot))
         (account-id (mastodon-tl--field 'id account))
         (handle (mastodon-tl--field 'acct account)))
    (mastodon-views--add-account-to-list nil account-id handle)))

(defun mastodon-views--remove-account-from-list-at-point ()
  "Prompt for account and remove from list at point."
  (interactive)
  (let ((id (mastodon-tl--property 'list-id :no-move)))
    (mastodon-views--remove-account-from-list id)))

(defun mastodon-views--remove-account-from-list (&optional id)
  "Prompt for a list, select an account and remove from list.
If ID is provided, use that list."
  (interactive)
  (let* ((list-name (if id
                        (mastodon-tl--property 'list-name :no-move)
                      (completing-read "Remove account from list: "
                                       (mastodon-views--get-lists-names) nil t)))
         (list-id (or id (mastodon-views--get-list-id list-name)))
         (accounts (mastodon-views--accounts-in-list list-id))
         (handles (mastodon-tl--map-alist-vals-to-alist 'acct 'id accounts))
         (account (completing-read "Account to remove: "
                                   handles nil t))
         (account-id (alist-get account handles))
         (url (mastodon-http--api (format "lists/%s/accounts" list-id)))
         (args (mastodon-http--build-array-params-alist "account_ids[]" `(,account-id)))
         (response (mastodon-http--delete url args)))
    (mastodon-views--list-action-triage
     response "%s removed from list %s!" account list-name)))

(defun mastodon-views--list-action-triage (response &rest args)
  "Call `mastodon-http--triage' on RESPONSE and call message on ARGS."
  (mastodon-http--triage response
                         (lambda (_)
                           (when (mastodon-tl--buffer-type-eq 'lists)
                             (mastodon-views--view-lists))
                           (apply #'message args))))

(defun mastodon-views--accounts-in-list (list-id)
  "Return the JSON of the accounts in list with LIST-ID."
  (let* ((url (mastodon-http--api (format "lists/%s/accounts" list-id))))
    (mastodon-http--get-json url)))


;;; FOLLOW REQUESTS

(defun mastodon-views--insert-follow-requests (json)
  "Insert the user's current follow requests.
JSON is the data returned by the server."
  (mastodon-views--minor-view
   "follow requests"
   #'mastodon-views--insert-users-propertized-note
   json))

(defun mastodon-views--view-follow-requests ()
  "Open a new buffer displaying the user's follow requests."
  (interactive)
  (mastodon-tl--init-sync "follow-requests"
                          "follow_requests"
                          'mastodon-views--insert-follow-requests
                          nil
                          '(("limit" . "40")) ; server max is 80
                          :headers
                          "follow requests"
                          "a/j - accept/reject request at point\n\
 n/p - go to next/prev request")
  (mastodon-tl--goto-first-item)
  (with-current-buffer "*mastodon-follow-requests*"
    (use-local-map mastodon-views--view-follow-requests-keymap)))


;;; SCHEDULED TOOTS

(defun mastodon-views--view-scheduled-toots ()
  "Show the user's scheduled toots in a new buffer."
  (interactive)
  (mastodon-tl--init-sync "scheduled-toots"
                          "scheduled_statuses"
                          'mastodon-views--insert-scheduled-toots
                          nil nil nil
                          "your scheduled toots"
                          "n/p - prev/next\n r - reschedule\n\
 e/RET - edit toot\n c - cancel")
  (with-current-buffer "*mastodon-scheduled-toots*"
    (use-local-map mastodon-views--scheduled-map)))

(defun mastodon-views--insert-scheduled-toots (json)
  "Insert the user's scheduled toots, from JSON."
  (mastodon-views--minor-view
   "scheduled toots"
   #'mastodon-views--insert-scheduled-toots-list
   json))

(defun mastodon-views--insert-scheduled-toots-list (scheduleds)
  "Insert scheduled toots in SCHEDULEDS."
  (mapc #'mastodon-views--insert-scheduled-toot scheduleds))

(defun mastodon-views--insert-scheduled-toot (toot)
  "Insert scheduled TOOT into the buffer."
  (let-alist toot
    (insert
     (propertize (concat .params.text
                         " | "
                         (mastodon-toot--iso-to-human .scheduled_at))
                 'byline t ; so we nav here
                 'item-id "0" ; so we nav here
                 'face 'font-lock-comment-face
                 'keymap mastodon-views--scheduled-map
                 'scheduled-json toot
                 'id .id)
     "\n")))

(defun mastodon-views--get-scheduled-toots (&optional id)
  "Get the user's currently scheduled toots.
If ID, just return that toot."
  (let* ((endpoint (if id
                       (format "scheduled_statuses/%s" id)
                     "scheduled_statuses"))
         (url (mastodon-http--api endpoint)))
    (mastodon-http--get-json url)))

(defun mastodon-views--reschedule-toot ()
  "Reschedule the scheduled toot at point."
  (interactive)
  (let ((id (mastodon-tl--property 'id :no-move)))
    (if (null id)
        (message "no scheduled toot at point?")
      (mastodon-toot--schedule-toot :reschedule))))

(defun mastodon-views--copy-scheduled-toot-text ()
  "Copy the text of the scheduled toot at point."
  (interactive)
  (let* ((toot (mastodon-tl--property 'toot :no-move))
         (params (alist-get 'params toot))
         (text (alist-get 'text params)))
    (kill-new text)))

(defun mastodon-views--cancel-scheduled-toot (&optional id no-confirm)
  "Cancel the scheduled toot at point.
ID is that of the scheduled toot to cancel.
NO-CONFIRM means there is no ask or message, there is only do."
  (interactive)
  (let ((id (or id (mastodon-tl--property 'id :no-move))))
    (if (null id)
        (message "no scheduled toot at point?")
      (when (or no-confirm
                (y-or-n-p "Cancel scheduled toot?"))
        (let* ((url (mastodon-http--api (format "scheduled_statuses/%s" id)))
               (response (mastodon-http--delete url)))
          (mastodon-http--triage response
                                 (lambda (_)
                                   (mastodon-views--view-scheduled-toots)
                                   (unless no-confirm
                                     (message "Toot cancelled!")))))))))

(defun mastodon-views--edit-scheduled-as-new ()
  "Edit scheduled status as new toot."
  (interactive)
  (let ((id (mastodon-tl--property 'id :no-move)))
    (if (null id)
        (message "no scheduled toot at point?")
      (let* ((toot (mastodon-tl--property 'scheduled-json :no-move))
             (scheduled (alist-get 'scheduled_at toot)))
        (let-alist (alist-get 'params toot)
          ;; (poll (alist-get 'poll params))
          ;; (media (alist-get 'media_attachments toot)))
          (mastodon-toot--compose-buffer)
          (goto-char (point-max))
          (insert .text)
          ;; adopt properties from scheduled toot:
          (mastodon-toot--set-toot-properties
           .in_reply_to_id .visibility .spoiler_text .language scheduled id))))))


;;; FILTERS

(defun mastodon-views--view-filters ()
  "View the user's filters in a new buffer."
  (interactive)
  (mastodon-tl--init-sync "filters" "filters"
                          'mastodon-views--insert-filters
                          nil nil nil
                          "current filters"
                          "c - create filter\n d - delete filter at point\n\
 n/p - go to next/prev filter")
  (with-current-buffer "*mastodon-filters*"
    (use-local-map mastodon-views--view-filters-keymap)))

(defun mastodon-views--insert-filters (json)
  "Insert the user's current filters.
JSON is what is returned by by the server."
  (mastodon-views--minor-view
   "filters"
   #'mastodon-views--insert-filter-string-set
   json))

(defun mastodon-views--insert-filter-string-set (json)
  "Insert a filter string plus a blank line.
JSON is the filters data."
  (mapc #'mastodon-views--insert-filter-string json))

(defun mastodon-views--insert-filter-string (filter)
  "Insert a single FILTER."
  (let* ((phrase (alist-get 'phrase filter))
         (contexts (alist-get 'context filter))
         (id (alist-get 'id filter))
         (filter-string (concat "- \"" phrase "\" filtered in: "
                                (mapconcat #'identity contexts ", "))))
    (insert
     (propertize filter-string
                 'item-id id ;for goto-next-filter compat
                 'phrase phrase
                 'byline t) ;for goto-next-filter compat
     "\n\n")))

(defun mastodon-views--create-filter ()
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
              (user-error "You must select at least one word for a filter")
            (completing-read-multiple
             "Contexts to filter [TAB for options]: "
             '("home" "notifications" "public" "thread")
             nil t)))
         (contexts-processed
          (if (equal nil contexts)
              (user-error "You must select at least one context for a filter")
            (mapcar (lambda (x)
                      (cons "context[]" x))
                    contexts)))
         (response (mastodon-http--post url (push
                                             `("phrase" . ,word)
                                             contexts-processed))))
    (mastodon-http--triage response
                           (lambda (_)
                             (message "Filter created for %s!" word)
                             (when (mastodon-tl--buffer-type-eq 'filters)
                               (mastodon-views--view-filters))))))

(defun mastodon-views--delete-filter ()
  "Delete filter at point."
  (interactive)
  (let* ((filter-id (mastodon-tl--property 'item-id :no-move))
         (phrase (mastodon-tl--property 'phrase :no-move))
         (url (mastodon-http--api (format "filters/%s" filter-id))))
    (if (null phrase)
        (user-error "No filter at point?")
      (when (y-or-n-p (format "Delete filter %s? " phrase))
        (let ((response (mastodon-http--delete url)))
          (mastodon-http--triage
           response (lambda (_)
                      (mastodon-views--view-filters)
                      (message "Filter for \"%s\" deleted!" phrase))))))))


;;; FOLLOW SUGGESTIONS
;; No pagination: max 80 results

(defun mastodon-views--view-follow-suggestions ()
  "Display a buffer of suggested accounts to follow."
  (interactive)
  (mastodon-tl--init-sync "follow-suggestions"
                          "suggestions"
                          'mastodon-views--insert-follow-suggestions
                          nil
                          '(("limit" . "80")) ; server max
                          nil
                          "suggested accounts")
  (with-current-buffer "*mastodon-follow-suggestions*"
    (use-local-map mastodon-views--follow-suggestions-map)))

(defun mastodon-views--insert-follow-suggestions (json)
  "Insert follow suggestions into buffer.
JSON is the data returned by the server."
  (mastodon-views--minor-view
   "suggested accounts"
   #'mastodon-views--insert-users-propertized-note
   json))

(defun mastodon-views--insert-users-propertized-note (json)
  "Insert users list into the buffer, including profile note.
JSON is the users list data."
  (mastodon-search--insert-users-propertized json :note))


;;; INSTANCES

(defun mastodon-views--view-own-instance (&optional brief)
  "View details of your own instance.
BRIEF means show fewer details."
  (interactive)
  (mastodon-views--view-instance-description :user brief))

(defun mastodon-views--view-own-instance-brief ()
  "View brief details of your own instance."
  (interactive)
  (mastodon-views--view-instance-description :user :brief))

(defun mastodon-views--view-instance-description-brief ()
  "View brief details of the instance the current post's author is on."
  (interactive)
  (mastodon-views--view-instance-description nil :brief))

(defun mastodon-views--get-instance-url (url username &optional instance)
  "Return an instance base url from a user account URL.
USERNAME is the name to cull.
If INSTANCE is given, use that."
  (cond (instance
         (concat "https://" instance))
        ;; pleroma URL is https://instance.com/users/username
        ((string-suffix-p "users/" (url-basepath url))
         (string-remove-suffix "/users/"
                               (url-basepath url)))
        ;; friendica is https://instance.com/profile/user
        ((string-suffix-p "profile/" (url-basepath url))
         (string-remove-suffix "/profile/"
                               (url-basepath url)))
        ;; mastodon is https://instance.com/@user
        (t
         (string-remove-suffix (concat "/@" username)
                               url))))

(defun mastodon-views--view-instance-description
    (&optional user brief instance misskey)
  "View the details of the instance the current post's author is on.
USER means to show the instance details for the logged in user.
BRIEF means to show fewer details.
INSTANCE is an instance domain name.
MISSKEY means the instance is a Misskey or derived server."
  (interactive)
  (if user
      (let ((response (mastodon-http--get-json
                       (mastodon-http--api "instance") nil nil :vector)))
        (mastodon-views--instance-response-fun response brief instance))
    (mastodon-tl--do-if-item
     (let* ((toot (if (mastodon-tl--profile-buffer-p)
                      ;; we may be on profile description itself:
                      (or (mastodon-tl--property 'profile-json)
                          ;; or on profile account listings, or just toots:
                          (mastodon-tl--property 'item-json))
                    ;; normal timeline/account listing:
                    (mastodon-tl--property 'item-json)))
            (reblog (alist-get 'reblog toot))
            (account (or (alist-get 'account reblog)
                         (alist-get 'account toot)
                         toot)) ; else `toot' is already an account listing.
            ;; we may be at toots/boosts/users in a profile buffer.
            ;; profile-json is a defacto test for if point is on the profile
            ;; details at the top of a profile buffer.
            (profile-note-p (and (mastodon-tl--profile-buffer-p)
                                 ;; only call this in profile buffers:
                                 (mastodon-tl--property 'profile-json)))
            (url (if profile-note-p
                     (alist-get 'url toot) ; profile description
                   (alist-get 'url account)))
            (username (if profile-note-p
                          (alist-get 'username toot) ;; profile
                        (alist-get 'username account)))
            (instance (mastodon-views--get-instance-url url username instance)))
       (if misskey
           (let* ((params `(("detail" . ,(or brief t))))
                  (headers '(("Content-Type" . "application/json")))
                  (url (concat instance "/api/meta"))
                  (response
                   (with-current-buffer (mastodon-http--post url params headers t :json)
                     (mastodon-http--process-response))))
             (mastodon-views--instance-response-fun response brief instance :misskey))
         (let ((response (mastodon-http--get-json
                          (concat instance "/api/v1/instance") nil nil :vector)))
           ;; if non-misskey attempt errors, try misskey instance:
           ;; akkoma i guess should not error here.
           (if (eq 'error (caar response))
               (mastodon-views--instance-desc-misskey)
             (mastodon-views--instance-response-fun response brief instance))))))))

(defun mastodon-views--instance-desc-misskey (&optional user brief instance)
  "Show instance description for a misskey/firefish server.
USER, BRIEF, and INSTANCE are all for
`mastodon-views--view-instance-description', which see."
  (interactive)
  (mastodon-views--view-instance-description user brief instance :miskey))

(defun mastodon-views--instance-response-fun (response brief instance
                                                       &optional misskey)
  "Display instance description RESPONSE in a new buffer.
BRIEF means to show fewer details.
INSTANCE is the instance were are working with.
MISSKEY means the instance is a Misskey or derived server."
  (when response
    (let* ((domain (url-file-nondirectory instance))
           (buf (get-buffer-create
                 (format "*mastodon-instance-%s*" domain))))
      (with-mastodon-buffer buf #'special-mode :other-window
        (if misskey
            (mastodon-views--insert-json response)
          (condition-case nil
              (progn
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
                (mastodon-views--print-json-keys response)
                (mastodon-tl--set-buffer-spec (buffer-name buf) "instance" nil)
                (goto-char (point-min)))
            (error ; just insert the raw response:
             (mastodon-views--insert-json response))))))))

(defun mastodon-views--insert-json (response)
  "Insert raw JSON RESPONSE in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (prin1-to-string response))
    (pp-buffer)
    (goto-char (point-min))))

(defun mastodon-views--format-key (el pad)
  "Format a key of element EL, a cons, with PAD padding."
  (format (concat "%-"
                  (number-to-string pad)
                  "s: ")
          (propertize (prin1-to-string (car el))
                      'face '(:underline t))))

(defun mastodon-views--print-json-keys (response &optional ind)
  "Print the JSON keys and values in RESPONSE.
IND is the optional indentation level to print at."
  (let* ((cars (mapcar (lambda (x) (symbol-name (car x)))
                       response))
         (pad (1+ (cl-reduce #'max (mapcar #'length cars)))))
    (while response
      (let ((el (pop response)))
        (cond
         ((and (vectorp (cdr el)) ; vector of alists (fields, instance rules):
               (not (seq-empty-p (cdr el)))
               (consp (seq-elt (cdr el) 0)))
          (insert (mastodon-views--format-key el pad)
                  "\n\n")
          (seq-do #'mastodon-views--print-instance-rules-or-fields (cdr el))
          (insert "\n"))
         ((and (vectorp (cdr el)) ; vector of strings (media types):
               (not (seq-empty-p (cdr el)))
               (< 1 (seq-length (cdr el)))
               (stringp (seq-elt (cdr el) 0)))
          (when ind (indent-to ind))
          (insert (mastodon-views--format-key el pad)
                  "\n"
                  (seq-mapcat
                   (lambda (x) (concat x ", "))
                   (cdr el) 'string)
                  "\n\n"))
         ((consp (cdr el)) ; basic nesting:
          (when ind (indent-to ind))
          (insert (mastodon-views--format-key el pad)
                  "\n\n")
          (mastodon-views--print-json-keys
           (cdr el) (if ind (+ ind 4) 4)))
         (t ; basic handling of raw booleans:
          (let ((val (cond ((equal (cdr el) :json-false)
                            "no")
                           ((equal (cdr el) 't)
                            "yes")
                           (t
                            (cdr el)))))
            (when ind (indent-to ind))
            (insert (mastodon-views--format-key el pad)
                    " "
                    (mastodon-views--newline-if-long (cdr el))
                    ;; only send strings to --render-text (for hyperlinks):
                    (mastodon-tl--render-text
                     (if (stringp val) val (prin1-to-string val)))
                    "\n"))))))))

(defun mastodon-views--print-instance-rules-or-fields (alist)
  "Print ALIST of instance rules or contact account or emoji fields."
  (let-alist alist
    (let ((key (or .id .name .shortcode))
          (value (or .text .value .url)))
      (indent-to 4)
      (insert (format "%-5s: "
                      (propertize key 'face '(:underline t)))
              (mastodon-views--newline-if-long value)
              (format "%s" (mastodon-tl--render-text
                            value))
              "\n"))))

(defun mastodon-views--newline-if-long (el)
  "Return a newline string if the cdr of EL is over 50 characters long."
  (let ((rend (if (stringp el) (mastodon-tl--render-text el) el)))
    (if (and (sequencep rend)
             (< 50 (length rend)))
        "\n"
      "")))

(provide 'mastodon-views)
;;; mastodon-views.el ends here
