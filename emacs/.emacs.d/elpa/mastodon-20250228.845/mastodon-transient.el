;;; mastodon-transient.el --- transient menus for mastodon.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  martian hiatus

;; Author: martian hiatus <mousebot@disroot.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'transient)
(require 'tp)

(defvar mastodon-active-user)
(defvar mastodon-toot-visibility-settings-list)
(defvar mastodon-iso-639-regional)
(defvar mastodon-toot-poll)

(autoload 'mastodon-toot-visibility-settings-list "mastodon-toot")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-http--patch "mastodon-http")
(autoload 'mastodon-profile-update-user-profile-note "mastodon-profile")
(autoload 'mastodon-toot--fetch-max-poll-options "mastodon-toot")
(autoload 'mastodon-toot--fetch-max-poll-option-chars "mastodon-toot")
(autoload 'mastodon-instance-data "mastodon")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")
(autoload 'mastodon-toot--read-poll-expiry "mastodon-toot")
(autoload 'mastodon-toot--poll-expiry-options-alist "mastodon-toot")
(autoload 'mastodon-toot-clear-poll "mastodon-toot")
(autoload 'mastodon-notifications-get-policy "mastodon-notifications")

;;; UTILS

(transient-define-suffix mastodon-transient--prefix-inspect ()
  "Inspect a transient prefix's arguments and scope."
  (interactive)
  :transient 'transient--do-return
  (let ((args (transient-args (oref transient-current-prefix command)))
        (scope (oref transient-current-prefix scope)))
    (message "prefix's scope: %s \ntransient-args: %s\n last: %s"
             scope args
             (length
              (cl-member-if
               (lambda (x)
                 (equal (car x) 'one))
               args)))))

;; some JSON fields that are returned under the "source" field need to be
;; sent back in the format source[key], while some others are sent kust as
;; key:
(defun mastodon-transient-parse-source-key (key)
  "Parse mastodon source KEY.
If KEY needs to be source[key], format like so, else just return
the inner key part."
  (let* ((split (split-string key "[][]"))
         (array-key (cadr split)))
    (if (or (= 1 (length split)) ;; no split
            (member array-key '("privacy" "sensitive" "language")))
        key
      array-key)))

(defun mastodon-transient-parse-source-keys (alist)
  "Parse ALIST containing source[key] keys."
  (cl-loop for a in alist
           collect (cons (mastodon-transient-parse-source-key (car a))
                         (cdr a))))

(defun mastodon-transient-get-creds ()
  "Fetch account data."
  (mastodon-http--get-json
   (mastodon-http--api "accounts/verify_credentials")
   nil :silent))

;; fields utils:
;; to PATCH fields, we just need fields[x][name] and fields[x][value]

(defun mastodon-transient--fields-alist (fields)
  "Convert fields in FIELDS to numbered conses.
The keys in the data are not numbered, so we convert the key into
the format fields.X.keyname."
  (cl-loop
   for f in fields
   for count from 1 to 5
   collect
   (cl-loop for x in f
            collect
            (cons (concat "fields." (number-to-string count)
                          "." (symbol-name (car x)))
                  (cdr x)))))

(defun mastodon-transient-field-dot-to-array (key)
  "Convert KEY from tp dot annotation to array[key] annotation."
  (tp-dot-to-array (symbol-name key) nil "_attributes"))

(defun mastodon-transient-dot-fields-to-arrays (alist)
  "Parse fields ALIST in dot notation to array notation."
  (cl-loop for y in alist
           collect
           (cons (mastodon-transient-field-dot-to-array (car y))
                 (cdr y))))

;;; TRANSIENTS

;; FIXME: PATCHing source vals as JSON request body doesn't work! existing
;; `mastodon-profile--update-preference' doesn't use it! it just uses
;; query params! strange thing is it works for non-source params
(transient-define-suffix mastodon-user-settings-update (&optional args)
  "Update current user settings on the server."
  :transient 'transient--do-exit
  (interactive (list (transient-args 'mastodon-user-settings)))
  (let* ((parsed (tp-parse-args-for-send args :strings))
         (strs (mastodon-transient-parse-source-keys parsed))
         (url (mastodon-http--api "accounts/update_credentials"))
         (resp (mastodon-http--patch url strs))) ;; :json fails
    (mastodon-http--triage
     resp
     (lambda (_resp)
       (message "Settings updated!\n%s" (pp-to-string strs))))))

(transient-define-prefix mastodon-user-settings ()
  "A transient for setting current user settings."
  :value (lambda () (tp-return-data
                     #'mastodon-transient-get-creds))
  [:description
   (lambda ()
     (format "User settings for %s" mastodon-active-user))
   (:info
    "Note: use the empty string (\"\") to remove a value from an option.")]
  ;; strings
  ["Account info"
   ("n" "display name" "display_name" :alist-key display_name :class tp-option-str)
   ("t" "update profile note" mastodon-update-profile-note)
   ("f" "update profile fields" mastodon-profile-fields)]
  ;; "choice" booleans (so we can PATCH :json-false explicitly):
  ["Account options"
   ("l" "locked" "locked" :alist-key locked :class tp-bool)
   ("b" "bot" "bot" :alist-key bot :class tp-bool)
   ("d"  "discoverable" "discoverable" :alist-key discoverable :class tp-bool)
   ("c" "hide follower/following lists" "source.hide_collections"
    :alist-key source.hide_collections :class tp-bool)
   ("i" "indexable" "source.indexable" :alist-key source.indexable :class tp-bool)]
  ["Tooting options"
   ("p" "default privacy" "source.privacy" :alist-key source.privacy
    :class tp-option
    :choices (lambda () mastodon-toot-visibility-settings-list))
   ("s" "mark sensitive" "source.sensitive" :alist-key source.sensitive :class tp-bool)
   ("g" "default language" "source.language" :alist-key source.language :class tp-option
    :choices (lambda () mastodon-iso-639-regional))]
  ["Update"
   ("C-c C-c" "Save settings" mastodon-user-settings-update)
   ("C-x C-k" :info "Revert all changes")]
  (interactive)
  (if (or (not (boundp 'mastodon-active-user))
          (not mastodon-active-user))
      (user-error "User not set")
    (transient-setup 'mastodon-user-settings)))

(transient-define-suffix mastodon-update-profile-note ()
  "Update current user profile note."
  :transient 'transient--do-exit
  (interactive)
  (mastodon-profile-update-user-profile-note))

(transient-define-suffix mastodon-profile-fields-update (args)
  "Update current user profile fields."
  :transient 'transient--do-return
  (interactive (list (transient-args 'mastodon-profile-fields)))
  (let* (;; FIXME: maybe only changed also won't work with fields, as
         ;; perhaps what is PATCHed overwrites whatever is on the server?
         ;; (only-changed (tp-only-changed-args alist))
         (arrays (mastodon-transient-dot-fields-to-arrays args))
         (endpoint "accounts/update_credentials")
         (url (mastodon-http--api endpoint))
         (resp (mastodon-http--patch url arrays))) ; :json)))
    (mastodon-http--triage
     resp (lambda (_resp) (message "Fields updated!")))))

(defun mastodon-transient-fetch-fields ()
  "Fetch profile fields (metadata)."
  (tp-return-data #'mastodon-transient-get-creds nil 'fields)
  (setq tp-transient-settings
        (mastodon-transient--fields-alist tp-transient-settings)))

(transient-define-prefix mastodon-profile-fields ()
  "A transient for setting profile fields."
  :value (lambda () (mastodon-transient-fetch-fields))
  [:description
   "Fields"
   ["Name"
    ("1 n" "" "fields.1.name" :alist-key fields.1.name :class mastodon-transient-field)
    ("2 n" "" "fields.2.name" :alist-key fields.2.name :class mastodon-transient-field)
    ("3 n" "" "fields.3.name" :alist-key fields.3.name :class mastodon-transient-field)
    ("4 n" "" "fields.4.name" :alist-key fields.4.name :class mastodon-transient-field)]
   ["Value"
    ("1 v" "" "fields.1.value" :alist-key fields.1.value :class mastodon-transient-field)
    ("2 v" "" "fields.2.value" :alist-key fields.2.value :class mastodon-transient-field)
    ("3 v" "" "fields.3.value" :alist-key fields.3.value :class mastodon-transient-field)
    ("4 v" "" "fields.4.value" :alist-key fields.4.value :class mastodon-transient-field)]]
  ["Update"
   ("C-c C-c" "Save settings" mastodon-profile-fields-update)
   ("C-x C-k" :info "Revert all changes")]
  (interactive)
  (if (not mastodon-active-user)
      (user-error "User not set")
    (transient-setup 'mastodon-profile-fields)))

(defun mastodon-transient-max-poll-opts ()
  "Return max poll options of user's instance."
  (let ((instance (mastodon-instance-data)))
    (mastodon-toot--fetch-max-poll-options instance)))

(defun mastodon-transient-max-poll-opt-chars ()
  "Return max poll option characters of user's instance."
  (let ((instance (mastodon-instance-data)))
    (mastodon-toot--fetch-max-poll-option-chars instance)))

(transient-define-suffix mastodon-transient-choice-add ()
  "Add another poll choice if possible.
Do not add more than 9 choices.
Do not add more than the server's maximum setting."
  (interactive)
  :transient 'transient--do-stay
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (choice-count (length
                        (cl-member-if
                         (lambda (x)
                           (equal (car x) 'one))
                         args)))
         (inc (1+ choice-count))
         (next (number-to-string inc))
         (next-symbol (pcase inc
                        (5 'five)
                        (6 'six)
                        (7 'seven)
                        (8 'eight)
                        (9 'nine))))
    (if (or (>= choice-count (mastodon-transient-max-poll-opts))
            (= choice-count 9))
        ;; FIXME when we hit '10', we get a binding clash with '1'. :/
        (message "Max choices reached")
      (transient-append-suffix
        'mastodon-create-poll
        '(2 -1)
        `(,next "" ,next
                :class mastodon-transient-poll-choice
                :alist-key ,next-symbol
                :transient t))))
  (transient-setup 'mastodon-create-poll))

(transient-define-prefix mastodon-create-poll ()
  "A transient for creating a poll."
  :value (lambda ()
           ;; we set `tp-transient-settings' here to the poll value poss
           ;; pulled from the server by
           ;; `mastodon-toot--server-poll-to-local'. when we are done with
           ;; the transient, we set `mastodon-toot-poll' again
           (setq tp-transient-settings mastodon-toot-poll))
  ["Create poll"
   (:info (lambda ()
            (format "Max options: %s"
                    (mastodon-transient-max-poll-opts))))
   (:info (lambda ()
            (format "Max option length: %s"
                    (mastodon-transient-max-poll-opt-chars))))]
  ["Options"
   ("m" "Multiple choice?" "multi" :alist-key multi
    :class mastodon-transient-poll-bool)
   ("h" "Hide vote count till expiry?" "hide" :alist-key hide
    :class mastodon-transient-poll-bool)
   ("e" "Expiry" "expiry" :alist-key expiry
    :class mastodon-transient-expiry)]
  ["Choices"
   ("1" "" "1" :alist-key one :class mastodon-transient-poll-choice)
   ("2" "" "2" :alist-key two :class mastodon-transient-poll-choice)
   ("3" "" "3" :alist-key three :class mastodon-transient-poll-choice)
   ("4" "" "4" :alist-key four :class mastodon-transient-poll-choice)]
  ;; TODO: display the max number of options or add options cmd
  ["Update"
   ("C-c C-s" "Add another poll choice" mastodon-transient-choice-add
    :if (lambda () (< 4 (mastodon-transient-max-poll-opts))))
   ("C-c C-c" "Save and done" mastodon-create-poll-done)
   ("C-x C-k" :info "Revert all")
   ("C-c C-k" "Delete all" mastodon-clear-poll)]
  (interactive)
  (if (not mastodon-active-user)
      (user-error "User not set")
    (transient-setup 'mastodon-create-poll)))

(transient-define-suffix mastodon-clear-poll ()
  "Clear current poll data."
  :transient 'transient--do-stay
  (interactive)
  (mastodon-toot-clear-poll :transient)
  (transient-reset))

(transient-define-suffix mastodon-create-poll-done (args)
  "Finish setting poll details."
  :transient 'transient--do-exit
  (interactive (list (transient-args 'mastodon-create-poll)))
  (let* ((options (cl-member-if (lambda (x)
                                  (eq (car x) 'one))
                                args))
         (opt-vals (cl-loop for x in options
                            collect (cdr x)))
         (lengths (mapcar #'length opt-vals))
         (vals (cl-remove 'nil
                          (cl-loop for x in args
                                   collect (cdr x))))
         (opts-count (length (cl-remove 'nil opt-vals))))
    ;; this way of checking gets annoying if we want to just cancel out of
    ;; the poll (but to actually cancel user should C-g, not C-c C-c):
    (if (or (and (< 50 (apply #'max lengths))
                 (not (y-or-n-p "Options longer than server max. Proceed? ")))
            (and (not (alist-get 'expiry args))
                 (not (y-or-n-p "No expiry. Proceed? ")))
            (and (not (< 1 opts-count))
                 (not (y-or-n-p "Need more than one option. Proceed? ")))
            (and (> opts-count (mastodon-transient-max-poll-opts))
                 (not (y-or-n-p "More options than server max. Proceed? "))))
        (call-interactively #'mastodon-create-poll)
      ;; if we are called with no poll data, do not set:
      (unless (not vals)
        ;; we set `mastodon-toot-poll' here not `tp-transient-settings'
        ;; as that is our var outside of our transient:
        (setq mastodon-toot-poll
              (tp-bools-to-strs args)))
      (mastodon-toot--update-status-fields))))

(defvar mastodon-notifications-policy-vals)
(declare-function mastodon-notifications-get-policy "mastodon-notifications")
(declare-function mastodon-notifications--update-policy "mastodon-notifications")

(transient-define-prefix mastodon-notifications-policy ()
  "A transient to set notifications policy options."
  ;; https://docs.joinmastodon.org/methods/notifications/#get-policy
  :value (lambda () (tp-return-data #'mastodon-notifications-get-policy))
  ["Notification policy options"
   ("f" "people you don't follow" "for_not_following"
    :alist-key for_not_following :class mastodon-transient-policy)
   ("F" "people not following you" "for_not_followers"
    :alist-key for_not_followers :class mastodon-transient-policy)
   ("n" "New accounts" "for_new_accounts"
    :alist-key for_new_accounts :class mastodon-transient-policy)
   ("p" "Unsolicited private mentions" "for_private_mentions"
    :alist-key for_private_mentions :class mastodon-transient-policy)
   ("l" "Moderated accounts" "for_limited_accounts"
    :alist-key for_limited_accounts :class mastodon-transient-policy)
   (:info "")
   (:info "\"accept\" = receive notifications")
   (:info "\"filter\" = mark as filtered")
   (:info "\"drop\" = do not receive any notifications")]
  ["Notification requests"
   (:info #'mastodon-notifications-requests-count)
   (:info #'mastodon-notifications-filtered-count)]
  ["Update"
   ("C-c C-c" "Save settings" mastodon-notifications-policy-update)
   ("C-x C-k" :info "Revert all changes")])

(defun mastodon-notifications-requests-count ()
  "Format a string for pending requests."
  (let ((val (oref transient--prefix value)))
    (format "Pending requests: %d"
            (or (map-nested-elt
                 val
                 '(summary pending_requests_count))
                0))))

(defun mastodon-notifications-filtered-count ()
  "Format a string for pending notifications."
  (let ((val (oref transient--prefix value)))
    (format "Pending notifications: %d"
            (or (map-nested-elt
                 val
                 '(summary pending_notifications_count))
                0))))

(transient-define-suffix mastodon-notifications-policy-update (args)
  "Send updated notification policy settings."
  :transient 'transient--do-exit
  ;; TODO:
  (interactive (list (transient-args 'mastodon-notifications-policy)))
  (let* ((parsed (tp-parse-args-for-send args))
         (resp (mastodon-notifications--update-policy parsed)))
    (mastodon-http--triage
     resp
     (lambda (_resp)
       (message "Settings updated!\n%s" (pp-to-string parsed))))))

;;; CLASSES

(defclass mastodon-transient-policy (tp-cycle)
  ((choices :initarg :choices :initform 'mastodon-notifications-policy-vals))
  "An option class for mastodon notification policy options.")

(defclass mastodon-transient-field (tp-option-str)
  ((always-read :initarg :always-read :initform t))
  "An infix option class for our options.
We always read.")

(cl-defmethod transient-init-value ((obj mastodon-transient-field))
  "Initialize value of OBJ."
  (let* ((prefix-val (oref transient--prefix value)))
    ;; (arg (oref obj alist-key)))
    (oset obj value
          (tp-get-server-val obj prefix-val))))

(cl-defmethod tp-get-server-val ((obj mastodon-transient-field) data)
  "Return the server value for OBJ from DATA.
If OBJ's key has dotted notation, drill down into the alist. Currently
only one level of nesting is supported."
  ;; TODO: handle nested alist keys
  (let* ((key (oref obj alist-key))
         (split (split-string (symbol-name key) "\\."))
         (num (string-to-number (cadr split))))
    (alist-get key
               (nth (1- num) data) nil nil #'string=)))

(cl-defmethod tp-arg-changed-p ((_obj mastodon-transient-field) cons)
  "T if value of OBJ is changed from the server value.
CONS is a cons of the form \"(fields.1.name . val)\"."
  (let* ((key-split (split-string
                     (symbol-name (car cons)) "\\."))
         (num (1- (string-to-number (nth 1 key-split))))
         (server-key (symbol-name (car cons)))
         (server-elt (nth num tp-transient-settings)))
    (not (equal (cdr cons)
                (alist-get server-key server-elt nil nil #'string=)))))

(defclass mastodon-transient-opt (tp-option tp-option-var)
  (()))

(defclass mastodon-transient-poll-bool (tp-bool tp-option-var)
  ())

(defclass mastodon-transient-poll-choice (tp-option-str tp-option-var)
  ())

(cl-defmethod transient-infix-read ((obj mastodon-transient-poll-choice))
  "Reader function for OBJ, a poll expiry."
  (let* ((value (transient-infix-value obj))
         (prompt (transient-prompt obj))
         (str (read-string prompt (cdr value)))
         (max (mastodon-transient-max-poll-opt-chars)))
    (if (not (> (length str) max))
        str
      (if (not
           (y-or-n-p
            (format "Poll option too long for server (%s/%s chars), retry?"
                    (length str) max)))
          str
        (oset obj value str)
        (transient-infix-read obj)))))

(defclass mastodon-transient-expiry (tp-option tp-option-var)
  ())

(cl-defmethod transient-infix-read ((_obj mastodon-transient-expiry))
  "Reader function for OBJ, a poll expiry."
  (cdr (mastodon-toot--read-poll-expiry)))

(cl-defmethod transient-format-value ((obj mastodon-transient-expiry))
  "Format function for OBJ, a poll expiry."
  (let* ((cons (transient-infix-value obj))
         (value (when cons (cdr cons))))
    (if (not value)
        ""
      (let ((readable
             (or (car
                  (rassoc value
                          (mastodon-toot--poll-expiry-options-alist)))
                 (concat value " secs")))) ;; editing a poll wont match expiry list
        (propertize readable
                    'face (if (tp-arg-changed-p obj cons)
                              'transient-value
                            'transient-inactive-value))))))

(provide 'mastodon-transient)
;;; mastodon-transient.el ends here
