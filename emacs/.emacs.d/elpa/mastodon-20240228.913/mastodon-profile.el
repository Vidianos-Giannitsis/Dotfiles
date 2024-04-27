;;; mastodon-profile.el --- Functions for inspecting Mastodon profiles -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
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

;; mastodon-profile.el generates a stream of users toots.
;; To add
;;  - Option to follow
;;  - wheather they follow you or not
;;  - Show only Media

;;; Code:
(require 'seq)
(require 'cl-lib)
(require 'persist)
(require 'parse-time)
(eval-when-compile
  (require 'mastodon-tl))

(autoload 'mastodon-auth--get-account-id "mastodon-auth")
(autoload 'mastodon-auth--get-account-name "mastodon-auth.el")
(autoload 'mastodon-http--api "mastodon-http.el")
(autoload 'mastodon-http--get-json "mastodon-http.el")
(autoload 'mastodon-http--get-json-async "mastodon-http.el")
(autoload 'mastodon-http--get-response "mastodon-http")
(autoload 'mastodon-http--patch "mastodon-http")
(autoload 'mastodon-http--patch-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http.el")
(autoload 'mastodon-http--triage "mastodon-http.el")
(autoload 'mastodon-kill-window "mastodon")
(autoload 'mastodon-media--get-media-link-rendering "mastodon-media.el")
(autoload 'mastodon-media--inline-images "mastodon-media.el")
(autoload 'mastodon-mode "mastodon.el")
(autoload 'mastodon-notifications--follow-request-accept "mastodon-notifications")
(autoload 'mastodon-notifications--follow-request-reject "mastodon-notifications")
(autoload 'mastodon-search--insert-users-propertized "mastodon-search")
(autoload 'mastodon-tl--as-string "mastodon-tl.el")
(autoload 'mastodon-tl--buffer-type-eq "mastodon tl")
(autoload 'mastodon-tl--byline-author "mastodon-tl.el")
(autoload 'mastodon-tl--find-property-range "mastodon-tl.el")
(autoload 'mastodon-tl--get-link-header-from-response "mastodon-tl")
(autoload 'mastodon-tl--init "mastodon-tl.el")
(autoload 'mastodon-tl--user-handles-get "mastodon-tl")
(autoload 'mastodon-tl--map-alist "mastodon-tl")
(autoload 'mastodon-tl--map-alist-vals-to-alist "mastodon-tl")
(autoload 'mastodon-tl--profile-buffer-p "mastodon tl")
(autoload 'mastodon-tl--property "mastodon-tl.el")
(autoload 'mastodon-tl--render-text "mastodon-tl.el")
(autoload 'mastodon-tl--set-buffer-spec "mastodon-tl")
(autoload 'mastodon-tl--set-face "mastodon-tl.el")
(autoload 'mastodon-tl--symbol "mastodon-tl")
(autoload 'mastodon-tl--timeline "mastodon-tl.el")
(autoload 'mastodon-tl--toot "mastodon-tl")
(autoload 'mastodon-tl--item-id "mastodon-tl")
(autoload 'mastodon-toot--count-toot-chars "mastodon-toot")
(autoload 'mastodon-toot--get-max-toot-chars "mastodon-toot")
(autoload 'mastodon-views--add-account-to-list "mastodon-views")
(autoload 'mastodon-return-credential-account "mastodon")
(autoload 'mastodon-tl--buffer-property "mastodon-tl")
(autoload 'mastodon-search--query "mastodon-search")

(defvar mastodon-tl--horiz-bar)
(defvar mastodon-tl--update-point)
(defvar mastodon-toot--max-toot-chars)
(defvar mastodon-toot--visibility)
(defvar mastodon-toot--content-nsfw)
(defvar mastodon-tl--timeline-posts-count)

(defvar-local mastodon-profile--account nil
  "The data for the account being described in the current profile buffer.")

(defvar mastodon-profile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-profile--account-view-cycle)
    (define-key map (kbd "C-c C-s") #'mastodon-profile--account-search)
    map)
  "Keymap for `mastodon-profile-mode'.")

(define-minor-mode mastodon-profile-mode
  "Toggle mastodon profile minor mode.
This minor mode is used for mastodon profile pages and adds a couple of
extra keybindings."
  :init-value nil
  :lighter " Profile"
  :keymap mastodon-profile-mode-map
  :group 'mastodon
  :global nil)

(defvar mastodon-profile-credential-account nil
  "Holds the JSON data of the CredentialAccount entity.
It contains details of the current user's account.")

(defvar mastodon-profile-acccount-preferences-data nil
  "Holds the JSON data of the current user's preferences.")

(defvar mastodon-profile-update-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-profile--user-profile-send-updated)
    (define-key map (kbd "C-c C-k") #'mastodon-profile--update-profile-note-cancel)
    map)
  "Keymap for `mastodon-profile-update-mode'.")

(persist-defvar mastodon-profile-account-settings nil
                "An alist of account settings saved from the server.
Other clients can change these settings on the server at any
time, so this list is not the canonical source for settings. It
is updated on entering mastodon mode and on toggle any setting it
contains")

(define-minor-mode mastodon-profile-update-mode
  "Minor mode to update Mastodon user profile."
  :group 'mastodon-profile
  :keymap mastodon-profile-update-mode-map
  :global nil)

(defun mastodon-profile--item-json ()
  "Get the next item-json."
  (mastodon-tl--property 'item-json))

(defun mastodon-profile--make-author-buffer
    (account &optional no-reblogs no-replies)
  "Take an ACCOUNT json and insert a user account into a new buffer.
NO-REBLOGS means do not display boosts in statuses."
  (mastodon-profile--make-profile-buffer-for
   account "statuses" #'mastodon-tl--timeline no-reblogs nil no-replies))

;; TODO: we shd just load all views' data then switch coz this is slow af:
(defun mastodon-profile--account-view-cycle ()
  "Cycle through profile view: toots, toot sans boosts, followers, and following."
  (interactive)
  (cond ((mastodon-tl--buffer-type-eq 'profile-statuses)
         (mastodon-profile--open-statuses-no-reblogs))
        ((mastodon-tl--buffer-type-eq 'profile-statuses-no-boosts)
         (mastodon-profile--open-statuses-no-replies))
        ((mastodon-tl--buffer-type-eq 'profile-statuses-no-replies)
         (mastodon-profile--open-followers))
        ((mastodon-tl--buffer-type-eq 'profile-followers)
         (mastodon-profile--open-following))
        ((mastodon-tl--buffer-type-eq 'profile-following)
         (mastodon-profile--make-author-buffer mastodon-profile--account))))

(defun mastodon-profile--open-statuses-no-replies ()
  "Open a profile buffer showing statuses including replies."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-author-buffer
       mastodon-profile--account nil :no-replies)
    (user-error "Not in a mastodon profile")))

(defun mastodon-profile--open-statuses-no-reblogs ()
  "Open a profile buffer showing statuses without reblogs."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-author-buffer
       mastodon-profile--account :no-reblogs)
    (user-error "Not in a mastodon profile")))

(defun mastodon-profile--open-following ()
  "Open a profile buffer showing the accounts that current profile follows."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-profile-buffer-for
       mastodon-profile--account "following"
       #'mastodon-profile--format-user nil :headers)
    (user-error "Not in a mastodon profile")))

(defun mastodon-profile--open-followers ()
  "Open a profile buffer showing the accounts following the current profile."
  (interactive)
  (if mastodon-profile--account
      (mastodon-profile--make-profile-buffer-for
       mastodon-profile--account "followers"
       #'mastodon-profile--format-user nil :headers)
    (user-error "Not in a mastodon profile")))

(defun mastodon-profile--view-favourites ()
  "Open a new buffer displaying the user's favourites."
  (interactive)
  (message "Loading your favourited toots...")
  (mastodon-tl--init "favourites" "favourites"
                     'mastodon-tl--timeline :headers))

(defun mastodon-profile--view-bookmarks ()
  "Open a new buffer displaying the user's bookmarks."
  (interactive)
  (message "Loading your bookmarked toots...")
  (mastodon-tl--init "bookmarks" "bookmarks"
                     'mastodon-tl--timeline :headers))

(defun mastodon-profile--add-account-to-list ()
  "Add account of current profile buffer to a list."
  (interactive)
  (when mastodon-profile--account
    (let* ((profile mastodon-profile--account)
           (id (alist-get 'id profile))
           (handle (alist-get 'acct profile)))
      (mastodon-views--add-account-to-list nil id handle))))

(defun mastodon-profile--account-search (query)
  "Run a statuses search QUERY for the currently viewed account."
  (interactive "sSearch account for: ")
  (let* ((ep (mastodon-tl--buffer-property 'endpoint))
         (id (nth 1 (split-string ep "/"))))
    (mastodon-search--query query "statuses" nil nil id)))


;;; ACCOUNT PREFERENCES

(defun mastodon-profile--get-json-value (val)
  "Fetch current VAL ue from account."
  (let* ((response (mastodon-return-credential-account)))
    (if (eq (alist-get val response) :json-false)
        nil
      (alist-get val response))))

(defun mastodon-profile--get-source-values ()
  "Return the \"source\" preferences from the server."
  (mastodon-profile--get-json-value 'source))

(defun mastodon-profile--get-source-value (pref)
  "Return account PREF erence from the \"source\" section on the server."
  (let ((source (mastodon-profile--get-source-values)))
    (if (eq (alist-get pref source) :json-false)
        nil
      (alist-get pref source))))

(defun mastodon-profile--update-user-profile-note ()
  "Fetch user's profile note and display for editing."
  (interactive)
  (let* ((json (mastodon-return-credential-account))
         (source (alist-get 'source json))
         (note (alist-get 'note source))
         (buffer (get-buffer-create "*mastodon-update-profile*"))
         (inhibit-read-only t)
         (msg-str (substitute-command-keys
                   "Edit your profile note. \\`C-c C-c' to send, \\`C-c C-k' to cancel.")))
    (switch-to-buffer-other-window buffer)
    (text-mode)
    (mastodon-tl--set-buffer-spec (buffer-name buffer) "accounts/verify_credentials" nil)
    (setq-local header-line-format msg-str)
    (mastodon-profile-update-mode t)
    (insert (propertize (concat (propertize "0"
                                            'note-counter t
                                            'display nil)
                                "/500 characters")
                        'read-only t
                        'face 'font-lock-comment-face
                        'note-header t)
            "\n")
    (make-local-variable 'after-change-functions)
    (cl-pushnew #'mastodon-profile--update-note-count after-change-functions)
    (let ((start-point (point)))
      (insert note)
      (goto-char start-point))
    (delete-trailing-whitespace) ; remove all ^M's
    (message msg-str)))

(defun mastodon-profile--update-note-count (&rest _args)
  "Display the character count of the profile note buffer."
  (let* ((inhibit-read-only t)
        (header-region (mastodon-tl--find-property-range 'note-header
                                                         (point-min)))
        (count-region (mastodon-tl--find-property-range 'note-counter
                                                        (point-min)))
        (count (number-to-string (mastodon-toot--count-toot-chars
                                  (buffer-substring-no-properties
                                   (cdr header-region) (point-max))))))
    (add-text-properties (car count-region) (cdr count-region)
                         (list 'display count))))

(defun mastodon-profile--update-profile-note-cancel ()
  "Cancel updating user profile and kill buffer and window."
  (interactive)
  (when (y-or-n-p "Cancel updating your profile note?")
    (mastodon-kill-window)))

(defun mastodon-profile--note-remove-header ()
  "Get the body of a toot from the current compose buffer."
  (let ((header-region (mastodon-tl--find-property-range 'note-header
                                                         (point-min))))
    (buffer-substring (cdr header-region) (point-max))))

(defun mastodon-profile--user-profile-send-updated ()
  "Send PATCH request with the updated profile note.
Ask for confirmation if length > 500 characters."
  (interactive)
  (let* ((note (mastodon-profile--note-remove-header))
         (url (mastodon-http--api "accounts/update_credentials")))
    (if (> (mastodon-toot--count-toot-chars note) 500)
        (when (y-or-n-p "Note is over mastodon's max for profile notes (500). Proceed?")
          (quit-window 'kill)
          (mastodon-profile--user-profile-send-updated-do url note))
      (quit-window 'kill)
      (mastodon-profile--user-profile-send-updated-do url note))))

(defun mastodon-profile--user-profile-send-updated-do (url note)
  "Send PATCH request with the updated profile NOTE to URL."
  (let ((response (mastodon-http--patch url `(("note" . ,note)))))
    (mastodon-http--triage response
                           (lambda (_) (message "Profile note updated!")))))

(defun mastodon-profile--update-preference (pref val &optional source)
  "Update account PREF erence to setting VAL.
Both args are strings.
SOURCE means that the preference is in the `source' part of the account JSON."
  (let* ((url (mastodon-http--api "accounts/update_credentials"))
         (pref-formatted (if source (concat "source[" pref "]") pref))
         (response (mastodon-http--patch url `((,pref-formatted . ,val)))))
    (mastodon-http--triage response
                           (lambda (_)
                             (mastodon-profile--fetch-server-account-settings)
                             (message "Account setting %s updated to %s!" pref val)))))

(defun mastodon-profile--get-pref (pref)
  "Return PREF from `mastodon-profile-account-settings'."
  (plist-get mastodon-profile-account-settings pref))

(defun mastodon-profile--update-preference-plist (pref val)
  "Set local account preference plist preference PREF to VAL.
This is done after changing the setting on the server."
  (setq mastodon-profile-account-settings
        (plist-put mastodon-profile-account-settings pref val)))

;; used in toot.el
(defun mastodon-profile--fetch-server-account-settings-maybe ()
  "Fetch account settings from the server.
Only do so if `mastodon-profile-account-settings' is nil."
  (mastodon-profile--fetch-server-account-settings :no-force))

(defun mastodon-profile--fetch-server-account-settings (&optional no-force)
  "Fetch basic account settings from the server.
Store the values in `mastodon-profile-account-settings'.
Run in `mastodon-mode-hook'.
If NO-FORCE, only fetch if `mastodon-profile-account-settings' is nil."
  (unless (and no-force mastodon-profile-account-settings)
    (let ((keys '(locked discoverable display_name bot))
          (source-keys '(privacy sensitive language)))
      (mapc (lambda (k)
              (mastodon-profile--update-preference-plist
               k (mastodon-profile--get-json-value k)))
            keys)
      (mapc (lambda (sk)
              (mastodon-profile--update-preference-plist
               sk (mastodon-profile--get-source-value sk)))
            source-keys)
      ;; hack for max toot chars:
      (mastodon-toot--get-max-toot-chars :no-toot)
      (mastodon-profile--update-preference-plist 'max_toot_chars
                                                 mastodon-toot--max-toot-chars)
      ;; TODO: remove now redundant vars, replace with fetchers from the plist
      (setq mastodon-toot--visibility (mastodon-profile--get-pref 'privacy)
            mastodon-toot--content-nsfw (mastodon-profile--get-pref 'sensitive))
      mastodon-profile-account-settings)))

(defun mastodon-profile--account-locked-toggle ()
  "Toggle the locked status of your account.
Locked means follow requests have to be approved."
  (interactive)
  (mastodon-profile--toggle-account-key 'locked))

(defun mastodon-profile--account-discoverable-toggle ()
  "Toggle the discoverable status of your account.
Discoverable means the account is listed in the server directory."
  (interactive)
  (mastodon-profile--toggle-account-key 'discoverable))

(defun mastodon-profile--account-bot-toggle ()
  "Toggle the bot status of your account."
  (interactive)
  (mastodon-profile--toggle-account-key 'bot))

(defun mastodon-profile--account-sensitive-toggle ()
  "Toggle the sensitive status of your account.
When enabled, statuses are marked as sensitive by default."
  (interactive)
  (mastodon-profile--toggle-account-key 'sensitive :source))

(defun mastodon-profile--toggle-account-key (key &optional source)
  "Toggle the boolean account setting KEY.
SOURCE means the setting is located under \"source\" in the account JSON.
Current settings are fetched from the server."
  (let* ((val (if source
                  (mastodon-profile--get-source-value key)
                (mastodon-profile--get-json-value key)))
         (prompt (format "Account setting %s is %s. Toggle?" key val)))
    (when (y-or-n-p prompt)
      (mastodon-profile--update-preference (symbol-name key)
                                           (if val "false" "true")
                                           source))))

(defun mastodon-profile--edit-string-value (key)
  "Edit the string for account preference KEY."
  (let* ((val (mastodon-profile--get-json-value key))
         (new-val (read-string (format "Edit account setting %s: " key)
                               val)))
    (mastodon-profile--update-preference (symbol-name key) new-val)))

(defun mastodon-profile--update-display-name ()
  "Update display name for your account."
  (interactive)
  (mastodon-profile--edit-string-value 'display_name))

(defun mastodon-profile--make-meta-fields-params (fields)
  "Construct a parameter query string from metadata alist FIELDS.
Returns an alist."
  (let ((keys (cl-loop for count from 1 to 5
                       collect (cons (format "fields_attributes[%s][name]" count)
                                     (format "fields_attributes[%s][value]" count)))))
    (cl-loop for a-pair in keys
             for b-pair in fields
             append (list (cons (car a-pair) (car b-pair))
                          (cons (cdr a-pair) (cdr b-pair))))))

(defun mastodon-profile--update-meta-fields ()
  "Prompt for new metadata fields information and PATCH the server."
  (interactive)
  (let* ((url (mastodon-http--api "accounts/update_credentials"))
         (fields-updated (mastodon-profile--update-meta-fields-alist))
         (params (mastodon-profile--make-meta-fields-params fields-updated))
         (response (mastodon-http--patch url params)))
    (mastodon-http--triage response
                           (lambda (_)
                             (mastodon-profile--fetch-server-account-settings)
                             (message "Metadata fields updated to %s!"
                                      fields-updated)))))

(defun mastodon-profile--update-meta-fields-alist ()
  "Prompt for new metadata fields information.
Returns the results as an alist."
  (let ((fields-old (mastodon-profile--fields-get
                     nil
                     ;; we must fetch the plaintext version:
                     (mastodon-profile--get-source-value 'fields))))
    ;; offer empty fields if user currently has less than four filled:
    (while (< (length fields-old) 4)
      (setq fields-old (append fields-old '(("" . "")))))
    (let* ((f-str "Metadata %s [%s/4] (max. 255 chars): ")
           (alist
            (cl-loop for f in fields-old
                     for x from 1 to 5
                     collect
                     (cons (read-string (format f-str "key" x) (car f))
                           (read-string (format f-str "value" x) (cdr f))))))
      (mapcar (lambda (x)
                (cons (mastodon-profile--limit-to-255 (car x))
                      (mastodon-profile--limit-to-255 (cdr x))))
              alist))))

(defun mastodon-profile--limit-to-255 (x)
  "Limit string X to 255 chars max."
  (if (> (length x) 255) (substring x 0 255) x))

;; used in tl.el
(defun mastodon-profile--get-preferences-pref (pref)
  "Fetch PREF from the endpoint \"/preferences\".
If `mastodon-profile-acccount-preferences-data' is set, fetch
from that instead.
The endpoint only holds a few preferences. For others, see
`mastodon-profile--update-preference' and its endpoint,
\"/accounts/update_credentials.\""
  (alist-get pref
             (or mastodon-profile-acccount-preferences-data
                 (setq mastodon-profile-acccount-preferences-data
                       (mastodon-http--get-json
                        (mastodon-http--api "preferences"))))))

(defun mastodon-profile--view-preferences ()
  "View user preferences in another window."
  (interactive)
  (let* ((url (mastodon-http--api "preferences"))
         (response (mastodon-http--get-json url))
         (buf (get-buffer-create "*mastodon-preferences*")))
    (with-mastodon-buffer buf #'special-mode :other-window
      (mastodon-tl--set-buffer-spec (buffer-name buf) "preferences" nil)
      (while response
        (let ((el (pop response)))
          (insert (format "%-30s %s"
                          (prin1-to-string (car el))
                          (prin1-to-string (cdr el)))
                  "\n\n")))
      (goto-char (point-min)))))


;;; PROFILE VIEW DETAILS

(defun mastodon-profile--relationships-get (id)
  "Fetch info about logged-in user's relationship to user with id ID."
  (let* ((their-id id)
         (args `(("id[]" . ,their-id)))
         (url (mastodon-http--api "accounts/relationships")))
    (car (mastodon-http--get-json url args)))) ; API takes array, just get 1st

(defun mastodon-profile--fields-get (&optional account fields)
  "Fetch the fields vector (aka profile metadata) from profile of ACCOUNT.
Returns an alist.
FIELDS means provide a fields vector fetched by other means."
  (let ((fields (or fields (alist-get 'fields account))))
    (when fields
      (mastodon-tl--map-alist-vals-to-alist 'name 'value fields))))

(defun mastodon-profile--fields-insert (fields)
  "Format and insert field pairs (a.k.a profile metadata) in FIELDS."
  (let* ((car-fields (mapcar #'car fields))
         (left-width (cl-reduce #'max (mapcar #'length car-fields))))
    (mapconcat (lambda (field)
                 (mastodon-tl--render-text
                  (concat
                   (format "_ %s " (car field))
                   (make-string (- (+ 1 left-width) (length (car field))) ?_)
                   (format " :: %s" (cdr field)))
                  field)) ; hack to make links tabstops
               fields "")))

(defun mastodon-profile--get-statuses-pinned (account)
  "Fetch the pinned toots for ACCOUNT."
  (let* ((id (alist-get 'id account))
         (args `(("pinned" . "true")))
         (url (mastodon-http--api (format "accounts/%s/statuses" id))))
    (mastodon-http--get-json url args)))

(defun mastodon-profile--insert-statuses-pinned (pinned-statuses)
  "Insert each of the PINNED-STATUSES for a given account."
  (mapc (lambda (pinned-status)
          (insert (mastodon-tl--set-face "   :pinned: " 'success))
          (mastodon-tl--toot pinned-status))
        pinned-statuses))

(defun mastodon-profile--follows-p (list)
  "T if you have any relationship with the accounts in LIST."
  (let (result)
    (dolist (x list result)
      (when (not (equal :json-false x))
        (setq result x)))))

(defun mastodon-profile--render-roles (roles)
  "Return a propertized string of badges for ROLES."
  (mapconcat
   (lambda (role)
     (propertize
      (alist-get 'name role)
      'face `(:box t :foreground ,(alist-get 'color role))))
   roles))

(defun mastodon-profile--make-profile-buffer-for
    (account endpoint-type update-function
             &optional no-reblogs headers no-replies)
  "Display profile of ACCOUNT, using ENDPOINT-TYPE and UPDATE-FUNCTION.
NO-REBLOGS means do not display boosts in statuses.
HEADERS means also fetch link headers for pagination."
  (let-alist account
    (let* ((args `(("limit" . ,mastodon-tl--timeline-posts-count)))
           (args (cond (no-reblogs
                        (push '("exclude_reblogs" . "t") args))
                       (no-replies
                        (push '("exclude_replies" . "t") args))
                       (t
                        args)))
           (endpoint (format "accounts/%s/%s" .id endpoint-type))
           (url (mastodon-http--api endpoint))
           (buffer (concat "*mastodon-" .acct "-"
                           (cond (no-reblogs
                                  (concat endpoint-type "-no-boosts"))
                                 (no-replies
                                  (concat endpoint-type "-no-replies"))
                                 (t
                                  endpoint-type))
                           "*"))
           (response (if headers
                         (mastodon-http--get-response url args)
                       (mastodon-http--get-json url args)))
           (json (if headers (car response) response))
           (link-header (when headers
                          (mastodon-tl--get-link-header-from-response
                           (cdr response))))
           (fields (mastodon-profile--fields-get account))
           (pinned (mastodon-profile--get-statuses-pinned account))
           (relationships (mastodon-profile--relationships-get .id)))
      (with-mastodon-buffer buffer #'mastodon-mode nil
        (mastodon-profile-mode)
        (remove-overlays)
        (setq mastodon-profile--account account)
        (mastodon-tl--set-buffer-spec buffer endpoint
                                      update-function link-header
                                      args)
        (let* ((inhibit-read-only t)
               (is-statuses (string= endpoint-type "statuses"))
               (is-followers (string= endpoint-type "followers"))
               (is-following (string= endpoint-type "following"))
               (endpoint-name (cond
                               (is-statuses (cond (no-reblogs
                                                   "  TOOTS (no boosts)")
                                                  (no-replies
                                                   "  TOOTS (no replies)")
                                                  (t
                                                   "    TOOTS    ")))
                               (is-followers "  FOLLOWERS  ")
                               (is-following "  FOLLOWING  "))))
          (insert
           (propertize
            (concat
             "\n"
             (mastodon-profile--image-from-account account 'avatar_static)
             (mastodon-profile--image-from-account account 'header_static)
             "\n"
             (propertize .display_name 'face 'mastodon-display-name-face)
             ;; roles
             (when .roles
               (concat " "
                       (mastodon-profile--render-roles .roles)))
             "\n"
             (propertize (concat "@" .acct) 'face 'default)
             (if (equal .locked t)
                 (concat " " (mastodon-tl--symbol 'locked))
               "")
             "\n " mastodon-tl--horiz-bar "\n"
             ;; profile note:
             (mastodon-tl--render-text .note account) ; account = tab-stops in profile
             ;; meta fields:
             (if fields
                 (concat "\n" (mastodon-tl--set-face
                               (mastodon-profile--fields-insert fields)
                               'success))
               "")
             "\n"
             ;; Joined date:
             (propertize
              (mastodon-profile--format-joined-date-string .created_at)
              'face 'success)
             "\n\n")
            'profile-json account)
           ;; insert counts
           (mastodon-tl--set-face
            (concat " " mastodon-tl--horiz-bar "\n"
                    " TOOTS: " (mastodon-tl--as-string .statuses_count) " | "
                    "FOLLOWERS: " (mastodon-tl--as-string .followers_count) " | "
                    "FOLLOWING: " (mastodon-tl--as-string .following_count) "\n"
                    " " mastodon-tl--horiz-bar "\n\n")
            'success)
           ;; insert relationship (follows)
           (let-alist relationships
             (let ((followsp (mastodon-profile--follows-p
                              (list .requested_by .following .followed_by))))
               (if followsp
                   (mastodon-tl--set-face
                    (concat (when (equal .following 't)
                              " | FOLLOWED BY YOU")
                            (when (equal .followed_by 't)
                              " | FOLLOWS YOU")
                            (when (equal .requested_by 't)
                              " | REQUESTED TO FOLLOW YOU")
                            "\n\n")
                    'success)
                 ""))) ; for insert call
           ;; insert endpoint
           (mastodon-tl--set-face (concat " " mastodon-tl--horiz-bar "\n"
                                          endpoint-name "\n"
                                          " " mastodon-tl--horiz-bar "\n")
                                  'success))
          (setq mastodon-tl--update-point (point))
          (mastodon-media--inline-images (point-min) (point))
          ;; insert pinned toots first
          (when (and pinned (equal endpoint-type "statuses"))
            (mastodon-profile--insert-statuses-pinned pinned)
            (setq mastodon-tl--update-point (point))) ; updates after pinned toots
          (funcall update-function json))
        (goto-char (point-min))
        (message
         (substitute-command-keys
          ;; "\\[mastodon-profile--account-view-cycle]" ; not always bound?
          "\\`C-c C-c' to cycle profile views: toots, no replies, no boosts,\
 followers, following.
\\`C-c C-s' to search user's toots."))))))

(defun mastodon-profile--format-joined-date-string (joined)
  "Format a human-readable Joined string from timestamp JOINED.
JOINED is the `created_at' field in profile account JSON, and of
the format \"2000-01-31T00:00:00.000Z\"."
  (format-time-string "Joined: %d %B %Y"
                      (parse-iso8601-time-string joined)))

(defun mastodon-profile--get-toot-author ()
  "Open profile of author of toot under point.
If toot is a boost, opens the profile of the booster."
  (interactive)
  (mastodon-profile--make-author-buffer
   (alist-get 'account (mastodon-profile--item-json))))

(defun mastodon-profile--image-from-account (account img-type)
  "Return a avatar image from ACCOUNT.
IMG-TYPE is the JSON key from the account data."
  (let ((img (alist-get img-type account)))
    (unless (equal img "/avatars/original/missing.png")
      (mastodon-media--get-media-link-rendering img))))

(defun mastodon-profile--show-user (user-handle)
  "Query for USER-HANDLE from current status and show that user's profile."
  (interactive
   (list
    (if (and (not (mastodon-tl--profile-buffer-p))
             (not (mastodon-tl--property 'item-json :no-move)))
        (message "Looks like there's no toot or user at point?")
      (let ((user-handles (mastodon-profile--extract-users-handles
                           (mastodon-profile--item-json))))
        (completing-read "View profile of user [choose or enter any handle]: "
                         user-handles
                         nil ; predicate
                         'confirm)))))
  (if (not (or ; own profile has no need for item-json test:
            (equal user-handle (mastodon-auth--get-account-name))
            (mastodon-tl--profile-buffer-p)
            (mastodon-tl--property 'item-json :no-move)))
      (message "Looks like there's no toot or user at point?")
    (let ((account (mastodon-profile--lookup-account-in-status
                    user-handle (mastodon-profile--item-json))))
      (if account
          (progn
            (message "Loading profile of user %s..." user-handle)
            (mastodon-profile--make-author-buffer account))
        (message "Cannot find a user with handle %S" user-handle)))))

(defun mastodon-profile--my-profile ()
  "Show the profile of the currently signed in user."
  (interactive)
  (message "Loading your profile...")
  (let ((account (mastodon-profile--account-from-id
                  (mastodon-auth--get-account-id))))
    (mastodon-profile--make-author-buffer account)))

(defun mastodon-profile--format-user (tootv)
  "Convert TOOTV into author-bylines and insert.
Also insert their profile note.
Used to view a user's followers and those they're following."
  (let ((inhibit-read-only t))
    (unless (seq-empty-p tootv)
      (mapc
       (lambda (toot)
         (let ((start-pos (point)))
           (insert "\n"
                   (propertize
                    (mastodon-tl--byline-author `((account . ,toot)) :avatar)
                    'byline  't
                    'item-id (alist-get 'id toot)
                    'base-item-id (mastodon-tl--item-id toot)
                    'item-json toot))
           (mastodon-media--inline-images start-pos (point))
           (insert "\n"
                   (propertize
                    (mastodon-tl--render-text (alist-get 'note toot) nil)
                    'item-json toot)
                   "\n")))
       tootv))))

(defun mastodon-profile--search-account-by-handle (handle)
  "Return an account based on a user's HANDLE.
If the handle does not match a search return then retun NIL."
  (let* ((handle (if (string= "@" (substring handle 0 1))
                     (substring handle 1 (length handle))
                   handle))
         (args `(("q" . ,handle)
                 ("type" . "accounts")))
         (result (mastodon-http--get-json (mastodon-http--api-search) args))
         (matching-account (seq-remove
                            (lambda (x)
                              (not (string= (alist-get 'acct x) handle)))
                            (alist-get 'accounts result))))
    (when (equal 1 (length matching-account))
      (elt matching-account 0))))

(defun mastodon-profile--account-from-id (user-id)
  "Request an account object relating to a USER-ID from Mastodon."
  (mastodon-http--get-json
   (mastodon-http--api (format "accounts/%s" user-id))))

(defun mastodon-profile--extract-users-handles (status)
  "Return all user handles found in STATUS.
These include the author, author of reblogged entries and any user mentioned."
  (when status
    (let ((this-account (or (alist-get 'account status) ; status is a toot
                            status)) ; status is a user listing
	  (mentions (or (alist-get 'mentions (alist-get 'status status))
                        (alist-get 'mentions status)))
	  (reblog (or (alist-get 'reblog (alist-get 'status status))
                      (alist-get 'reblog status))))
      (seq-filter #'stringp
                  (seq-uniq
                   (seq-concatenate
                    'list
                    (list (alist-get 'acct this-account))
                    (mastodon-profile--extract-users-handles reblog)
                    (mastodon-tl--map-alist 'acct mentions)))))))

(defun mastodon-profile--lookup-account-in-status (handle status)
  "Return account for HANDLE using hints in STATUS if possible."
  (let* ((this-account (alist-get 'account status))
         (reblog-account (alist-get 'account (alist-get 'reblog status)))
         (mention-id (seq-some
                      (lambda (mention)
                        (when (string= handle (alist-get 'acct mention))
                          (alist-get 'id mention)))
                      (alist-get 'mentions status))))
    (cond ((string= handle (alist-get 'acct this-account))
           this-account)
          ((string= handle (alist-get 'acct reblog-account))
           reblog-account)
          (mention-id
           (mastodon-profile--account-from-id mention-id))
          (t
           (mastodon-profile--search-account-by-handle handle)))))

(defun mastodon-profile--remove-user-from-followers (&optional id)
  "Remove a user from your followers.
Optionally provide the ID of the account to remove."
  (interactive)
  (let* ((account (unless id (mastodon-tl--property 'item-json :no-move)))
         (id (or id (alist-get 'id account)))
         (handle (if account
                     (alist-get 'acct account)
                   (let ((account (mastodon-profile--account-from-id id)))
                     (alist-get 'acct account))))
         (url (mastodon-http--api
               (format "accounts/%s/remove_from_followers" id))))
    (when (y-or-n-p (format "Remove follower %s? " handle))
      (let ((response (mastodon-http--post url)))
        (mastodon-http--triage response
                               (lambda (_)
                                 (message "Follower %s removed!" handle)))))))

(defun mastodon-profile--remove-from-followers-at-point ()
  "Prompt for a user in the item at point and remove from followers."
  (interactive)
  (let* ((handles (mastodon-profile--extract-users-handles
                   (mastodon-profile--item-json)))
         (handle (completing-read "Remove from followers: " handles nil))
         (account (mastodon-profile--lookup-account-in-status
                   handle (mastodon-profile--item-json)))
         (id (alist-get 'id account)))
    (mastodon-profile--remove-user-from-followers id)))

(defun mastodon-profile--remove-from-followers-list ()
  "Select a user from your followers and remove from followers.
Currently limited to 100 handles. If not found, try
`mastodon-search--query'."
  (interactive)
  (let* ((endpoint (format "accounts/%s/followers"
                           (mastodon-auth--get-account-id)))
         (url (mastodon-http--api endpoint))
         (response (mastodon-http--get-json url `(("limit" . "100"))))
         (handles (mastodon-tl--map-alist-vals-to-alist 'acct 'id response))
         (choice (completing-read "Remove from followers: " handles))
         (id (alist-get choice handles)))
    (mastodon-profile--remove-user-from-followers id)))

(defun mastodon-profile--add-private-note-to-account ()
  "Add a private note to an account.
Can be called from a profile page or normal timeline.
Send an empty note to clear an existing one."
  (interactive)
  (mastodon-profile--add-or-view-private-note
   'mastodon-profile--post-private-note-to-account
   "add a note to"))

(defun mastodon-profile--post-private-note-to-account (id handle note-old)
  "POST a private note onto an account ID with user HANDLE on the server.
NOTE-OLD is the text of any existing note."
  (let* ((note (read-string (format "Add private note to account %s: " handle)
                            note-old))
         (params `(("comment" . ,note)))
         (url (mastodon-http--api (format "accounts/%s/note" id)))
         (response (mastodon-http--post url params)))
    (mastodon-http--triage response
                           (lambda (_)
                             (message "Private note on %s added!" handle)))))

(defun mastodon-profile--view-account-private-note ()
  "Display the private note about a user."
  (interactive)
  (mastodon-profile--add-or-view-private-note
   'mastodon-profile--display-private-note
   "view private note of"
   :view))

(defun mastodon-profile--display-private-note (note)
  "Display private NOTE in a temporary buffer."
  (with-output-to-temp-buffer "*mastodon-profile-private-note*"
    (let ((inhibit-read-only t))
      (princ note))))

(defun mastodon-profile--profile-json ()
  "Return the profile-json property if we are in a profile buffer."
  (when (mastodon-tl--profile-buffer-p)
    (save-excursion
      (goto-char (point-min))
      (or (mastodon-tl--property 'profile-json :no-move)
          (error "No profile data found")))))

(defun mastodon-profile--add-or-view-private-note (action-fun &optional message view)
  "Add or view a private note for an account.
ACTION-FUN does the adding or viewing, MESSAGE is a prompt for
`mastodon-tl--user-handles-get', VIEW is a flag."
  (let* ((profile-json (mastodon-profile--profile-json))
         (handle (if (mastodon-tl--profile-buffer-p)
                     (alist-get 'acct profile-json)
                   (mastodon-tl--user-handles-get message)))
         (account (if (mastodon-tl--profile-buffer-p)
                      profile-json
                    (mastodon-profile--search-account-by-handle handle)))
         (id (alist-get 'id account))
         (relationships (mastodon-profile--relationships-get id))
         (note (alist-get 'note relationships)))
    (if view
        (if (string-empty-p note)
            (message "No private note for %s" handle)
          (funcall action-fun note))
      (funcall action-fun id handle note))))

(defun mastodon-profile--show-familiar-followers ()
  "Show a list of familiar followers.
Familiar followers are accounts that you follow, and that follow
the given account."
  (interactive)
  (let* ((profile-json (mastodon-profile--profile-json))
         (handle
          (if (mastodon-tl--profile-buffer-p)
              (alist-get 'acct profile-json)
            (mastodon-tl--user-handles-get "show familiar followers of")))
         (account (if (mastodon-tl--profile-buffer-p)
                      profile-json
                    (mastodon-profile--search-account-by-handle handle)))
         (id (alist-get 'id account)))
    (mastodon-profile--get-familiar-followers id)))

(defun mastodon-profile--get-familiar-followers (id)
  "Return JSON data of familiar followers for account ID."
  ;; the server handles multiple IDs, but we just handle one.
  (let* ((params `(("id" . ,id)))
         (url (mastodon-http--api "accounts/familiar_followers"))
         (json (mastodon-http--get-json url params))
         (accounts (alist-get 'accounts (car json))) ; first id
         (handles (mastodon-tl--map-alist 'acct accounts)))
    (if (null handles)
        (message "Looks like there are no familiar followers for this account")
      (let ((choice (completing-read "Show profile of user: " handles)))
        (mastodon-profile--show-user choice)))))

(provide 'mastodon-profile)
;;; mastodon-profile.el ends here
