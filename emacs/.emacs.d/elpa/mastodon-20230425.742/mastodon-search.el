;;; mastodon-search.el --- Search functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Marty Hiatt
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

;; A basic search function for mastodon.el

;;; Code:
(require 'json)

(autoload 'mastodon-auth--access-token "mastodon-auth")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--get-search-json "mastodon-http")
(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-tl--set-buffer-spec "mastodon-tl")
(autoload 'mastodon-tl--set-face "mastodon-tl")
(autoload 'mastodon-tl--timeline "mastodon-tl")
(autoload 'mastodon-tl--toot "mastodon-tl")

(defvar mastodon-toot--completion-style-for-mentions)
(defvar mastodon-instance-url)
(defvar mastodon-tl--link-keymap)
(defvar mastodon-tl--horiz-bar)

;; functions for completion of mentions in mastodon-toot

(defun mastodon-search--get-user-info-@ (account)
  "Get user handle, display name and account URL from ACCOUNT."
  (list (concat "@" (cdr (assoc 'acct account)))
        (cdr (assoc 'url account))
        (cdr (assoc 'display_name account))))

(defun mastodon-search--search-accounts-query (query)
  "Prompt for a search QUERY and return accounts synchronously.
Returns a nested list containing user handle, display name, and URL."
  (let* ((url (mastodon-http--api "accounts/search"))
         (response (if (equal mastodon-toot--completion-style-for-mentions "following")
                       (mastodon-http--get-json url `(("q" . ,query) ("following" . "true")) :silent)
                     (mastodon-http--get-json url `(("q" . ,query)) :silent))))
    (mapcar #'mastodon-search--get-user-info-@ response)))

;; functions for tags completion:

(defun mastodon-search--search-tags-query (query)
  "Return an alist containing tag strings plus their URLs.
QUERY is the string to search."
  (let* ((url (format "%s/api/v2/search" mastodon-instance-url))
         (params `(("q" . ,query)
                   ("type" . "hashtags")))
         (response (mastodon-http--get-json url params :silent))
         (tags (alist-get 'hashtags response)))
    (mapcar #'mastodon-search--get-hashtag-info tags)))

;; trending tags

(defun mastodon-search--trending-tags ()
  "Display a list of tags trending on your instance."
  (interactive)
  (mastodon-search--view-trending "tags"
                                  #'mastodon-search--print-tags-list))

(defun mastodon-search--trending-statuses ()
  "Display a list of statuses trending on your instance."
  (interactive)
  (mastodon-search--view-trending "statuses"
                                  #'mastodon-tl--timeline))

(defun mastodon-search--get-full-statuses-data (response)
  "For statuses list in RESPONSE, fetch and return full status JSON."
  (let ((status-ids-list
         (mapcar #'mastodon-search--get-id-from-status response)))
    (mapcar #'mastodon-search--fetch-full-status-from-id
            status-ids-list)))

(defun mastodon-search--view-trending (type print-fun)
  "Display a list of tags trending on your instance.
TYPE is a string, either tags, statuses, or links.
PRINT-FUN is the function used to print the data from the response."
  (let* ((url (mastodon-http--api
               (format "trends/%s" type)))
         ;; max for statuses = 40, for others = 20
         (params (if (equal type "statuses")
                     `(("limit" . "40"))
                   `(("limit" . "20")) ))
         (response (mastodon-http--get-json url params))
         (data (cond ((equal type "tags")
                      (mapcar #'mastodon-search--get-hashtag-info
                              response))
                     ((equal type "statuses")
                      (mastodon-search--get-full-statuses-data response))
                     ((equal type "links")
                      (message "todo"))))
         (buffer (get-buffer-create
                  (format "*mastodon-trending-%s*" type))))
    (with-current-buffer buffer
      (switch-to-buffer (current-buffer))
      (mastodon-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mastodon-tl--set-buffer-spec (buffer-name buffer)
                                      (format "api/v1/trends/%s" type)
                                      nil)
        (insert (mastodon-tl--set-face
                 (concat "\n " mastodon-tl--horiz-bar "\n"
                         (upcase (format " TRENDING %s\n" type))
                         " " mastodon-tl--horiz-bar "\n\n")
                 'success))
        (funcall print-fun data)
        (unless (equal type "statuses")
          (goto-char (point-min)))))))

;; functions for mastodon search

(defun mastodon-search--search-query (query)
  "Prompt for a search QUERY and return accounts, statuses, and hashtags."
  (interactive "sSearch mastodon for: ")
  (let* ((url (format "%s/api/v2/search" mastodon-instance-url))
         (buffer (format "*mastodon-search-%s*" query))
         (response (mastodon-http--get-json url `(("q" . ,query))))
         (accts (alist-get 'accounts response))
         (tags (alist-get 'hashtags response))
         (statuses (alist-get 'statuses response))
         ;; this is now done in search--insert-users-propertized
         ;; (user-ids (mapcar #'mastodon-search--get-user-info
         ;; accts)) ; returns a list of three-item lists
         (tags-list (mapcar #'mastodon-search--get-hashtag-info
                            tags))
         (toots-list-json
          (mastodon-search--get-full-statuses-data statuses)))
    (with-current-buffer (get-buffer-create buffer)
      (switch-to-buffer buffer)
      (mastodon-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mastodon-tl--set-buffer-spec buffer
                                      "api/v2/search"
                                      nil)
        ;; user results:
        (insert (mastodon-tl--set-face
                 (concat "\n " mastodon-tl--horiz-bar "\n"
                         " USERS\n"
                         " " mastodon-tl--horiz-bar "\n\n")
                 'success))
        (mastodon-search--insert-users-propertized accts :note)
        ;; hashtag results:
        (insert (mastodon-tl--set-face
                 (concat "\n " mastodon-tl--horiz-bar "\n"
                         " HASHTAGS\n"
                         " " mastodon-tl--horiz-bar "\n\n")
                 'success))
        (mastodon-search--print-tags-list tags-list)
        ;; status results:
        (insert (mastodon-tl--set-face
                 (concat "\n " mastodon-tl--horiz-bar "\n"
                         " STATUSES\n"
                         " " mastodon-tl--horiz-bar "\n")
                 'success))
        (mapc #'mastodon-tl--toot toots-list-json)
        (goto-char (point-min))))))

(defun mastodon-search--insert-users-propertized (json &optional note)
  "Insert users list into the buffer.
JSON is the data from the server. If NOTE is non-nil, include
user's profile note. This is also called by
`mastodon-tl--get-follow-suggestions' and
`mastodon-profile--insert-follow-requests'."
  (mapc (lambda (acct)
          (insert (concat (mastodon-search--propertize-user acct note)
                          mastodon-tl--horiz-bar
                          "\n\n")))
        json))

(defun mastodon-search--propertize-user (acct &optional note)
  "Propertize display string for ACCT, optionally including profile NOTE."
  (let ((user (mastodon-search--get-user-info acct)))
    (propertize
     (concat (propertize (car user)
                         'face 'mastodon-display-name-face
                         'byline t
                         'toot-id "0")
             " : \n : "
             (propertize (concat "@" (cadr user))
                         'face 'mastodon-handle-face
                         'mouse-face 'highlight
		         'mastodon-tab-stop 'user-handle
		         'keymap mastodon-tl--link-keymap
                         'mastodon-handle (concat "@" (cadr user))
		         'help-echo (concat "Browse user profile of @" (cadr user)))
             " : \n"
             (if note
                 (mastodon-tl--render-text (cadddr user) acct)
               "")
             "\n")
     'toot-json acct))) ; so named for compat w other processing functions

(defun mastodon-search--print-tags-list (tags)
  "Insert a propertized list of TAGS."
  (mapc (lambda (el)
          (insert
           " : "
           (propertize (concat "#"
                               (car el))
                       'face '(:box t)
                       'mouse-face 'highlight
                       'mastodon-tag (car el)
                       'mastodon-tab-stop 'hashtag
                       'help-echo (concat "Browse tag #" (car el))
                       'keymap mastodon-tl--link-keymap)
           " : \n\n"))
        tags))

(defun mastodon-search--get-user-info (account)
  "Get user handle, display name, account URL and profile note from ACCOUNT."
  (list (if (not (string-empty-p (alist-get 'display_name account)))
            (alist-get 'display_name account)
          (alist-get 'username account))
        (alist-get 'acct account)
        (alist-get 'url account)
        (alist-get 'note account)))

(defun mastodon-search--get-hashtag-info (tag)
  "Get hashtag name and URL from TAG."
  (list (alist-get 'name tag)
        (alist-get 'url tag)))

(defun mastodon-search--get-status-info (status)
  "Get ID, timestamp, content, and spoiler from STATUS."
  (list (alist-get 'id status)
        (alist-get 'created_at status)
        (alist-get 'spoiler_text status)
        (alist-get 'content status)))

(defun mastodon-search--get-id-from-status (status)
  "Fetch the id from a STATUS returned by a search call to the server.
We use this to fetch the complete status from the server."
  (alist-get 'id status))

(defun mastodon-search--fetch-full-status-from-id (id)
  "Fetch the full status with id ID from the server.
This allows us to access the full account etc. details and to
render them properly."
  (let* ((url (concat mastodon-instance-url "/api/v1/statuses/" (mastodon-tl--as-string id)))
         (json (mastodon-http--get-json url)))
    json))

(provide 'mastodon-search)
;;; mastodon-search.el ends here
