;;; mastodon-client.el --- Client functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2021 Abhiseck Paira <abhiseckpaira@disroot.org>
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

;; mastodon-client.el supports registering the Emacs client with your Mastodon instance.

;;; Code:

(require 'plstore)
(require 'json)
(require 'url)

(defvar mastodon-instance-url)
(defvar mastodon-active-user)

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")

(defcustom mastodon-client--token-file (concat user-emacs-directory "mastodon.plstore")
  "File path where Mastodon access tokens are stored."
  :group 'mastodon
  :type 'file)

(defvar mastodon-client--client-details-alist nil
  "An alist of Client id and secrets keyed by the instance url.")

(defvar mastodon-client--active-user-details-plist nil
  "A plist of active user details.")

(defvar mastodon-client-scopes "read write follow"
  "Scopes to pass to oauth during registration.")

(defvar mastodon-client-website "https://codeberg.org/martianh/mastodon.el"
  "Website of mastodon.el.")

(defvar mastodon-client-redirect-uri "urn:ietf:wg:oauth:2.0:oob"
  "Redirect_uri as required by oauth.")

(defun mastodon-client--register ()
  "POST client to Mastodon."
  (mastodon-http--post (mastodon-http--api "apps")
                       `(("client_name" . "mastodon.el")
                         ("redirect_uris" . ,mastodon-client-redirect-uri)
                         ("scopes" . ,mastodon-client-scopes)
                         ("website" . ,mastodon-client-website))
                       nil
                       :unauthenticated))

(defun mastodon-client--fetch ()
  "Return JSON from `mastodon-client--register' call."
  (with-current-buffer (mastodon-client--register)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'vector)
          (json-string (buffer-substring-no-properties (point) (point-max))))
      (json-read-from-string json-string))))

(defun mastodon-client--token-file ()
  "Return `mastodon-client--token-file'."
  mastodon-client--token-file)

(defun mastodon-client--store ()
  "Store client_id and client_secret in `mastodon-client--token-file'.

Make `mastodon-client--fetch' call to determine client values."
  (let ((plstore (plstore-open (mastodon-client--token-file)))
	(client (mastodon-client--fetch))
	;; alexgriffith reported seeing ellipses in the saved output
	;; which indicate some output truncating. Nothing in `plstore-save'
	;; seems to ensure this cannot happen so let's do that ourselves:
	(print-length nil)
	(print-level nil))
    (plstore-put plstore (concat "mastodon-" mastodon-instance-url) client nil)
    (plstore-save plstore)
    (plstore-close plstore)
    client))

(defun mastodon-client--remove-key-from-plstore (plstore)
  "Remove KEY from PLSTORE."
  (cdr plstore))

;; Actually it returns a plist with client-details if such details are
;; already stored in mastodon.plstore
(defun mastodon-client--read ()
  "Retrieve client_id and client_secret from `mastodon-client--token-file'."
  (let* ((plstore (plstore-open (mastodon-client--token-file)))
         (mastodon (plstore-get plstore (concat "mastodon-" mastodon-instance-url))))
    (mastodon-client--remove-key-from-plstore mastodon)))

(defun mastodon-client--general-read (key)
  "Retrieve the plstore item keyed by KEY.
Return plist without the KEY."
  (let* ((plstore (plstore-open (mastodon-client--token-file)))
         (plstore-item (plstore-get plstore key)))
    (mastodon-client--remove-key-from-plstore plstore-item)))

(defun mastodon-client--make-user-details-plist ()
  "Make a plist with current user details.  Return it."
  `(:username ,(mastodon-client--form-user-from-vars)
              :instance ,mastodon-instance-url
              :client_id ,(plist-get (mastodon-client) :client_id)
              :client_secret ,(plist-get (mastodon-client) :client_secret)))

(defun mastodon-client--store-access-token (token)
  "Save TOKEN as :access_token in plstore of the current user.
Return the plist after the operation."
  (let* ((user-details (mastodon-client--make-user-details-plist))
         (plstore (plstore-open (mastodon-client--token-file)))
         (username (plist-get user-details :username))
         (plstore-value (setq user-details
                              (plist-put user-details :access_token token)))
         (print-length nil)
         (print-level nil))
    (plstore-put plstore (concat "user-" username) plstore-value nil)
    (plstore-save plstore)
    (plstore-close plstore)
    plstore-value))

(defun mastodon-client--make-user-active (user-details)
  "USER-DETAILS is a plist consisting of user details."
  (let ((plstore (plstore-open (mastodon-client--token-file)))
        (print-length nil)
        (print-level nil))
    (plstore-put plstore "active-user" user-details nil)
    (plstore-save plstore)
    (plstore-close plstore)))

(defun mastodon-client--form-user-from-vars ()
  "Create a username from user variable.  Return that username.
Username in the form user@instance.com is formed from the
variables `mastodon-instance-url' and `mastodon-active-user'."
  (concat mastodon-active-user
          "@"
          (url-host (url-generic-parse-url mastodon-instance-url))))

(defun mastodon-client--make-current-user-active ()
  "Make the user specified by user variables active user.
Return the details (plist)."
  (let ((username (mastodon-client--form-user-from-vars))
        user-plist)
    (when (setq user-plist
                (mastodon-client--general-read (concat "user-" username)))
      (mastodon-client--make-user-active user-plist))
    user-plist))

(defun mastodon-client--current-user-active-p ()
  "Return user-details if the current user is active.
Otherwise return nil."
  (let ((username (mastodon-client--form-user-from-vars))
        (user-details (mastodon-client--general-read "active-user")))
    (when (and user-details
               (equal (plist-get user-details :username) username))
      user-details)))

(defun mastodon-client--active-user ()
  "Return the details of the currently active user.
Details is a plist."
  (let ((active-user-details mastodon-client--active-user-details-plist))
    (unless active-user-details
      (setq active-user-details
            (or (mastodon-client--current-user-active-p)
                (mastodon-client--make-current-user-active)))
      (setq mastodon-client--active-user-details-plist
            active-user-details))
    active-user-details))

(defun mastodon-client ()
  "Return variable client secrets to use for `mastodon-instance-url'.
Read plist from `mastodon-client--token-file' if variable is nil.
Fetch and store plist if `mastodon-client--read' returns nil."
  (let ((client-details
         (cdr (assoc mastodon-instance-url mastodon-client--client-details-alist))))
    (unless client-details
      (setq client-details
            (or (mastodon-client--read)
                (mastodon-client--store)))
      (push (cons mastodon-instance-url client-details)
            mastodon-client--client-details-alist))
    client-details))

(provide 'mastodon-client)
;;; mastodon-client.el ends here

