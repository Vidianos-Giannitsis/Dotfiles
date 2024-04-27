;;; mastodon-auth.el --- Auth functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2021 Abhiseck Paira <abhiseckpaira@disroot.org>
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

;; mastodon-auth.el supports authorizing and authenticating with Mastodon.

;;; Code:

(require 'plstore)
(require 'auth-source)
(require 'json)
(eval-when-compile (require 'subr-x)) ; for if-let

(autoload 'mastodon-client "mastodon-client")
(autoload 'mastodon-client--active-user "mastodon-client")
(autoload 'mastodon-client--form-user-from-vars "mastodon-client")
(autoload 'mastodon-client--make-user-active "mastodon-client")
(autoload 'mastodon-client--store-access-token "mastodon-client")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--concat-params-to-url "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-return-credential-account "mastodon")

(defvar mastodon-instance-url)
(defvar mastodon-client-scopes)
(defvar mastodon-client-redirect-uri)
(defvar mastodon-active-user)

(defgroup mastodon-auth nil
  "Authenticate with Mastodon."
  :prefix "mastodon-auth-"
  :group 'mastodon)

(defvar mastodon-auth-source-file nil
  "This variable is obsolete.
This variable currently serves no purpose and will be removed in
the future.")

(defvar mastodon-auth--token-alist nil
  "Alist of User access tokens keyed by instance url.")

(defvar mastodon-auth--acct-alist nil
  "Alist of account accts (name@domain) keyed by instance url.")

(defvar mastodon-auth--user-unaware
  "          ** MASTODON.EL - NOTICE **

It appears that you are not aware of the recent developments in
mastodon.el.  In short we now require that you also set the
variable `mastodon-active-user' in your init file in addition to
`mastodon-instance-url'.

Please see its documentation to understand what value it accepts
by running M-x describe-variable on it or visiting our web page:
https://codeberg.org/martianh/mastodon.el

We apologize for the inconvenience.
")

(defun mastodon-auth--get-browser-login-url ()
  "Return properly formed browser login url."
  (mastodon-http--concat-params-to-url
   (concat mastodon-instance-url "/oauth/authorize/")
   `(("response_type" . "code")
     ("redirect_uri" . ,mastodon-client-redirect-uri)
     ("scope" . ,mastodon-client-scopes)
     ("client_id" . ,(plist-get (mastodon-client) :client_id)))))

(defvar mastodon-auth--explanation
  (format
   "
1. A URL has been copied to your clipboard.  Open this URL in a
javascript capable browser and your browser will take you to your
Mastodon instance's login page.

2. Login to your account (%s) and authorize \"mastodon.el\".

3. After authorization you will be presented an authorization
code. Copy this code and paste it in the minibuffer prompt."
   (mastodon-client--form-user-from-vars)))

(defun mastodon-auth--show-notice (notice buffer-name &optional ask)
  "Display NOTICE to user.
NOTICE is displayed in vertical split occupying 50% of total
width.  The buffer name of the buffer being displayed in the
window is BUFFER-NAME.
When optional argument ASK is given which should be a string, use
ASK as the minibuffer prompt.  Return whatever user types in
response to the prompt.
When ASK is absent return nil."
  (let ((buffer (get-buffer-create buffer-name))
        (inhibit-read-only t)
        ask-value window)
    (set-buffer buffer)
    (erase-buffer)
    (insert notice)
    (fill-region (point-min) (point-max))
    (read-only-mode)
    (setq window (select-window
                  (split-window (frame-root-window) nil 'left)
                  t))
    (switch-to-buffer buffer t)
    (when ask
      (setq ask-value (read-string ask))
      (kill-buffer buffer)
      (delete-window window))
    ask-value))

(defun mastodon-auth--request-authorization-code ()
  "Ask authorization code and return it."
  (let ((url (mastodon-auth--get-browser-login-url))
        (select-enable-clipboard t)
        authorization-code)
    (kill-new url)
    (message "%s" url)
    (setq authorization-code
          (mastodon-auth--show-notice mastodon-auth--explanation
                                      "*mastodon-notice*"
                                      "Authorization Code: "))
    authorization-code))

(defun mastodon-auth--generate-token ()
  "Generate access_token for the user.  Return response buffer."
  (let ((authorization-code (mastodon-auth--request-authorization-code)))
    (mastodon-http--post
     (concat mastodon-instance-url "/oauth/token")
     `(("grant_type" . "authorization_code")
       ("client_secret" . ,(plist-get (mastodon-client) :client_secret))
       ("client_id" . ,(plist-get (mastodon-client) :client_id))
       ("code" . ,authorization-code)
       ("redirect_uri" . ,mastodon-client-redirect-uri))
     nil
     :unauthenticated)))

(defun mastodon-auth--get-token ()
  "Make a request to generate an auth token and return JSON response."
  (with-current-buffer (mastodon-auth--generate-token)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'vector)
          (json-string (buffer-substring-no-properties (point) (point-max))))
      (json-read-from-string json-string))))

(defun mastodon-auth--access-token ()
  "Return the access token to use with `mastodon-instance-url'.
Generate/save token if none known yet."
  (cond (mastodon-auth--token-alist
         ;; user variables are known and initialised.
         (alist-get mastodon-instance-url mastodon-auth--token-alist))
        ((plist-get (mastodon-client--active-user) :access_token)
         ;; user variables need to be read from plstore.
         (push (cons mastodon-instance-url
                     (plist-get (mastodon-client--active-user) :access_token))
               mastodon-auth--token-alist)
         (alist-get mastodon-instance-url mastodon-auth--token-alist))
        ((null mastodon-active-user)
         ;; user not aware of 2FA-related changes and has not set
         ;; `mastodon-active-user'. Make user aware and error out.
         (mastodon-auth--show-notice mastodon-auth--user-unaware
                                     "*mastodon-notice*")
         (error "Variables not set properly"))
        (t
         ;; user access-token needs to fetched from the server and
         ;; stored and variables initialised.
         (mastodon-auth--handle-token-response (mastodon-auth--get-token)))))

(defun mastodon-auth--handle-token-response (response)
  "Add token RESPONSE to `mastodon-auth--token-alist'.
The token is returned by `mastodon-auth--get-token'.
Handle any errors from the server."
  (pcase response
    ((and (let token (plist-get response :access_token))
          (guard token))
     (mastodon-client--make-user-active
      (mastodon-client--store-access-token token))
     (cdar (push (cons mastodon-instance-url token)
                 mastodon-auth--token-alist)))
    (`(:error ,class :error_description ,error)
     (error "Mastodon-auth--access-token: %s: %s" class error))
    (_ (error "Unknown response from mastodon-auth--get-token!"))))

(defun mastodon-auth--get-account-name ()
  "Request user credentials and return an account name."
  (alist-get 'acct
             (mastodon-return-credential-account)))

(defun mastodon-auth--get-account-id ()
  "Request user credentials and return an account name."
  (alist-get 'id
             (mastodon-return-credential-account)))

(defun mastodon-auth--user-acct ()
  "Return a mastodon user acct name."
  (or (cdr (assoc mastodon-instance-url mastodon-auth--acct-alist))
      (let ((acct (mastodon-auth--get-account-name)))
        (push (cons mastodon-instance-url acct) mastodon-auth--acct-alist)
        acct)))

(provide 'mastodon-auth)
;;; mastodon-auth.el ends here
