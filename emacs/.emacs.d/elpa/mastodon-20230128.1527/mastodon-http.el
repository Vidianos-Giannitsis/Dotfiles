;;; mastodon-http.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2022 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <martianhiatus@riseup.net>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
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

;; mastodon-http.el provides HTTP request/response functions.

;;; Code:

(require 'json)
(require 'request) ; for attachments upload
(require 'url)

(defvar mastodon-instance-url)
(defvar mastodon-toot--media-attachment-ids)
(defvar mastodon-toot--media-attachment-filenames)

(autoload 'mastodon-auth--access-token "mastodon-auth")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")


(defvar mastodon-http--api-version "v1")

(defconst mastodon-http--timeout 15
  "HTTP request timeout, in seconds.  Has no effect on Emacs < 26.1.")

(defun mastodon-http--api (endpoint)
  "Return Mastodon API URL for ENDPOINT."
  (concat mastodon-instance-url "/api/"
          mastodon-http--api-version "/" endpoint))

(defun mastodon-http--response ()
  "Capture response buffer content as string."
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mastodon-http--response-body (pattern)
  "Return substring matching PATTERN from `mastodon-http--response'."
  (let ((resp (mastodon-http--response)))
    (string-match pattern resp)
    (match-string 0 resp)))

(defun mastodon-http--status ()
  "Return HTTP Response Status Code from `mastodon-http--response'."
  (let* ((status-line (mastodon-http--response-body "^HTTP/1.*$")))
    (string-match "[0-9][0-9][0-9]" status-line)
    (match-string 0 status-line)))

(defun mastodon-http--url-retrieve-synchronously (url &optional silent)
  "Retrieve URL asynchronously.
This is a thin abstraction over the system
`url-retrieve-synchronously'.  Depending on which version of this
is available we will call it with or without a timeout.
SILENT means don't message."
  (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
      (url-retrieve-synchronously url)
    (url-retrieve-synchronously url (or silent nil) nil mastodon-http--timeout)))

(defun mastodon-http--triage (response success)
  "Determine if RESPONSE was successful. Call SUCCESS if successful.
Message status and JSON error from RESPONSE if unsuccessful."
  (let ((status (with-current-buffer response
                  (mastodon-http--status))))
    (if (string-prefix-p "2" status)
        (funcall success)
      ;; don't switch to buffer, just with-current-buffer the response:
      ;; (switch-to-buffer response)
      ;; 404 sometimes returns http response so --process-json fails:
      (if (string-prefix-p "404" status)
          (message "Error %s: page not found" status)
        (let ((json-response (with-current-buffer response
                               (mastodon-http--process-json))))
          (message "Error %s: %s" status (alist-get 'error json-response)))))))

(defun mastodon-http--read-file-as-string (filename)
  "Read a file FILENAME as a string. Used to generate image preview."
  (with-temp-buffer
    (insert-file-contents filename)
    (string-to-unibyte (buffer-string))))

(defmacro mastodon-http--authorized-request (method body &optional unauthenticated-p)
  "Make a METHOD type request using BODY, with Mastodon authorization.
Unless UNAUTHENTICATED-P is non-nil."
  (declare (debug 'body))
  `(let ((url-request-method ,method)
         (url-request-extra-headers
          (unless ,unauthenticated-p
            (list (cons "Authorization"
                        (concat "Bearer " (mastodon-auth--access-token)))))))
     ,body))

(defun mastodon-http--build-params-string (params)
  "Build a request parameters string from parameters alist PARAMS."
  ;; (url-build-query-string args nil))
  ;; url-build-query-string adds 'nil' to empty params so lets stay with our
  ;; own:
  (mapconcat (lambda (p)
               (concat (url-hexify-string (car p))
                       "="
                       (url-hexify-string (cdr p))))
             params
             "&"))

(defun mastodon-http--build-array-params-alist (param-str array)
  "Return parameters alist using PARAM-STR and ARRAY param values.
Used for API form data parameters that take an array."
  (cl-loop for x in array
           collect (cons param-str x)))

(defun mastodon-http--post (url &optional params headers unauthenticated-p)
  "POST synchronously to URL, optionally with PARAMS and HEADERS.
Authorization header is included by default unless UNAUTHENTICATED-P is non-nil."
  (mastodon-http--authorized-request
   "POST"
   (let ((url-request-data
          (when params
            (mastodon-http--build-params-string params)))
         (url-request-extra-headers
          (append url-request-extra-headers ; auth set in macro
                  ;; pleroma compat:
                  (unless (assoc "Content-Type" headers)
                    '(("Content-Type" . "application/x-www-form-urlencoded")))
                  headers)))
     (with-temp-buffer
       (mastodon-http--url-retrieve-synchronously url)))
   unauthenticated-p))

(defun mastodon-http--get (url &optional params silent)
  "Make synchronous GET request to URL.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message."
  (mastodon-http--authorized-request
   "GET"
   ;; url-request-data doesn't seem to work with GET requests:
   (let ((url (if params
                  (concat url "?"
                          (mastodon-http--build-params-string params))
                url)))
     (mastodon-http--url-retrieve-synchronously url silent))))

(defun mastodon-http--get-response (url &optional params no-headers silent vector)
  "Make synchronous GET request to URL. Return JSON and response headers.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
NO-HEADERS means don't collect http response headers.
VECTOR means return json arrays as vectors."
  (with-current-buffer (mastodon-http--get url params silent)
    (mastodon-http--process-response no-headers vector)))

(defun mastodon-http--get-json (url &optional params silent vector)
  "Return only JSON data from URL request.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
VECTOR means return json arrays as vectors."
  (car (mastodon-http--get-response url params :no-headers silent vector)))

(defun mastodon-http--process-json ()
  "Return only JSON data from async URL request.
Callback to `mastodon-http--get-json-async', usually
`mastodon-tl--init*', is run on the result."
  (car (mastodon-http--process-response :no-headers)))

(defun mastodon-http--process-response (&optional no-headers vector)
  "Process http response.
Return a cons of JSON list and http response headers.
If NO-HEADERS is non-nil, just return the JSON.
VECTOR means return json arrays as vectors.
Callback to `mastodon-http--get-response-async', usually
`mastodon-tl--init*', is run on the result."
  ;; view raw response:
  ;; (switch-to-buffer (current-buffer))
  (let ((headers (unless no-headers
                   (mastodon-http--process-headers))))
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-array-type (if vector 'vector 'list))
          (json-string
           (decode-coding-string
            (buffer-substring-no-properties (point) (point-max))
            'utf-8)))
      (kill-buffer)
      ;; (unless (or (string-empty-p json-string) (null json-string))
      (cond ((or (string-empty-p json-string) (null json-string))
             nil)
            ;; if we don't have json, maybe we have a plain string error
            ;; message (misskey works like this for instance, but there are
            ;; probably less dunce ways to do this):
            ;; FIXME: friendica at least sends plain html if endpoint not found.
            ((not (or (string-prefix-p "\n{" json-string)
                      (string-prefix-p "\n[" json-string)))
             (error "%s" json-string))
            (t
             `(,(json-read-from-string json-string) . ,headers))))))

(defun mastodon-http--process-headers ()
  "Return an alist of http response headers."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let* ((head-str (buffer-substring-no-properties
                    (point-min)
                    (re-search-forward "^$" nil 'move)))
         (head-list (split-string head-str "\n")))
    (mapcar (lambda (x)
              (let ((list (split-string x ": ")))
                (cons (car list) (cadr list))))
            head-list)))

(defun mastodon-http--delete (url &optional params)
  "Make DELETE request to URL.
PARAMS is an alist of any extra parameters to send with the request."
  ;; url-request-data only works with POST requests?
  (let ((url
         (if params
             (concat url "?"
                     (mastodon-http--build-params-string params))
           url)))
    (mastodon-http--authorized-request
     "DELETE"
     (with-temp-buffer
       (mastodon-http--url-retrieve-synchronously url)))))

(defun mastodon-http--put (url &optional params headers)
  "Make PUT request to URL.
PARAMS is an alist of any extra parameters to send with the request.
HEADERS is an alist of any extra headers to send with the request."
  (mastodon-http--authorized-request
   "PUT"
   (let ((url-request-data
          (when params (mastodon-http--build-params-string params)))
         (url-request-extra-headers
          (append url-request-extra-headers ; auth set in macro
                  ;; pleroma compat:
                  (unless (assoc "Content-Type" headers)
                    '(("Content-Type" . "application/x-www-form-urlencoded")))
                  headers)))
     (with-temp-buffer (mastodon-http--url-retrieve-synchronously url)))))

(defun mastodon-http--append-query-string (url params)
  "Append PARAMS to URL as query strings and return it.
PARAMS should be an alist as required by `url-build-query-string'."
  (let ((query-string (url-build-query-string params)))
    (concat url "?" query-string)))

;; profile update functions

(defun mastodon-http--patch-json (url &optional params)
  "Make synchronous PATCH request to URL. Return JSON response.
Optionally specify the PARAMS to send."
  (with-current-buffer (mastodon-http--patch url params)
    (mastodon-http--process-json)))

(defun mastodon-http--patch (base-url &optional params)
  "Make synchronous PATCH request to BASE-URL.
Optionally specify the PARAMS to send."
  (mastodon-http--authorized-request
   "PATCH"
   (let ((url
          (concat base-url "?"
                  (mastodon-http--build-params-string params))))
     (mastodon-http--url-retrieve-synchronously url))))

 ;; Asynchronous functions

(defun mastodon-http--get-async (url &optional params callback &rest cbargs)
  "Make GET request to URL.
Pass response buffer to CALLBACK function with args CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (let ((url (if params
                 (concat url "?"
                         (mastodon-http--build-params-string params))
               url)))
    (mastodon-http--authorized-request
     "GET"
     (url-retrieve url callback cbargs))))

(defun mastodon-http--get-response-async (url &optional params callback &rest cbargs)
  "Make GET request to URL. Call CALLBACK with http response and CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (mastodon-http--get-async
   url
   params
   (lambda (status)
     (when status ;; only when we actually get sth?
       (apply callback (mastodon-http--process-response) cbargs)))))

(defun mastodon-http--get-json-async (url &optional params callback &rest cbargs)
  "Make GET request to URL. Call CALLBACK with json-list and CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (mastodon-http--get-async
   url
   params
   (lambda (status)
     (when status ;; only when we actually get sth?
       (apply callback (mastodon-http--process-json) cbargs)))))

(defun mastodon-http--post-async (url params headers &optional callback &rest cbargs)
  "POST asynchronously to URL with PARAMS and HEADERS.
Then run function CALLBACK with arguements CBARGS.
Authorization header is included by default unless UNAUTHENTICED-P is non-nil."
  (mastodon-http--authorized-request
   "POST"
   (let ((request-timeout 5)
         (url-request-data
          (when params
            (mastodon-http--build-params-string params))))
     (with-temp-buffer
       (url-retrieve url callback cbargs)))))

;; TODO: test for curl first?
(defun mastodon-http--post-media-attachment (url filename caption)
  "Make POST request to upload FILENAME with CAPTION to the server's media URL.
The upload is asynchronous. On succeeding,
`mastodon-toot--media-attachment-ids' is set to the id(s) of the
item uploaded, and `mastodon-toot--update-status-fields' is run."
  (let* ((file (file-name-nondirectory filename))
         (request-backend 'curl))
    (request
      url
      :type "POST"
      :params `(("description" . ,caption))
      :files `(("file" . (,file :file ,filename
                                :mime-type "multipart/form-data")))
      :parser 'json-read
      :headers `(("Authorization" . ,(concat "Bearer "
                                             (mastodon-auth--access-token))))
      :sync nil
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (push (alist-get 'id data)
                          mastodon-toot--media-attachment-ids) ; add ID to list
                    (message "%s file %s with id %S and caption '%s' uploaded!"
                             (capitalize (alist-get 'type data))
                             file
                             (alist-get 'id data)
                             (alist-get 'description data))
                    (mastodon-toot--update-status-fields))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (cond
                 ;; handle curl errors first (eg 26, can't read file/path)
                 ;; because the '=' test below fails for them
                 ;; they have the form (error . error message 24)
                 ((not (proper-list-p error-thrown)) ; not dotted list
		          (message "Got error: %s. Shit went south." (cdr error-thrown)))
                 ;; handle mastodon api errors
                 ;; they have the form (error http 401)
		         ((= (car (last error-thrown)) 401)
                  (message "Got error: %s Unauthorized: The access token is invalid" error-thrown))
                 ((= (car (last error-thrown)) 422)
                  (message "Got error: %s Unprocessable entity: file or file type is unsupported or invalid" error-thrown))
                 (t
                  (message "Got error: %s Shit went south"
                           error-thrown))))))))

(provide 'mastodon-http)
;;; mastodon-http.el ends here
