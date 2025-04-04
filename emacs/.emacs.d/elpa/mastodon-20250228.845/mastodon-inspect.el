;;; mastodon-inspect.el --- Client for Mastodon  -*- lexical-binding: t -*-

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

;; Some tools to help inspect / debug mastodon.el

;;; Code:
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--get-search-json "mastodon-http")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-mode "mastodon")
(autoload 'mastodon-tl--as-string "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--toot "mastodon-tl")

(defvar mastodon-instance-url)

(defgroup mastodon-inspect nil
  "Tools to help inspect toots."
  :prefix "mastodon-inspect-"
  :group 'external)

(defun mastodon-inspect--dump-json-in-buffer (name json)
  "Buffer NAME is opened and JSON in printed into it."
  (switch-to-buffer-other-window name)
  (erase-buffer)
  (let ((print-level nil)
        (print-length nil))
    (insert (pp json t)))
  (goto-char (point-min))
  (emacs-lisp-mode)
  (message "success"))

(defun mastodon-inspect--toot ()
  "Find next toot and dump its meta data into new buffer."
  (interactive)
  (mastodon-inspect--dump-json-in-buffer
   (concat "*mastodon-inspect-toot-"
           (mastodon-tl--as-string (mastodon-tl--property 'item-id))
           "*")
   (mastodon-tl--property 'item-json)))

(defun mastodon-inspect--download-single-toot (item-id)
  "Download the toot/status represented by ITEM-ID."
  (mastodon-http--get-json
   (mastodon-http--api (concat "statuses/" item-id))))

(defun mastodon-inspect--view-single-toot (item-id)
  "View the toot/status represented by ITEM-ID."
  (interactive "s Toot ID: ")
  (let ((buffer (get-buffer-create (concat "*mastodon-status-" item-id "*"))))
    (with-current-buffer buffer
      (let ((toot (mastodon-inspect--download-single-toot item-id )))
        (mastodon-tl--toot toot)
        (goto-char (point-min))
        (while (search-forward "\n\n\n | " nil t)
          (replace-match "\n | "))
        (mastodon-media--inline-images (point-min) (point-max))))
    (switch-to-buffer-other-window buffer)
    (mastodon-mode)))

(defun mastodon-inspect--view-single-toot-source (item-id)
  "View the ess source of a toot/status represented by ITEM-ID."
  (interactive "s Toot ID: ")
  (mastodon-inspect--dump-json-in-buffer
   (concat "*mastodon-status-raw-" item-id "*")
   (mastodon-inspect--download-single-toot item-id)))


(defvar mastodon-inspect--search-query-accounts-result)
(defvar mastodon-inspect--single-account-json)

(defvar mastodon-inspect--search-query-full-result)
(defvar mastodon-inspect--search-result-tags)

(defun mastodon-inspect--get-search-result (query)
  "Inspect function for a search result for QUERY."
  (interactive)
  (setq mastodon-inspect--search-query-full-result
        (append ; convert vector to list
         (mastodon-http--get-search-json
         (format "%s/api/v2/search" mastodon-instance-url)
         query)
         nil))
  (setq mastodon-inspect--search-result-tags
        (append (cdr
                 (caddr mastodon-inspect--search-query-full-result))
                nil)))

(defun mastodon-inspect--get-search-account (query)
  "Return JSON for a single account after search QUERY."
  (interactive)
  (setq mastodon-inspect--search-query-accounts-result
        (append ; convert vector to list
         (mastodon-http--get-search-json
         (format "%s/api/v1/accounts/search" mastodon-instance-url)
         query)
         nil))
  (setq mastodon-inspect--single-account-json
      (car mastodon-inspect--search-query-accounts-result)))


(provide 'mastodon-inspect)
;;; mastodon-inspect.el ends here
