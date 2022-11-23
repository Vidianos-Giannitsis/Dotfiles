;;; mastodon-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mastodon" "mastodon.el" (0 0 0 0))
;;; Generated autoloads from mastodon.el

(autoload 'mastodon "mastodon" "\
Connect Mastodon client to `mastodon-instance-url' instance." t nil)

(autoload 'mastodon-toot "mastodon" "\
Update instance with new toot. Content is captured in a new buffer.
If USER is non-nil, insert after @ symbol to begin new toot.
If REPLY-TO-ID is non-nil, attach new toot to a conversation.
If REPLY-JSON is the json of the toot being replied to.

\(fn &optional USER REPLY-TO-ID REPLY-JSON)" t nil)

(autoload 'mastodon-url-lookup "mastodon" "\
If a URL resembles a mastodon link, try to load in `mastodon.el'.
Does a WebFinger lookup.
URL can be arg QUERY-URL, or URL at point, or provided by the user.
If a status or account is found, load it in `mastodon.el', if
not, just browse the URL in the normal fashion.

\(fn &optional QUERY-URL)" t nil)

(add-hook 'mastodon-mode-hook (lambda nil (when (require 'emojify nil :noerror) (emojify-mode t) (when mastodon-toot--enable-custom-instance-emoji (mastodon-toot--enable-custom-emoji)))))

(add-hook 'mastodon-mode-hook #'mastodon-profile--fetch-server-account-settings)

(register-definition-prefixes "mastodon" '("mastodon-"))

;;;***

;;;### (autoloads nil "mastodon-async" "mastodon-async.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mastodon-async.el

(autoload 'mastodon-async-mode "mastodon-async" "\
Async Mastodon.

This is a minor mode.  If called interactively, toggle the
`Mastodon-Async mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `mastodon-async-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "mastodon-async" '("mastodon-async--"))

;;;***

;;;### (autoloads nil "mastodon-auth" "mastodon-auth.el" (0 0 0 0))
;;; Generated autoloads from mastodon-auth.el

(register-definition-prefixes "mastodon-auth" '("mastodon-auth-"))

;;;***

;;;### (autoloads nil "mastodon-client" "mastodon-client.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from mastodon-client.el

(register-definition-prefixes "mastodon-client" '("mastodon-client"))

;;;***

;;;### (autoloads nil "mastodon-discover" "mastodon-discover.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from mastodon-discover.el

(register-definition-prefixes "mastodon-discover" '("mastodon-discover"))

;;;***

;;;### (autoloads nil "mastodon-http" "mastodon-http.el" (0 0 0 0))
;;; Generated autoloads from mastodon-http.el

(register-definition-prefixes "mastodon-http" '("mastodon-http--"))

;;;***

;;;### (autoloads nil "mastodon-inspect" "mastodon-inspect.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from mastodon-inspect.el

(register-definition-prefixes "mastodon-inspect" '("mastodon-inspect--"))

;;;***

;;;### (autoloads nil "mastodon-media" "mastodon-media.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mastodon-media.el

(register-definition-prefixes "mastodon-media" '("mastodon-media--"))

;;;***

;;;### (autoloads nil "mastodon-notifications" "mastodon-notifications.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from mastodon-notifications.el

(register-definition-prefixes "mastodon-notifications" '("mastodon-notifications--"))

;;;***

;;;### (autoloads nil "mastodon-profile" "mastodon-profile.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from mastodon-profile.el

(register-definition-prefixes "mastodon-profile" '("mastodon-profile-"))

;;;***

;;;### (autoloads nil "mastodon-search" "mastodon-search.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from mastodon-search.el

(register-definition-prefixes "mastodon-search" '("mastodon-search--"))

;;;***

;;;### (autoloads nil "mastodon-tl" "mastodon-tl.el" (0 0 0 0))
;;; Generated autoloads from mastodon-tl.el

(register-definition-prefixes "mastodon-tl" '("mastodon-tl--"))

;;;***

;;;### (autoloads nil "mastodon-toot" "mastodon-toot.el" (0 0 0 0))
;;; Generated autoloads from mastodon-toot.el

(add-hook 'mastodon-toot-mode-hook #'mastodon-profile--fetch-server-account-settings-maybe)

(register-definition-prefixes "mastodon-toot" '("mastodon-toot-"))

;;;***

;;;### (autoloads nil nil ("mastodon-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mastodon-autoloads.el ends here
