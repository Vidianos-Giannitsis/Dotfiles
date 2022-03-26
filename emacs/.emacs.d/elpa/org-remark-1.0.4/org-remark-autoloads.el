;;; org-remark-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-remark" "org-remark.el" (0 0 0 0))
;;; Generated autoloads from org-remark.el

(autoload 'org-remark-mode "org-remark" "\
Highlight and annotate any text file with using Org mode.
This is a local minor-mode.

If called interactively, enable Org-Remark mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

On activation, it loads your saved highlights from the notes file
and enables automatic saving of highlights thereafter.

The automatic saving is achieved via function
`org-remark-save' added to `after-save-hook'.

On deactivation, it removes all the overlays and stops tracking
the highlights in this buffer by setting variable
`org-remark-highlights' to nil.  Be careful of behavior, if
you still wish to retain the locations of highlights.

It is recommended to use `org-remark-toggle' if you wish to
temporarily hide highlights in the current buffer.  It keeps
`org-remark-highlights' unchanged.

While the tracking of highlights is stopped,
editing the buffer will likely result in mismatch between the
saved highlights' locations and the current buffer's text
content.

Highlights tracked by variable `org-remark-highlights' cannot
persist when you kill the buffer or quit Emacs.  When you
re-launch Emacs and visit the same file, ensure to turn on
`org-remark-mode' to load the highlights from the marginalia
file.  `org-remark-global-tracking-mode' automates this.  It is
recommended to turn it on as part of Emacs initialization.

\\{org-remark-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'org-remark-mark "org-remark" "\
Apply face `org-remark-highlighter' to the region between BEG and END.

When this function is used interactively, it will generate a new
ID, always assuming it is working on a new highlighted text
region.

A Org headline entry for the highlight will be created in the
marginal notes file specified by
`org-remark-notes-get-file-name'.  If the file does not exist
yet, it will be created.

When this function is called from Elisp, ID can be
optionally passed, indicating to Org-remark that it is to load an
existing highlight.  In this case, no new ID gets generated and
the highlight saved again, avoiding the unnecessary round-trip
back to the database.

MODE is also an argument which can be passed from Elisp.  It
determines whether or not highlight is to be saved in the
marginal notes file.  The expected values are nil, :load and
:change.

\(fn BEG END &optional ID MODE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-remark" '("org-remark-")))

;;;***

;;;### (autoloads nil "org-remark-convert-legacy" "org-remark-convert-legacy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-remark-convert-legacy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-remark-convert-legacy" '("org-remark-convert-legacy-data")))

;;;***

;;;### (autoloads nil "org-remark-global-tracking" "org-remark-global-tracking.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-remark-global-tracking.el

(defvar org-remark-global-tracking-mode nil "\
Non-nil if Org-Remark-Global-Tracking mode is enabled.
See the `org-remark-global-tracking-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-remark-global-tracking-mode'.")

(custom-autoload 'org-remark-global-tracking-mode "org-remark-global-tracking" nil)

(autoload 'org-remark-global-tracking-mode "org-remark-global-tracking" "\
Automatically activates local minor mode `org-remark-mode'.
When this global minor mode is active, a function added to
`find-file-hook' will look for a marginal notes file for the file
as defined by `org-remark-notes-file-path'.  If it is found and
readable, the function automatically activates `org-remark'.

If called interactively, enable Org-Remark-Global-Tracking mode
if ARG is positive, and disable it if ARG is zero or negative.
If called from Lisp, also enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'; disable the mode
otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-remark-global-tracking" '("org-remark-")))

;;;***

;;;### (autoloads nil nil ("org-remark-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-remark-autoloads.el ends here
