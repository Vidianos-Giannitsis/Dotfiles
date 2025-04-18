;;; org-node-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from org-node.el

(defvar org-node-cache-mode nil "\
Non-nil if Org-Node-Cache mode is enabled.
See the `org-node-cache-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-node-cache-mode'.")
(custom-autoload 'org-node-cache-mode "org-node" nil)
(autoload 'org-node-cache-mode "org-node" "\
Instruct various hooks to keep the cache updated.

-----

This is a global minor mode.  If called interactively, toggle the
`Org-Node-Cache mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='org-node-cache-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'org-node-reset "org-node" "\
Wipe and rebuild the cache." t)
(autoload 'org-node-find "org-node" "\
Select and visit one of your ID nodes.

To behave like `org-roam-node-find' when creating new nodes, set
`org-node-creation-fn' to `org-node-fakeroam-new-via-roam-capture'." t)
(autoload 'org-node-visit-random "org-node" "\
Visit a random node." t)
(autoload 'org-node-insert-link "org-node" "\
Insert a link to one of your ID nodes.

To behave exactly like org-roam\\='s `org-roam-node-insert',
see `org-node-insert-link*' and its docstring.

Optional argument REGION-AS-INITIAL-INPUT t means behave as
`org-node-insert-link*'.

Argument IMMEDIATE means behave as
`org-node-insert-link-novisit'.

(fn &optional REGION-AS-INITIAL-INPUT IMMEDIATE)" '(org-mode))
(autoload 'org-node-insert-link* "org-node" "\
Insert a link to one of your ID nodes.

Unlike `org-node-insert-link', emulate `org-roam-node-insert' by
always copying any active region as initial input.

That behavior can be convenient if you often want to use the
selected region as a new node title, or you already know it
matches a node title.

On the other hand if you always find yourself erasing the
minibuffer before selecting some other node you had in mind, to
which the region should be linkified, you\\='ll prefer
`org-node-insert-link'.

The commands are the same, just differing in initial input." '(org-mode))
(autoload 'org-node-insert-link-novisit "org-node" "\
Insert a link to one of your ID nodes without ever visiting it.

Normally, if the node does not exist, `org-node-insert-link' would
create it and then visit it.  This will not visit it." '(org-mode))
(autoload 'org-node-insert-link-novisit* "org-node" "\
Insert a link to one of your ID nodes without ever visiting it.

Normally, if the node does not exist, `org-node-insert-link*' would
create it and then visit it.  This will not visit it." '(org-mode))
(autoload 'org-node-insert-transclusion "org-node" "\
Insert a #+transclude: referring to NODE, prompting if needed.

(fn &optional NODE)" '(org-mode))
(autoload 'org-node-insert-transclusion-as-subtree "org-node" "\
Insert a link and a transclusion.
Prompt for NODE if needed.

Result will basically look like:

** [[Note]]
#+transclude: [[Note]] :level 3

but adapt to the surrounding outline level.  I recommend
adding keywords to the things to exclude:

(setq org-transclusion-exclude-elements
      \\='(property-drawer comment keyword))

(fn &optional NODE)" '(org-mode))
(autoload 'org-node-refile "org-node" "\
Experimental." '(org-mode))
(autoload 'org-node-extract-subtree "org-node" "\
Extract subtree at point into a file of its own.
Leave a link in the source file, and show the newly created file
as current buffer.

You may find it a common situation that the subtree had not yet
been assigned an ID nor any other property that you normally
assign to your nodes.  Thus, this creates an ID if there was
no ID, copies over all inherited tags (making them explicit),
and runs `org-node-creation-hook'.

Adding to that, see below for an example advice that copies any
inherited \"CREATED\" property, if an ancestor had such a
property.  It is subjective whether you\\='d want this behavior,
but it can be desirable if you know the subtree had been part of
the source file for ages so that you regard the ancestor\\='s
creation-date as more \"truthful\" than today\\='s date.

(advice-add \\='org-node-extract-subtree :around
            (defun my-inherit-creation-date (orig-fn &rest args)
                   (let ((parent-creation
                          (org-entry-get-with-inheritance \"CREATED\")))
                     (apply orig-fn args)
                     ;; Now in the new buffer
                     (org-entry-put nil \"CREATED\"
                                    (or parent-creation
                                        (format-time-string
                                         (org-time-stamp-format t t)))))))" '(org-mode))
(autoload 'org-node-rename-file-by-title "org-node" "\
Rename the current file according to `org-node-slug-fn'.

Also attempt to check for a prefix in the style of
`org-node-datestamp-format', and avoid overwriting it.

Suitable at the end of `after-save-hook'.  If called from a hook
(or from Lisp in general), only operate on files in
`org-node-renames-allowed-dirs'.  When called interactively as a
command, always prompt for confirmation.

Argument INTERACTIVE automatically set.

(fn &optional INTERACTIVE)" '(org-mode))
(autoload 'org-node-rewrite-links-ask "org-node" "\
Update desynced link descriptions, interactively.

Search all files, or just FILES if non-nil, for ID-links where
the link description has gotten out of sync from the
destination\\='s current title.

At each link, prompt for user consent, then auto-update the link
so it matches the destination\\='s current title.

(fn &optional FILES)" t)
(autoload 'org-node-rename-asset-and-rewrite-links "org-node" "\
Helper for renaming images and all links that point to them.

Prompt for an asset such as an image file to be renamed, then search
recursively for Org files containing a link to that asset, open a wgrep
buffer of the search hits, and start an interactive search-replace that
updates the links.  After the user consents or doesn\\='t consent to
replacing all the links, finally rename the asset file itself.  If the
user quits, do not apply any modifications." t)
(autoload 'org-node-insert-heading "org-node" "\
Insert a heading with ID and run `org-node-creation-hook'." '(org-mode))
(autoload 'org-node-nodeify-entry "org-node" "\
Add an ID to entry at point and run `org-node-creation-hook'." '(org-mode))
(autoload 'org-node-put-created "org-node" "\
Add a CREATED property to entry at point, if none already." '(org-mode))
(autoload 'org-node-forget-dir "org-node" "\
Remove references in `org-id-locations' to files in DIR.

Note that if DIR can be found under `org-node-extra-id-dirs',
this action may make no practical impact unless you also add DIR
to `org-node-extra-id-dirs-exclude'.

In case of unsolvable problems, how to wipe org-id-locations:

(progn
 (delete-file org-id-locations-file)
 (setq org-id-locations nil)
 (setq org-id--locations-checksum nil)
 (setq org-agenda-text-search-extra-files nil)
 (setq org-id-files nil)
 (setq org-id-extra-files nil))

(fn DIR)" t)
(autoload 'org-node-grep "org-node" "\
Grep across all files known to org-node." t)
(put 'org-node-complete-at-point-mode 'globalized-minor-mode t)
(defvar org-node-complete-at-point-mode nil "\
Non-nil if Org-Node-Complete-At-Point mode is enabled.
See the `org-node-complete-at-point-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-node-complete-at-point-mode'.")
(custom-autoload 'org-node-complete-at-point-mode "org-node" nil)
(autoload 'org-node-complete-at-point-mode "org-node" "\
Toggle Org-Node-Complete-At-Point-Local mode in all buffers.
With prefix ARG, enable Org-Node-Complete-At-Point mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Org-Node-Complete-At-Point-Local mode is enabled in all buffers where
`org-node-complete-at-point--try-enable' would do it.

See `org-node-complete-at-point-local-mode' for more information on
Org-Node-Complete-At-Point-Local mode.

(fn &optional ARG)" t)
(register-definition-prefixes "org-node" '("org-"))


;;; Generated autoloads from org-node-backlink.el

(defvar org-node-backlink-mode nil "\
Non-nil if Org-Node-Backlink mode is enabled.
See the `org-node-backlink-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-node-backlink-mode'.")
(custom-autoload 'org-node-backlink-mode "org-node-backlink" nil)
(autoload 'org-node-backlink-mode "org-node-backlink" "\
Keep :BACKLINKS: properties updated.

See Info node `(org-node)'.

-----

This is a global minor mode.  If called interactively, toggle the
`Org-Node-Backlink mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='org-node-backlink-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "org-node-backlink" '("org-node-backlink-"))


;;; Generated autoloads from org-node-changes.el

(register-definition-prefixes "org-node-changes" '("org-node-changes--"))


;;; Generated autoloads from org-node-context.el

(autoload 'org-node-context-follow-local-mode "org-node-context" "\
Update the context buffer when point moves in an Org buffer.

-----

This is a minor mode.  If called interactively, toggle the
`Org-Node-Context-Follow-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable the
mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `org-node-context-follow-local-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(put 'org-node-context-follow-mode 'globalized-minor-mode t)
(defvar org-node-context-follow-mode nil "\
Non-nil if Org-Node-Context-Follow mode is enabled.
See the `org-node-context-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-node-context-follow-mode'.")
(custom-autoload 'org-node-context-follow-mode "org-node-context" nil)
(autoload 'org-node-context-follow-mode "org-node-context" "\
Toggle Org-Node-Context-Follow-Local mode in all buffers.
With prefix ARG, enable Org-Node-Context-Follow mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Org-Node-Context-Follow-Local mode is enabled in all buffers where
`org-node-context-follow-local-mode' would do it.

See `org-node-context-follow-local-mode' for more information on
Org-Node-Context-Follow-Local mode.

(fn &optional ARG)" t)
(autoload 'org-node-context-raise "org-node-context" "\
Either display a context buffer or refresh an already visible one.

To reiterate: if it was not visible, only bring it up for
display, do NOT also refresh it.  Leave that for the second time
the user invokes the command." t)
(autoload 'org-node-context-toggle "org-node-context" "\
Show the main context buffer, or hide it if already showing." t)
(register-definition-prefixes "org-node-context" '("org-node-"))


;;; Generated autoloads from org-node-parser.el

(register-definition-prefixes "org-node-parser" '("org-node-parser--"))


;;; Generated autoloads from org-node-seq.el

(autoload 'org-node-seq-def-on-any-sort-by-property "org-node-seq" "\
Define a sequence sorted by property PROP.
If an ID-node does not have property PROP, it is excluded.

For KEY, NAME and CAPTURE, see `org-node-seq-defs'.

(fn KEY NAME PROP &optional CAPTURE)")
(autoload 'org-node-seq-def-on-tags-sort-by-property "org-node-seq" "\
Define a sequence filtered by TAGS sorted by property PROP.
If a node does not have property PROP, it is excluded.
TAGS is a string of tags separated by colons.

Tag inheritance does not apply; a node must have one or more of TAGS
itself, even if a parent in the outline tree also has them.

For KEY, NAME and CAPTURE, see `org-node-seq-defs'.

(fn KEY NAME TAGS PROP &optional CAPTURE)")
(autoload 'org-node-seq-def-on-filepath-sort-by-basename "org-node-seq" "\
Define a sequence as all files under directory DIR.
The files need not contain a top-level property drawer, but
they do need to contain at least one ID-node.

For KEY, NAME and CAPTURE, see `org-node-seq-defs'.

When optional argument DATE-PICKER is non-nil, let the prompter use the
interactive Org date picker.

(Tip: No idea how to use the Org date picker?  See `org-read-date'!)
(Tip: If you never make notes for the future, you might prefer setting
       `org-read-date-prefer-future' to nil.)

For the date-picker to work as expected, you need file names in
YYYY-MM-DD format, e.g. \"2024-01-31.org\".

(fn KEY NAME DIR &optional CAPTURE DATE-PICKER)")
(autoload 'org-node-seq--guess-daily-dir "org-node-seq" "\
Do not rely on this.
Better insert a hardcoded string in your seq def,
instead of calling this function.")
(autoload 'org-node-seq-try-goto-id "org-node-seq" "\
Try to visit org-id ID and return non-nil, else nil on fail.

(fn ID)")
(autoload 'org-node-seq-try-visit-file "org-node-seq" "\
If FILE exists or a buffer has it as filename, visit that.
On success, return non-nil; else nil.  Never create FILE anew.

(fn FILE)")
(autoload 'org-node-seq-filename->ymd "org-node-seq" "\
Check the filename PATH for a date, and return that date.
On failing to coerce a date, return nil.

Only works for names starting with either an YYYY-MM-DD date, or a
datestamp matching the style of `org-node-datestamp-format'.

The latter uses a sloppy algorithm so not all formats work, see
`org-node-seq-extract-ymd'.

(fn PATH)")
(autoload 'org-node-seq-extract-ymd "org-node-seq" "\
Try to extract a YYYY-MM-DD date out of string INSTANCE.
Assume INSTANCE is a string produced by TIME-FORMAT, e.g. if
TIME-FORMAT is %Y%m%dT%H%M%SZ then a possible INSTANCE is
20240814T123307Z.  In that case, return 2024-08-14.

Will throw an error if TIME-FORMAT does not include either %F or
all three of %Y, %m and %d.  May return odd results if other
format-constructs occur before these.

(fn INSTANCE TIME-FORMAT)")
(defvar org-node-seqs nil "\
Alist of data for each node sequence.")
 (autoload 'org-node-seq-dispatch "org-node-seq" nil t)
(register-definition-prefixes "org-node-seq" '("org-node-seq-"))

;;; End of scraped data

(provide 'org-node-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; org-node-autoloads.el ends here
