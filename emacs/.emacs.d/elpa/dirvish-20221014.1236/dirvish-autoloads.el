;;; dirvish-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dirvish" "dirvish.el" (0 0 0 0))
;;; Generated autoloads from dirvish.el

(defvar dirvish-override-dired-mode nil "\
Non-nil if Dirvish-Override-Dired mode is enabled.
See the `dirvish-override-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dirvish-override-dired-mode'.")

(custom-autoload 'dirvish-override-dired-mode "dirvish" nil)

(autoload 'dirvish-override-dired-mode "dirvish" "\
Let Dirvish take over Dired globally.

This is a minor mode.  If called interactively, toggle the
`Dirvish-Override-Dired mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dirvish-override-dired-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'dirvish "dirvish" "\
Start a full frame Dirvish session with optional PATH.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `default-directory'.

\(fn &optional PATH)" t nil)

(autoload 'dirvish-dwim "dirvish" "\
Start a fullframe session only when `one-window-p'.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `default-directory'.
If `one-window-p' returns nil, open PATH using regular Dired.

\(fn &optional PATH)" t nil)

(register-definition-prefixes "dirvish" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-collapse" "dirvish-collapse.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from dirvish-collapse.el

(register-definition-prefixes "dirvish-collapse" '("dirvish-collapse--cache"))

;;;***

;;;### (autoloads nil "dirvish-emerge" "dirvish-emerge.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dirvish-emerge.el

(autoload 'dirvish-emerge-menu "dirvish-emerge" "\
Manage pinned files in Dirvish." t nil)

(autoload 'dirvish-emerge-mode "dirvish-emerge" "\
Toggle grouping of files in Dirvish.

This is a minor mode.  If called interactively, toggle the
`Dirvish-Emerge mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dirvish-emerge-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dirvish-emerge" '("dirvish-emerge-"))

;;;***

;;;### (autoloads nil "dirvish-extras" "dirvish-extras.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dirvish-extras.el
 (autoload 'dirvish-setup-menu "dirvish-extras" nil t)

(autoload 'dirvish-copy-file-name "dirvish-extras" "\
Copy filename of marked files.
If MULTI-LINE, make every name occupy a new line.

\(fn &optional MULTI-LINE)" t nil)

(autoload 'dirvish-copy-file-path "dirvish-extras" "\
Copy filepath of marked files.
If MULTI-LINE, make every path occupy a new line.

\(fn &optional MULTI-LINE)" t nil)

(autoload 'dirvish-total-file-size "dirvish-extras" "\
Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'.

\(fn &optional FILESET)" t nil)

(autoload 'dirvish-layout-toggle "dirvish-extras" "\
Toggle layout of current Dirvish session.
A session with layout means it has a companion preview window and
possibly one or more parent windows." t nil)

(autoload 'dirvish-layout-switch "dirvish-extras" "\
Switch Dirvish layout according to RECIPE.
If RECIPE is not provided, switch to the recipe next to the
current layout defined in `dirvish-layout-recipes'.

\(fn &optional RECIPE)" t nil)
 (autoload 'dirvish-file-info-menu "dirvish-extras" nil t)
 (autoload 'dirvish-chxxx-menu "dirvish-extras" nil t)
 (autoload 'dirvish-mark-menu "dirvish-extras" nil t)
 (autoload 'dirvish-dired-cheatsheet "dirvish-extras" nil t)

(register-definition-prefixes "dirvish-extras" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-fd" "dirvish-fd.el" (0 0 0 0))
;;; Generated autoloads from dirvish-fd.el
 (autoload 'dirvish-fd-switches-menu "dirvish-fd" nil t)

(autoload 'dirvish-fd-jump "dirvish-fd" "\
Browse directories using `fd' command.
This command takes a while to index all the directories the first
time you run it.  After the indexing, it fires up instantly.

If called with \\`C-u' or if CURRENT-DIR-P holds the value 4,
search for directories in the current directory.  Otherwise,
search for directories in `dirvish-fd-default-dir'.

If prefixed twice with \\`C-u' or if CURRENT-DIR-P holds the
value 16, let the user choose the root directory of their search.

\(fn &optional CURRENT-DIR-P)" t nil)

(autoload 'dirvish-fd "dirvish-fd" "\
Run `fd' on DIR and go into Dired mode on a buffer of the output.
The command run is essentially:

  fd --color=never -0 `dirvish-fd-switches' PATTERN
     --exec-batch `dirvish-fd-ls-program' `dired-listing-switches' --directory.

\(fn DIR PATTERN)" t nil)

(register-definition-prefixes "dirvish-fd" '("dirvish-fd-"))

;;;***

;;;### (autoloads nil "dirvish-history" "dirvish-history.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from dirvish-history.el

(autoload 'dirvish-history-jump "dirvish-history" "\
Open a target directory from `dirvish--history'." t nil)

(autoload 'dirvish-history-last "dirvish-history" "\
Switch to the most recently visited dirvish buffer." t nil)

(autoload 'dirvish-history-go-forward "dirvish-history" "\
Navigate to next ARG directory in history.
ARG defaults to 1.

\(fn ARG)" t nil)

(autoload 'dirvish-history-go-backward "dirvish-history" "\
Navigate to previous ARG directory in history.
ARG defaults to 1.

\(fn ARG)" t nil)
 (autoload 'dirvish-history-menu "dirvish-history" nil t)

;;;***

;;;### (autoloads nil "dirvish-icons" "dirvish-icons.el" (0 0 0 0))
;;; Generated autoloads from dirvish-icons.el

(register-definition-prefixes "dirvish-icons" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-ls" "dirvish-ls.el" (0 0 0 0))
;;; Generated autoloads from dirvish-ls.el
 (autoload 'dirvish-quicksort "dirvish-ls" nil t)
 (autoload 'dirvish-ls-switches-menu "dirvish-ls" nil t)

(register-definition-prefixes "dirvish-ls" '("dirvish-ls-"))

;;;***

;;;### (autoloads nil "dirvish-narrow" "dirvish-narrow.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dirvish-narrow.el

(autoload 'dirvish-narrow "dirvish-narrow" "\
Narrow a Dirvish buffer to the files matching a regex." t nil)

(register-definition-prefixes "dirvish-narrow" '("dirvish-narrow-"))

;;;***

;;;### (autoloads nil "dirvish-peek" "dirvish-peek.el" (0 0 0 0))
;;; Generated autoloads from dirvish-peek.el

(defvar dirvish-peek-mode nil "\
Non-nil if Dirvish-Peek mode is enabled.
See the `dirvish-peek-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dirvish-peek-mode'.")

(custom-autoload 'dirvish-peek-mode "dirvish-peek" nil)

(autoload 'dirvish-peek-mode "dirvish-peek" "\
Show file preview when narrowing candidates using minibuffer.

This is a minor mode.  If called interactively, toggle the
`Dirvish-Peek mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dirvish-peek-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dirvish-peek" '("dirvish-peek-"))

;;;***

;;;### (autoloads nil "dirvish-quick-access" "dirvish-quick-access.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from dirvish-quick-access.el
 (autoload 'dirvish-quick-access "dirvish-quick-access" nil t)

(register-definition-prefixes "dirvish-quick-access" '("dirvish-quick-access-"))

;;;***

;;;### (autoloads nil "dirvish-side" "dirvish-side.el" (0 0 0 0))
;;; Generated autoloads from dirvish-side.el

(defvar dirvish-side-follow-mode nil "\
Non-nil if Dirvish-Side-Follow mode is enabled.
See the `dirvish-side-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dirvish-side-follow-mode'.")

(custom-autoload 'dirvish-side-follow-mode "dirvish-side" nil)

(autoload 'dirvish-side-follow-mode "dirvish-side" "\
Toggle `dirvish-side-follow-mode'.
When enabled the visible side session will select the current
buffer's filename.  It will also visits the latest `project-root'
after switching to a new project.

This is a minor mode.  If called interactively, toggle the
`Dirvish-Side-Follow mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dirvish-side-follow-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'dirvish-side "dirvish-side" "\
Toggle a Dirvish session at the side window.

- If the current window is a side session window, hide it.
- If a side session is visible, select it.
- If a side session exists but is not visible, show it.
- If there is no side session exists,create a new one with PATH.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'.

\(fn &optional PATH)" t nil)

(register-definition-prefixes "dirvish-side" '("dirvish-side-"))

;;;***

;;;### (autoloads nil "dirvish-subtree" "dirvish-subtree.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from dirvish-subtree.el

(autoload 'dirvish-subtree-up "dirvish-subtree" "\
Jump to beginning of current subtree." t nil)

(autoload 'dirvish-subtree-remove "dirvish-subtree" "\
Remove subtree at point." t nil)

(autoload 'dirvish-subtree-clear "dirvish-subtree" "\
Clear all subtrees in the buffer." t nil)

(autoload 'dirvish-subtree-toggle "dirvish-subtree" "\
Insert subtree at point or remove it if it was not present." t nil)
 (autoload 'dirvish-subtree-menu "dirvish-subtree" nil t)

(register-definition-prefixes "dirvish-subtree" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-vc" "dirvish-vc.el" (0 0 0 0))
;;; Generated autoloads from dirvish-vc.el
 (autoload 'dirvish-vc-menu "dirvish-vc" nil t)

(register-definition-prefixes "dirvish-vc" '("dirvish-vc-"))

;;;***

;;;### (autoloads nil "dirvish-widgets" "dirvish-widgets.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from dirvish-widgets.el

(register-definition-prefixes "dirvish-widgets" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-yank" "dirvish-yank.el" (0 0 0 0))
;;; Generated autoloads from dirvish-yank.el
 (autoload 'dirvish-yank-menu "dirvish-yank" nil t)

(autoload 'dirvish-yank "dirvish-yank" "\
Paste marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory.'

\(fn &optional DEST)" t nil)

(autoload 'dirvish-move "dirvish-yank" "\
Move marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-symlink "dirvish-yank" "\
Symlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-relative-symlink "dirvish-yank" "\
Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-hardlink "dirvish-yank" "\
Hardlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-rsync "dirvish-yank" "\
Rsync marked files to DEST, prompt for DEST if not called with.
If either the sources or the DEST is located in a remote host,
the `dirvish-yank-rsync-program' and `dirvish-yank-rsync-args'
are used to transfer the files.

This command requires proper ssh authentication setup to work
correctly for file transfer involving remote hosts, because rsync
command is always run locally, the password prompts may lead to
unexpected errors.

\(fn DEST)" t nil)

(register-definition-prefixes "dirvish-yank" '("dirvish-"))

;;;***

;;;### (autoloads nil nil ("dirvish-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dirvish-autoloads.el ends here
