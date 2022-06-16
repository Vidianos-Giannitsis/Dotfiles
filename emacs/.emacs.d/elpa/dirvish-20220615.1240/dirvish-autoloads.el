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
otherwise it defaults to variable `buffer-file-name'.

\(fn &optional PATH)" t nil)

(register-definition-prefixes "dirvish" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-bookmark" "dirvish-bookmark.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from dirvish-bookmark.el
 (autoload 'dirvish-bookmark-goto "dirvish-bookmark" nil t)

(register-definition-prefixes "dirvish-bookmark" '("dirvish-bookmark-entries"))

;;;***

;;;### (autoloads nil "dirvish-extras" "dirvish-extras.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dirvish-extras.el

(autoload 'dirvish-find-file-true-path "dirvish-extras" "\
Open truename of (maybe) symlink file under the cursor." t nil)

(autoload 'dirvish-copy-file-true-path "dirvish-extras" "\
Copy truename of (maybe) symlink file under the cursor." t nil)

(autoload 'dirvish-copy-file-name "dirvish-extras" "\
Copy filename of marked files.
If MULTI-LINE, make every name occupy a separate line.

\(fn &optional MULTI-LINE)" t nil)

(autoload 'dirvish-copy-file-path "dirvish-extras" "\
Copy filepath of marked files.
If MULTI-LINE, make every path occupy a separate line.

\(fn &optional MULTI-LINE)" t nil)

(autoload 'dirvish-copy-file-directory "dirvish-extras" "\
Copy directory name of file under the cursor." t nil)

(autoload 'dirvish-total-file-size "dirvish-extras" "\
Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'.

\(fn &optional FILESET)" t nil)

(autoload 'dirvish-rename-space-to-underscore "dirvish-extras" "\
Rename marked files by replacing space to underscore." t nil)

(autoload 'dirvish-switch-layout "dirvish-extras" "\
Switch Dirvish layout according to RECIPE.
If RECIPE is not provided, switch to the recipe next to the
current layout defined in `dirvish-layout-recipes'.

\(fn &optional RECIPE)" t nil)

(autoload 'dirvish-dwim "dirvish-extras" "\
Start a Dirvish session with optional PATH.
The session takes the whole frame when `one-window-p'.

\(fn &optional PATH)" t nil)

(register-definition-prefixes "dirvish-extras" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-fd" "dirvish-fd.el" (0 0 0 0))
;;; Generated autoloads from dirvish-fd.el

(autoload 'dirvish-fd "dirvish-fd" "\
Run `fd' on DIR and go into Dired mode on a buffer of the output.
The command run is essentially:

  fd --color=never -0 `dirvish-fd-switches' PATTERN
     --exec-batch `dirvish-fd-ls-program' `dired-listing-switches' --directory.

\(fn DIR PATTERN)" t nil)

(autoload 'dirvish-fd-roam "dirvish-fd" "\
Browse all directories using `fd' command.
This command takes a while to index all the directories the first
time you run it.  After the indexing, it fires up instantly." t nil)

(register-definition-prefixes "dirvish-fd" '("dirvish-fd-"))

;;;***

;;;### (autoloads nil "dirvish-history" "dirvish-history.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from dirvish-history.el

(autoload 'dirvish-history-jump "dirvish-history" "\
Open a target directory from `dirvish-history--ring'." t nil)

(autoload 'dirvish-history-last "dirvish-history" "\
Switch to the most recently visited dirvish buffer." t nil)

(autoload 'dirvish-history-go-forward "dirvish-history" "\
Navigate to next ARG directory in history.
ARG defaults to 1.

\(fn &optional ARG)" t nil)

(autoload 'dirvish-history-go-backward "dirvish-history" "\
Navigate to last ARG directory in history.
ARG defaults to -1.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "dirvish-history" '("dirvish-history-"))

;;;***

;;;### (autoloads nil "dirvish-icons" "dirvish-icons.el" (0 0 0 0))
;;; Generated autoloads from dirvish-icons.el

(register-definition-prefixes "dirvish-icons" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-menu" "dirvish-menu.el" (0 0 0 0))
;;; Generated autoloads from dirvish-menu.el
 (autoload 'dirvish-dispatch "dirvish-menu" nil t)
 (autoload 'dirvish-ls-switches-menu "dirvish-menu" nil t)
 (autoload 'dirvish-mark-menu "dirvish-menu" nil t)
 (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
 (autoload 'dirvish-filter-menu "dirvish-menu" nil t)
 (autoload 'dirvish-quicksort "dirvish-menu" nil t)
 (autoload 'dirvish-yank-menu "dirvish-menu" nil t)
 (autoload 'dirvish-setup-menu "dirvish-menu" nil t)

(register-definition-prefixes "dirvish-menu" '("dirvish-"))

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

;;;### (autoloads nil "dirvish-side" "dirvish-side.el" (0 0 0 0))
;;; Generated autoloads from dirvish-side.el

(autoload 'dirvish-side "dirvish-side" "\
Toggle a Dirvish session at the side window.
- If the side window is visible hide it.
- If a side session within the current `dirvish-side-scope'
  exists but is not visible, show it.
- If there is no session exists within the scope,
  create the session with PATH and display it.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'.

\(fn &optional PATH)" t nil)

(register-definition-prefixes "dirvish-side" '("dirvish-side-"))

;;;***

;;;### (autoloads nil "dirvish-subtree" "dirvish-subtree.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from dirvish-subtree.el

(autoload 'dirvish-subtree-toggle "dirvish-subtree" "\
Insert subtree at point or remove it if it was not present." t nil)

(register-definition-prefixes "dirvish-subtree" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-vc" "dirvish-vc.el" (0 0 0 0))
;;; Generated autoloads from dirvish-vc.el

(register-definition-prefixes "dirvish-vc" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-yank" "dirvish-yank.el" (0 0 0 0))
;;; Generated autoloads from dirvish-yank.el

(autoload 'dirvish-yank "dirvish-yank" "\
Paste marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].

If you want to use this command and friends (such as
`dirvish-move') for file transfer involving remote hosts, you'll
need to have proper ssh configuration for those hosts, because an
asynchronous TRAMP connection and the rsync command (which always
run locally) require working SSH authentication which bypasses
the password entering to work, which see Info
node `(tramp)Improving performance of asynchronous remote
processes' and the man page `rsync(1)'.  If the remote host does
not come with proper ssh configuration, the fallback command
defined in `dirvish-yank-fallback-methods' are used.

To make TRAMP more responsive, follow the instructions in Info
node `(tramp)Frequently Asked Questions' to speed it up.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-move "dirvish-yank" "\
Move marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-symlink "dirvish-yank" "\
Symlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-relative-symlink "dirvish-yank" "\
Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information.

\(fn &optional DEST)" t nil)

(autoload 'dirvish-hardlink "dirvish-yank" "\
Hardlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information.

\(fn &optional DEST)" t nil)

(register-definition-prefixes "dirvish-yank" '("dirvish-yank-"))

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
