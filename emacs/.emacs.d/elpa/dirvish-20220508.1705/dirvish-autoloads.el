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
Override Dired with `dirvish-dired' globally.

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

(autoload 'dirvish-dired "dirvish" "\
Start a Dirvish session with optional PATH in current window.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to variable `buffer-file-name'.  Execute it
in other window when OTHER-WINDOW is non-nil.

\(fn &optional PATH OTHER-WINDOW)" t nil)

(register-definition-prefixes "dirvish" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-extras" "dirvish-extras.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dirvish-extras.el

(autoload 'dirvish-show-history "dirvish-extras" "\
Open a target directory from `dirvish--history-ring'." t nil)

(autoload 'dirvish-other-buffer "dirvish-extras" "\
Switch to the most recently visited dirvish buffer." t nil)

(autoload 'dirvish-go-forward-history "dirvish-extras" "\
Navigate to next ARG directory in history.
ARG defaults to 1.

\(fn &optional ARG)" t nil)

(autoload 'dirvish-go-backward-history "dirvish-extras" "\
Navigate to last ARG directory in history.
ARG defaults to -1.

\(fn &optional ARG)" t nil)

(autoload 'dirvish-find-file-true-path "dirvish-extras" "\
Open truename of (maybe) symlink file under the cursor." t nil)

(autoload 'dirvish-copy-file-true-path "dirvish-extras" "\
Copy truename of (maybe) symlink file under the cursor." t nil)

(autoload 'dirvish-copy-file-name "dirvish-extras" "\
Copy filename under the cursor." t nil)

(autoload 'dirvish-copy-file-path "dirvish-extras" "\
Copy filename under the cursor." t nil)

(autoload 'dirvish-copy-file-directory "dirvish-extras" "\
Copy the current directory's (`default-directory''s) absolute path." t nil)

(autoload 'dirvish-total-file-size "dirvish-extras" "\
Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'.

\(fn &optional FILESET)" t nil)

(autoload 'dirvish-rename-space-to-underscore "dirvish-extras" "\
Rename marked files by replacing space to underscore." t nil)

(autoload 'dirvish-roam "dirvish-extras" "\
Browse all directories using `fd' command." t nil)

(register-definition-prefixes "dirvish-extras" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-menu" "dirvish-menu.el" (0 0 0 0))
;;; Generated autoloads from dirvish-menu.el
 (autoload 'dirvish-ls-switches-menu "dirvish-menu" nil t)
 (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
 (autoload 'dirvish-mark-actions-menu "dirvish-menu" nil t)

(autoload 'dirvish-dispatch "dirvish-menu" "\
Summon dirvish commands menu." t nil)
 (autoload 'dirvish-quicksort "dirvish-menu" nil t)
 (autoload 'dirvish-yank-menu "dirvish-menu" nil t)
 (autoload 'dirvish-goto-bookmark "dirvish-menu" nil t)
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

;;;### (autoloads nil "dirvish-vc" "dirvish-vc.el" (0 0 0 0))
;;; Generated autoloads from dirvish-vc.el
 (autoload 'dirvish-vc-diff-preview-dp "dirvish-vc")
 (autoload 'dirvish-vc-info-ml "dirvish-vc")

(register-definition-prefixes "dirvish-vc" '("dirvish-"))

;;;***

;;;### (autoloads nil "dirvish-yank" "dirvish-yank.el" (0 0 0 0))
;;; Generated autoloads from dirvish-yank.el
 (autoload 'dirvish-yank-ml "dirvish-yank" nil t)

(autoload 'dirvish-yank "dirvish-yank" "\
Paste marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].

\(fn &optional DEST)" t nil)

(autoload 'dirvish-move "dirvish-yank" "\
Move marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].

\(fn &optional DEST)" t nil)

(autoload 'dirvish-symlink "dirvish-yank" "\
Symlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].

\(fn &optional DEST)" t nil)

(autoload 'dirvish-relative-symlink "dirvish-yank" "\
Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument].

\(fn &optional DEST)" t nil)

(autoload 'dirvish-hardlink "dirvish-yank" "\
Hardlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].

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
