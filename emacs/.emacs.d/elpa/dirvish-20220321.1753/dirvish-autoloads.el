;;; dirvish-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dirvish" "dirvish.el" (0 0 0 0))
;;; Generated autoloads from dirvish.el

(autoload 'dirvish-define-attribute "dirvish" "\
Define a Dirvish attribute NAME.
An attribute contains a pair of predicate/rendering functions
that are being called on `post-command-hook'.  The predicate fn
takes current session DV as argument and execute IF once.  When
IF evaluates to t, the rendering fn runs FORM for every line with
following arguments:

- `f-name'  from `dired-get-filename'
- `f-attrs' from `file-attributes'
- `f-beg'   from `dired-move-to-filename'
- `f-end'   from `dired-move-to-end-of-filename'
- `l-beg'   from `line-beginning-position'
- `l-end'   from `line-end-position'
- `hl-face' a face that is only passed in on current line
Optional keywords LEFT, RIGHT and DOC are supported.

\(fn NAME &key IF FORM LEFT RIGHT DOC)" nil t)

(function-put 'dirvish-define-attribute 'lisp-indent-function 'defun)

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

If called interactively, enable Dirvish-Override-Dired mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish" '("dirvish-")))

;;;***

;;;### (autoloads nil "dirvish-extras" "dirvish-extras.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dirvish-extras.el

(autoload 'dirvish-show-history "dirvish-extras" "\
Open a target directory from `dirvish--history-ring'." t nil)

(autoload 'dirvish-other-buffer "dirvish-extras" "\
Switch to the most recently visited dirvish buffer." t nil)

(autoload 'dirvish-find-file-true-path "dirvish-extras" "\
Open truename of (maybe) symlink file under the cursor." t nil)

(autoload 'dirvish-copy-file-name "dirvish-extras" "\
Copy filename under the cursor." t nil)

(autoload 'dirvish-copy-file-path "dirvish-extras" "\
Copy filename under the cursor." t nil)

(autoload 'dirvish-copy-file-directory "dirvish-extras" "\
Copy the current directory's (`default-directory''s) absolute path." t nil)

(autoload 'dirvish-rename-space-to-underscore "dirvish-extras" "\
Rename marked files by replacing space to underscore." t nil)

(autoload 'dirvish-roam "dirvish-extras" "\
Browse all directories using `fd' command." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish-extras" '("dirvish-")))

;;;***

;;;### (autoloads nil "dirvish-menu" "dirvish-menu.el" (0 0 0 0))
;;; Generated autoloads from dirvish-menu.el
 (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
 (autoload 'dirvish-mark-actions-menu "dirvish-menu" nil t)

(autoload 'dirvish-dispatch "dirvish-menu" "\
Summon dirvish commands menu." t nil)
 (autoload 'dirvish-goto-bookmark "dirvish-menu" nil t)
 (autoload 'dirvish-setup-menu "dirvish-menu" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish-menu" '("dirvish-")))

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

If called interactively, enable Dirvish-Peek mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish-peek" '("dirvish-peek-")))

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish-side" '("dirvish-side-")))

;;;***

;;;### (autoloads nil "dirvish-vc" "dirvish-vc.el" (0 0 0 0))
;;; Generated autoloads from dirvish-vc.el
 (autoload 'dirvish-vc-diff-preview-dp "dirvish-vc")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish-vc" '("dirvish-")))

;;;***

;;;### (autoloads nil "dirvish-yank" "dirvish-yank.el" (0 0 0 0))
;;; Generated autoloads from dirvish-yank.el
 (autoload 'dirvish-yank-ml "dirvish-yank" nil t)

(autoload 'dirvish-yank-retrive-progress-h "dirvish-yank" "\
Retrieve progress of current paste/move task." nil nil)

(autoload 'dirvish-yank "dirvish-yank" "\
Paste marked files/directory to current directory.
With optional prefix ARG, delete source files/directories.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dirvish-yank" '("dirvish-")))

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
