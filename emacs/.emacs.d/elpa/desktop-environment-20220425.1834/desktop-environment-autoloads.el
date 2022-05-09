;;; desktop-environment-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "desktop-environment" "desktop-environment.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from desktop-environment.el

(autoload 'desktop-environment-brightness-increment "desktop-environment" "\
Increment brightness by `desktop-environment-brightness-normal-increment'." t nil)

(autoload 'desktop-environment-brightness-decrement "desktop-environment" "\
Decrement brightness by `desktop-environment-brightness-normal-decrement'." t nil)

(autoload 'desktop-environment-brightness-increment-slowly "desktop-environment" "\
Increment brightness by `desktop-environment-brightness-small-increment'." t nil)

(autoload 'desktop-environment-brightness-decrement-slowly "desktop-environment" "\
Decrement brightness by `desktop-environment-brightness-small-decrement'." t nil)

(autoload 'desktop-environment-volume-increment "desktop-environment" "\
Increment volume by `desktop-environment-volume-normal-increment'." t nil)

(autoload 'desktop-environment-volume-decrement "desktop-environment" "\
Decrement volume by `desktop-environment-volume-normal-decrement'." t nil)

(autoload 'desktop-environment-volume-increment-slowly "desktop-environment" "\
Increment volume by `desktop-environment-volume-small-increment'." t nil)

(autoload 'desktop-environment-volume-decrement-slowly "desktop-environment" "\
Decrement volume by `desktop-environment-volume-small-decrement'." t nil)

(autoload 'desktop-environment-toggle-mute "desktop-environment" "\
Toggle between muted and un-muted." t nil)

(autoload 'desktop-environment-toggle-microphone-mute "desktop-environment" "\
Toggle microphone between muted and un-muted." t nil)

(autoload 'desktop-environment-keyboard-backlight-increment "desktop-environment" "\
Increment keyboard backlight.
The increment step is configured with
`desktop-environment-keyboard-backlight-normal-increment'." t nil)

(autoload 'desktop-environment-screenshot "desktop-environment" "\
Take a screenshot of the screen.

Screenshots are stored in the directory
`desktop-environment-screenshot-directory'.

When DELAY is a positive integer, delay taking the screenshot by
DELAY seconds.  When the function is called interactively, DELAY
is the provided prefix argument.

In order to delay the screenshot,
`desktop-environment-screenshot-delay-argument' is appended to
the command `desktop-environment-screenshot-command'.

\(fn &optional DELAY)" t nil)

(autoload 'desktop-environment-screenshot-part "desktop-environment" "\
Take a screenshot of part of the screen.

Screenshots are stored in the directory
`desktop-environment-screenshot-directory'.

The command asks the user to interactively select a portion of
the screen.

When DELAY is a positive integer, delay taking the screenshot by
DELAY seconds.  When the function is called interactively, DELAY
is the provided prefix argument.

In order to delay the screenshot,
`desktop-environment-screenshot-delay-argument' is appended to
the command `desktop-environment-screenshot-partial-command'.

\(fn &optional DELAY)" t nil)

(autoload 'desktop-environment-lock-screen "desktop-environment" "\
Lock the screen, preventing anyone without a password from using the system." t nil)

(autoload 'desktop-environment-toggle-wifi "desktop-environment" "\
Toggle WiFi adapter on and off." t nil)

(autoload 'desktop-environment-toggle-bluetooth "desktop-environment" "\
Toggle Bluetooth on and off." t nil)

(defvar desktop-environment-mode nil "\
Non-nil if DEsktop-Environment moDE is enabled.
See the `desktop-environment-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `desktop-environment-mode'.")

(custom-autoload 'desktop-environment-mode "desktop-environment" nil)

(autoload 'desktop-environment-mode "desktop-environment" "\
Activate keybindings to control your desktop environment.

This is a minor mode.  If called interactively, toggle the
`DEsktop-Environment moDE' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='desktop-environment-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{desktop-environment-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "desktop-environment" '("desktop-environment-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; desktop-environment-autoloads.el ends here
