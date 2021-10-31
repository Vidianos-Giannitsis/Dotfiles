;;; python-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-mode" "python-mode.el" (0 0 0 0))
;;; Generated autoloads from python-mode.el

(autoload 'py-backward-class-bol "python-mode" "\
Go to beginning of ‘class’, go to BOL.
If already at beginning, go one ‘class’ backward.
Return beginning of ‘class’ if successful, nil otherwise" t nil)

(autoload 'py-backward-def-bol "python-mode" "\
Go to beginning of ‘def’, go to BOL.
If already at beginning, go one ‘def’ backward.
Return beginning of ‘def’ if successful, nil otherwise" t nil)

(autoload 'py-backward-def-or-class-bol "python-mode" "\
Go to beginning of ‘def-or-class’, go to BOL.
If already at beginning, go one ‘def-or-class’ backward.
Return beginning of ‘def-or-class’ if successful, nil otherwise" t nil)

(autoload 'py-forward-class "python-mode" "\
Go to end of class.

Return end of ‘class’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position

\(fn &optional ORIG BOL)" t nil)

(autoload 'py-forward-def "python-mode" "\
Go to end of def.

Return end of ‘def’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position

\(fn &optional ORIG BOL)" t nil)

(autoload 'py-forward-def-or-class "python-mode" "\
Go to end of def-or-class.

Return end of ‘def-or-class’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position

\(fn &optional ORIG BOL)" t nil)

(autoload 'py-auto-completion-mode "python-mode" "\
Run auto-completion

\(fn)" t nil)

(autoload 'python-mode "python-mode" "\
Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]'
from a`python-mode' buffer.
Do `\\[py-describe-mode]' for detailed documentation.
To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation,
tokens, comments (and continuation lines.
Paragraphs are separated by blank lines only.

COMMANDS

`py-shell'	Start an interactive Python interpreter in another window
`py-execute-statement'	Send statement at point to Python default interpreter
`py-backward-statement'	Go to the initial line of a simple statement

etc.

See available commands listed in files commands-python-mode at directory doc

VARIABLES

`py-indent-offset'	indentation increment
`py-shell-name'		shell command to invoke Python interpreter
`py-split-window-on-execute'		When non-nil split windows
`py-switch-buffers-on-execute-p'	When non-nil switch to the Python output buffer

See available customizations listed in files variables-python-mode at directory doc

\\{python-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-mode" '("all-mode-setting" "autopair-mode" "flake8" "force-py-shell-name-p-o" "highlight-indent-active" "hs-hide-comments-when-hiding-all" "info-lookup-mode" "ipython" "iypthon" "jython" "pdb-track-stack-from-shell-p" "pep8" "pst-here" "stri" "toggle-force-py-shell-name-p" "turn-o" "virtualenv-")))

;;;***

;;;### (autoloads nil nil ("python-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-mode-autoloads.el ends here
