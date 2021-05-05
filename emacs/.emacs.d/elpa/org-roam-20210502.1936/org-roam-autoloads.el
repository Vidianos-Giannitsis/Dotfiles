;;; org-roam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-roam" "org-roam.el" (0 0 0 0))
;;; Generated autoloads from org-roam.el

(defvar org-roam-mode nil "\
Non-nil if Org-Roam mode is enabled.
See the `org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-mode'.")

(custom-autoload 'org-roam-mode "org-roam" nil)

(autoload 'org-roam-mode "org-roam" "\
Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively.

\(fn &optional ARG)" t nil)

(defalias 'org-roam 'org-roam-buffer-toggle-display)

(autoload 'org-roam-diagnostics "org-roam" "\
Collect and print info for `org-roam' issues." t nil)

(autoload 'org-roam-find-file "org-roam" "\
Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt.

\(fn &optional INITIAL-PROMPT COMPLETIONS FILTER-FN NO-CONFIRM)" t nil)

(autoload 'org-roam-find-directory "org-roam" "\
Find and open `org-roam-directory'." t nil)

(autoload 'org-roam-find-ref "org-roam" "\
Find and open an Org-roam file from a ref.
ARG is used to forward interactive calls to
`org-roam--get-ref-path-completions'
FILTER can either be a string or a function:
- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)
- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate.

\(fn ARG &optional FILTER)" t nil)

(autoload 'org-roam-random-note "org-roam" "\
Find a random Org-roam file." t nil)

(autoload 'org-roam-insert "org-roam" "\
Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
LINK-TYPE is the type of link to be created. It defaults to \"file\".
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label.  See
`org-roam--get-title-path-completions' for details.

\(fn &optional LOWERCASE COMPLETIONS FILTER-FN DESCRIPTION LINK-TYPE)" t nil)

(autoload 'org-roam-insert-immediate "org-roam" "\
Find an Org-roam file, and insert a relative org link to it at point.
This variant of `org-roam-insert' inserts the link immediately by
using the template in `org-roam-capture-immediate-template'. The
interactive ARG and ARGS are passed to `org-roam-insert'.
See `org-roam-insert' for details.

\(fn ARG &rest ARGS)" t nil)

(autoload 'org-roam-find-file-immediate "org-roam" "\
Find and open an Org-roam file.
This variant of `org-roam-find-file' uses the template in
`org-roam-capture-immediate-template', avoiding the capture
process. The interactive ARG and ARGS are passed to
`org-roam-find-file'. See `org-roam-find-file' for details.

\(fn ARG &rest ARGS)" t nil)

(autoload 'org-roam-jump-to-index "org-roam" "\
Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one." t nil)

(autoload 'org-roam-alias-add "org-roam" "\
Add an alias to Org-roam file.

Return added alias." t nil)

(autoload 'org-roam-alias-delete "org-roam" "\
Delete an alias from Org-roam file." t nil)

(autoload 'org-roam-switch-to-buffer "org-roam" "\
Switch to an existing Org-roam buffer." t nil)

(autoload 'org-roam-version "org-roam" "\
Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area.

\(fn &optional MESSAGE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-buffer" "org-roam-buffer.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-buffer" '("org-roam-buffer")))

;;;***

;;;### (autoloads nil "org-roam-capture" "org-roam-capture.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-capture.el

(autoload 'org-roam-capture "org-roam-capture" "\
Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.

\(fn &optional GOTO KEYS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-")))

;;;***

;;;### (autoloads nil "org-roam-completion" "org-roam-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-completion" '("org-roam-completion-")))

;;;***

;;;### (autoloads nil "org-roam-dailies" "org-roam-dailies.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-dailies.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-")))

;;;***

;;;### (autoloads nil "org-roam-db" "org-roam-db.el" (0 0 0 0))
;;; Generated autoloads from org-roam-db.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("org-roam-db")))

;;;***

;;;### (autoloads nil "org-roam-dev" "org-roam-dev.el" (0 0 0 0))
;;; Generated autoloads from org-roam-dev.el

(autoload 'org-roam-dev-mode "org-roam-dev" "\
Minor mode for setting the dev environment of Org-roam.

If called interactively, enable Org-Roam-Dev mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "org-roam-doctor" "org-roam-doctor.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-doctor.el

(autoload 'org-roam-doctor "org-roam-doctor" "\
Perform a check on the current buffer to ensure cleanliness.
If CHECKALL, run the check for all Org-roam files.

\(fn &optional CHECKALL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-doctor" '("org-roam-doctor-")))

;;;***

;;;### (autoloads nil "org-roam-graph" "org-roam-graph.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-roam-graph.el

(autoload 'org-roam-graph "org-roam-graph" "\
Build and possibly display a graph for FILE from NODE-QUERY.
If FILE is nil, default to current buffer's file name.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for FILE.
  - `\\[universal-argument]' N   show the graph for FILE limiting nodes to N steps.
  - `\\[universal-argument] \\[universal-argument]' build the graph.
  - `\\[universal-argument]' -   build the graph for FILE.
  - `\\[universal-argument]' -N  build the graph for FILE limiting nodes to N steps.

\(fn &optional ARG FILE NODE-QUERY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-graph-")))

;;;***

;;;### (autoloads nil "org-roam-link" "org-roam-link.el" (0 0 0 0))
;;; Generated autoloads from org-roam-link.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-link" '("org-roam-link-")))

;;;***

;;;### (autoloads nil "org-roam-macs" "org-roam-macs.el" (0 0 0 0))
;;; Generated autoloads from org-roam-macs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-macs" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-protocol" "org-roam-protocol.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-protocol-")))

;;;***

;;;### (autoloads nil nil ("org-roam-compat.el" "org-roam-faces.el"
;;;;;;  "org-roam-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-autoloads.el ends here
