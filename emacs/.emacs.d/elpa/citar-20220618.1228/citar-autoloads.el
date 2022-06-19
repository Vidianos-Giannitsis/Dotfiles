;;; citar-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "citar" "citar.el" (0 0 0 0))
;;; Generated autoloads from citar.el

(autoload 'citar-refresh "citar" "\
Reload the candidates cache.

If called interactively with a prefix or if FORCE-REBUILD-CACHE
is non-nil, also run the `citar-before-refresh-hook' hook.

If SCOPE is `global' only global cache is refreshed, if it is
`local' only local cache is refreshed.  With any other value both
are refreshed.

\(fn &optional FORCE-REBUILD-CACHE SCOPE)" t nil)

(autoload 'citar-insert-preset "citar" "\
Prompt for and insert a predefined search." t nil)

(autoload 'citar-key-finder "citar" "\
Return the citation key at point." nil nil)

(autoload 'citar-citation-finder "citar" "\
Return the keys of the citation at point." nil nil)

(with-eval-after-load 'embark (add-to-list 'embark-target-finders 'citar-citation-finder) (add-to-list 'embark-transformer-alist '(citar-reference . citar--reference-transformer)) (add-to-list 'embark-target-finders 'citar-key-finder) (add-to-list 'embark-candidate-collectors #'citar--embark-selected))

(autoload 'citar-open "citar" "\
Open related resources (links or files) for KEYS-ENTRIES.

\(fn KEYS-ENTRIES)" t nil)

(autoload 'citar-open-library-file "citar" "\
Open library file associated with the KEY-ENTRY.

With prefix, rebuild the cache before offering candidates.

\(fn KEY-ENTRY)" t nil)

(autoload 'citar-open-notes "citar" "\
Open notes associated with the KEY-ENTRY.
With prefix, rebuild the cache before offering candidates.

\(fn KEY-ENTRY)" t nil)

(autoload 'citar-open-entry "citar" "\
Open bibliographic entry associated with the KEY-ENTRY.
With prefix, rebuild the cache before offering candidates.

\(fn KEY-ENTRY)" t nil)

(autoload 'citar-insert-bibtex "citar" "\
Insert bibliographic entry associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS-ENTRIES)" t nil)

(autoload 'citar-export-local-bib-file "citar" "\
Create a new bibliography file from citations in current buffer.

The file is titled \"local-bib\", given the same extention as
the first entry in `citar-bibliography', and created in the same
directory as current buffer." t nil)

(autoload 'citar-open-link "citar" "\
Open URL or DOI link associated with the KEY-ENTRY in a browser.

With prefix, rebuild the cache before offering candidates.

\(fn KEY-ENTRY)" t nil)

(autoload 'citar-insert-citation "citar" "\
Insert citation for the KEYS-ENTRIES.

Prefix ARG is passed to the mode-specific insertion function. It
should invert the default behaviour for that mode with respect to
citation styles. See specific functions for more detail.

\(fn KEYS-ENTRIES &optional ARG)" t nil)

(autoload 'citar-insert-reference "citar" "\
Insert formatted reference(s) associated with the KEYS-ENTRIES.

\(fn KEYS-ENTRIES)" t nil)

(autoload 'citar-copy-reference "citar" "\
Copy formatted reference(s) associated with the KEYS-ENTRIES.

\(fn KEYS-ENTRIES)" t nil)

(autoload 'citar-insert-keys "citar" "\
Insert KEYS-ENTRIES citekeys.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS-ENTRIES)" t nil)

(autoload 'citar-attach-library-file "citar" "\
Attach library file associated with KEY-ENTRY to outgoing MIME message.

With prefix, rebuild the cache before offering candidates.

\(fn KEY-ENTRY)" t nil)

(autoload 'citar-add-file-to-library "citar" "\
Add a file to the library for KEY-ENTRY.
The FILE can be added either from an open buffer, a file, or a
URL.

\(fn KEY-ENTRY)" t nil)

(autoload 'citar-run-default-action "citar" "\
Run the default action `citar-default-action' on KEYS-ENTRIES.

\(fn KEYS-ENTRIES)" nil nil)

(autoload 'citar-dwim "citar" "\
Run the default action on citation keys found at point." t nil)

(register-definition-prefixes "citar" '("citar-"))

;;;***

;;;### (autoloads nil "citar-capf" "citar-capf.el" (0 0 0 0))
;;; Generated autoloads from citar-capf.el

(autoload 'citar-capf "citar-capf" "\
Citation key `completion-at-point` for org, markdown, or latex." nil nil)

(register-definition-prefixes "citar-capf" '("citar-capf--"))

;;;***

;;;### (autoloads nil "citar-citeproc" "citar-citeproc.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from citar-citeproc.el

(autoload 'citar-citeproc-select-csl-style "citar-citeproc" "\
Select CSL style to be used with `citar-citeproc-format-reference`." t nil)

(autoload 'citar-citeproc-format-reference "citar-citeproc" "\
Return formatted reference(s) for KEYS-ENTRIES via `citeproc-el`.
Formatting follows CSL style set in `citar-citeproc-csl-style`.
With prefix-argument, select CSL style.

\(fn KEYS-ENTRIES)" nil nil)

(register-definition-prefixes "citar-citeproc" '("citar-citeproc-csl-"))

;;;***

;;;### (autoloads nil "citar-file" "citar-file.el" (0 0 0 0))
;;; Generated autoloads from citar-file.el

(register-definition-prefixes "citar-file" '("citar-file-"))

;;;***

;;;### (autoloads nil "citar-filenotify" "citar-filenotify.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from citar-filenotify.el

(autoload 'citar-filenotify-setup "citar-filenotify" "\
Setup filenotify watches for local and global bibliography related files.

This functions adds watches to the files in
`citar-filenotify-files' and adds a hook to the
major mode hooks in 'MODE-HOOKS' which adds watches for the
local bib files.  These local watches are removed when the buffer
closes.

\(fn MODE-HOOKS)" nil nil)

(register-definition-prefixes "citar-filenotify" '("citar-filenotify-"))

;;;***

;;;### (autoloads nil "citar-latex" "citar-latex.el" (0 0 0 0))
;;; Generated autoloads from citar-latex.el

(autoload 'citar-latex-local-bib-files "citar-latex" "\
Local bibliographic for latex retrieved using reftex." nil nil)

(autoload 'citar-latex-key-at-point "citar-latex" "\
Return citation key at point with its bounds.

The return value is (KEY . BOUNDS), where KEY is the citation key
at point and BOUNDS is a pair of buffer positions.

Return nil if there is no key at point." nil nil)

(autoload 'citar-latex-citation-at-point "citar-latex" "\
Find citation macro at point and extract keys.

Find brace-delimited strings inside the bounds of the macro,
splits them at comma characters, and trims whitespace.

Return (KEYS . BOUNDS), where KEYS is a list of the found
citation keys and BOUNDS is a pair of buffer positions indicating
the start and end of the citation macro." nil nil)

(autoload 'citar-latex-insert-citation "citar-latex" "\
Insert a citation consisting of KEYS.

If the command is inside a citation command keys are added to it. Otherwise
a new command is started.

If the optional COMMAND is provided use it (ignoring INVERT-PROMPT).
Otherwise prompt for a citation command, depending on the value of
`citar-latex-prompt-for-cite-style'. If INVERT-PROMPT is non-nil, invert
whether or not to prompt.

The availiable commands and how to provide them arguments are configured
by `citar-latex-cite-commands'.

If `citar-latex-prompt-for-extra-arguments' is `nil`, every
command is assumed to have a single argument into which keys are
inserted.

\(fn KEYS &optional INVERT-PROMPT COMMAND)" nil nil)

(autoload 'citar-latex-insert-edit "citar-latex" "\
Prompt for keys and call `citar-latex-insert-citation.
With ARG non-nil, rebuild the cache before offering candidates.

\(fn &optional ARG)" nil nil)

(register-definition-prefixes "citar-latex" '("citar-latex-"))

;;;***

;;;### (autoloads nil "citar-markdown" "citar-markdown.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from citar-markdown.el

(autoload 'citar-markdown-insert-keys "citar-markdown" "\
Insert semicolon-separated and @-prefixed KEYS in a markdown buffer.

\(fn KEYS)" nil nil)

(autoload 'citar-markdown-insert-citation "citar-markdown" "\
Insert a pandoc-style citation consisting of KEYS.

If the point is inside a citation, add new keys after the current
key.

If point is immediately after the opening [, add new keys
to the beginning of the citation.

If INVERT-PROMPT is non-nil, invert the meaning of
`citar-markdown-prompt-for-extra-arguments`.

\(fn KEYS &optional INVERT-PROMPT)" nil nil)

(autoload 'citar-markdown-insert-edit "citar-markdown" "\
Prompt for keys and call `citar-markdown-insert-citation.
With ARG non-nil, rebuild the cache before offering candidates.

\(fn &optional ARG)" nil nil)

(autoload 'citar-markdown-key-at-point "citar-markdown" "\
Return citation key at point (with its bounds) for pandoc markdown citations.
Returns (KEY . BOUNDS), where KEY is the citation key at point
and BOUNDS is a pair of buffer positions.  Citation keys are
found using `citar-markdown-citation-key-regexp`.  Returns nil if
there is no key at point." t nil)

(autoload 'citar-markdown-citation-at-point "citar-markdown" "\
Return keys of citation at point.
Find balanced expressions starting and ending with square
brackets and containing at least one citation key (matching
`citar-markdown-citation-key-regexp`).  Return (KEYS . BOUNDS),
where KEYS is a list of the found citation keys and BOUNDS is a
pair of buffer positions indicating the start and end of the
citation." nil nil)

(autoload 'citar-markdown-list-keys "citar-markdown" "\
Return a list of all keys from markdown citations in buffer." nil nil)

(register-definition-prefixes "citar-markdown" '("citar-markdown-"))

;;;***

;;;### (autoloads nil "citar-org" "citar-org.el" (0 0 0 0))
;;; Generated autoloads from citar-org.el

(autoload 'citar-org-select-key "citar-org" "\
Return a list of keys when MULTIPLE, or else a key string.

\(fn &optional MULTIPLE)" nil nil)

(autoload 'citar-org-insert-citation "citar-org" "\
Insert KEYS in org-cite format, with STYLE.

\(fn KEYS &optional STYLE)" nil nil)

(autoload 'citar-org-insert-edit "citar-org" "\
Run `org-cite-insert` with citar insert processor.
ARG is used as the prefix argument.

\(fn &optional ARG)" nil nil)

(autoload 'citar-org-follow "citar-org" "\
Follow processor for org-cite.

\(fn DATUM ARG)" nil nil)

(autoload 'citar-org-select-style "citar-org" "\
Complete a citation style for org-cite with preview.

\(fn &optional ARG)" nil nil)

(autoload 'citar-org-local-bib-files "citar-org" "\
Return local bib file paths for org buffer.

\(fn &rest ARGS)" nil nil)

(autoload 'citar-org-roam-make-preamble "citar-org" "\
Add a preamble to org-roam note, with KEY.

\(fn KEY)" nil nil)

(autoload 'citar-org-format-note-default "citar-org" "\
Format a note FILEPATH from KEY and ENTRY.

\(fn KEY ENTRY FILEPATH)" nil nil)

(autoload 'citar-org-key-at-point "citar-org" "\
Return key at point for org-cite citation-reference." nil nil)

(autoload 'citar-org-citation-at-point "citar-org" "\
Return org-cite citation keys at point as a list for `embark'." nil nil)

(autoload 'citar-org-activate "citar-org" "\
Run all the activation functions in `citar-org-activation-functions'.
Argument CITATION is an org-element holding the references.

\(fn CITATION)" nil nil)

(with-eval-after-load 'oc (org-cite-register-processor 'citar :insert (org-cite-make-insert-processor #'citar-org-select-key #'citar-org-select-style) :follow #'citar-org-follow :activate #'citar-org-activate))

(register-definition-prefixes "citar-org" '("citar-org-"))

;;;***

;;;### (autoloads nil nil ("citar-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; citar-autoloads.el ends here
