;;; org-roam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-roam" "org-roam.el" (0 0 0 0))
;;; Generated autoloads from org-roam.el

(autoload 'org-roam-setup "org-roam" "\
Setup Org-roam and initialize its database.
This will install the needed hooks and advices to keep everything
in sync with the connected databases." t nil)

(autoload 'org-roam-node-find "org-roam" "\
Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)

\(fn &optional OTHER-WINDOW INITIAL-INPUT FILTER-FN &key TEMPLATES)" t nil)

(autoload 'org-roam-node-insert "org-roam" "\
Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)

\(fn &optional FILTER-FN &key TEMPLATES)" t nil)

(autoload 'org-roam-node-random "org-roam" "\
Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'org-roam-ref-find "org-roam" "\
Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.

\(fn &optional INITIAL-INPUT FILTER-FN)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-capture" "org-roam-capture.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-capture.el
 (autoload 'org-roam-capture- "org-roam" nil t)
 (autoload 'org-roam-capture "org-roam" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-")))

;;;***

;;;### (autoloads nil "org-roam-compat" "org-roam-compat.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-compat" '("org-roam--directory-files-recursively")))

;;;***

;;;### (autoloads nil "org-roam-completion" "org-roam-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-completion" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-dailies" "org-roam-dailies.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-dailies.el
 (autoload 'org-roam-dailies-find-directory "org-roam" nil t)
 (autoload 'org-roam-dailies-capture-today "org-roam" nil t)
 (autoload 'org-roam-dailies-goto-today "org-roam" nil t)
 (autoload 'org-roam-dailies-capture-tomorrow "org-roam" nil t)
 (autoload 'org-roam-dailies-goto-tomorrow "org-roam" nil t)
 (autoload 'org-roam-dailies-capture-yesterday "org-roam" nil t)
 (autoload 'org-roam-dailies-goto-yesterday "org-roam" nil t)
 (autoload 'org-roam-dailies-capture-date "org-roam" nil t)
 (autoload 'org-roam-dailies-goto-date "org-roam" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-")))

;;;***

;;;### (autoloads nil "org-roam-db" "org-roam-db.el" (0 0 0 0))
;;; Generated autoloads from org-roam-db.el
 (autoload 'org-roam-db-sync "org-roam" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("emacsql-constraint" "org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-graph" "org-roam-graph.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-roam-graph.el

(autoload 'org-roam-graph "org-roam-graph" "\
Build and possibly display a graph for NODE.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for NODE.
  - `\\[universal-argument]' N   show the graph for NODE limiting nodes to N steps.

\(fn &optional ARG NODE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-graph-")))

;;;***

;;;### (autoloads nil "org-roam-macs" "org-roam-macs.el" (0 0 0 0))
;;; Generated autoloads from org-roam-macs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-macs" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-migrate" "org-roam-migrate.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-migrate.el
 (autoload 'org-roam-migrate-wizard "org-roam" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-migrate" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-mode" "org-roam-mode.el" (0 0 0 0))
;;; Generated autoloads from org-roam-mode.el
 (autoload 'org-roam-buffer-display-dedicated "org-roam" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-mode" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-overlay" "org-roam-overlay.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-overlay.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-overlay" '("org-roam-overlay-")))

;;;***

;;;### (autoloads nil "org-roam-protocol" "org-roam-protocol.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-protocol-")))

;;;***

;;;### (autoloads nil "org-roam-utils" "org-roam-utils.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-roam-utils.el
 (autoload 'org-roam-version "org-roam" nil t)
 (autoload 'org-roam-diagnostics "org-roam" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-utils" '("org-roam-")))

;;;***

;;;### (autoloads nil nil ("org-roam-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-autoloads.el ends here
