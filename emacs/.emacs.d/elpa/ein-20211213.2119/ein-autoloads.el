;;; ein-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ein-cell" "ein-cell.el" (0 0 0 0))
;;; Generated autoloads from ein-cell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-cell" '("ein:")))

;;;***

;;;### (autoloads nil "ein-classes" "ein-classes.el" (0 0 0 0))
;;; Generated autoloads from ein-classes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-classes" '("ein:")))

;;;***

;;;### (autoloads nil "ein-contents-api" "ein-contents-api.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ein-contents-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-contents-api" '("*ein:content-hierarchy*" "ein:")))

;;;***

;;;### (autoloads nil "ein-core" "ein-core.el" (0 0 0 0))
;;; Generated autoloads from ein-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-core" '("*ein:" "ein:")))

;;;***

;;;### (autoloads nil "ein-dev" "ein-dev.el" (0 0 0 0))
;;; Generated autoloads from ein-dev.el

(autoload 'ein:dev-start-debug "ein-dev" "\
Start logging a bunch of stuff." t nil)

(autoload 'ein:dev-stop-debug "ein-dev" "\
Inverse of `ein:dev-start-debug'.
Impossible to maintain because it needs to match start." t nil)

(autoload 'ein:dev-bug-report-template "ein-dev" "\
Open a buffer with bug report template." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-dev" '("ein:dev-")))

;;;***

;;;### (autoloads nil "ein-events" "ein-events.el" (0 0 0 0))
;;; Generated autoloads from ein-events.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-events" '("ein:events-")))

;;;***

;;;### (autoloads nil "ein-file" "ein-file.el" (0 0 0 0))
;;; Generated autoloads from ein-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-file" '("*ein:file-buffername-template*" "ein:")))

;;;***

;;;### (autoloads nil "ein-gat" "ein-gat.el" (0 0 0 0))
;;; Generated autoloads from ein-gat.el

(autoload 'ein:gat-create "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-local-batch "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-local "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-remote-batch "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-remote "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-gat" '("ein:gat-")))

;;;***

;;;### (autoloads nil "ein-ipdb" "ein-ipdb.el" (0 0 0 0))
;;; Generated autoloads from ein-ipdb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-ipdb" '("*ein:ipdb-sessions*" "ein:ipdb-")))

;;;***

;;;### (autoloads nil "ein-ipynb-mode" "ein-ipynb-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ein-ipynb-mode.el

(autoload 'ein:ipynb-mode "ein-ipynb-mode" "\
A simple mode for ipynb file.

\\{ein:ipynb-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . ein:ipynb-mode))

;;;***

;;;### (autoloads nil "ein-jupyter" "ein-jupyter.el" (0 0 0 0))
;;; Generated autoloads from ein-jupyter.el

(autoload 'ein:jupyter-crib-token "ein-jupyter" "\
Shell out to jupyter for its credentials knowledge.  Return list of (PASSWORD TOKEN).

\(fn URL-OR-PORT)" nil nil)

(autoload 'ein:jupyter-crib-running-servers "ein-jupyter" "\
Shell out to jupyter for running servers." nil nil)

(autoload 'ein:jupyter-server-start "ein-jupyter" "\
Start SERVER-COMMAND with `--notebook-dir' NOTEBOOK-DIRECTORY.

Login after connection established unless NO-LOGIN-P is set.
LOGIN-CALLBACK takes two arguments, the buffer created by
`ein:notebooklist-open--finish', and the url-or-port argument
of `ein:notebooklist-open*'.

With \\[universal-argument] prefix arg, prompt the user for the
server command.

\(fn SERVER-COMMAND NOTEBOOK-DIRECTORY &optional NO-LOGIN-P LOGIN-CALLBACK PORT)" t nil)

(defalias 'ein:run 'ein:jupyter-server-start)

(defalias 'ein:stop 'ein:jupyter-server-stop)

(autoload 'ein:jupyter-server-stop "ein-jupyter" "\


\(fn &optional ASK-P URL-OR-PORT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-jupyter" '("*ein:jupyter-server-" "ein:")))

;;;***

;;;### (autoloads nil "ein-kernel" "ein-kernel.el" (0 0 0 0))
;;; Generated autoloads from ein-kernel.el

(defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port)

(defalias 'ein:kernel-id 'ein:$kernel-kernel-id)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-kernel" '("ein:")))

;;;***

;;;### (autoloads nil "ein-kernelinfo" "ein-kernelinfo.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ein-kernelinfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-kernelinfo" '("ein:kernelinfo")))

;;;***

;;;### (autoloads nil "ein-kill-ring" "ein-kill-ring.el" (0 0 0 0))
;;; Generated autoloads from ein-kill-ring.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-kill-ring" '("ein:")))

;;;***

;;;### (autoloads nil "ein-log" "ein-log.el" (0 0 0 0))
;;; Generated autoloads from ein-log.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-log" '("ein:")))

;;;***

;;;### (autoloads nil "ein-markdown-mode" "ein-markdown-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ein-markdown-mode.el

(autoload 'ein:markdown-mode "ein-markdown-mode" "\
Major mode for editing ein:markdown files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-markdown-mode" '("defun-markdown-" "ein:markdown")))

;;;***

;;;### (autoloads nil "ein-node" "ein-node.el" (0 0 0 0))
;;; Generated autoloads from ein-node.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-node" '("ein:node-")))

;;;***

;;;### (autoloads nil "ein-notebook" "ein-notebook.el" (0 0 0 0))
;;; Generated autoloads from ein-notebook.el

(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

(autoload 'ein:notebook-jump-to-opened-notebook "ein-notebook" "\
List all opened notebook buffers and switch to one that the user selects.

\(fn NOTEBOOK)" t nil)

(autoload 'ein:notebook-open "ein-notebook" "\
Returns notebook at URL-OR-PORT/PATH.

Note that notebook sends for its contents and won't have them right away.

After the notebook is opened, CALLBACK is called as::

  (funcall CALLBACK notebook created)

where `created' indicates a new notebook or an existing one.

\(fn URL-OR-PORT PATH &optional KERNELSPEC CALLBACK ERRBACK NO-POP)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-notebook" '("*ein:notebook--pending-query*" "ein:")))

;;;***

;;;### (autoloads nil "ein-notebooklist" "ein-notebooklist.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ein-notebooklist.el

(autoload 'ein:notebooklist-reload "ein-notebooklist" "\
Reload current Notebook list.

\(fn &optional NBLIST RESYNC CALLBACK)" t nil)

(autoload 'ein:notebooklist-new-notebook "ein-notebooklist" "\


\(fn URL-OR-PORT KERNELSPEC &optional CALLBACK NO-POP RETRY EXPLICIT-PATH)" t nil)

(autoload 'ein:notebooklist-new-notebook-with-name "ein-notebooklist" "\
Upon notebook-open, rename the notebook, then funcall CALLBACK.

\(fn URL-OR-PORT KERNELSPEC NAME &optional CALLBACK NO-POP)" t nil)

(autoload 'ein:notebooklist-list-paths "ein-notebooklist" "\
Return all files of CONTENT-TYPE for all sessions

\(fn &optional CONTENT-TYPE)" nil nil)

(autoload 'ein:notebooklist-load "ein-notebooklist" "\
Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein:notebooklist-load)

\(fn &optional URL-OR-PORT)" nil nil)

(autoload 'ein:notebooklist-open "ein-notebooklist" "\
This is now an alias for `ein:notebooklist-login'.

\(fn URL-OR-PORT CALLBACK)" t nil)

(defalias 'ein:login 'ein:notebooklist-login)

(autoload 'ein:notebooklist-login "ein-notebooklist" "\
Deal with security before main entry of ein:notebooklist-open*.

CALLBACK takes two arguments, the buffer created by ein:notebooklist-open--success
and the url-or-port argument of ein:notebooklist-open*.

\(fn URL-OR-PORT CALLBACK &optional COOKIE-NAME COOKIE-CONTENT TOKEN)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-notebooklist" '("ein:" "generate-breadcrumbs" "render-")))

;;;***

;;;### (autoloads nil "ein-notification" "ein-notification.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ein-notification.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-notification" '("ein:")))

;;;***

;;;### (autoloads nil "ein-output-area" "ein-output-area.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ein-output-area.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-output-area" '("ein:")))

;;;***

;;;### (autoloads nil "ein-pager" "ein-pager.el" (0 0 0 0))
;;; Generated autoloads from ein-pager.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-pager" '("ein:pager-")))

;;;***

;;;### (autoloads nil "ein-process" "ein-process.el" (0 0 0 0))
;;; Generated autoloads from ein-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-process" '("ein:process-")))

;;;***

;;;### (autoloads nil "ein-python-send" "ein-python-send.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ein-python-send.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-python-send" '("ein:python-send-")))

;;;***

;;;### (autoloads nil "ein-pytools" "ein-pytools.el" (0 0 0 0))
;;; Generated autoloads from ein-pytools.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-pytools" '("ein:pytools-jump-")))

;;;***

;;;### (autoloads nil "ein-query" "ein-query.el" (0 0 0 0))
;;; Generated autoloads from ein-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-query" '("ein:")))

;;;***

;;;### (autoloads nil "ein-scratchsheet" "ein-scratchsheet.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ein-scratchsheet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-scratchsheet" '("ein:scratchsheet")))

;;;***

;;;### (autoloads nil "ein-shared-output" "ein-shared-output.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ein-shared-output.el

(autoload 'ein:shared-output-pop-to-buffer "ein-shared-output" "\
Open shared output buffer." t nil)

(autoload 'ein:shared-output-show-code-cell-at-point "ein-shared-output" "\
Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein:cell-max-num-outputs'." t nil)

(autoload 'ein:shared-output-eval-string "ein-shared-output" "\
Entry to `ein:cell-execute-internal' from the shared output cell.

\(fn KERNEL CODE &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-shared-output" '("*ein:shared-output*" "ein:")))

;;;***

;;;### (autoloads nil "ein-traceback" "ein-traceback.el" (0 0 0 0))
;;; Generated autoloads from ein-traceback.el

(autoload 'ein:tb-show "ein-traceback" "\
Show full traceback in traceback viewer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-traceback" '("ein:t")))

;;;***

;;;### (autoloads nil "ein-utils" "ein-utils.el" (0 0 0 0))
;;; Generated autoloads from ein-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-utils" '("ein:")))

;;;***

;;;### (autoloads nil "ein-websocket" "ein-websocket.el" (0 0 0 0))
;;; Generated autoloads from ein-websocket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-websocket" '("ein:" "fix-request-netscape-cookie-parse")))

;;;***

;;;### (autoloads nil "ein-worksheet" "ein-worksheet.el" (0 0 0 0))
;;; Generated autoloads from ein-worksheet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-worksheet" '("ein:" "hof-add")))

;;;***

;;;### (autoloads nil "ob-ein" "ob-ein.el" (0 0 0 0))
;;; Generated autoloads from ob-ein.el

(when (featurep 'org) (let* ((orig (get 'org-babel-load-languages 'custom-type)) (orig-cdr (cdr orig)) (choices (plist-get orig-cdr :key-type))) (push '(const :tag "Ein" ein) (nthcdr 1 choices)) (put 'org-babel-load-languages 'custom-type (cons (car orig) (plist-put orig-cdr :key-type choices)))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ein" '("*ob-ein-sentinel*" "ob-ein-")))

;;;***

;;;### (autoloads nil "poly-ein" "poly-ein.el" (0 0 0 0))
;;; Generated autoloads from poly-ein.el
 (autoload 'poly-ein-mode "poly-ein")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-ein" '("pm-" "poly-ein-")))

;;;***

;;;### (autoloads nil nil ("ein-completer.el" "ein-pkg.el" "ein.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ein-autoloads.el ends here
