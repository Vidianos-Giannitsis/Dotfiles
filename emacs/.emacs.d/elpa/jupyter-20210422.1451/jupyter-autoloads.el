;;; jupyter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jupyter-base" "jupyter-base.el" (0 0 0 0))
;;; Generated autoloads from jupyter-base.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-base" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-channel" "jupyter-channel.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jupyter-channel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-channel" '("jupyter-channel")))

;;;***

;;;### (autoloads nil "jupyter-channel-ioloop" "jupyter-channel-ioloop.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-channel-ioloop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-channel-ioloop" '("jupyter-channel-ioloop")))

;;;***

;;;### (autoloads nil "jupyter-channel-ioloop-comm" "jupyter-channel-ioloop-comm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-channel-ioloop-comm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-channel-ioloop-comm" '("jupyter-channel-ioloop-comm")))

;;;***

;;;### (autoloads nil "jupyter-client" "jupyter-client.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jupyter-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-client" '("com" "define-" "execute-request" "history-request" "is-complete-request" "jupyter-" "shutdown-request")))

;;;***

;;;### (autoloads nil "jupyter-comm-layer" "jupyter-comm-layer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-comm-layer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-comm-layer" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-env" "jupyter-env.el" (0 0 0 0))
;;; Generated autoloads from jupyter-env.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-env" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-ioloop" "jupyter-ioloop.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jupyter-ioloop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-ioloop" '("jupyter-ioloop")))

;;;***

;;;### (autoloads nil "jupyter-ioloop-comm" "jupyter-ioloop-comm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-ioloop-comm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-ioloop-comm" '("jupyter-ioloop-comm")))

;;;***

;;;### (autoloads nil "jupyter-julia" "jupyter-julia.el" (0 0 0 0))
;;; Generated autoloads from jupyter-julia.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-julia" '("jupyter-julia-")))

;;;***

;;;### (autoloads nil "jupyter-kernel-manager" "jupyter-kernel-manager.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-kernel-manager.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-kernel-manager" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-kernel-process-manager" "jupyter-kernel-process-manager.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-kernel-process-manager.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-kernel-process-manager" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-kernelspec" "jupyter-kernelspec.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-kernelspec.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-kernelspec" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-messages" "jupyter-messages.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jupyter-messages.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-messages" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-mime" "jupyter-mime.el" (0 0 0 0))
;;; Generated autoloads from jupyter-mime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-mime" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-org-client" "jupyter-org-client.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-org-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-org-client" '("jupyter-org-")))

;;;***

;;;### (autoloads nil "jupyter-org-extensions" "jupyter-org-extensions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-org-extensions.el

(autoload 'jupyter-org-insert-src-block "jupyter-org-extensions" "\
Insert a src-block above `point'.
With prefix arg BELOW, insert it below `point'.

If `point' is in a src-block use the language of the src-block and
copy the header to the new block.

If QUERY is non-nil and `point' is not in a src-block, ask for
the language to use for the new block.  Otherwise try to select a
language based on the src-block's near `point'.

\(fn &optional BELOW QUERY)" t nil)

(autoload 'jupyter-org-split-src-block "jupyter-org-extensions" "\
Split the current src block with point in upper block.

With a prefix BELOW move point to lower block.

\(fn &optional BELOW)" t nil)

(autoload 'jupyter-org-execute-and-next-block "jupyter-org-extensions" "\
Execute his block and jump or add a new one.

If a new block is created, use the same language, switches and parameters.
With prefix arg NEW, always insert new cell.

\(fn &optional NEW)" t nil)

(autoload 'jupyter-org-execute-to-point "jupyter-org-extensions" "\
Execute Jupyter source blocks that start before point.
Only execute Jupyter source blocks that have the same session.
Non-Jupyter source blocks are evaluated conditionally.

The session is selected in the following way:

   * If `point' is at a Jupyter source block, use its session.

   * If `point' is not at a Jupyter source block, examine the
     source blocks before `point' and ask the user to select a
     session if multiple exist.  If there is only one session, use
     it without asking.

   * Finally, if a session could not be found, then no Jupyter
     source blocks exist before `point'.  In this case, no session
     is selected and all the source blocks before `point' will be
     evaluated, e.g. when all source blocks before `point' are
     shell source blocks.

NOTE: If a session could be selected, only Jupyter source blocks
that have the same session are evaluated *without* evaluating any
other source blocks.  You can also evaluate ANY source block that
doesn't have a Jupyter session by providing a prefix argument.
This is useful, e.g. to evaluate shell source blocks along with
Jupyter source blocks.

\(fn ANY)" t nil)

(autoload 'jupyter-org-execute-subtree "jupyter-org-extensions" "\
Execute Jupyter source blocks that start before point in the current subtree.
This function narrows the buffer to the current subtree and calls
`jupyter-org-execute-to-point'.  See that function for the meaning
of the ANY argument.

\(fn ANY)" t nil)

(autoload 'jupyter-org-next-busy-src-block "jupyter-org-extensions" "\
Jump to the next busy source block.

With a prefix argument ARG, jump forward ARG many blocks.

When BACKWARD is non-nil, jump to the previous block.

\(fn ARG &optional BACKWARD)" t nil)

(autoload 'jupyter-org-previous-busy-src-block "jupyter-org-extensions" "\
Jump to the previous busy source block.

With a prefix argument ARG, jump backward ARG many source blocks.

\(fn ARG)" t nil)

(autoload 'jupyter-org-inspect-src-block "jupyter-org-extensions" "\
Inspect the symbol under point when in a source block." t nil)

(autoload 'jupyter-org-restart-kernel-execute-block "jupyter-org-extensions" "\
Restart the kernel of the source block where point is and execute it." t nil)

(autoload 'jupyter-org-restart-and-execute-to-point "jupyter-org-extensions" "\
Kill the kernel and run all Jupyter src-blocks to point.
With a prefix argument, run ANY source block that doesn't have a
Jupyter session as well.

See `jupyter-org-execute-to-point' for more information on which
source blocks are evaluated.

\(fn &optional ANY)" t nil)

(autoload 'jupyter-org-restart-kernel-execute-buffer "jupyter-org-extensions" "\
Restart kernel and execute buffer." t nil)

(autoload 'jupyter-org-jump-to-block "jupyter-org-extensions" "\
Jump to a source block in the buffer using `ivy'.
If narrowing is in effect, jump to a block in the narrowed region.
Use a numeric prefix CONTEXT to specify how many lines of context to showin the
process of selecting a source block.
Defaults to `jupyter-org-jump-to-block-context-lines'.

\(fn &optional CONTEXT)" t nil)

(autoload 'jupyter-org-jump-to-visible-block "jupyter-org-extensions" "\
Jump to a visible src block with avy." t nil)

(autoload 'jupyter-org-edit-header "jupyter-org-extensions" "\
Edit the src-block header in the minibuffer." t nil)

(autoload 'jupyter-org-kill-block-and-results "jupyter-org-extensions" "\
Kill the block and its results." t nil)

(autoload 'jupyter-org-copy-block-and-results "jupyter-org-extensions" "\
Copy the src block at the current point and its results." t nil)

(autoload 'jupyter-org-clone-block "jupyter-org-extensions" "\
Clone the block above the current block.

If BELOW is non-nil, add the cloned block below.

\(fn &optional BELOW)" t nil)

(autoload 'jupyter-org-merge-blocks "jupyter-org-extensions" "\
Merge the current block with the next block." t nil)

(autoload 'jupyter-org-move-src-block "jupyter-org-extensions" "\
Move source block before of after another.

If BELOW is non-nil, move the block down, otherwise move it up.

\(fn &optional BELOW)" t nil)

(autoload 'jupyter-org-clear-all-results "jupyter-org-extensions" "\
Clear all results in the buffer." t nil)

(autoload 'jupyter-org-interrupt-kernel "jupyter-org-extensions" "\
Interrupt the kernel." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-org-extensions" '("jupyter-org-")))

;;;***

;;;### (autoloads nil "jupyter-repl" "jupyter-repl.el" (0 0 0 0))
;;; Generated autoloads from jupyter-repl.el

(autoload 'jupyter-repl-associate-buffer "jupyter-repl" "\
Associate the `current-buffer' with a REPL CLIENT.
If the `major-mode' of the `current-buffer' is the
`jupyter-repl-lang-mode' of CLIENT, call the function
`jupyter-repl-interaction-mode' to enable the corresponding mode.

CLIENT should be the symbol `jupyter-repl-client' or the symbol
of a subclass.  If CLIENT is a buffer or the name of a buffer, use
the `jupyter-current-client' local to the buffer.

\(fn CLIENT)" t nil)

(defvar jupyter-repl-persistent-mode nil "\
Non-nil if Jupyter-Repl-Persistent mode is enabled.
See the `jupyter-repl-persistent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `jupyter-repl-persistent-mode'.")

(custom-autoload 'jupyter-repl-persistent-mode "jupyter-repl" nil)

(autoload 'jupyter-repl-persistent-mode "jupyter-repl" "\
Global minor mode to persist Jupyter REPL connections.
When the `jupyter-current-client' of the current buffer is a REPL
client, its value is propagated to all buffers switched to that
have the same `major-mode' as the client's kernel language and
`jupyter-repl-interaction-mode' is enabled in those buffers.

If called interactively, enable Jupyter-Repl-Persistent mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'jupyter-run-repl "jupyter-repl" "\
Run a Jupyter REPL connected to a kernel with name, KERNEL-NAME.
KERNEL-NAME will be passed to `jupyter-find-kernelspecs' and the
first kernel found will be used to start the new kernel.

With a prefix argument give a new REPL-NAME for the REPL.

Optional argument ASSOCIATE-BUFFER, if non-nil, means to enable
the REPL interaction mode by calling the function
`jupyter-repl-interaction-mode' in the `current-buffer' and
associate it with the REPL created.  When called interactively,
ASSOCIATE-BUFFER is set to t.  If the `current-buffer's
`major-mode' does not correspond to the language of the kernel
started, ASSOCIATE-BUFFER has no effect.

Optional argument CLIENT-CLASS is the class that will be passed
to `jupyter-start-new-kernel' and should be a class symbol like
the symbol `jupyter-repl-client', which is the default.

When called interactively, DISPLAY the new REPL buffer.
Otherwise, in a non-interactive call, return the REPL client
connected to the kernel.

Note, if `default-directory' is a remote directory, a kernel will
start on the remote host by using the \"jupyter kernel\" shell
command on the host.

\(fn KERNEL-NAME &optional REPL-NAME ASSOCIATE-BUFFER CLIENT-CLASS DISPLAY)" t nil)

(autoload 'jupyter-connect-repl "jupyter-repl" "\
Run a Jupyter REPL using a kernel's connection FILE-OR-PLIST.
FILE-OR-PLIST can be either a file holding the connection
information or a property list of connection information.
ASSOCIATE-BUFFER has the same meaning as in `jupyter-run-repl'.

With a prefix argument give a new REPL-NAME for the REPL.

Optional argument CLIENT-CLASS is the class of the client that
will be used to initialize the REPL and should be a class symbol
like the symbol `jupyter-repl-client', which is the default.

Return the REPL client connected to the kernel.  When called
interactively, DISPLAY the new REPL buffer as well.

\(fn FILE-OR-PLIST &optional REPL-NAME ASSOCIATE-BUFFER CLIENT-CLASS DISPLAY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-repl" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-rest-api" "jupyter-rest-api.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jupyter-rest-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-rest-api" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-server" "jupyter-server.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jupyter-server.el

(autoload 'jupyter-server-launch-kernel "jupyter-server" "\
Start a kernel on SERVER.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

\(fn SERVER)" t nil)

(autoload 'jupyter-run-server-repl "jupyter-server" "\
On SERVER start a kernel with KERNEL-NAME.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

REPL-NAME, ASSOCIATE-BUFFER, CLIENT-CLASS, and DISPLAY all have
the same meaning as in `jupyter-run-repl'.

\(fn SERVER KERNEL-NAME &optional REPL-NAME ASSOCIATE-BUFFER CLIENT-CLASS DISPLAY)" t nil)

(autoload 'jupyter-connect-server-repl "jupyter-server" "\
On SERVER, connect to the kernel with KERNEL-ID.

With a prefix argument, ask to select a server if there are
mutiple to choose from, otherwise the most recently used server
is used as determined by `jupyter-current-server'.

REPL-NAME, ASSOCIATE-BUFFER, CLIENT-CLASS, and DISPLAY all have
the same meaning as in `jupyter-connect-repl'.

\(fn SERVER KERNEL-ID &optional REPL-NAME ASSOCIATE-BUFFER CLIENT-CLASS DISPLAY)" t nil)

(autoload 'jupyter-server-list-kernels "jupyter-server" "\
Display a list of live kernels on SERVER.
When called interactively, ask to select a SERVER when given a
prefix argument otherwise the `jupyter-current-server' will be
used.

\(fn SERVER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-server" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-server-ioloop" "jupyter-server-ioloop.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-server-ioloop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-server-ioloop" '("jupyter-server-")))

;;;***

;;;### (autoloads nil "jupyter-tramp" "jupyter-tramp.el" (0 0 0 0))
;;; Generated autoloads from jupyter-tramp.el

(defconst jupyter-tramp-file-name-handler-alist '((add-name-to-file . tramp-handle-add-name-to-file) (copy-file . jupyter-tramp-copy-file) (delete-directory . jupyter-tramp-delete-directory) (delete-file . jupyter-tramp-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (expand-file-name . jupyter-tramp-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . jupyter-tramp-file-attributes) (file-directory-p . jupyter-tramp-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-handle-file-exists-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . jupyter-tramp-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . jupyter-tramp-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-handle-file-exists-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . jupyter-tramp-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . jupyter-tramp-file-symlink-p) (file-system-info . ignore) (file-truename . tramp-handle-file-truename) (file-writable-p . jupyter-tramp-file-writable-p) (find-backup-file-name . ignore) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory-internal . jupyter-tramp-make-directory-internal) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-symbolic-link . tramp-handle-make-symbolic-link) (rename-file . jupyter-tramp-rename-file) (set-file-acl . ignore) (set-file-modes . ignore) (set-file-selinux-context . ignore) (set-file-times . ignore) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . jupyter-tramp-write-region)) "\
Alist of handler functions for Tramp Jupyter method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defconst jupyter-tramp-methods '("jpy" "jpys") "\
Methods to connect Jupyter kernel servers.")

(with-eval-after-load 'tramp (mapc (lambda (method) (add-to-list 'tramp-methods (list method (list 'tramp-default-port 8888) (list 'tramp-tmpdir "/tmp")))) jupyter-tramp-methods) (tramp-register-foreign-file-name-handler 'jupyter-tramp-file-name-p 'jupyter-tramp-file-name-handler) (add-to-list 'tramp-default-host-alist '("\\`jpys?\\'" nil "localhost")))

(defsubst jupyter-tramp-file-name-method-p (method) "\
Return METHOD if it corresponds to a Jupyter filename method or nil." (and (string-match-p "\\`jpys?\\'" method) method))

(defsubst jupyter-tramp-file-name-p (filename) "\
If FILENAME is a Jupyter filename, return its method otherwise nil." (when (tramp-tramp-file-p filename) (jupyter-tramp-file-name-method-p (tramp-file-name-method (tramp-dissect-file-name filename)))))

(autoload 'jupyter-tramp-file-name-handler "jupyter-tramp" "\


\(fn OPERATION &rest ARGS)" nil nil)

(autoload 'jupyter-tramp-file-name-from-url "jupyter-tramp" "\
Return a Jupyter TRAMP filename for the root directory of a kernel server.
The filename is based off of URL's host and port if any.

\(fn URL)" nil nil)

(autoload 'jupyter-tramp-url-from-file-name "jupyter-tramp" "\
Return a URL string based off the method, host, and port of FILENAME.

\(fn FILENAME)" nil nil)

(autoload 'jupyter-tramp-server-from-file-name "jupyter-tramp" "\
Return a `jupyter-server' instance based off of FILENAME's remote components.
If the connection has not been authenticated by the server,
attempt to authenticate the connection.  Raise an error if that
fails.

\(fn FILENAME)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-tramp" '("jupyter-tramp-")))

;;;***

;;;### (autoloads nil "jupyter-widget-client" "jupyter-widget-client.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-widget-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-widget-client" '("httpd/jupyter" "jupyter-widget")))

;;;***

;;;### (autoloads nil "jupyter-zmq-channel" "jupyter-zmq-channel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-zmq-channel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-zmq-channel" '("jupyter-")))

;;;***

;;;### (autoloads nil "jupyter-zmq-channel-ioloop" "jupyter-zmq-channel-ioloop.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupyter-zmq-channel-ioloop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupyter-zmq-channel-ioloop" '("jupyter-zmq-channel-ioloop")))

;;;***

;;;### (autoloads nil "ob-jupyter" "ob-jupyter.el" (0 0 0 0))
;;; Generated autoloads from ob-jupyter.el

(autoload 'org-babel-jupyter-scratch-buffer "ob-jupyter" "\
Display a scratch buffer connected to the current block's session." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-jupyter" '("org-babel-")))

;;;***

;;;### (autoloads nil nil ("jupyter-R.el" "jupyter-c++.el" "jupyter-javascript.el"
;;;;;;  "jupyter-pkg.el" "jupyter-python.el" "jupyter.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jupyter-autoloads.el ends here
