;;; session-async-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "session-async" "session-async.el" (0 0 0 0))
;;; Generated autoloads from session-async.el

(autoload 'session-async-new "session-async" "\
Create a new Emacs process ready to communicate through TCP.

Returned session is named as SESSION-NAME

Creates a server on random port, creates separate Emacs session who will connect
here (user-facing Emacs porcess).

\(fn &optional (SESSION-NAME (session-async--create-unique-session-name)))" nil nil)

(autoload 'session-async-start "session-async" "\
Like `async-start', execute REMOTE-SEXP in separate process.

The result will be passed to RECEIVE-FUNCTION, which defaults to `ignore'

If RUNNING-SESSION is not provided, will create a new one-shot session.

Returns nil.

\(fn &optional (REMOTE-SEXP \\=`(lambda nil)) (RECEIVE-FUNCTION \\='ignore) RUNNING-SESSION)" nil nil)

(autoload 'session-async-future "session-async" "\
Return an iterator for future value for running REMOTE-SEXP in separate proc.

Underneath calls `session-async-start' and returns an iterator that can be
`iter-next'-ed for its value, for which Emacs will block until value is
available.

If RUNNING-SESSION is not provided, will create a new one-shot session.

\(fn &optional (REMOTE-SEXP \\=`(lambda nil)) RUNNING-SESSION)" nil nil)

(autoload 'session-async-get-session-create "session-async" "\
Get a running session and create it if it's necessary.

SYM is the symbol the session is being used to handle the session.
SYM should be quoted (see `set' and `symbol-value').

SESSION-NAME is ignored if session is already running.

\(fn SYM &key (SESSION-NAME (session-async--create-unique-session-name)))" nil nil)

(register-definition-prefixes "session-async" '("session-async-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; session-async-autoloads.el ends here
