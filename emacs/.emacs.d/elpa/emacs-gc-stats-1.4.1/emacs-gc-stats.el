;;; emacs-gc-stats.el --- Collect Emacs GC statistics  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92@posteo.net>
;; Maintainer: Ihor Radchenko <yantar92@posteo.net>
;; URL: https://git.sr.ht/~yantar92/emacs-gc-stats
;; Package-Requires: ((emacs "25.1"))

;; Version: 1.4.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package collects Emacs garbage statistics over time and saves
;; it in the format that can be shared with Emacs maintainers.
;;
;;; Usage:
;;
;; Add
;; (require 'emacs-gc-stats)
;; (emacs-gc-stats-mode +1)
;; to your init file to enable the statistics acquiring.
;;
;; When you are ready to share the results, run
;; M-x emacs-gc-stats-save-session
;; and then share the saved `emacs-gc-stats-file'.
;;
;; You can use `emacs-gc-stats-clear' to clear the currently collected
;; session data.

;; 
;;; Code:

(require 'dired-aux)

(defgroup emacs-gc-stats nil
  "Collect and share Emacs GC statistics."
  :tag "GC stats"
  :group 'development)

(defcustom emacs-gc-stats-file (expand-file-name "emacs-gc-stats.eld" user-emacs-directory)
  "File used to store the statistics across Emacs sessions."
  :type 'file)

(defcustom emacs-gc-stats-gc-defaults nil
  "GC strategy to be active in `emacs-gc-stats-mode'.
This setting, when non-nil, will override the existing values of
`gc-cons-threshold' and `gc-cons-percentage'."
  :type '(choice
	  (const :tag "Do not change existing GC settings" nil)
          (const :tag "Force emacs defaults" emacs-defaults)))

(defcustom emacs-gc-stats-inhibit-command-name-logging nil
  "When non-nil, do not collect command names (`this-command')."
  :type 'boolean
  :package-version '(emacs-gc-stats . 1.3))

(defcustom emacs-gc-stats-remind nil
  "When non-nil, remind about submitting the data to Emacs devs.
The value is wither nil (do not remind), t (remind in 3 weeks), or a
number of days."
  :type '(choice
	  (const :tag "No reminder" nil)
          (const :tag "Remind in 3 weeks" t)
          (integer :tag "Remind after N days"))
  :package-version '(emacs-gc-stats . 1.3))

(defcustom emacs-gc-stats-setting-vars
  '(gc-cons-threshold
    gc-cons-percentage
    memory-limit
    emacs-version
    doom-version
    spacemacs-version
    prelude-tips
    ;; Only on this system.  Do not try fetching remote system info via TRAMP.
    (let ((default-directory user-emacs-directory)) (memory-info))
    (memory-use-counts))
  "List of variable/function symbols to collect after loading init.el."
  :type '(list sexp)
  :package-version '(emacs-gc-stats . 1.3))

(defcustom emacs-gc-stats-command-vars
  '(gc-cons-threshold
    gc-cons-percentage
    gcmh-mode
    gc-elapsed
    gcs-done
    this-command
    memory-limit
    ;; Only on this system.  Do not try fetching remote system info via TRAMP.
    (let ((default-directory user-emacs-directory)) (memory-info))
    (memory-use-counts)
    emacs-gc-stats--idle-tic)
  "List of variable/function symbols to collect for each GC or command."
  :type '(list sexp)
  :package-version '(emacs-gc-stats . 1.3))

(defcustom emacs-gc-stats-summary-vars
  '(gc-cons-threshold
    gc-cons-percentage
    gc-elapsed
    gcs-done
    memory-limit
    ;; Only on this system.  Do not try fetching remote system info via TRAMP.
    (let ((default-directory user-emacs-directory)) (memory-info))
    emacs-uptime
    (memory-use-counts)
    emacs-gc-stats-idle-delay
    emacs-gc-stats--idle-tic)
  "List of variables to collect at session end."
  :type '(list sexp)
  :package-version '(emacs-gc-stats . 1.3))

(defun emacs-gc-stats--collect (&rest symbols)
  "Collect SYMBOLS values.
If symbol is a variable, collect (symbol . value).
If symbol is a function name, collect (symbol . (funcall symbol)).
If symbol is a list, collect (symbol . (eval symbol)).
Otherwise, collect symbol."
  (let (data)
    (dolist (var symbols)
      (pcase var
        ((pred keywordp) (push var data))
        ((and (pred symbolp) (pred boundp))
         (unless (and emacs-gc-stats-inhibit-command-name-logging
		      (memq var '( this-command real-this-command
				   last-command real-last-command)))
           (push (cons var (symbol-value var)) data)))
        ((pred functionp)
         (push (cons var (funcall var)) data))
        ((pred listp)
         (push (cons var (eval var)) data))
        (_ (push var data))))
    (nreverse data)))

(defvar emacs-gc-stats--data nil
  "Collected statistics.")

(defun emacs-gc-stats--collect-init ()
  "Collect initial stats."
  (push
   (apply #'emacs-gc-stats--collect
          "Initial stats"
          (current-time-string)
          emacs-gc-stats-setting-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--collect-gc ()
  "Collect single GC stats."
  (push
   (apply #'emacs-gc-stats--collect
          (current-time-string)
          emacs-gc-stats-command-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--collect-init-end ()
  "Collect init.el stats."
  (push
   (apply #'emacs-gc-stats--collect
          "Init.el stats"
          (current-time-string)
          emacs-gc-stats-summary-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--collect-end ()
  "Collect initial stats."
  (push
   (apply #'emacs-gc-stats--collect
          "Session end stats"
          (current-time-string)
          emacs-gc-stats-summary-vars)
   emacs-gc-stats--data))

(defun emacs-gc-stats--compress ()
  "Compress `emacs-gc-stats-file' and return non-nil on success."
  (when (file-readable-p emacs-gc-stats-file)
    (ignore-errors ; If compression failed, just leave it be.
      ;; Confirm overwrite.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
	(dired-compress-file emacs-gc-stats-file)))))

(defun emacs-gc-stats--uncompress ()
  "Uncompress `emacs-gc-stats-file' and return non-nil on success."
  (let ((compressed
	 (concat emacs-gc-stats-file
		 (or dired-compress-file-default-suffix ".gz"))))
    (when (file-readable-p compressed)
      ;; Extract archive.
      (dired-compress-file compressed))))

(defun emacs-gc-stats--get-repvious-session-data ()
  "Return previously saved session data."
  (and (or (file-readable-p emacs-gc-stats-file)
	   (emacs-gc-stats--uncompress))
       (unwind-protect
	   (with-temp-buffer
             (insert-file-contents emacs-gc-stats-file)
             (ignore-errors (read (current-buffer))))
         (emacs-gc-stats--compress)
         nil)))

(defun emacs-gc-stats-save-session ()
  "Save stats to disk."
  (interactive)
  (emacs-gc-stats--collect-end)
  (let ((previous-sessions (emacs-gc-stats--get-repvious-session-data))
        (session (reverse emacs-gc-stats--data))
        (write-region-inhibit-fsync t)
        ;; We set UTF-8 here to avoid the overhead from
        ;; `find-auto-coding'.
        (coding-system-for-write 'utf-8)
        print-level
        print-length
        print-quoted
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-continuous-numbering t)
        print-number-table)
    ;; remove end data in case if we continue recording.
    (pop emacs-gc-stats--data)
    (with-temp-file emacs-gc-stats-file
      ;; Override partially saved session.
      (let ((existing (assoc (car session) previous-sessions)))
        (if existing
            (setcdr (cdr existing) (cdr session))
          (push session previous-sessions)))
      (prin1 previous-sessions (current-buffer)))
    ;; Attempt to compress the file.
    (let ((emacs-gc-stats-file
	   (or (emacs-gc-stats--compress)
               emacs-gc-stats-file)))
      (if (and (called-interactively-p 'interactive)
	       (yes-or-no-p
		(format "GC stats saved to \"%s\".  Send email to emacs-gc-stats@gnu.org? " emacs-gc-stats-file)))
	  (browse-url "mailto:emacs-gc-stats@gnu.org")
	(message "GC stats saved to \"%s\".  You can share the file by sending email to emacs-gc-stats@gnu.org" emacs-gc-stats-file)))))

(defvar emacs-gc-stats-mode) ; defined later
(defun emacs-gc-stats-clear ()
  "Clear GC stats collected so far."
  (interactive)
  (setq emacs-gc-stats--data nil)
  ;; Restart.
  (when emacs-gc-stats-mode
    (emacs-gc-stats-mode -1)
    (emacs-gc-stats-mode +1)))

(defvar emacs-gc-stats-idle-delay 300
  "Delay in seconds to count idle time.")

(defvar emacs-gc-stats--idle-tic 0
  "Idle counter.
Idle time is counted with `emacs-gc-stats-idle-delay' granularity.")
(defvar emacs-gc-stats--idle-timer nil
  "Time counting idle time.")
(defun emacs-gc-stats-idle-tic ()
  "Increase idle counter."
  (when (and (current-idle-time)
             (> (time-to-seconds (current-idle-time)) emacs-gc-stats-idle-delay))
    (cl-incf emacs-gc-stats--idle-tic)))

(defvar emacs-gc-stats--gc-old nil
  "Alist of variable symbols and values storing original GC settings.")

(defun emacs-gc-stats--set-gc-defaults (&optional restore)
  "Set GC settings according to `emacs-gc-stats-gc-defaults'.
Revert original settings when RESTORE is non-nil."
  (if restore
      (dolist (pair emacs-gc-stats--gc-old)
	(set (car pair) (cdr pair)))
    (dolist (var '(gc-cons-threshold gc-cons-percentage))
      (push (cons (symbol-name var) (symbol-value var))
	    emacs-gc-stats--gc-old))
    (pcase emacs-gc-stats-gc-defaults
      (`nil nil)
      (`emacs-defaults
       (setq gc-cons-threshold 800000
	     gc-cons-percentage 0.1))
      (other (error "Unknown value of `emacs-gc-stats-gc-defaults': %S" other)))))

(defun emacs-gc-stats--remind-maybe ()
  "Show a reminder according to `emacs-gc-stats-remind'."
  (require 'notifications)
  (when emacs-gc-stats-remind
    (when-let* ((days-threshold (if (numberp emacs-gc-stats-remind)
				    emacs-gc-stats-remind 21))
		(first-session (or (car (last (emacs-gc-stats--get-repvious-session-data)))
				   (reverse emacs-gc-stats--data)))
		(first-record (car first-session))
                (first-date-string
                 (if (equal "Initial stats" (car first-record))
                     (cadr first-record) (car first-record)))
                (first-time (parse-time-string first-date-string))
                (days-passed (time-to-number-of-days
			      (time-subtract (current-time)
					     (encode-time first-time)))))
      (when (> days-passed days-threshold)
        (notifications-notify
         :title "emacs-gc-stats reminder"
         :body
         (format
          "%.1f days have passed since first record.
Consider M-x emacs-gc-stats-save-session or reporting back to emacs-gc-stats@gnu.org"
	  days-passed))
        (warn
         "emacs-gc-stats: %.1f days have passed since first record.
Consider M-x emacs-gc-stats-save-session or reporting back to emacs-gc-stats@gnu.org"
	 days-passed)))))

;;;###autoload
(define-minor-mode emacs-gc-stats-mode
  "Toggle collecting Emacs GC statistics."
  :global t
  (if emacs-gc-stats-mode
      (progn
	(emacs-gc-stats--set-gc-defaults)
        (add-hook 'after-init-hook #'emacs-gc-stats--set-gc-defaults)
        (add-hook 'after-init-hook #'emacs-gc-stats--remind-maybe)
        (add-hook 'kill-emacs-hook #'emacs-gc-stats--remind-maybe)
        (unless emacs-gc-stats--data
          (emacs-gc-stats--collect-init))
        ;; 5 minutes counter.
        (setq emacs-gc-stats--idle-timer
              (run-with-timer
               emacs-gc-stats-idle-delay
               emacs-gc-stats-idle-delay
               #'emacs-gc-stats-idle-tic))
        (add-hook 'post-gc-hook #'emacs-gc-stats--collect-gc)
        (add-hook 'after-init-hook #'emacs-gc-stats--collect-init-end)
        (add-hook 'kill-emacs-hook #'emacs-gc-stats-save-session))
    (remove-hook 'after-init-hook #'emacs-gc-stats--set-gc-defaults)
    (remove-hook 'after-init-hook #'emacs-gc-stats--remind-maybe)
    (remove-hook 'kill-emacs-hook #'emacs-gc-stats--remind-maybe)
    (emacs-gc-stats--set-gc-defaults 'restore)
    (when (timerp emacs-gc-stats--idle-timer)
      (cancel-timer emacs-gc-stats--idle-timer))
    (remove-hook 'post-gc-hook #'emacs-gc-stats--collect-gc)
    (remove-hook 'after-init-hook #'emacs-gc-stats--collect-init-end)
    (remove-hook 'kill-emacs-hook #'emacs-gc-stats-save-session)))

(provide 'emacs-gc-stats)
;;; emacs-gc-stats.el ends here
