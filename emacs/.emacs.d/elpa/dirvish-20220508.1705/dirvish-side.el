;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.2.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.2.0"))

;;; Commentary:

;; Toggle Dirvish in side window like treemacs.

;;; Code:

(require 'dirvish)
(declare-function get-current-persp "persp-mode")
(declare-function persp-curr "perspective")

(defvar dirvish-side--state-alist '())
(defvar dirvish-side-scope-fn nil)

(defcustom dirvish-side-attributes dirvish-attributes
  "Same as `dirvish-attributes', but for side sessions."
  :group 'dirvish :type '(repeat dirvish-attribute))

(defcustom dirvish-side-preview-dispatchers dirvish-preview-dispatchers
  "Same as `dirvish-preview-dispatchers', but for side sessions."
  :group 'dirvish :type 'list)

(defvar dirvish-side--ml-fmt nil)
(defcustom dirvish-side-mode-line-format dirvish-mode-line-format
  "Same as `dirvish-mode-line-format', but for side sessions."
  :group 'dirvish :type 'plist
  :set (lambda (k v) (set k v) (setq dirvish-side--ml-fmt (dirvish--mode-line-fmt-setter v))))

(defcustom dirvish-side-scope 'tab
  "SCOPE for Dirvish side window.
Every SCOPE only have one (toggleable) side Dirvish session.  For
example, set it to `perspective' will make every `perspective'
have an unique `dirvish-side' session.  SCOPE can be `emacs',
`tab', `frame', `persp', or `perspective'."
  :group 'dirvish :type 'symbol
  :options '(emacs tab frame persp)
  :set
  (lambda (k v)
    (set k v)
    (cl-case v
      ('tab
       (add-hook 'tab-bar-tab-pre-close-functions #'dirvish-side--remove-state)
       (setq dirvish-side-scope-fn #'tab-bar--current-tab-index))
      ('frame
       (add-hook 'delete-frame-functions #'dirvish-side--remove-state)
       (setq dirvish-side-scope-fn #'selected-frame))
      ('persp
       (if (require 'persp-mode nil t)
           (progn
             (add-hook 'persp-before-kill-functions #'dirvish-side--remove-state)
             (add-hook 'persp-activated-functions
                       (lambda (_scope) (when (car (dirvish-side--get-state)) (dotimes (_ 2) (dirvish-side)))))
             (setq dirvish-side-scope-fn (lambda () (or (get-current-persp) 'none))))
         (set k 'tab)
         (user-error "Unable to find package `persp-mode'")))
      ('perspective
       (if (require 'perspective nil t)
           (progn
             (add-hook 'persp-killed-hook #'dirvish-side--remove-state)
             (setq dirvish-side-scope-fn #'persp-curr))
         (set k 'tab)
         (user-error "Unable to find package `perspective'"))))
    (cl-loop for (_scope . (dv . state)) in dirvish-side--state-alist
             do (when dv (dirvish-kill dv)))
    (setq dirvish-side--state-alist '())))

(defcustom dirvish-side-display-alist
  '((side . left) (slot . -1) (window-width . 0.2))
  "Display alist for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-window-parameters '((no-delete-other-windows . t))
  "Window parameters for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-open-file-window-function
  (lambda () (get-mru-window nil nil t))
  "A function that returns a window for the `find-file' buffer.
This function is called before opening files in a `dirvish-side'
session.  For example, if you have `ace-window' installed, you
can set it to `ace-select-window', which prompts you for a target
window to place the file buffer.  Note that if this value is
`selected-window', the session closes after opening a file."
  :group 'dirvish :type 'function)

(defcustom dirvish-side-follow-buffer-file nil
  "Whether to follow current buffer's filename.
If this variable is non-nil, when the current buffer is visiting
a file, the summoned side sessions updates its index path
according to the filename."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-side-follow-project-switch t
  "Whether visible side session update index on project switch.
If this variable is non-nil, the visible `dirvish-side' session
will visit the latest `project-root' after executing
`project-switch-project' or `projectile-switch-project'."
  :group 'dirvish :type 'boolean
  :set
  (lambda (key enabled)
    (set key enabled)
    (if enabled
        (progn
          (advice-add 'project-switch-project :after #'dirvish-side-find-file)
          (add-hook 'projectile-after-switch-project-hook #'dirvish-side-find-file))
      (advice-remove 'project-switch-project #'dirvish-side-find-file)
      (remove-hook 'projectile-after-switch-project-hook #'dirvish-side-find-file))))

(defun dirvish-side--get-state ()
  "Get state of side session for current scope."
  (or (alist-get (funcall dirvish-side-scope-fn) dirvish-side--state-alist)
      (cons nil 'uninitialized)))

(defun dirvish-side--set-state (dv state)
  "Set state of current scope to DV and its STATE."
  (setf (alist-get (funcall dirvish-side-scope-fn) dirvish-side--state-alist) (cons dv state)))

(defun dirvish-side--remove-state (scope &rest _)
  "Remove invalid state info within SCOPE."
  (let* ((dv-w/state (alist-get scope dirvish-side--state-alist))
         (dv (car-safe dv-w/state)))
    (when dv (dirvish-kill dv))
    (setq dirvish-side--state-alist
          (delq (assoc scope dirvish-side--state-alist) dirvish-side--state-alist))))

(defun dirvish-side-find-file-window-fn ()
  "Return a window for opening files in `dirvish-side'."
  (if (window-parameter (selected-window) 'window-side)
      (funcall dirvish-side-open-file-window-function)
    (selected-window)))

(defun dirvish-side-quit-window-fn (_dv)
  "Quit window action for `dirvish-side'."
  (dirvish-side--set-state nil 'uninitialized))

(defun dirvish-side-root-window-fn (dv)
  "Display DV's window according to `dirvish-side-display-alist'."
  (if (>= (dv-depth dv) 0)
      (frame-selected-window)
    (let ((win (display-buffer-in-side-window
                (dirvish--ensure-temp-buffer) dirvish-side-display-alist)))
      (cl-loop for (key . value) in dirvish-side-window-parameters
               do (set-window-parameter win key value))
      (set-window-dedicated-p win t)
      (select-window win))))

(defun dirvish-side-header-string-fn ()
  "Return a string showing current project."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (window-buffer (dv-root-window dv))
      (let ((project (dirvish--get-project-root)))
        (if project
            (setq project (file-name-base (directory-file-name project)))
          (setq project "-"))
        (format " %s [%s]"
                (propertize "Project:" 'face 'bold)
                (propertize project 'face 'font-lock-string-face))))))

(defun dirvish-side-find-file (&optional filename)
  "Visit FILENAME in current visible `dirvish-side' session."
  (pcase-let ((`(,dv . ,state) (dirvish-side--get-state)))
    (let ((win (and dv (dv-root-window dv)))
          (dirname (or (and filename (file-name-directory filename))
                       (dirvish--get-project-root))))
      (when (and (eq state 'visible) (window-live-p win) dirname)
        (with-selected-window win
          (dirvish-reclaim)
          (setf (dv-index-dir dv) dirname)
          (dirvish-with-no-dedication
           (switch-to-buffer (dirvish--buffer-for-dir dv dirname)))
          (when (and filename (not (file-directory-p filename)))
            (setq-local dirvish--child-entry filename))
          (dirvish-build dv))))))

;;;###autoload
(defun dirvish-side (&optional path)
  "Toggle a Dirvish session at the side window.
- If the side window is visible hide it.
- If a side session within the current `dirvish-side-scope'
  exists but is not visible, show it.
- If there is no session exists within the scope,
  create the session with PATH and display it.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish side: "))))
  (pcase-let ((`(,dv . ,state) (dirvish-side--get-state)))
    (cl-case state
      ('visible
       (unless (dirvish-dired-p dv) (dirvish-toggle-fullscreen))
       (dirvish-side--set-state dv 'exists)
       (let ((win (dv-root-window dv)))
         (unless (window-live-p win)
           (user-error
            "Session closed unexpectedly, call `%s' again to reset" this-command))
         (delete-window win)))
      ('exists
       (let ((followed (buffer-file-name))
             (last (dv-index-dir dv)))
         (with-selected-window (dirvish--create-root-window dv)
           (dirvish-with-no-dedication
            (switch-to-buffer (dirvish--buffer-for-dir dv last)))
           (dirvish-reclaim)
           (if (and dirvish-side-follow-buffer-file followed)
               (progn
                 (dirvish-find-file (file-name-directory followed))
                 (dired-goto-file followed)
                 (dirvish-update-body-h))
             (dirvish-build dv)))
         (dirvish-side--set-state dv 'visible)))
      ('uninitialized
       (dirvish-new t
         :path (or (and path (file-name-directory path))
                   (dirvish--get-project-root)
                   default-directory)
         :attributes dirvish-side-attributes
         :preview-dispatchers dirvish-side-preview-dispatchers
         :mode-line-format dirvish-side--ml-fmt
         :depth -1
         :root-window-fn #'dirvish-side-root-window-fn
         :header-string-fn #'dirvish-side-header-string-fn
         :find-file-window-fn #'dirvish-side-find-file-window-fn
         :quit-window-fn #'dirvish-side-quit-window-fn)
       (dirvish-side--set-state (dirvish-curr) 'visible)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here
