;;; helm-dired-open.el --- "Open with" dialog for helm   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/helm-dired-open
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.3"))
;; Keywords: comm, dired, open-with, xdg

;;; License:

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
;; An 'Open with' dialog for opening files in external applications from Dired.


;;; Code:

;;;; Requirements

(require 's)
(require 'xdg)
(require 'mailcap)

;;;; Customization

(defcustom helm-dired-open-functions
  '(helm-dired-open-configured-applications
    helm-dired-open-xdg-applications)
  "A list of functions returning a list of applications that can open a given
file extension. A first non-nil result is used, the rest of the functions is
not called.
Customize this variable to either provide your own source, or disable some of
the preconfigured ones.
"
  :type '(repeat (choice symbol))
  :group 'helm-dired-open)

(defcustom helm-dired-open-extensions nil
  "Alist of extensions mapping to a programs to run them in.
Programs are Alists as-well, consisting of executable and description that is
going to be displayed in helm selection. The filename is appended after the
program executable."
  :type '(alist
          :key-type (string :tag "Extension")
          :value-type
          (alist
           :key-type (string :tag "Program")
           :value-type (string :tag "Description")))
  :group 'helm-dired-open)

;;;; Commands

;;;###autoload
(defun helm-dired-open ()
  "Provide an 'Open with' dialog for opening files in external applications
from Dired. Such dialogs are commonly known from GUI file managers, when
right-clicking a file.
Configure your filetype to programs associations in
`helm-dired-open-extensions'. If any association is found, this function
fallback to simply running `dired-open-file'."
  (interactive)
  (let* ((source (helm-dired-open--source))
         (candidates (alist-get 'candidates source)))
    (if candidates
        (helm :sources source)
      (dired-open-file))))

;;;; Functions

;;;;; Public

(defun helm-dired-open-configured-applications (extension)
  "Provide a list of applications that can open this file extension.
The applications are returned as `(EXECUTABLE . TITLE)' tuples.
The list of applications is generated from an user-configured variable
`helm-dired-open-extensions'."
  (let ((associations (cdr (assoc extension helm-dired-open-extensions))))
    (mapcar
     (lambda (pair)
       (cons (cdr pair) (car pair)))
     associations)))

(defun helm-dired-open-xdg-applications (extension)
  "Provide a list of applications that can open this file extension.
The applications are returned as `(EXECUTABLE . TITLE)' tuples.
The list of applications is generated from the xdg subsystem."
  (let* ((extension (helm-dired-open--dired-file-extension))
         (mimetype (mailcap-extension-to-mime extension))
         (applications (xdg-mime-apps mimetype)))
    (mapcar
     (lambda (app)
       (let ((hash (xdg-desktop-read-file app)))
         (cons
          (gethash "Name" hash)
          (gethash "Exec" hash))))
     applications)))

;;;;; Private

(defun helm-dired-open--dired-file-extension ()
  "Return the extension of the currently selected file in Dired."
  (file-name-extension (dired-get-file-for-visit)))

(defun helm-dired-open--candidates ()
  "Return the Helm candidates for the currently selected file in Dired.
We find the list of candidates by calling functions from
`helm-dired-open-functions' one by one until any of them returns non-nil."
  (let* ((extension (helm-dired-open--dired-file-extension)))
    (some
     (lambda (f)
       (funcall f extension))
     helm-dired-open-functions)))

(defun helm-dired-open--source ()
  "Helm source providing a list of programs that can open a file"
  `((name . "Open with")
    (candidates . ,(helm-dired-open--candidates))
    (action . ,'helm-dired-open--action)))

(defun helm-dired-open--action (candidate)
  "Use the selected candidate executable to open a file."
  (let* ((path (dired-get-file-for-visit))
         (extension (helm-dired-open--dired-file-extension))
         (dired-open-extensions (list (cons extension candidate)))
         (cmd (helm-dired-open--xdg-format-exec candidate path)))
    ;; If the executable string doesn't contain any XDG exec formatting, we can
    ;; rely on `dired-open-by-extension' command from `dired-open' but otherwise
    ;; we need to use our custom function to run the process.
    (if (string-equal candidate cmd)
        (dired-open-by-extension)
      (helm-dired-open--start-process cmd))))

(defun helm-dired-open--xdg-format-exec (exec path)
  "Format XDG application Exec string and return a full command that can be
executed. For the list of keys and their meaning, please see
https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html#exec-variables
Oh cmon ... this must be already implemented somewhere.
"
  (let* ((url path)
         (cmd exec)
         (cmd (s-replace "%f" path cmd))
         (cmd (s-replace "%F" path cmd))
         (cmd (s-replace "%u" url cmd))
         (cmd (s-replace "%U" url cmd))
         (cmd (s-replace "%i" "" cmd))
         (cmd (s-replace "%c" "" cmd))
         (cmd (s-replace "%k" "" cmd)))
    cmd))

(defun helm-dired-open--start-process (cmd)
  "The functions for running processes implemented in `dired-open' doesn't
support inputting only a command (already containing the file) but always
operate with an executable and then concatenating a file at the end of the
line. That is not suitable for XDG applications that contain formatting in their
Exec and expect us to inject the filename into a specific part of the string."
  (apply 'start-process
         (append '("helm-dired-open" nil)
                 (split-string cmd))))

;;;; Footer

(provide 'helm-dired-open)

;;; helm-dired-open.el ends here
