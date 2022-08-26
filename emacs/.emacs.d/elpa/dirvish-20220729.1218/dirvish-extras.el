;;; dirvish-extras.el --- Extra commands and mode line segments -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provided:
;;
;; Commands
;; - `dirvish-find-file-true-path'
;; - `dirvish-copy-file-name'
;; - `dirvish-copy-file-path'
;; - `dirvish-copy-file-directory'
;; - `dirvish-total-file-size'
;; - `dirvish-rename-space-to-underscore'
;;
;; Attributes
;; - `file-size'
;;
;; Mode-line segments
;; - `free-space'
;; - `file-link-number'
;; - `file-user'
;; - `file-group'
;; - `file-time'
;; - `file-size'
;; - `file-modes'
;; - `file-inode-number'
;; - `file-device-number'

;;; Code:

(require 'dirvish)

(defcustom dirvish-time-format-string "%R-%x"
  "FORMAT-STRING for `file-time' mode line segment.
This value is passed to function `format-time-string'."
  :group 'dirvish :type 'string)

(defface dirvish-free-space
  '((t (:inherit font-lock-constant-face)))
  "Face used for `free-space' mode-line segment."
  :group 'dirvish)

(defface dirvish-file-link-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for file link number mode-line segment."
  :group 'dirvish)

(defface dirvish-file-user-id
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for file size attributes / mode-line segment."
  :group 'dirvish)

(defface dirvish-file-group-id
  '((t (:inherit dirvish-file-user-id)))
  "Face used for file group id mode-line segment."
  :group 'dirvish)

(defface dirvish-file-time
  '((t (:inherit font-lock-string-face)))
  "Face used for file access/modify/change time mode-line segment."
  :group 'dirvish)

(defface dirvish-file-size
  '((t (:inherit completions-annotations)))
  "Face used for display file size attributes / mode-line segment."
  :group 'dirvish)

(defface dirvish-file-modes
  '((t (:inherit font-lock-builtin-face)))
  "Face used for file mode (privilege) mode-line segment."
  :group 'dirvish)

(defface dirvish-file-inode-number
  '((t (:inherit dirvish-file-link-number)))
  "Face used for file inode number mode-line segment."
  :group 'dirvish)

(defface dirvish-file-device-number
  '((t (:inherit dirvish-file-link-number)))
  "Face used for filesystem device number mode-line segment."
  :group 'dirvish)

;; A small value (< 7) would cause line skipping on Emacs 28-, see #77
(defconst dirvish--file-size-str-len 8)

(defun dirvish--count-file-size (fileset)
  "Return file size of FILESET in bytes."
  (cl-labels ((f-name (f) (if (file-directory-p f)
                              (directory-files-recursively f ".*" nil t)
                            f))
              (f-size (f) (file-attribute-size (file-attributes f))))
    (cl-reduce #'+ (mapcar #'f-size (flatten-tree (mapcar #'f-name fileset))))))

(defun dirvish--file-size-add-spaces (str)
  "Fill file size STR with leading spaces."
  (let* ((spc (concat str " "))
         (len (- dirvish--file-size-str-len (length spc))))
    (if (> len 0) (concat (make-string len ?\ ) spc) spc)))

(defun dirvish--get-file-size-or-count (name attrs)
  "Get file size of file NAME from ATTRS."
  (let ((type (file-attribute-type attrs)))
    (cond ((dirvish-prop :tramp)
           (dirvish--file-size-add-spaces
            (or (file-attribute-size attrs) "?")))
          ((stringp type)
           (let ((count
                  (dirvish-attribute-cache name :f-count
                    (condition-case nil
                        (dirvish--file-size-add-spaces
                         (number-to-string
                          (- (length (directory-files name nil nil t)) 2)))
                      (file-error 'file)))))
             (if (eq count 'file)
                 (dirvish-attribute-cache name :f-size
                   (dirvish--file-size-add-spaces
                    (file-size-human-readable
                     (file-attribute-size (file-attributes name)))))
               count)))
          (type
           (let ((count
                  (dirvish-attribute-cache name :f-count
                    (condition-case nil
                        (dirvish--file-size-add-spaces
                         (number-to-string
                          (- (length (directory-files name nil nil t)) 2)))
                      (file-error 'no-permission)))))
             (if (eq count 'no-permission) " NOPERM " count)))
          (t (dirvish--file-size-add-spaces
              (dirvish-attribute-cache name :f-size
                (file-size-human-readable (or (file-attribute-size attrs) 0))))))))

(defun dirvish--format-file-attr (attr-name)
  "Return a string of cursor file's attribute ATTR-NAME."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (attr-getter (intern (format "file-attribute-%s" attr-name)))
              (attr-face (intern (format "dirvish-file-%s" attr-name)))
              (attr-val (and attrs (funcall attr-getter attrs))))
    (propertize (format "%s" attr-val) 'face attr-face)))

(dirvish-define-attribute file-size
  "Show file size or directories file count at right fringe."
  (:if (and (eq (dv-root-window dv) (selected-window))
            dired-hide-details-mode)
       :width (1+ dirvish--file-size-str-len))
  (let* ((str (dirvish--get-file-size-or-count f-name f-attrs))
         (ov-pos (if (> remain f-wid) l-end (+ f-beg remain)))
         (face (or hl-face 'dirvish-file-size))
         (spc (propertize " " 'display
                          `(space :align-to (- right-fringe
                                               ,dirvish--file-size-str-len))
                          'face face))
         (ov (make-overlay ov-pos ov-pos)))
    (setq str (concat spc str))
    (add-face-text-property 0 (1+ dirvish--file-size-str-len) face t str)
    (overlay-put ov 'after-string str)
    ov))

;;;###autoload (autoload 'dirvish-free-space-ml "dirvish-extras" nil t)
(dirvish-define-mode-line free-space
  "Amount of free space on `default-directory''s file system."
  (let ((free-space (or (dirvish-prop :free-space)
                        (get-free-disk-space default-directory) "")))
    (dirvish-prop :free-space free-space)
    (format " %s %s " (propertize free-space 'face 'dirvish-free-space)
            (propertize "free" 'face 'font-lock-doc-face))))

;;;###autoload (autoload 'dirvish-file-link-number-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-link-number
  "Number of links to file."
  (dirvish--format-file-attr 'link-number))

;;;###autoload (autoload 'dirvish-file-link-number-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-user
  "User name of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (uid (and attrs (file-attribute-user-id attrs)))
              (uname (if (dirvish-prop :tramp) uid (user-login-name uid))))
    (propertize uname 'face 'dirvish-file-user-id)))

;;;###autoload (autoload 'dirvish-file-group-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-group
  "Group name of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (gid (and attrs (file-attribute-group-id attrs)))
              (gname (if (dirvish-prop :tramp) gid (group-name gid))))
    (propertize gname 'face 'dirvish-file-group-id)))

;;;###autoload (autoload 'dirvish-file-time-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-time
  "Last modification time of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (f-mtime (file-attribute-modification-time attrs))
              (time-string
               (if (dirvish-prop :tramp) f-mtime
                 (format-time-string dirvish-time-format-string f-mtime))))
    (format "%s" (propertize time-string 'face 'dirvish-file-time))))

;;;###autoload (autoload 'dirvish-file-size-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-size
  "File size of files or file count of directories."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (size (and attrs (dirvish--get-file-size-or-count f-name attrs))))
    (format "%s" (propertize size 'face 'dirvish-file-size))))

;;;###autoload (autoload 'dirvish-file-modes-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-modes
  "File modes, as a string of ten letters or dashes as in ls -l."
  (dirvish--format-file-attr 'modes))

;;;###autoload (autoload 'dirvish-file-inode-number-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-inode-number
  "File's inode number, as a nonnegative integer."
  (dirvish--format-file-attr 'inode-number))

;;;###autoload (autoload 'dirvish-file-device-number-ml "dirvish-extras" nil t)
(dirvish-define-mode-line file-device-number
  "Filesystem device number, as an integer."
  (dirvish--format-file-attr 'device-number))

;;;###autoload
(defun dirvish-find-file-true-path ()
  "Open truename of (maybe) symlink file under the cursor."
  (interactive)
  (dired-jump nil (file-truename (dired-get-filename nil t))))

(defun dirvish--kill-and-echo (string)
  "Echo last killed STRING."
  (kill-new string)
  (let ((hint (propertize
               "Copied: " 'face 'font-lock-builtin-face)))
    (message "%s" (format "%s%s" hint string))))

;;;###autoload
(defun dirvish-copy-file-true-path ()
  "Copy truename of (maybe) symlink file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (file-truename (dired-get-filename nil t))))

;;;###autoload
(defun dirvish-copy-file-name (&optional multi-line)
  "Copy filename of marked files.
If MULTI-LINE, make every name occupy a separate line."
  (interactive "P")
  (let* ((files (dired-get-marked-files t))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

;;;###autoload
(defun dirvish-copy-file-path (&optional multi-line)
  "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a separate line."
  (interactive "P")
  (let* ((files (dired-get-marked-files))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

;;;###autoload
(defun dirvish-copy-file-directory ()
  "Copy directory name of file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (expand-file-name default-directory)))

;;;###autoload
(defun dirvish-total-file-size (&optional fileset)
  "Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'."
  (interactive)
  (let* ((fileset (or fileset (dired-get-marked-files)))
         (count (propertize (number-to-string (length fileset))
                            'face 'font-lock-builtin-face))
         (size (file-size-human-readable (dirvish--count-file-size fileset))))
    (message "%s" (format "Total size of %s entries: %s" count size))))

;;;###autoload
(defun dirvish-rename-space-to-underscore ()
  "Rename marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (derived-mode-p 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (revert-buffer))
    (user-error "Not in a Dired buffer")))

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here
