;;; pdftotext.el --- View PDFs as text using pdftotext and fmt -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 TEC
;;
;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: September 15, 2021
;; Modified: September 15, 2021
;; Version: 0.0.1
;; Keywords: convenience tools
;; Homepage: https://github.com/tecosaur/pdftotext
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  View PDFs as text using pdftotext and fmt.
;;
;;; Code:

(defun pdftotext--update (&optional _window)
  (when (eq major-mode 'pdftotext-mode)
    (let* ((converted-file
            (expand-file-name
             (concat
              (file-name-base buffer-file-name)
              "-"
              (substring (secure-hash 'md5 (expand-file-name buffer-file-name)) 0 6)
              ".txt")
             temporary-file-directory))
           (width (number-to-string (- (min (window-width) fill-column)
                                       (if display-line-numbers display-line-numbers-width 0))))
           (width-adjusted-file (concat (file-name-sans-extension converted-file) "-w" width ".txt")))
      (unless (and (file-exists-p converted-file)
                   (> (time-convert (file-attribute-modification-time (file-attributes converted-file)) 'integer)
                      (time-convert (file-attribute-modification-time (file-attributes buffer-file-name)) 'integer)))
        (call-process "pdftotext" nil nil nil "-layout" "-eol" "unix" buffer-file-name converted-file))
      (unless (and (file-exists-p width-adjusted-file)
                   (>= (time-convert (file-attribute-modification-time (file-attributes width-adjusted-file)) 'integer)
                       (time-convert (file-attribute-modification-time (file-attributes converted-file)) 'integer)))
        (call-process "fmt" nil (list :file width-adjusted-file) nil "-w" width converted-file))
      (unless (and (boundp 'pdftotext--file)
                   (string= pdftotext--file width-adjusted-file))
        (let ((pos (when (boundp 'pdftotext--file) (pdftotext--position-info))))
          (with-silent-modifications
            (let ((inhibit-read-only t)
                  (coding-system-for-read 'utf-8))
              (erase-buffer)
              (insert-file-contents width-adjusted-file)
              (while (re-search-forward "\n?\f\n?" nil t)
                (replace-match "\n\f\n"))
              (goto-char (point-min)))
            (setq-local pdftotext--file width-adjusted-file))
          (setq-default saved-pos pos)
          (when pos (ignore-errors (pdftotext--goto-pos pos))))))))

 ;; The mode uses so-long to reduce the initial buffer load time
 ;; however it feels like there should be a better solution?
;;;###autoload
(define-derived-mode pdftotext-mode so-long-mode "PDF Text"
  "Major mode for viewing the plaintext version of a PDF."
  (set-buffer-multibyte t)
  (read-only-mode t)
  (add-hook 'before-save-hook (lambda () (user-error "Will not overwrite PDF with plaintext version")))
  (dolist (hook '(window-configuration-change-hook
                  window-size-change-functions
                  display-line-numbers-mode-hook))
    (add-hook hook 'pdftotext--update))
  (pdftotext--update)
  (text-mode)
  (setq mode-name "PDF Text"))

;; In `pdftotext--update' there's mention of position saving and
;; restoring. This needs to be implemented, and it's a bit difficult since the line
;; numbers and buffer positions are liable to change. So, instead we can try to
;; take note of some markers (such as the line breaks) and try to make our way to
;; them.

;; Unfortunately while in isolated testing this position restoring works well, for
;; some reason as it's currently used it doesn't seem to work at all.

(defun pdftotext--position-info ()
  (list :page-no (let ((current-point (point))
                       (page-no 0))
                   (save-excursion
                     (while (search-forward "\f" current-point t)
                       (setq page-no (1+ page-no))))
                   page-no)
        :par-start (save-excursion
                     (forward-paragraph -1)
                     (forward-line 1)
                     (thing-at-point 'line t))
        :previous-line-content (save-excursion
                                 (forward-line -1)
                                 (thing-at-point 'line t))))

(defun pdftotext--goto-pos (pos)
  (goto-char (point-min))
  (search-forward "\f" nil nil (plist-get pos :page-no))
  (re-search-forward (replace-regexp-in-string " +" "[ \n]+" (regexp-quote (plist-get pos :par-start))))
  (unless (string= (plist-get pos :par-start)
                   (plist-get pos :previous-line-content))
    (re-search-forward (replace-regexp-in-string " +" "[ \n]+" (regexp-quote (plist-get pos :previous-line-content)))
                       (save-excursion (forward-paragraph 1) (point)))))

(defconst pdftotext-auto-mode-alist-entry
  '("\\.[pP][dD][fF]\\'" . pdftotext-mode)
  "The entry to use for `auto-mode-alist'.")
(defconst pdftotext-magic-mode-alist-entry
  '("%PDF" . pdftotext-mode)
  "The entry to use for `magic-mode-alist'.")

;;;###autoload
(defun pdftotext-install ()
  "Add a \".pdf\" associaton for all future buffers."
  (interactive)
  (add-to-list 'auto-mode-alist pdftotext-auto-mode-alist-entry)
  (add-to-list 'magic-mode-alist pdftotext-magic-mode-alist-entry)
  (when (featurep 'pdf-tools)
    (setq-default auto-mode-alist
                  (remove pdf-tools-auto-mode-alist-entry auto-mode-alist))
    (setq-default magic-mode-alist
                  (remove pdf-tools-magic-mode-alist-entry magic-mode-alist))))

(defun pdftotext-uninstall ()
  "Remove the \".pdf\" associaton for all future buffers."
  (interactive)
  (setq-default auto-mode-alist
                (remove pdftotext-auto-mode-alist-entry auto-mode-alist))
  (setq-default magic-mode-alist
                (remove pdftotext-magic-mode-alist-entry auto-mode-alist)))

(provide 'pdftotext)
;;; pdftotext.el ends here
