;;; sidecap.el --- AUCTeX style for `sidecap.sty' (v1.6f)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2021-12-11
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `sidecap.sty' (v1.6f) from 2003/06/06.
;; `sidecap.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

(defun LaTeX-env-sidecap-float (environment)
  "Create ENVIRONMENT with \\caption and \\label commands.
This function is a copy of `LaTeX-env-figure' and adjusted to
read the first optional argument 'relwidth' provided by
environments of the package sidecap."
  (let* ((relwidth (TeX-read-string
                    (TeX-argument-prompt t nil "Relative caption width")))
         (float (and LaTeX-float        ; LaTeX-float can be nil, i.e.
                                        ; do not prompt
                     (TeX-read-string "(Optional) Float position: " LaTeX-float)))
         (caption (TeX-read-string "Caption: "))
         (short-caption (when (>= (length caption) LaTeX-short-caption-prompt-length)
                          (TeX-read-string "(Optional) Short caption: ")))
         (center (y-or-n-p "Center? "))
         (active-mark (and (TeX-active-mark)
                           (not (eq (mark) (point)))))
         start-marker end-marker)
    (when active-mark
      (if (< (mark) (point))
          (exchange-point-and-mark))
      (setq start-marker (point-marker))
      (set-marker-insertion-type start-marker t)
      (setq end-marker (copy-marker (mark))))
    (setq LaTeX-float float)
    (LaTeX-insert-environment environment
                              (concat
                               ;; First check if 'relwidth' is given:
                               (when (and relwidth
                                          (not (string= relwidth "")))
                                 (concat LaTeX-optop relwidth
                                         LaTeX-optcl))
                               ;; We have to insert a pair of brackets
                               ;; if 'float' is given and 'relwidth'
                               ;; was empty, otherwise 'float' becomes
                               ;; 'relwidth':
                               (unless (zerop (length float))
                                 (concat
                                  (when (or (null relwidth)
                                            (string= relwidth ""))
                                    (concat LaTeX-optop LaTeX-optcl))
                                  LaTeX-optop float LaTeX-optcl))))
    (when active-mark
      (goto-char start-marker)
      (set-marker start-marker nil))
    (when center
      (insert TeX-esc "centering")
      (indent-according-to-mode)
      (LaTeX-newline)
      (indent-according-to-mode))
    ;; Insert caption and ask for a label, do nothing if user skips caption
    (unless (zerop (length caption))
      (if (member environment LaTeX-top-caption-list)
          ;; top caption
          (progn
            (insert (LaTeX-compose-caption-macro caption short-caption))
            ;; If `auto-fill-mode' is active, fill the caption.
            (if auto-fill-function (LaTeX-fill-paragraph))
            (LaTeX-newline)
            (indent-according-to-mode)
            ;; ask for a label and insert a new line only if a label is
            ;; actually inserted
            (when (LaTeX-label environment 'environment)
              (LaTeX-newline)
              (indent-according-to-mode)))
        ;; bottom caption (default)
        (when active-mark (goto-char end-marker))
        (save-excursion
          (LaTeX-newline)
          (indent-according-to-mode)
          ;; If there is an active region point is before the backslash of
          ;; "\end" macro, go one line upwards.
          (when active-mark (forward-line -1) (indent-according-to-mode))
          (insert (LaTeX-compose-caption-macro caption short-caption))
          ;; If `auto-fill-mode' is active, fill the caption.
          (if auto-fill-function (LaTeX-fill-paragraph))
          ;; ask for a label and if necessary insert a new line between caption
          ;; and label
          (when (save-excursion (LaTeX-label environment 'environment))
            (LaTeX-newline)
            (indent-according-to-mode)))
        ;; Insert an empty line between caption and marked region, if any.
        (when active-mark (LaTeX-newline) (forward-line -1))
        (indent-according-to-mode)))
    (when (markerp end-marker)
      (set-marker end-marker nil))
    (when (and (member environment '("SCtable" "SCtable*"))
               ;; Suppose an existing tabular environment should just
               ;; be wrapped into a table if there is an active region.
               (not active-mark))
      (LaTeX-environment-menu LaTeX-default-tabular-environment))))

(TeX-add-style-hook
 "sidecap"
 (lambda ()

   ;; Add the environments provided by the package:
   (LaTeX-add-environments
    '("SCtable"   LaTeX-env-sidecap-float)
    '("SCtable*"  LaTeX-env-sidecap-float)
    '("SCfigure"  LaTeX-env-sidecap-float)
    '("SCfigure*" LaTeX-env-sidecap-float)
    '("wide"))

   ;; Add the float environments to `LaTeX-label-alist':
   (dolist (env '("SCfigure" "SCfigure*"))
     (add-to-list 'LaTeX-label-alist `(,env . LaTeX-figure-label) t))

   (dolist (env '("SCtable" "SCtable*"))
     (add-to-list 'LaTeX-label-alist `(,env . LaTeX-table-label) t))

   ;; The next 2 can be set with '\renewcommand':
   (TeX-add-symbols
    "sidecaptionsep"
    "sidecaptionrelwidth"))
 TeX-dialect)

(defvar LaTeX-sidecap-package-options
  '("outercaption" "innercaption"
    "leftcaption" "rightcaption"
    "wide"
    "raggedright" "raggedleft" "ragged")
  "Package options for the sidecap package.")

;;; sidecap.el ends here
