;;; plext.el --- AUCTeX style for the plext package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014--2024 Free Software Foundation, Inc.

;; Author: Ikumi Keita <ikumi@ikumi.que.jp>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-07-05
;; Keywords: tex, japanese

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for the plext package.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "plext"
 (lambda ()
   ;; plext.sty extends some environments to accept option, e.g. <t>,
   ;; for vertical typesetting.
   (LaTeX-add-environments
    ;; TODO: Add support for minipage and picture
    ;; environments extension.
    '("array"    LaTeX-plext-env-array)
    '("tabular"  LaTeX-plext-env-array)
    '("tabular*" LaTeX-plext-env-array))

   (setq-local LaTeX-array-skipping-regexp
               (concat "\\(?:<[tyz]>\\)?[ \t]*"
                       (regexp-opt '("[t]" "[b]" ""))))
   (setq-local LaTeX-tabular*-skipping-regexp
               (concat "\\(?:<[tyz]>\\)?[ \t]*{[^}]*}[ \t]*"
                       (regexp-opt '("[t]" "[b]" "")))))
 TeX-dialect)

(defun LaTeX-plext-env-array (env)
  (let ((dir (TeX-read-string "(Optional) Direction (t or y or z): "))
        (width (if (string= env "tabular*")
                   (TeX-read-string
                    (format "Width (default %s): " LaTeX-default-width)
                    nil nil LaTeX-default-width)))
        (pos (and LaTeX-default-position ; LaTeX-default-position can
                                        ; be nil, i.e. do not prompt
                  (TeX-read-string "(Optional) Position: " LaTeX-default-position)))
        (fmt (TeX-read-string
              (if (string= LaTeX-default-format "")
                  "Format: "
                (format "Format (default %s): " LaTeX-default-format))
              nil nil
              (if (string= LaTeX-default-format "")
                  nil
                LaTeX-default-format))))
    (unless (zerop (length dir))
      (setq dir (concat "<" dir ">")))
    (if (string= env "tabular*")
        (setq LaTeX-default-width width))
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment env
                              (concat
                               dir
                               (if (string= env "tabular*")
                                   (concat TeX-grop width TeX-grcl))
                               (unless (zerop (length pos))
                                 (concat LaTeX-optop pos LaTeX-optcl))
                               (concat TeX-grop fmt TeX-grcl)))
    (if (string= env "tabular*")
        (LaTeX-item-tabular* t)
      (LaTeX-item-array t))))

;;; plext.el ends here.
