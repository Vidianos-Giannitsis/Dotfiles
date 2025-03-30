;;; diagbox.el --- AUCTeX style for `diagbox.sty' (v2.3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-05-30
;; Keywords: tex

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

;; This file adds support for `diagbox.sty' (v2.3) from 2020/02/09.
;; `diagbox.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-diagbox-key-val-options
  `(("width")
    ("height" ("\\line"))
    ("dir" ("NW" "NE" "SW" "SE"))
    ("innerwidth")
    ("innerleftsep")
    ("innerrightsep")
    ("outerleftsep")
    ("outerrightsep")
    ("leftsep")
    ("rightsep")
    ("trim" ("l" "r" "lr" "rl"))
    ("font" ,(mapcar (lambda (x) (concat TeX-esc x))
                     (append LaTeX-font-size LaTeX-font-shape
                             LaTeX-font-series)))
    ("linewidth"))
  "Key=val options for the \\diagbox macro.")

(defun LaTeX-diagbox-key-val-options ()
  "Return updated key=val options for the \\diagbox macro."
  (append
   `(("linecolor" ,(cond ((and (fboundp 'LaTeX-xcolor-definecolor-list)
                               (member "xcolor" (TeX-style-list)))
                          (mapcar #'car (LaTeX-xcolor-definecolor-list)))
                         ((and (fboundp 'LaTeX-color-definecolor-list)
                               (member "color" TeX-active-styles))
                          (mapcar #'car (LaTeX-color-definecolor-list))))))
   LaTeX-diagbox-key-val-options))

(TeX-add-style-hook
 "diagbox"
 (lambda ()
   (TeX-add-symbols
    '("diagbox"
      [TeX-arg-key-val (LaTeX-diagbox-key-val-options)]
      (TeX-arg-conditional (y-or-n-p "With 2 arguments? ")
          (2)
        (3))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("diagbox" "[{{"))
                              ;; Fontify only 2 args and not 3 since the
                              ;; last one is somehow optional
                              'textual)))
 TeX-dialect)

(defvar LaTeX-diagbox-package-options nil
  "Package options for the diagbox package.")

;;; diagbox.el ends here
