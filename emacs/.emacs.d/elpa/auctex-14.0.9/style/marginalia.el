;;; marginalia.el --- AUCTeX style for `marginalia.sty' (v0.80.1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2025-02-18
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

;; This file adds support for `marginalia.sty' (v0.80.1) from
;; 2025-02-17.  `marginalia.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(defun LaTeX-marginalia-key-val-options ()
  "Return an updated key=val alist for the marginalia package."
  (let ((len (mapcar (lambda (x) (concat TeX-esc (car x)))
                     (LaTeX-length-list)))
        (macs '("xsep" "xsep outer" "xsep inner" "xsep between"
                "xsep recto outer"  "xsep recto inner"
                "xsep verso outer" "xsep verso inner"
                "xsep right between" "xsep left between"
                "yshift" "ysep" "ysep above" "ysep below" "ysep page top"
                "ysep page top"
                "width" "width outer" "width inner" "width between"
                "width recto outer" "width recto inner"
                "width verso outer" "width verso inner"
                "width right between" "width left between"))
        (fnt (mapcar (lambda (x) (concat TeX-esc x))
                     (append LaTeX-font-family LaTeX-font-series
                             LaTeX-font-size LaTeX-font-shape)))
        (macs1 '("style"
                 "style recto outer"
                 "style recto inner"
                 "style verso outer"
                 "style verso inner"
                 "style right between"
                 "style left between"))
        result)
    (append
     (prog1
         (dolist (mac macs result) (push `(,mac ,len) result))
       (setq result nil))
     (dolist (mac macs1 result) (push `(,mac ,fnt) result))
     '(("type" ("normal" "fixed" "optfixed"))
       ("pos" ("auto" "reverse" "left" "right" "nearest"))
       ("column" ("auto" "one" "left" "right"))
       ("valign" ("t" "b"))
       ("style")
       ("style recto outer")
       ("style recto inner")
       ("style verso outer")
       ("style verso inner")
       ("style right between")
       ("style left between")))))

(TeX-add-style-hook
 "marginalia"
 (lambda ()

   ;; marginalia.sty is LuaLaTeX only:
   (TeX-check-engine-add-engines 'luatex)

   ;; 5 User commands
   (TeX-add-symbols
    '("marginalia"
      [TeX-arg-key-val (LaTeX-marginalia-key-val-options)]
      "Text")
    '("marginaliasetup"
      (TeX-arg-key-val (LaTeX-marginalia-key-val-options)))
    '("marginalianewgeometry" 0))

   ;; 5.1 Access to page and column
   (LaTeX-add-counters "marginaliapage" "marginaliacolumn")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("marginalia"  "[{"))
                              'reference)
     (font-latex-add-keywords '(("marginaliasetup" "{"))
                              'function)
     (font-latex-add-keywords '("marginalianewgeometry")
                              'function-noarg)))
 TeX-dialect)

(defvar LaTeX-marginalia-package-options nil
  "Package options for the marginalia package.")

;;; marginalia.el ends here
