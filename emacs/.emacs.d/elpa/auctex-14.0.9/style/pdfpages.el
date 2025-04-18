;;; pdfpages.el --- AUCTeX style for `pdfpages.sty' (v0.6d)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-05-23
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

;; This file adds support for `pdfpages.sty' (v0.6d) from 2025/01/20.
;; `pdfpages.sty' is part of TeXLive.

;; Thanks to Andreas Matthias for testing this style and writing
;; `pdfpages.sty' in the first place.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-pdfpages-key-val-options
  '(;; Main options:
    ("pages" ("-"))
    ("nup")
    ("landscape"    ("true" "false"))
    ;; Layout options:
    ("delta")
    ("offset")
    ("frame"         ("true" "false"))
    ("column"        ("true" "false"))
    ("columnstrict"  ("true" "false"))
    ("openright"     ("true" "false"))
    ("openrighteach" ("true" "false"))
    ("pagecommand")
    ("pagecommand*")
    ("turn"         ("true" "false"))
    ("noautoscale"  ("true" "false"))
    ("fitpaper"     ("true" "false"))
    ("reflect"      ("true" "false"))
    ("signature")
    ("signature*")
    ("booklet"         ("true" "false"))
    ("booklet*"        ("true" "false"))
    ("flip-other-edge" ("true" "false"))
    ("picturecommand")
    ("picturecommand*")
    ("pagetemplate")
    ("templatesize")
    ("rotateoversize"       ("true" "false"))
    ("doublepages"          ("true" "false"))
    ("doublepagestwist"     ("true" "false"))
    ("doublepagestwistodd"  ("true" "false"))
    ("doublepagestwist*"    ("true" "false"))
    ("doublepagestwistodd*" ("true" "false"))
    ("duplicatepages")
    ;; Miscellaneous options:
    ("lastpage")
    ;; Hypertext options:
    ("link"                 ("true" "false"))
    ("linkname")
    ("thread"               ("true" "false"))
    ("threadname")
    ("linktodoc"            ("true" "false"))
    ;; Additional hypertext options:
    ("linkfit"      ("Fit"  "FitH "  "FitV "  "FitB"  "FitBH "  "FitBV "  "Region"))
    ("linktodocfit" ("/Fit" "/FitH " "/FitV " "/FitB" "/FitBH " "/FitBV " "/Region"))
    ("newwindow" ("true" "false"))
    ("linkfilename")
    ;; Experimental options: (Syntax may change in future versions!)
    ("addtotoc")
    ("addtolist")
    ("survey"               ("true" "false"))
    ("survey-nolink"        ("true" "false"))
    ("xr-prefix"))
  "Key=value options for pdfpages macros.")

(TeX-add-style-hook
 "pdfpages"
 (lambda ()
   ;; Run style hook for packages loaded by pdfpages; all packages
   ;; are required for running LaTeX, but not necessary within AUCTeX
   (TeX-run-style-hooks "graphicx" "eso-pic" "everyshi" "ifthen" "calc")

   (TeX-add-symbols
    ;; \includepdf[<options>]{<filename>}
    `("includepdf"
      [TeX-arg-key-val LaTeX-pdfpages-key-val-options]
      ,(lambda (optional)
         (let ((pdffile (file-relative-name
                         (read-file-name
                          (TeX-argument-prompt optional nil "File to include")
                          nil nil nil nil
                          (lambda (pdfs)
                            (or (file-directory-p pdfs)
                                (string-match "\\.pdf\\'" pdfs))))
                         (TeX-master-directory))))
           (TeX-argument-insert pdffile optional))))

    ;; \includepdfmerge[<options>]{<file-page-list>}
    ;; The mandatory argument is complex, we just insert a pair of
    ;; braces and leave the rest to the user
    '("includepdfmerge"
      [TeX-arg-key-val LaTeX-pdfpages-key-val-options] t)

    ;; \includepdfset{<options>}
    '("includepdfset"
      (TeX-arg-key-val LaTeX-pdfpages-key-val-options))

    '("threadinfodict" 0))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("includepdfset"    "{"))
                              'function)
     (font-latex-add-keywords '(("includepdf"       "[{")
                                ("includepdfmerge"  "[{"))
                              'reference)))
 TeX-dialect)

(defvar LaTeX-pdfpages-package-options
  '("final" "draft" "enable-survey")
  "Prompt for package options for the pdfpages package.")

;;; pdfpages.el ends here
