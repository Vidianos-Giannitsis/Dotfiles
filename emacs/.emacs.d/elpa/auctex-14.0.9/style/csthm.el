;;; csthm.el --- AUCTeX style for `csthm.sty' (v1.3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2025-01-21
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

;; This file adds support for `csthm.sty' (v1.3) from 2025-01-16.
;; `csthm.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(declare-function LaTeX-add-enumitem-newlists "enumitem")
(declare-function LaTeX-add-thmtools-declaretheorems "thmtools")
(declare-function LaTeX-add-thmtools-declaretheoremstyles "thmtools")

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "csthm"
 (lambda ()

   ;; Load the mandatory style hooks:
   (TeX-run-style-hooks "amsmath" "amssymb" "amsthm"
                        "enumitem" "thmtools" "xcolor")

   ;; 1.2. Basic Usage: cleveref is loaded with the right option and
   ;; hyperref being loaded as well:
   (when (and (LaTeX-provided-package-options-member "csthm" "cleveref")
              (member "hyperref" (TeX-style-list)))
     (TeX-run-style-hooks "cleveref"))

   ;; 2. Environments and styles:
   (LaTeX-add-thmtools-declaretheoremstyles "thmstyle"
                                            "defstyle"
                                            "remarkstyle"
                                            "hltstyle"
                                            "proofstyle")

   ;; These have a numbered and an unnumbered version:
   (let ((envs '(;; Mathematical Theorems
                 "theorem"
                 "assumption"
                 "axiom"
                 "claim"
                 "conjecture"
                 "corollary"
                 "fact"
                 "hypothesis"
                 "lemma"
                 "property"
                 "proposition"
                 ;; Definitions and Protocols
                 "definition"
                 "notation"
                 "problem"
                 "protocol"
                 ;; Remarks and Examples
                 "example"
                 "note"
                 "remark"
                 ;; Highlights and Important Elements
                 "exercise"
                 "highlight"
                 "important"
                 "keypoint")))
     (apply #'LaTeX-add-thmtools-declaretheorems envs)
     (apply #'LaTeX-add-thmtools-declaretheorems (mapcar (lambda (x)
                                                           (concat x "*"))
                                                         envs)))

   ;; Proof doesn't have a starred version:
   (LaTeX-add-thmtools-declaretheorems "proof")

   ;; case is a list defined with enumitem:
   (LaTeX-add-enumitem-newlists '("case" "enumerate"))

   ;; 2.7. Customization
   (TeX-add-symbols
    '("setaccentcolor"
      (TeX-arg-completing-read (LaTeX-xcolor-definecolor-list)
                               "Color name"))
    "accentcolor")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setaccentcolor" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-csthm-package-options '("cleveref")
  "Package options for the csthm package.")

;;; csthm.el ends here
