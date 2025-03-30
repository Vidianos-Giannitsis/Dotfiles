;;; elsarticle.el - AUCTeX style for `elsarticle.cls' (v3.4c) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2025-01-12
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

;; This file adds support for `elsarticle.cls' (v3.4c) from 2025-01-11.
;; `elsarticle.cls' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(declare-function font-latex-set-syntactic-keywords "font-latex")
(defvar LaTeX-article-class-options)

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "elsarticle"
 (lambda ()

   ;; Load the styles for files which are always loaded:
   (TeX-run-style-hooks "article" "graphicx")

   ;; Load natbib.el based on given option:
   (unless (LaTeX-provided-class-options-member "elsarticle" "nonatbib")
     (TeX-run-style-hooks "natbib"))

   (TeX-add-symbols
    ;; 5. Frontmatter
    '("author" ["Marker"] LaTeX-arg-author)

    '("tnoteref" "Label(s)")
    '("corref"   "Label(s)")
    '("fnref"    "Label(s)")

    '("tnotetext" ["Label"] "Title note")
    '("cortext"   ["Label"] "Corresponding author note")
    '("fntext"    ["Label"] "Author footnote")

    '("affiliation" ["Label"] (TeX-arg-key-val (("organization")
                                                ("addressline")
                                                ("city")
                                                ("citysep")
                                                ("postcode")
                                                ("postcodesep")
                                                ("state")
                                                ("country"))))
    '("ead" ["Type"] "Address")

    ;; 5.1 New pages
    '("newpageafter"
      (TeX-arg-completing-read ("title" "author" "abstract"))))

   ;; Cater for verbatim-like content:
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "ead")

   (LaTeX-add-environments
    ;; 8. Enumerated and Itemized Lists
    '("enumerate"  LaTeX-env-item-args ["Counter style"])
    '("itemize"    LaTeX-env-item-args ["Item label"])

    ;; 12. Graphical abstract and highlights
    '("highlights" LaTeX-env-item)
    "graphicalabstract"
    "keyword")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newpageafter" "{"))
                              'function)
     (font-latex-add-keywords '(("author" "[{"))
                              'textual)
     (font-latex-add-keywords '(("tnoteref"    "{")
                                ("corref"      "{")
                                ("fnref"       "{")
                                ("tnotetext"   "[{")
                                ("cortext"     "[{")
                                ("fntext"      "[{")
                                ("affiliation" "[{")
                                ("ead"         "["))
                              'reference)

     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defvar LaTeX-elsarticle-class-options
  (progn
    (TeX-load-style "article")
    (append LaTeX-article-class-options
            '("preprint" "review" "1p" "3p" "5p"
              "authoryear" "number" "sort&compress" "longtitle"
              "times" "reversenotenum" "lefttitle" "centertitle"
              "endfloat" "nonatbib" "doubleblind")))
  "Class options for the elsarticle class.")

;;; elsarticle.el ends here
