;;; langscibook.el --- AUCTeX style for `langscibook.cls'   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-08-15
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

;; This file adds support for `langscibook.cls' from 2024/02/07.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(defvar LaTeX-babel-language-list)

(TeX-add-style-hook
 "langscibook"
 (lambda ()

   (TeX-check-engine-add-engines 'xetex)
   (TeX-run-style-hooks "scrbook")

   (TeX-add-symbols
    '("abstract" t)

    ;; 5.3 Metadata commands
    "BackBody"
    "BackTitle"
    "BookDOI"
    "ChapterDOI"
    "dedication"
    "lsISBNdigital"
    "lsISBNhardcover"
    "lsISBNsoftcover"
    "lsSeries"
    "lsSeriesNumber"
    "lsID"

    ;; 5.4 Overrides
    "lsImpressionCitationAuthor"
    "lsImpressionCitationText"
    "papernote"
    "lsChapterFooterSize"
    "includespinelogo"
    "includestoragelogo"
    "includepublisherlogo"
    "includechapterfooterlogo"
    "publisherstreetaddress"
    "publisherurl"
    "storageinstitution"
    "githubtext"
    "paperhivetext"
    "lsURL"

    ;; 5.5 Command redefinitions
    "CoverTitleSizes"
    "lsBackBodyFont"
    "lsBackTitleFont"
    "lsCopyright"
    "lsCoverAuthorFont"
    "lsCoverSubTitleFont"
    "lsCoverTitleFont"
    "lsEditorPrefix"
    "lsEditorSuffix"
    "lsLanguageIndexTitle"
    "lsNameIndexTitle"
    "lsSubjectIndexTitle"
    "lsYear"

    ;; 5.6 Additions
    "lsAdditionalFontsImprint"
    "lsImpressumExtra")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("abstract" "{"))
                              'textual)))
 TeX-dialect)

(defvar LaTeX-langscibook-class-options-list
  (progn
    (TeX-load-style "babel")
    `(("collection")
      ("draftmode")
      ("openreview")
      ("minimal")
      ("nobabel")
      ("showindex")
      ("chinesefont")
      ("japanesefont")
      ("hebrewfont")
      ("arabicfont")
      ("syriacfont")
      ("output" ("book" "paper" "minimal" "guidelines"))
      ("booklanguage" ,LaTeX-babel-language-list)
      ("copyright" ("CC-BY"))))
  "Class options for the langscibook class.")

(defun LaTeX-langscibook-class-options ()
  "Class options for the langscibook class."
  (TeX-read-key-val t LaTeX-langscibook-class-options-list))

;;; langscibook.el ends here
