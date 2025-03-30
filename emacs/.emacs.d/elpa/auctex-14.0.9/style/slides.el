;;; slides.el --- AUCTeX style for the `slides' document class  -*- lexical-binding: t; -*-

;; Copyright (C) 2004, 2013, 2020 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-04-21
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

;; This file adds support for the `slides' document class.  Currently
;; the support is very limited.  You are welcome to improve it.

;;; Code:

(require 'tex)
(require 'latex)

(defvar LaTeX-slides-class-options
  '("a4paper" "a5paper" "b5paper" "letterpaper" "legalpaper" "executivepaper"
    "landscape" "clock" "draft" "final" "titlepage" "notitlepage" "onecolumn"
    "twocolumn" "leqno" "fleqn")
  "Package options for the slides class.")

(TeX-add-style-hook
 "slides"
 (lambda ()
   (LaTeX-add-environments "slide"
                           "overlay"
                           "note")
   (LaTeX-add-counters "minutes" "seconds")
   (LaTeX-add-pagestyles "headings" "slide" "overlay" "note")
   (setq LaTeX-default-document-environment "slide"))
 TeX-dialect)

;;; slides.el ends here
