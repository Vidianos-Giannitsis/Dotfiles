;;; jarticle.el - Special code for jarticle class.  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'tex)
(require 'latex)

(defvar LaTeX-jarticle-class-options
  '("a4paper" "a5paper" "b4paper" "b5paper" "a4j" "a5j" "b4j" "b5j"
    "a4p" "b4p" "b5p" "10pt" "11pt" "12pt" "landscape" "tombow" "tombo"
    "mentuke" "oneside" "twoside" "onecolumn" "twocolumn"
    "titlepage" "notitlepage" "leqno" "fleqn"
    "openbib" "disablejfam" "mathrmmc" "draft" "final")
  "Class options for the jarticle class.")

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")
   (LaTeX-add-counters "part" "section" "subsection" "subsubsection" "paragraph"
                       "subparagraph" "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings")
   (LaTeX-add-environments "abstract"))
 TeX-dialect)

;;; jarticle.el ends here
