;;; report.el - Special code for report style.  -*- lexical-binding: t; -*-

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

(defvar LaTeX-report-class-options
  '("a4paper" "a5paper" "b5paper" "letterpaper" "legalpaper" "executivepaper"
    "landscape" "10pt" "11pt" "12pt" "oneside" "twoside" "draft" "final"
    "titlepage" "notitlepage" "openright" "openany" "onecolumn" "twocolumn"
    "leqno" "fleqn" "openbib")
  "Package options for the report class.")

(TeX-add-style-hook
 "report"
 (lambda ()
   (LaTeX-largest-level-set "chapter")
   (LaTeX-add-counters "part" "chapter" "section" "subsection" "subsubsection"
                       "paragraph" "subparagraph" "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings")
   (LaTeX-add-environments "abstract"))
 TeX-dialect)

;;; report.el ends here
