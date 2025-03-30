;;; jsbook.el - Special code for jsbook class.  -*- lexical-binding: t; -*-

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

(defvar LaTeX-jsbook-class-options
  '("a3paper" "a4paper" "a5paper" "a6paper" "b4paper" "b5paper" "b6paper"
    "a4j" "a5j" "b4j" "b5j" "a4var" "b5var" "letterpaper" "legalpaper"
    "executivepaper" "landscape" "report"
    "8pt" "9pt" "10pt" "11pt" "12pt" "14pt" "17pt" "20pt" "21pt" "25pt"
    "30pt" "36pt" "43pt" "12Q" "14Q" "usemag" "nomag" "nomag*"
    "tombow" "tombo" "mentuke" "oneside" "twoside" "vartwoside"
    "onecolumn" "twocolumn" "titlepage" "notitlepage"
    "openright" "openleft" "openany" "leqno" "fleqn"
    "disablejfam" "draft" "final" "mingoth" "wingoth" "jis"
    "uplatex" "autodetect-engine" "papersize" "english" "jslogo" "nojslogo")
  "Class options for the jsbook class.")

(TeX-add-style-hook
 "jsbook"
 (lambda ()
   (if (LaTeX-provided-class-options-member "jsbook" "report")
       (progn
         (LaTeX-largest-level-set "chapter")
         (LaTeX-add-environments "abstract"))
     (LaTeX-largest-level-set "part"))
   (LaTeX-add-counters "part" "chapter" "section" "subsection" "subsubsection"
                       "paragraph" "subparagraph" "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings"))
 TeX-dialect)

;;; jsbook.el ends here
