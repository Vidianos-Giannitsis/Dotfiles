;;; j-report.el - Special code for j-report style.  -*- lexical-binding: t; -*-

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

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter"))
 TeX-dialect)

;;; j-report.el ends here
