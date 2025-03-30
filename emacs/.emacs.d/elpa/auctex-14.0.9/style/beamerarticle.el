;;; beamerarticle.el --- AUCTeX style for the latex-beamerarticle class  -*- lexical-binding: t; -*-

;; Copyright (C) 2019--2023 Free Software Foundation, Inc.

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

;; Triggers the beamer style when using the beamerarticle style.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "beamerarticle"
 (lambda ()
   (TeX-run-style-hooks "beamer")

   (LaTeX-section-list-add-locally
    '(("paragraph" 4)
      ("subparagraph" 5)))))
