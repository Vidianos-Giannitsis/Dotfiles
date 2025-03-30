;;; scrartcl.el -- AUCTeX style for scrartcl.cls  -*- lexical-binding: t; -*-

;; Copyright (C) 2002--2024 Free Software Foundation

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
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

;; This file adds support for `scrartcl.cls'. This file needs
;; `scrbase.el'.

;; This file is part of  AUCTeX.

(require 'tex)
(require 'latex)

;;; Code:
(TeX-add-style-hook
 "scrartcl"
 (lambda ()
   (LaTeX-largest-level-set "section")
   ;; load basic definitons
   (TeX-run-style-hooks "scrbase")
   (LaTeX-add-environments "abstract"))
 TeX-dialect)

;;; scrartcl.el ends here
