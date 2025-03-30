;;; lstlinebgrd.el --- AUCTeX style for `lstlinebgrd.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-08-21
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

;; This file adds support for `lstlinebgrd.sty' v0.2 from 2024-08-18.

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "lstlinebgrd"
 (lambda ()
   (TeX-run-style-hooks "listings" "xcolor"))
 TeX-dialect)

(defvar LaTeX-lstlinebgrd-package-options nil
  "Package options for the lstlinebgrd package.")

;;; lstlinebgrd.el ends here
