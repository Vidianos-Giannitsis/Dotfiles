;;; CJKutf8.el --- AUCTeX style for the CJKutf8 package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2020 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2009-01-04
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

;; This file adds support for the CJKutf8 package.

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "CJKutf8"
 (lambda ()
   (TeX-run-style-hooks "CJK"))
 TeX-dialect)

;;; CJKutf8.el ends here
