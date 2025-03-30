;;; newzealand.el --- AUCTeX style for the `newzealand' babel option.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2017-07-02
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

;; Set up AUCTeX for editing Newzealand text in connection with the
;; `newzealand' babel option.

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "newzealand"
 (lambda ()
   (TeX-run-style-hooks "english"))
 TeX-dialect)

;;; newzealand.el ends here
