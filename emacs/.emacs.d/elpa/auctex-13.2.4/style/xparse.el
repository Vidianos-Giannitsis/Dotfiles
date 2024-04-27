;;; xparse.el --- AUCTeX style for `xparse.sty' version 2022-07-05  -*- lexical-binding: t; -*-

;; Copyright (C) 2013--2023 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <mose@gnu.org>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; The content of this file is now (December 2023) part of latex.el
;; and font-latex.el.  This style provides only completion for xparse
;; package options.

;; The "yet not more supported" specifiers `l', `u', `g' and `G' are
;; ignored completely and may lead to wrong parsing results.

;;; Code:

(require 'latex)

(defvar LaTeX-xparse-package-options-list
  '(("log-declarations" ("true" "false")))
  "Package options for the xparse package.")

(defun LaTeX-xparse-package-options ()
  "Read the xparse package options from the user."
  (TeX-read-key-val t LaTeX-xparse-package-options-list))

;;; xparse.el ends here
