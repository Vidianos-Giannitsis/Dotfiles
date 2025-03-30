;;; enumerate.el - AUCTeX style for `enumerate.sty' (v3.00) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2025-01-13
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

;; This file adds support for `enumerate.sty' (v3.00) from 2023-07-04.
;; `enumerate.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "enumerate"
 (lambda ()
   (LaTeX-add-environments
    '("enumerate" LaTeX-env-item-args ["Counter style"])))
 TeX-dialect)

(defvar LaTeX-enumerate-package-options nil
  "Package options for the enumerate package.")

;;; enumerate.el ends here
