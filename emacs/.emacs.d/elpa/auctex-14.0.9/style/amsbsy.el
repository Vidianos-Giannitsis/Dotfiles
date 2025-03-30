;;; amsbsy.el --- Style hook for the AMS-LaTeX amsbsy package.  -*- lexical-binding: t; -*-

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>

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
 "amsbsy"
 (lambda ()
   (TeX-add-symbols
    '("boldsymbol" "Symbol")
    '("pmb"        "Symbol")))
 TeX-dialect)

(defvar LaTeX-amsbsy-package-options nil
  "Package options for the amsbsy package.")

;;; amsbsy.el ends here.
