;;; graphics.el --- Handle graphical commands in LaTeX 2e.  -*- lexical-binding: t; -*-

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

;; Load "graphicx" explicitly to access `LaTeX-graphicx-package-options'
;; before running style hook "graphics".  This is necessary to have
;; support for completion of package options of "usepackage".

(require 'tex)

(TeX-load-style "graphicx")
(defvar LaTeX-graphics-package-options LaTeX-graphicx-package-options)

(TeX-add-style-hook
 "graphics"
 (lambda ()
   (TeX-run-style-hooks "graphicx"))
 TeX-dialect)

;;; graphics.el ends here.
