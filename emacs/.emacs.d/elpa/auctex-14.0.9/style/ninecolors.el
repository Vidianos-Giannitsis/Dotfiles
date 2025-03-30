;;; ninecolors.el --- AUCTeX style for `ninecolors.sty' (v2022D)  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-09-13
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

;; This file adds support for `ninecolors.sty' (v2022D) from
;; 2022/02/13.  `ninecolors.sty' is part of TeXLive.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function LaTeX-add-xcolor-definecolors "xcolor"
                  (&rest xcolor-definecolors))
(declare-function font-latex-add-keywords "font-latex"
                  (keywords class))

(defvar LaTeX-ninecolors-colornames
  '("gray1"    "red1"      "brown1"  "yellow1"  "olive1"
    "gray2"    "red2"      "brown2"  "yellow2"  "olive2"
    "gray3"    "red3"      "brown3"  "yellow3"  "olive3"
    "gray4"    "red4"      "brown4"  "yellow4"  "olive4"
    "gray5"    "red5"      "brown5"  "yellow5"  "olive5"
    "gray6"    "red6"      "brown6"  "yellow6"  "olive6"
    "gray7"    "red7"      "brown7"  "yellow7"  "olive7"
    "gray8"    "red8"      "brown8"  "yellow8"  "olive8"
    "gray9"    "red9"      "brown9"  "yellow9"  "olive9"
    "green1"   "teal1"     "cyan1"   "azure1"   "blue1"
    "green2"   "teal2"     "cyan2"   "azure2"   "blue2"
    "green3"   "teal3"     "cyan3"   "azure3"   "blue3"
    "green4"   "teal4"     "cyan4"   "azure4"   "blue4"
    "green5"   "teal5"     "cyan5"   "azure5"   "blue5"
    "green6"   "teal6"     "cyan6"   "azure6"   "blue6"
    "green7"   "teal7"     "cyan7"   "azure7"   "blue7"
    "green8"   "teal8"     "cyan8"   "azure8"   "blue8"
    "green9"   "teal9"     "cyan9"   "azure9"   "blue9"
    "violet1"  "magenta1"  "purple1"
    "violet2"  "magenta2"  "purple2"
    "violet3"  "magenta3"  "purple3"
    "violet4"  "magenta4"  "purple4"
    "violet5"  "magenta5"  "purple5"
    "violet6"  "magenta6"  "purple6"
    "violet7"  "magenta7"  "purple7"
    "violet8"  "magenta8"  "purple8"
    "violet9"  "magenta9" " purple9")
  "List of colors provided by ninecolors package.")

(TeX-add-style-hook
 "ninecolors"
 (lambda ()
   ;; Run the style hook for xcolor.sty:
   (TeX-run-style-hooks "xcolor")

   ;; Make the colors defined in ninecolors.sty available:
   (apply #'LaTeX-add-xcolor-definecolors LaTeX-ninecolors-colornames)

   ;; Cater for the only macro provided by the package:
   (TeX-add-symbols
    '("NineColors"
      (TeX-arg-key-val (("saturation" ("low" "medium" "high"))))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("NineColors" "{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-ninecolors-package-options nil
  "Package options for the ninecolors package.")

;;; ninecolors.el ends here
