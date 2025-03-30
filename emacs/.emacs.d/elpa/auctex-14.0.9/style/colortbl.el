;;; colortbl.el --- AUCTeX style for `colortbl.sty' (v1.0a)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-03-22
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

;; This file adds support for `colortbl.sty' (v1.0a) from 2012/02/13.
;; `colortbl.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "colortbl"
 (lambda ()

   ;; array.el is always loaded:
   (TeX-run-style-hooks "array")

   ;; Load color.el only if xcolor.el is not already loaded.  This is
   ;; mainly for the option `table' from xcolor.sty which loads
   ;; colortbl.sty where we don't want to load color.el:
   (if (member "xcolor" (TeX-style-list))
       ;; xcolor.sty
       (TeX-add-symbols
        '("columncolor"
          [TeX-arg-completing-read-multiple (LaTeX-xcolor-color-models)
                                            "Color model"
                                            nil nil "/" "/"]
          (TeX-arg-conditional (LaTeX-xcolor-cmd-requires-spec-p 'col)
              (TeX-arg-xcolor)
            ((TeX-arg-completing-read (LaTeX-xcolor-definecolor-list)
                                      "Color name")))
          [TeX-arg-length "Left overhang"] [TeX-arg-length "Right overhang"] )

        '("rowcolor"
          [TeX-arg-completing-read-multiple (LaTeX-xcolor-color-models)
                                            "Color model"
                                            nil nil "/" "/"]
          (TeX-arg-conditional (LaTeX-xcolor-cmd-requires-spec-p 'col)
              (TeX-arg-xcolor)
            ((TeX-arg-completing-read (LaTeX-xcolor-definecolor-list)
                                      "Color name")))
          [TeX-arg-length "Left overhang"] [TeX-arg-length "Right overhang"] )

        '("cellcolor"
          [TeX-arg-completing-read-multiple (LaTeX-xcolor-color-models)
                                            "Color model"
                                            nil nil "/" "/"]
          (TeX-arg-conditional (LaTeX-xcolor-cmd-requires-spec-p 'col)
              (TeX-arg-xcolor)
            ((TeX-arg-completing-read (LaTeX-xcolor-definecolor-list)
                                      "Color name")))
          [TeX-arg-length "Left overhang"] [TeX-arg-length "Right overhang"] )

        '("arrayrulecolor"
          [TeX-arg-completing-read-multiple (LaTeX-xcolor-color-models)
                                            "Color model"
                                            nil nil "/" "/"]
          (TeX-arg-conditional (LaTeX-xcolor-cmd-requires-spec-p 'col)
              (TeX-arg-xcolor)
            ((TeX-arg-completing-read (LaTeX-xcolor-definecolor-list)
                                      "Color name"))))

        '("doublerulesepcolor"
          [TeX-arg-completing-read-multiple (LaTeX-xcolor-color-models)
                                            "Color model"
                                            nil nil "/" "/"]
          (TeX-arg-conditional (LaTeX-xcolor-cmd-requires-spec-p 'col)
              (TeX-arg-xcolor)
            ((TeX-arg-completing-read (LaTeX-xcolor-definecolor-list)
                                      "Color name")))))
     ;; color.sty
     (TeX-run-style-hooks "color")
     (TeX-add-symbols
      '("columncolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name")))
        [TeX-arg-length "Left overhang"] [TeX-arg-length "Right overhang"])

      '("rowcolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name")))
        [TeX-arg-length "Left overhang"] [TeX-arg-length "Right overhang"])

      '("cellcolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name")))
        [TeX-arg-length "Left overhang"] [TeX-arg-length "Right overhang"] )

      '("arrayrulecolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name"))))

      '("doublerulesepcolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name"))))))

   (LaTeX-add-lengths "minrowclearance")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("columncolor"  "[{[[")
                                ("rowcolor"     "[{[[")
                                ("cellcolor"    "[{[[")
                                ("arrayrulecolor"     "[{")
                                ("doublerulesepcolor" "[{"))
                              'function)))
 TeX-dialect)

;; colortbl.sty has one option `debugshow'.  We ignore that since it
;; would only take more time during insertation in a buffer and we
;; presume that not many users use it anyway.
(defvar LaTeX-colortbl-package-options nil
  "Package option for the colortbl package.")

;;; colortbl.el ends here
