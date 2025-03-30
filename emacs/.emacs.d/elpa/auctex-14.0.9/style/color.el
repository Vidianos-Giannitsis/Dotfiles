;;; color.el --- AUCTeX style for `color.sty' (v1.3d)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2015-01-16
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

;; This file adds support for `color.sty' (v1.3d) from 2022/01/06.
;; `color.sty' is part of TeXLive.

;; Many thanks to Tassilo Horn for his percetive comments on
;; implementation of this style and testing.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-color-colour-models
  '("cmyk" "gray" "named" "rgb")
  "List of color models provided by `color.sty'.")

(defvar LaTeX-color-dvipsnames-colors
  '("Apricot"        "Aquamarine"      "Bittersweet"  "Black"
    "Blue"           "BlueGreen"       "BlueViolet"   "BrickRed"
    "Brown"          "BurntOrange"     "CadetBlue"    "CarnationPink"
    "Cerulean"       "CornflowerBlue"  "Cyan"         "Dandelion"
    "DarkOrchid"     "Emerald"         "ForestGreen"  "Fuchsia"
    "Goldenrod"      "Gray"            "Green"        "GreenYellow"
    "JungleGreen"    "Lavender"        "LimeGreen"    "Magenta"
    "Mahogany"       "Maroon"          "Melon"        "MidnightBlue"
    "Mulberry"       "NavyBlue"        "OliveGreen"   "Orange"
    "OrangeRed"      "Orchid"          "Peach"        "Periwinkle"
    "PineGreen"      "Plum"            "ProcessBlue"  "Purple"
    "RawSienna"      "Red"             "RedOrange"    "RedViolet"
    "Rhodamine"      "RoyalBlue"       "RoyalPurple"  "RubineRed"
    "Salmon"         "SeaGreen"        "Sepia"        "SkyBlue"
    "SpringGreen"    "Tan"             "TealBlue"     "Thistle"
    "Turquoise"      "Violet"          "VioletRed"    "White"
    "WildStrawberry" "Yellow"          "YellowGreen"  "YellowOrange")
  "List of colors defined by package option `dvipsnames' from `color.sty'.")

;; Plug \definecolor into the parser
(TeX-auto-add-type "color-definecolor" "LaTeX")

(defvar LaTeX-color-definecolor-regexp
  '("\\\\definecolor{\\([^}]+\\)}" 1 LaTeX-auto-color-definecolor)
  "Matches the argument of \\definecolor from color package.")

(defun LaTeX-color-auto-prepare ()
  "Clear `LaTeX-auto-color-definecolor' before parsing."
  (setq LaTeX-auto-color-definecolor nil))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-color-auto-prepare t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defconst LaTeX-color-used-model-regexp
  (concat (regexp-quote TeX-esc)
          (regexp-opt '("color" "textcolor"
                        "mathcolor" "pagecolor"
                        "colorbox" "fcolorbox"
                        ;; changebar.el
                        "cbcolor"
                        ;; colortbl.el
                        "columncolor" "rowcolor" "cellcolor"
                        "arrayrulecolor" "doublerulesepcolor"
                        ;; paracol.el also provides a columncolor which
                        ;; we don't repeat:
                        "colseprulecolor"))
          "\\(?:\\[\\([^]]+\\)\\]\\)?")
  "Regexp for matching the optional argument of color macros.")

(defvar-local LaTeX-color-used-model nil
  "Variable containing the color model from last search.
It is set by the function `LaTeX-color-used-model'.")

(defun LaTeX-color-used-model-requires-spec-p ()
  "Return non-nil if the used color model requires color specification."
  (and (save-excursion
         (re-search-backward LaTeX-color-used-model-regexp
                             (line-beginning-position) t)
         (setq LaTeX-color-used-model (match-string-no-properties 1)))
       (not (string= LaTeX-color-used-model "named"))))

(defun LaTeX-color-available-models ()
  "Return a list of available color models."
  (if (or (LaTeX-provided-package-options-member "color" "dvips")
          (LaTeX-provided-package-options-member "color" "dvipsnames"))
      LaTeX-color-colour-models
    (remove "named" LaTeX-color-colour-models)))

(defun LaTeX-color-available-colors ()
  "Return a list of available colors."
  (if (string= LaTeX-color-used-model "named")
      LaTeX-color-dvipsnames-colors
    (LaTeX-color-definecolor-list)))

(defun TeX-arg-color-definecolor (optional)
  "Insert <color spec> argument of \\definecolor from color.sty.
If OPTIONAL is non-nil, insert the argument when non-empty and in
brackets.  The \"named\" color model is handled inside the hook and not
in this function."
  ;; \definecolor{<name>}{<model>}{<color spec>}
  ;; Depending on <model>, ask for <color spec> and insert it
  (pcase LaTeX-color-used-model
    ;; <cmyk> model
    ("cmyk" (let ((cyan    (TeX-read-string "Value Cyan (between 0,1): "))
                  (magenta (TeX-read-string "Value Magenta (between 0,1): "))
                  (yellow  (TeX-read-string "Value Yellow (between 0,1): "))
                  (black   (TeX-read-string "Value Black (between 0,1): ")))
              (TeX-argument-insert
               (mapconcat #'identity (list cyan magenta yellow black) ",")
               optional)))
    ;; <rgb> model
    ("rgb" (let ((red   (TeX-read-string "Value Red (between 0,1): "))
                 (green (TeX-read-string "Value Green (between 0,1): "))
                 (blue  (TeX-read-string "Value Blue (between 0,1): ")))
             (TeX-argument-insert
              (mapconcat #'identity (list red green blue) ",")
              optional)))
    ;; <gray> model
    ("gray" (TeX-argument-insert
             (TeX-read-string "Value Gray (between 0,1): ")
             optional))
    (_ (error "%s" "Finding color model failed"))))

(defun TeX-arg-color (optional)
  "Insert <color spec> argument of various color commands from color.sty.
If OPTIONAL is non-nil, insert the argument when non-empty and in
brackets.  The \"named\" color model is handled inside the hook and not
in this function."
  ;; \color[<model>]{<color spec>}: Query for <color spec> based on
  ;; `LaTeX-color-used-model':
  (pcase LaTeX-color-used-model
    ;; <cmyk> model
    ("cmyk" (let ((cyan    (TeX-read-string "Value Cyan (between 0,1): "))
                  (magenta (TeX-read-string "Value Magenta (between 0,1): "))
                  (yellow  (TeX-read-string "Value Yellow (between 0,1): "))
                  (black   (TeX-read-string "Value Black (between 0,1): ")))
              (TeX-argument-insert
               (mapconcat #'identity (list cyan magenta yellow black) ",")
               optional)))
    ;; <rgb> model
    ("rgb" (let ((red   (TeX-read-string "Value Red (between 0,1): "))
                 (green (TeX-read-string "Value Green (between 0,1): "))
                 (blue  (TeX-read-string "Value Blue (between 0,1): ")))
             (TeX-argument-insert
              (mapconcat #'identity (list red green blue) ",")
              optional)))
    ;; <gray> model
    ("gray" (TeX-argument-insert
             (TeX-read-string "Value Gray (between 0,1): ")
             optional))
    (_ (error "%s" "Finding color model failed"))))

(defun TeX-arg-color-fcolorbox (optional &optional prompt)
  "Insert <color spec> argument of `\\fcolorbox' from `color.sty'.
If OPTIONAL is non-nil, insert the argument when non-empty and in
brackets.  PROMPT is only \"Box\" when non-nil.  The \"named\" color
model is handled inside the hook and not in this function."
  ;; \fcolorbox{<frame color name>}{<box color name>}{<text>} or
  ;; \fcolorbox[<model>]{<frame color spec>}{<box color spec>}{<text>}
  (pcase LaTeX-color-used-model
    ;; <cmyk> model
    ("cmyk" (let ((cyan    (TeX-read-string
                            (concat (or prompt "Frame")
                                    " value Cyan (between 0,1): ")))
                  (magenta (TeX-read-string
                            (concat (or prompt "Frame")
                                    " value Magenta (between 0,1): ")))
                  (yellow  (TeX-read-string
                            (concat (or prompt "Frame")
                                    " value Yellow (between 0,1): ")))
                  (black   (TeX-read-string
                            (concat (or prompt "Frame")
                                    " value Black (between 0,1): "))))
              (TeX-argument-insert
               (mapconcat #'identity (list cyan magenta yellow black) ",")
               optional)))
    ;; <rgb> model
    ("rgb" (let ((red   (TeX-read-string
                         (concat (or prompt "Frame")
                                 " value Red (between 0,1): ")))
                 (green (TeX-read-string
                         (concat (or prompt "Frame")
                                 " value Green (between 0,1): ")))
                 (blue  (TeX-read-string
                         (concat (or prompt "Frame")
                                 " value Blue (between 0,1): "))))
             (TeX-argument-insert
              (mapconcat #'identity (list red green blue) ",")
              optional)))
    ;; <gray> model
    ("gray" (TeX-argument-insert (TeX-read-string
                                  (concat (or prompt "Frame")
                                          " value Gray (between 0,1): "))
                                 optional))
    (_ (error "%s" "Finding color model failed"))))

(TeX-add-style-hook
 "color"
 (lambda ()
   ;; Add color to the parser.
   (TeX-auto-add-regexp LaTeX-color-definecolor-regexp)

   ;; Add list of colors which are always available.
   (LaTeX-add-color-definecolors
    "black" "blue" "cyan" "green" "magenta" "red" "white" "yellow")

   ;; Add dvips colors in conjunction with `usenames'.
   (when (and (LaTeX-provided-package-options-member "color" "usenames")
              (or (LaTeX-provided-package-options-member "color" "dvips")
                  (LaTeX-provided-package-options-member "color" "dvipsnames")))
     (apply #'LaTeX-add-color-definecolors LaTeX-color-dvipsnames-colors))

   (unless (member "xcolor" (TeX-style-list))
     (TeX-add-symbols
      ;; \definecolor{<name>}{<model>}{<color spec>}
      '("definecolor"
        (lambda (optional)
          (let ((colorname (TeX-read-string
                            (TeX-argument-prompt optional nil "Color name"))))
            (LaTeX-add-color-definecolors colorname)
            (TeX-argument-insert colorname optional)))
        (TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model")
        (TeX-arg-conditional
            (and (save-excursion
                   (re-search-backward "\\\\definecolor{[^}]+}{\\([^}]+\\)}"
                                       (line-beginning-position) t)
                   (setq LaTeX-color-used-model (match-string-no-properties 1)))
                 (not (string= LaTeX-color-used-model "named")))
            (TeX-arg-color-definecolor)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name"))))

      ;; \color{<name>} or \color[<model>]{<color spec>}
      '("color"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name"))))

      ;; \textcolor{<name>}{<text>} or
      ;; \textcolor[<model>]{<color spec>}{<text>}
      '("textcolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name")))
        "Text")

      ;; \mathcolor{<name>}{<math>} or
      ;; \mathcolor[<model>]{<color spec>}{<math>}
      '("mathcolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name")))
        "Math")

      ;; \pagecolor{<name>} or
      ;; \pagecolor[<model>]{<color spec>}
      '("pagecolor"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name"))))

      ;; \nopagecolor
      '("nopagecolor" 0)

      ;; \colorbox{<name>}{<text>} or
      ;; \colorbox[<model>]{<color spec>}{<text>}
      '("colorbox"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Color name")))
        "Text")

      ;; \fcolorbox{<frame color name>}{<box color name>}{<text>} or
      ;; \fcolorbox[<model>]{<frame color spec>}{<box color spec>}{<text>}
      '("fcolorbox"
        [TeX-arg-completing-read (LaTeX-color-available-models)
                                 "Color model"]
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            (TeX-arg-color-fcolorbox)
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Frame color name")))
        (TeX-arg-conditional (LaTeX-color-used-model-requires-spec-p)
            ((TeX-arg-color-fcolorbox "Box"))
          ((TeX-arg-completing-read (LaTeX-color-available-colors)
                                    "Box color name")))
        "Text"))

     ;; Fontification
     (when (and (featurep 'font-latex)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("color"         "[{")
                                  ("pagecolor"     "[{"))
                                'type-declaration)
       (font-latex-add-keywords '(("textcolor"     "[{{")
                                  ("colorbox"      "[{{" )
                                  ("fcolorbox"     "[{{{"))
                                'type-command)
       (font-latex-add-keywords '(("definecolor"    "{{{"))
                                'function))))
 TeX-dialect)

(defvar LaTeX-color-package-options
  '("debugshow" "dvipdf" "dvipdfm" "dvipdfmx" "dvips" "dvipsnames"
    "dvipsone" "dvisvgm" "dviwin" "dviwindo" "emtex" "luatex"
    "monochrome" "nodvipsnames" "nosetpagesize" "oztex" "pctex32"
    "pctexhp" "pctexps" "pctexwin" "pdftex" "setpagesize" "tcidvi"
    "textures" "truetex" "usenames" "vtex" "xdvi" "xetex")
  "Package options for the color package.")

;;; color.el ends here
