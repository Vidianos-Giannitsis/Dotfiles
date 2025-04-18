;;; microtype.el --- AUCTeX style for `microtype.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022--2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2022-06-19
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

;; This file adds support for `microtype.sty' v3.2 from 2024-12-12.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-microtype-key-val-options
  '(;; 3.1 Enabling the micro-typographic features
    ("protrusion" ("true" "false" "compatibility" "nocompatibility"
                   "all"  "alltext" "allmath"
                   "alltext-nott"   "allmath-nott"
                   "basictext"      "basicmath"
                   "smallcaps" "footnotesize" "scriptsize" "normalfont"))
    ("expansion"  ("true" "false" "compatibility" "nocompatibility"
                   "all"  "alltext" "allmath"
                   "alltext-nott"   "allmath-nott"
                   "basictext"      "basicmath"
                   "smallcaps" "footnotesize" "scriptsize" "normalfont"))
    ("activate"   ("true" "false" "compatibility" "nocompatibility"))
    ("tracking"   ("true" "false"
                   "all"  "alltext" "allmath"
                   "alltext-nott"   "allmath-nott"
                   "basictext"      "basicmath"
                   "smallcaps" "footnotesize" "scriptsize" "normalfont"))
    ("kerning"    ("true" "false"
                   "all"  "alltext" "allmath"
                   "alltext-nott"   "allmath-nott"
                   "basictext"      "basicmath"
                   "smallcaps" "footnotesize" "scriptsize" "normalfont"))
    ("spacing"    ("true" "false"
                   "all"  "alltext" "allmath"
                   "alltext-nott"   "allmath-nott"
                   "basictext"      "basicmath"
                   "smallcaps" "footnotesize" "scriptsize" "normalfont"))
    ;; 3.2 Character protrusion
    ("factor")
    ("patch"   ("all" "none" "item" "toc" "footnote" "verbatim" "eqnum"))
    ("nopatch" ("all" "none" "item" "toc" "footnote" "verbatim" "eqnum"))
    ("unit")
    ;; 3.3 Font expansion
    ("auto" ("true" "false"))
    ("stretch")
    ("step")
    ("selected" ("true" "false"))
    ;; 3.4 Tracking
    ("letterspace")
    ;; 3.5 Miscellaneous options
    ("disable" ("true" "false" "ifdraft"))
    ("verbose" ("true" "false" "errors" "silent"))
    ("babel" ("true" "false"))
    ;; "config" is allowed only when the package is loaded, so we add
    ;; it below:
    ;; ("config")
    ("DVIoutput" ("true" "false")))
  "Key=value options for microtype package.")

(TeX-add-style-hook
 "microtype"
 (lambda ()

   (TeX-add-symbols
    '("microtypesetup"
      (TeX-arg-key-val LaTeX-microtype-key-val-options))

    ;; 4 Selecting fonts for micro-typography
    '("DeclareMicrotypeSet"
      [TeX-arg-completing-read-multiple ("protrusion" "expansion"
                                         "tracking" "kerning" "spacing")
                                        "Features"]
      "Set name"
      (TeX-arg-key-val (("encoding") ("family") ("series")
                        ("shape")    ("size")   ("font"))))

    '("DeclareMicrotypeSet*"
      [TeX-arg-completing-read-multiple ("protrusion" "expansion"
                                         "tracking" "kerning" "spacing")
                                        "Features"]
      "Set name"
      (TeX-arg-key-val (("encoding") ("family") ("series")
                        ("shape")    ("size")   ("font"))))

    '("UseMicrotypeSet"
      [TeX-arg-completing-read-multiple ("protrusion" "expansion"
                                         "tracking" "kerning" "spacing")
                                        "Features"]
      "Set name")

    '("DeclareMicrotypeSetDefault"
      [TeX-arg-completing-read-multiple ("protrusion" "expansion"
                                         "tracking" "kerning" "spacing")
                                        "Features"]
      "Set name")

    ;; 5.1 Character protrusion
    '("SetProtrusion"
      [TeX-arg-key-val (("name") ("load") ("factor") ("unit") ("preset")
                        ("inputenc") ("context"))]
      2)

    ;; 5.2 Font expansion
    '("SetExpansion"
      [TeX-arg-key-val (("name") ("load") ("factor") ("unit") ("preset")
                        ("inputenc") ("context")
                        ("auto") ("stretch") ("shrink") ("step"))]
      2)

    ;; 5.3 Tracking
    '("SetTracking"
      [TeX-arg-key-val
       (lambda ()
         (append
          (when (member "fontspec" (TeX-style-list))
            `(("features" ,(cadr (assoc "Ligatures"
                                        LaTeX-fontspec-font-features)))))
          '(("name") ("unit") ("context") ("spacing")
            ("outer spacing") ("outer kerning")
            ("no ligatures" ("all" "none")))))]
      2)

    ;; 5.4 Additional kerning
    '("SetExtraKerning"
      [TeX-arg-key-val (("name") ("load") ("factor") ("preset") ("inputenc")
                        ("unit") ("context"))]
      2)

    ;; 5.5 Interword spacing
    '("SetExtraSpacing"
      [TeX-arg-key-val (("name") ("load") ("factor") ("preset")
                        ("inputenc") ("context") ("unit"))]
      2)

    ;; 5.6 Character inheritance
    '("DeclareCharacterInheritance"
      [TeX-arg-key-val (("inputenc"))]
      2)

    ;; 5.7 Configuration files
    '("DeclareMicrotypeVariants" t)
    '("DeclareMicrotypeVariants*" t)
    '("DeclareMicrotypeAlias" "Font name" "Alias name")
    '("LoadMicrotypeFile" "Font name")
    '("DeclareMicrotypeFilePrefix" "Prefix")

    ;; 6 Context-sensitive setup
    '("microtypecontext"
      (TeX-arg-key-val (("protrusion") ("expansion") ("activate")
                        ("tracking") ("spacing") ("kerning"))))

    '("textmicrotypecontext"
      (TeX-arg-key-val (("protrusion") ("expansion") ("activate")
                        ("tracking") ("spacing") ("kerning")))
      "Text")

    '("DeclareMicrotypeBabelHook"
      "Language(s)"
      (TeX-arg-key-val (("protrusion") ("expansion") ("activate")
                        ("tracking") ("spacing") ("kerning"))))

    ;; 7 Letterspacing revisited
    '("textls" ["Amount"] "Text")
    "lsstyle"
    '("lslig" "Ligature")

    ;; 8 Disabling ligatures
    '("DisableLigatures" ["Characters"] t)

    ;; 9 Being pedantic about protrusion
    '("leftprotrusion" "Text")
    '("rightprotrusion" "Text")
    "noprotrusion"
    "noprotrusionifhmode")

   (LaTeX-add-environments
    '("microtypecontext" LaTeX-env-args
      (TeX-arg-key-val (("protrusion") ("expansion") ("activate")
                        ("tracking") ("spacing") ("kerning")))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("microtypesetup"      "{")
                                ("DeclareMicrotypeSet" "*[{{")
                                ("UseMicrotypeSet"     "[{")
                                ("DeclareMicrotypeSetDefault"  "[{")
                                ("SetProtrusion"       "[{{")
                                ("SetExpansion"        "[{{")
                                ("SetTracking"         "[{{")
                                ("SetExtraKerning"     "[{{")
                                ("SetExtraSpacing"     "[{{")
                                ("DeclareCharacterInheritance" "[{{")
                                ("DeclareMicrotypeVariants"    "*{")
                                ("DeclareMicrotypeAlias"       "{{")
                                ("LoadMicrotypeFile"   "{")
                                ("DeclareMicrotypeFilePrefix" "{")
                                ("microtypecontext"    "{")
                                ("DeclareMicrotypeBabelHook"   "{{")
                                ("DisableLigatures"    "[{"))
                              'function)
     (font-latex-add-keywords '(("textmicrotypecontext" "{{")
                                ("textls"   "*[{"))
                              'textual)
     (font-latex-add-keywords '(("textls"   "*[{"))
                              'type-command)
     (font-latex-add-keywords '("lsstyle")
                              'type-declaration)))
 TeX-dialect)

(defvar LaTeX-microtype-package-options-list
  (append '((config))
          LaTeX-microtype-key-val-options)
  "Package options for the microtype package.")

(defun LaTeX-microtype-package-options ()
  "Prompt for package options for the microtype package."
  (TeX-read-key-val t LaTeX-microtype-package-options-list))

;;; microtype.el ends here
