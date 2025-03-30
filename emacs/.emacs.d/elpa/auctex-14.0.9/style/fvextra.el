;;; fvextra.el --- AUCTeX style for `fvextra.sty' (v1.11)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017--2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2017-03-05
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

;; This file adds support for `fvextra.sty' (v1.11) from 2025/02/09.
;; `fvextra.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(declare-function font-latex-set-syntactic-keywords "font-latex")

(declare-function LaTeX-color-definecolor-list "color" ())
(declare-function LaTeX-xcolor-definecolor-list "xcolor" ())
(declare-function LaTeX-fancyvrb-key-val-options "fancyvrb" ())

(defvar LaTeX-fvextra-key-val-options
  '(;; 3 General options
    ;; backgroundcolor et al. are added in fancyvrb.el inside
    ;; `LaTeX-fancyvrb-key-val-options'
    ("beameroverlays" ("true" "false"))
    ("curlyquotes" ("true" "false"))
    ("extra" ("true" "false"))
    ("fontencoding" (;; Reset to default document font encoding
                     "none"
                     ;; 128+ glyph encodings (text)
                     "OT1" "OT2" "OT3" "OT4" "OT6"
                     ;; 256 glyph encodings (text)
                     "T1" "T2A" "T2B" "T2C" "T3" "T4" "T5"
                     ;; 256 glyph encodings (text extended)
                     "X2"
                     ;; Other encodings
                     "LY1" "LV1" "LGR"))
    ("highlightcolor")
    ("highlightlines")
    ("linenos" ("true" "false"))
    ("mathescape" ("true" "false"))
    ("numberfirstline" ("true" "false"))
    ("numbers" ("none" "left" "right" "both"))
    ("retokenize" ("true" "false"))
    ("space" ("\\textvisiblespace"))
    ("spacecolor" ("none"))
    ("stepnumberfromfirst" ("true" "false"))
    ("stepnumberoffsetvalues" ("true" "false"))
    ("tab" ("\\FancyVerbTab"))
    ("tabcolor" ("none"))
    ("vargsingleline" ("true" "false"))
    ;; 7.1 Line breaking options
    ("breakafter" ("none"))
    ("breakafterinrun" ("true" "false"))
    ("breakaftersymbolpre")
    ("breakaftersymbolpost")
    ("breakanywhere" ("true" "false"))
    ("breakanywheresymbolpre")
    ("breakanywheresymbolpost")
    ("breakautoindent" ("true" "false"))
    ("breakbefore")
    ("breakbeforeinrun" ("true" "false"))
    ("breakbeforesymbolpre")
    ("breakbeforesymbolpost")
    ("breakcollapsespaces" ("true" "false"))
    ("breakindent")
    ("breakindentnchars")
    ("breaklines" ("true" "false"))
    ("breaknonspaceingroup" ("true" "false"))
    ("breaksymbol")
    ("breaksymbolleft")
    ("breaksymbolright")
    ("breaksymbolindent")
    ("breaksymbolindentnchars")
    ("breaksymbolindentleft")
    ("breaksymbolindentleftnchars")
    ("breaksymbolindentright")
    ("breaksymbolindentrightnchars")
    ("breaksymbolsep")
    ("breaksymbolsepnchars")
    ("breaksymbolsepleft")
    ("breaksymbolsepleftnchars")
    ("breaksymbolsepright")
    ("breaksymbolseprightnchars")
    ("spacebreak"))
  "Key=value options for fvextra macros and environments.")

(TeX-add-style-hook
 "fvextra"
 (lambda ()

   ;; Run the style hook for "fancyvrb"
   (TeX-run-style-hooks "fancyvrb")

   (TeX-add-symbols
    ;; 4.1 Inline formatting with \fvinlineset
    '("fvinlineset" (TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)))

    ;; 4.2 Custom formatting for inline commands like \Verb with
    ;; \FancyVerbFormatInline
    "FancyVerbFormatInline"

    ;; 4.3 Custom formatting for environments like Verbatim with
    ;; \FancyVerbFormatLine and \FancyVerbFormatText
    "FancyVerbFormatLine"
    "FancyVerbFormatText"

    ;; 6 New commands and environments
    ;; 6.1 \EscVerb
    '("EscVerb"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)] "Text")
    '("EscVerb*"
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)] "Text")

    ;; 6.5 \VerbatimInsertBuffer
    ;; FIXME: Not sure it the options are correct:
    '("VerbatimInsertBuffer"
      [TeX-arg-key-val (lambda ()
                         (append (LaTeX-fancyvrb-key-val-options)
                                 '(("formatcom" ("\\formatter"))
                                   ("bufferlengthname")
                                   ("bufferlinename")
                                   ("buffername")
                                   ("insertenvname" ("Verbatim" "BVerbatim")))))])

    ;; 6.6 \VerbatimClearBuffer
    '("VerbatimClearBuffer"
      [TeX-arg-key-val (("formatcom" ("\\formatter"))
                        ("bufferlengthname")
                        ("bufferlinename")
                        ("buffername")
                        ("insertenvname" ("Verbatim" "BVerbatim")))])

    ;; 6.7 \InsertBuffer
    '("InsertBuffer"
      [TeX-arg-key-val (lambda ()
                         (append (LaTeX-fancyvrb-key-val-options)
                                 '(("formatcom" ("\\formatter"))
                                   ("bufferlengthname")
                                   ("bufferlinename")
                                   ("buffername")
                                   ("insertenvname" ("Verbatim" "BVerbatim")))))])

    ;; 6.8 \ClearBuffer
    '("ClearBuffer"
      [TeX-arg-key-val (("formatcom" ("\\formatter"))
                        ("bufferlengthname")
                        ("bufferlinename")
                        ("buffername")
                        ("insertenvname" ("Verbatim" "BVerbatim")))])

    ;; 7.3.2 Breaks within macro arguments
    "FancyVerbBreakStart"
    "FancyVerbBreakStop"

    ;; 7.3.3 Customizing break behavior
    "FancyVerbBreakAnywhereBreak"
    "FancyVerbBreakBeforeBreak"
    "FancyVerbBreakAfterBreak")

   ;; 6.2 VerbEnv environment
   (LaTeX-add-environments
    '("VerbEnv" LaTeX-env-args
      [TeX-arg-key-val (LaTeX-fancyvrb-key-val-options)])

    ;; 6.3 VerbatimWrite
    '("VerbatimWrite" LaTeX-env-args
      [TeX-arg-key-val (("writefilehandle") ("writer"))])

    ;; 6.4 VerbatimBuffer
    '("VerbatimBuffer" LaTeX-env-args
      [TeX-arg-key-val (("afterbuffer")
                        ("bufferer")
                        ("bufferlengthname")
                        ("bufferlinename")
                        ("buffername")
                        ("globalbuffer" ("true" "false")))]) )

   ;; Filling
   (let ((envs '("VerbEnv" "VerbatimWrite")))
     (make-local-variable 'LaTeX-indent-environment-list)
     (dolist (env envs)
       (add-to-list 'LaTeX-verbatim-environments-local env)
       (add-to-list 'LaTeX-indent-environment-list
                    `(,env current-indentation) t)))

   ;; `VerbatimBuffer' is special; the buffer can be retrieved with
   ;; \VerbatimInsertBuffer and \InsertBuffer, where the latter
   ;; interprets buffer as LaTeX.  So we don't add `VerbatimBuffer' to
   ;; `LaTeX-verbatim-environments-local', but mimic such an environment:
   (unless (string-match-p "VerbatimBuffer" LaTeX-document-regexp)
     (make-local-variable 'LaTeX-document-regexp)
     (setq LaTeX-document-regexp (concat LaTeX-document-regexp "\\|VerbatimBuffer")))
   (add-to-list 'LaTeX-indent-environment-list
                '("VerbatimBuffer" current-indentation)
                t)

   ;; Add \Verb*? and \EscVerb*? to
   ;; `LaTeX-verbatim-macros-with-braces-local':
   (let ((macs '("Verb" "Verb*"
                 "EscVerb" "EscVerb*")))
     (dolist (mac macs)
       (add-to-list 'LaTeX-verbatim-macros-with-braces-local mac t)))

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fvinlineset"          "{")
                                ("VerbatimInsertBuffer" "[")
                                ("VerbatimClearBuffer"  "[")
                                ("ClearBuffer"  "[")
                                ("InsertBuffer" "["))
                              'function)
     (font-latex-add-keywords '(("EscVerb"     "*["))
                              'textual)
     (font-latex-add-keywords '(("VerbatimInsertBuffer" "["))
                              'reference)
     (font-latex-set-syntactic-keywords)))
 TeX-dialect)

(defvar LaTeX-fvextra-package-options nil
  "Package options for the fvextra package.")

;;; fvextra.el ends here
