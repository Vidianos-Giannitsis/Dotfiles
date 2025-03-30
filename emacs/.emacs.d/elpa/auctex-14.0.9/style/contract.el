;;; contract.el --- AUCTeX style for `contract.sty' (v0.91)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024--2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-06-11
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

;; This file adds support for the contract package.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defconst LaTeX-contract-clause-key-val
  '(("dummy")
    ("head")
    ("nohead")
    ("notocentry")
    ("number")
    ("preskip")
    ("postskip")
    ("title")
    ("tocentry"))
  "Key=val options for the \\Clause macro.")

(defvar LaTeX-contract-DeclareNewJuraEnvironment-regexp
  '("\\\\DeclareNewJuraEnvironment{\\([^}]+\\)}"
    1 LaTeX-auto-environment)
  "Regexp for matching argument of \\DeclareNewJuraEnvironment.")

(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(TeX-add-style-hook
 "contract"
 (lambda ()

   ;; Add the macro to the parser:
   (TeX-auto-add-regexp LaTeX-contract-DeclareNewJuraEnvironment-regexp)

   ;; 4 Environment for Contracts
   (LaTeX-add-environments '("contract"))

   ;; 4.1 Clauses
   (TeX-add-symbols
    '("contractSetup"
      (TeX-arg-key-val (LaTeX-contract-package-options-list)))
    '("Clause"
      [TeX-arg-key-val LaTeX-contract-clause-key-val
                       nil nil ?\s])
    '("SubClause"
      [TeX-arg-key-val LaTeX-contract-clause-key-val
                       nil nil ?\s])
    "Clauseformat"

    ;; 4.2 Paragraphs
    "thepar" "parformat" "parformatseparation" "withoutparnumber"
    '("ellipsispar" ["Number of paragraphs to omit"])
    "parellipsis"

    ;; 4.3 Sentences
    "thesentence" "sentencenumberformat" "Sentence"

    ;; 6 Additional Contract Environments
    '("DeclareNewJuraEnvironment"
      TeX-arg-define-environment
      [TeX-arg-key-val (("Clause")
                        ("ClauseFont")
                        ("SubClause")
                        ("Sentence")
                        ("ClauseNumberFormat"))]
      2)

    ;; 7 Support for Different Languages
    "parname"
    "partshortname"
    "sentencename"
    "sentenceshortname")

   ;; 5 Cross References
   (let ((macs '("refL" "refS" "refN" "refClause" "refClauseN"
                 "refPar" "refParL" "refParS" "refParN"
                 "refSentence" "refSentenceL"" refSentenceS" "refSentenceN")))
     (dolist (mac macs)
       (TeX-add-symbols `(,mac TeX-arg-ref)))
     (when (and (featurep 'font-latex)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords (mapcar (lambda (x) (list x "{")) macs)
                                'reference)))

   (LaTeX-paragraph-commands-add-locally '("Clause" "SubClause"))

   (LaTeX-add-counters "par" "sentence")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("contractSetup"             "{")
                                ("ellipsispar"               "[")
                                ("DeclareNewJuraEnvironment" "{[{{"))
                              'function)
     (font-latex-add-keywords '(("Clause" "["))
                              'sectioning-2)
     (font-latex-add-keywords '(("SubClause" "["))
                              'sectioning-3)))
 TeX-dialect)

(defun LaTeX-contract-package-options-list ()
  "Return an alist of package options for contract package."
  (let ((len (mapcar (lambda (x) (concat TeX-esc (car x)))
                     (LaTeX-length-list))))
    `(("juratotoc" ("true" "false" "number"))
      ("juratocindent" ,len)
      ("juratocnumberwidth" ,len)
      ("contract")
      ("juratitlepagebreak" ("true" "false"))
      ("clausemark" ("both" "false" "forceboth" "forceright" "right"))
      ("parnumber" ("true" "false" "auto" "manual"))
      ("ref" ("long" "numeric"
              "clauseonly" "onlyclause" "ClauseOnly" "OnlyClause"
              "parlong" "longpar" "ParL"
              "parnumeric" "numericpar" "ParN"
              "paroff" "nopar"
              "parshort" "shortpar" "ParS" "sentencelong" "longsentence"
              "SentenceL"
              "sentencenumeric" "numericsentence" "SentenceN"
              "sentenceoff" "nosentence" "sentenceshort" "shortsentence"
              "SentenceS"
              "short")))))

(defun LaTeX-contract-package-options ()
  "Read the contract package options from the user."
  (TeX-read-key-val t (LaTeX-contract-package-options-list)))

;;; contract.el ends here
