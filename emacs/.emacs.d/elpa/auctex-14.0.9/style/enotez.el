;;; enotez.el --- AUCTeX style for `enotez.sty' (v0.10d)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-06-28
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

;; This file adds support for `enotez.sty' (v0.10d) from 2022/01/04.
;; `enotez.sty' is part of TeXLive.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-enotez-key-val-options
  '(("list-name")
    ("reset" ("true" "false"))
    ("counter-format" ("arabic" "alph" "Alph" "roman" "Roman" "symbols"))
    ("mark-format")
    ("mark-cs")
    ("backref" ("true" "false"))
    ("totoc" ("subsection" "section" "chapter" "part" "auto" "false"))
    ("list-heading")
    ("list-style" ("plain" "custom" "description" "itemize"))
    ("split" ("section" "chapter" "false"))
    ("split-sectioning")
    ("split-heading")
    ("split-title"))
  "Key=val options for the \\setenotez macro.")

(defun LaTeX-enotez-key-val-options ()
  "Return updated key=val options for the \\setenotez macro."
  (let ((len (mapcar (lambda (x) (concat TeX-esc x))
                     (mapcar #'car (LaTeX-length-list)))))
    (append
     `(("list-preamble-skip" ,len)
       ("list-postamble-skip" ,len))
     LaTeX-enotez-key-val-options)))

(TeX-add-style-hook
 "enotez"
 (lambda ()
   (TeX-add-symbols
    ;; 3.1 Placing the Notes
    '("endnote" ["Mark"] t)
    '("endnotemark" ["Mark"])
    '("endnotetext"  t)

    ;; 3.2 Printing the Notes
    '("printendnotes"
      [TeX-arg-completing-read ("plain" "custom" "description" "itemize")])
    '("printendnotes*"
      [TeX-arg-completing-read ("plain" "custom" "description" "itemize")])
    '("AtEveryEndnotesList" t)
    '("AtNextEndnotesList"  t)
    '("AfterEveryEndnotesList" t)
    '("AfterNextEndnotesList"  t)

    ;; 4.1 Package Options
    '("setenotez" (TeX-arg-key-val (LaTeX-enotez-key-val-options)))

    ;; 5 Collect Notes Section-wise and Print List Stepwise
    '("AtEveryListSplit" t)
    '("AfterEveryListSplit" t)
    "EnotezCurrentSplitTitle"
    '("NewSplitTitleTag" 2))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("endnote"     "[{")
                                ("endnotemark" "[")
                                ("endnotetext" "{"))
                              'reference)
     (font-latex-add-keywords '(("printendnotes"          "*[")
                                ("setenotez"              "{")
                                ("AtEveryEndnotesList"    "{")
                                ("AtNextEndnotesList"     "{")
                                ("AfterEveryEndnotesList" "{")
                                ("AfterNextEndnotesList"  "{")
                                ("AtEveryListSplit"       "{")
                                ("AfterEveryListSplit"    "{")
                                ("NewSplitTitleTag"       "{{"))
                              'function)))
 TeX-dialect)

(defvar LaTeX-enotez-package-options nil
  "Package options for the enotez package.")

;;; enotez.el ends here
