;;; acro.el --- AUCTeX style for `acro.sty' version v3.8  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
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

;; This file adds support for `acro.sty' version v3.8.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-acro-acsetup-key-val-options
  '(("case-sensitive" ("true" "false"))
    ("case-insensitive" ("true" "false"))
    ;; 6.1. Basic properties
    ("use-id-as-short" ("true" "false"))
    ;; 9. The first or full appearance
    ("first-style" ("long-short" "short-long" "short" "long" "footnote"))
    ("subsequent-style" ("long-short" "short-long" "short" "long" "footnote"))
    ;; 10. Single appearances of an acronym
    ("single" ("true" "false"))
    ("single-style" ("long-short" "short-long" "short" "long" "footnote"))
    ;; 11.1. The main command and its options
    ("list/template" ("description" "tabular" "longtable"
                      "supertabular" "tabularray"))
    ("list/display" ("all" "used"))
    ("list/include")
    ("list/exclude")
    ("list/add" ("true" "false"))
    ("list/heading" ("none" "section" "section*" "chapter" "chapter*"))
    ("list/name")
    ("list/preamble")
    ("list/locale/display" ("true" "false"))
    ;; 11.2. Add page numbers to the list
    ("pages/display" ("first" "all" "none"))
    ("pages/seq/use" ("true" "false"))
    ("pages/seq/pre")
    ("pages/seq/threshold")
    ("pages/fill")
    ("pages/name" ("true" "false"))
    ;; 11.3. Filter lists using tags
    ("list/exclude")
    ("list/include")
    ;; 11.4. Local lists
    ("barriers/use" ("true" "false"))
    ("list/local" ("true" "false"))
    ;; 12. Formatting
    ("format")
    ("format/short")
    ("format/long")
    ("format/first-long")
    ("format/alt")
    ("format/extra")
    ("format/foreign")
    ("format/list")
    ("format/replace" ("true" "false"))
    ("short-plural-ending")
    ("long-plural-ending")
    ;; 15. Foreign language acronyms
    ("foreign/display" ("true" "false"))
    ("list/foreign/display" ("true" "false"))
    ("locale/display" ("true" "false"))
    ("list/locale/display" ("true" "false"))
    ("locale/format")
    ;; 16. Uppercasing
    ("uppercase/first")
    ("uppercase/title")
    ("uppercase/all")
    ("uppercase/none")
    ("uppercase/cmd")
    ("uppercase/short" ("true" "false"))
    ;; 17.1. Citing
    ("cite/cmd")
    ("cite/group" ("true" "false"))
    ("cite/display" ("first" "all" "none"))
    ("cite/pre")
    ("cite/group/cmd")
    ("cite/group/pre")
    ;; 17.2. Indexing
    ("index/use" ("true" "false" "indexed"))
    ("index/cmd")
    ("index/disable")
    ("index/clear")
    ;; 18. Barriers
    ("barriers/use" ("true" "false"))
    ("barriers/reset" ("true" "false"))
    ("barriers/single" ("true" "false"))
    ;; 19. Trailing tokens
    ("trailing/define")
    ("trailing/activate")
    ("trailing/deactivate")
    ;; 21.1. Backlinks
    ("make-links" ("true" "false"))
    ("link-only-first" ("true" "false"))
    ;; 21.3. pdf comments
    ("pdfcomments/use" ("true" "false"))
    ("pdfcomments/cmd")
    ;; 21.4. Accessibility support
    ("accsupp/use" ("true" "false"))
    ("accsupp/options")
    ("accsupp/method")
    ;; 22. Localisation
    ("language" ("auto"))
    ;; 23. Patches
    ("patch/floats" ("true" "false"))
    ("patch/lists" ("true" "false"))
    ("patch/tabularx" ("true" "false"))
    ("patch/longtable" ("true" "false"))
    ("patch/ltxtable" ("true" "false"))
    ("patch/tabu" ("true" "false"))
    ("patch/caption" ("true" "false"))
    ("patch/maketitle" ("true" "false")))
  "AList of key=vals for the \\acsetup macro from the acro package.")

(defvar LaTeX-acro-declareacronym-key-val-options
  '(("case-sensitive" ("true" "false"))
    ("case-insensitive" ("true" "false"))
    ;; 6.1. Basic properties
    ("short")
    ("use-id-as-short" ("true" "false"))
    ("long")
    ("alt")
    ("extra")
    ("foreign")
    ("long-post")
    ("post")
    ("single")
    ("sort")
    ("tag")
    ("cite")
    ("before-citation")
    ("index")
    ("index-sort")
    ("index-cmd")
    ;; 6.2. Properties related to plural and indefinite forms
    ("short-plural")
    ("short-plural-form")
    ("long-plural")
    ("long-plural-form")
    ("alt-plural")
    ("alt-plural-form")
    ("foreign-plural")
    ("foreign-plural-form")
    ("short-indefinite")
    ("long-indefinite")
    ("alt-indefinite")
    ;; 6.3. Properties related to formatting
    ("format")
    ("short-format")
    ("long-format")
    ("first-long-format")
    ("alt-format")
    ("extra-format")
    ("foreign-format")
    ("list-format")
    ("first-style" ("long-short" "short-long" "short" "long" "footnote"))
    ("subsequent-style" ("long-short" "short-long" "short" "long" "footnote"))
    ("single-style" ("long-short" "short-long" "short" "long" "footnote"))
    ;; 6.4. Properties related to the created pdf file
    ("pdfstring")
    ("pdfcomment")
    ("short-acc")
    ("long-acc")
    ("alt-acc")
    ("foreign-acc")
    ("extra-acc")
    ("single-acc")
    ("list-acc")
    ;;  6.5. Further properties
    ("list")
    ("foreign-babel")
    ("foreign-locale")
    ("preset")
    ("uselist"))
  "AList of key=vals for the \\DeclareAcronym macro from the acro package.
These are used for the second mandatory argument.")

(defvar LaTeX-acro-printacronyms-key-val-options
  '(;; 11.1. The main command and its options
    ("template" ("description" "tabular" "longtable" "supertabular" "tabularray"))
    ("sort" ("true" "false"))
    ("display" ("all" "used"))
    ("include")
    ("exclude")
    ("add" ("true" "false"))
    ("heading" ("none" "section" "section*" "chapter" "chapter*"))
    ("name")
    ("preamble")
    ("display" ("true" "false"))
    ;; 11.2. Add page numbers to the list
    ("pages")
    ;; 11.4. Local lists
    ("barriers")
    ("local" ("true" "false")))
  "AList of key=vals for the \\printacronyms macro from the acro package.
These are used in its optional argument.")

(defvar LaTeX-acro-ac-key-val-options
  '(("case-sensitive" ("true" "false"))
    ("case-insensitive" ("true" "false"))
    ("use-id-as-short" ("true" "false"))
    ;; 9. The first or full appearance
    ("first-style" ("long-short" "short-long" "short" "long" "footnote"))
    ("subsequent-style" ("long-short" "short-long" "short" "long" "footnote"))
    ;; 10. Single appearances of an acronym
    ("single" ("true" "false"))
    ("single-style" ("long-short" "short-long" "short" "long" "footnote")))
  "AList of key=vals for the various \\acX macros from the acro package.")

(TeX-auto-add-type "acro-acronym" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
(defvar LaTeX-acro-regexp
  (concat "\\\\DeclareAcronym" "{\\([^\n\r%\\{}]+\\)}")
  "Matches `acro' acronym definitions.")

(defvar LaTeX-auto-acro-acronym nil
  "Temporary for parsing `acro' acronym definitions.")

(defun LaTeX-acro-prepare ()
  "Clear `LaTex-auto-acro-acronym' before use."
  (setq LaTeX-auto-acro-acronym nil))

(defun LaTeX-acro-cleanup ()
  "Move acronyms from `LaTeX-auto-acro-acronym' to `LaTeX-acro-acronym-list'."
  (mapc (lambda (acronym)
          (add-to-list 'LaTeX-acro-acronym-list (list acronym)))
        LaTeX-auto-acro-acronym))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-acro-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-acro-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defvar LaTeX-acro-acronym-history nil
  "History of acronyms in acro.")

(defun LaTeX-arg-acro-acronym (optional &optional prompt definition)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen acronym to the
list of defined acronyms."
  (let ((acronym (completing-read (TeX-argument-prompt optional prompt "Acronym")
                                  (LaTeX-acro-acronym-list) nil nil nil
                                  'LaTeX-acro-acronym-history)))
    (if (and definition (not (string-empty-p acronym)))
        (LaTeX-add-acro-acronyms acronym))
    (TeX-argument-insert acronym optional)))

(defun LaTeX-arg-define-acro-acronym (optional &optional prompt)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-acro-acronym optional prompt t))

(TeX-add-style-hook
 "acro"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-acro-regexp 1 LaTeX-auto-acro-acronym))

   (TeX-add-symbols

    ;; 5.2. Setup command
    '("acsetup"
      (TeX-arg-key-val LaTeX-acro-acsetup-key-val-options))

    ;; 6. Declaring acronyms and other abbreviations
    '("DeclareAcronym" LaTeX-arg-define-acro-acronym
      (TeX-arg-key-val LaTeX-acro-declareacronym-key-val-options
                       "List of properties"
                       nil ?\s))

    ;;  6.6. Presets
    '("NewAcroPreset" "Set name" t)
    '("RenewAcroPreset" "Set name" t)
    '("DeclareAcroPreset" "Set name" t)

    ;; 7. Using acronyms
    '("ac"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("ac*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Ac"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Ac*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iac"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iac*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iac"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iac*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))

    ;;
    '("acs"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acs*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acs"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acs*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acsp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acsp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acsp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acsp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacs"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacs*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacs"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacs*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    ;;
    '("acl"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acl*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acl"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acl*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("aclp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("aclp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Aclp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Aclp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacl"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacl*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacl"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacl*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    ;;
    '("aca"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("aca*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Aca"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Aca*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acap"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acap*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acap"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acap*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iaca"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iaca*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iaca"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iaca*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    ;;
    '("acf"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acf*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acf"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acf*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acfp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acfp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acfp"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Acfp*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacf"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacf*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacf"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacf*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    ;;
    '("iacflike"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("iacflike*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacflike"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("Iacflike*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    ;; Simulating the First Appearance
    '("acflike"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acflike*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acfplike"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acfplike*"
      [TeX-arg-key-val LaTeX-acro-ac-key-val-options]
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))

    ;; FIXME: PDF bookmarks macros seems to be gone in v3.8; we leave
    ;; these macros here in case they are still used with older
    ;; versions:
    '("acpdfstring"
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))
    '("acpdfstringplural"
      (TeX-arg-completing-read (LaTeX-acro-acronym-list)
                               "Acronym"
                               nil nil nil nil nil nil nil
                               LaTeX-acro-acronym-history))

    ;; 11. Printing the List
    '("printacronyms"
      [TeX-arg-key-val LaTeX-acro-printacronyms-key-val-options
                       nil nil ?\s])

    ;; 20. Using or resetting acronyms
    '("acreset" "List of acronyms")
    '("acresetall" 0)
    '("acuse" "List of acronyms")
    '("acuseall" 0)

    ;; 23. Patches
    '("acswitchoff" 0)
    '("acswitchon"  0)

    ;; 25.2. Defining new templates
    '("NewAcroTemplate" ["Type"] "Name" t)
    '("RenewAcroTemplate" ["Type"] "Name" t)
    '("SetupAcroTemplate" ["Type"] "Name" t)
    '("SetupNextAcroTemplate" ["Type"] "Name" t)
    "AcroTemplateType" "AcroTemplateName")


   (TeX-run-style-hooks
    "l3sort"
    "xspace"
    "xtemplate"
    "l3keys2e"
    "expl3")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("acsetup"           "{")
                                ("DeclareAcronym"    "{{")
                                ("NewAcroPreset"     "{{")
                                ("RenewAcroPreset"   "{{")
                                ("DeclareAcroPreset" "{{")
                                ("acreset"           "{")
                                ("acresetall"        "")
                                ("acuse"             "{")
                                ("acuseall"          "")
                                ("acswitchoff"       "")
                                ("acswitchon"        "")
                                ("NewAcroTemplate"       "[{{")
                                ("RenewAcroTemplate"     "[{{")
                                ("SetupAcroTemplate"     "[{{")
                                ("SetupNextAcroTemplate" "[{{"))
                              'function)
     (font-latex-add-keywords '(("ac"       "*[{")
                                ("Ac"       "*[{")
                                ("acp"      "*[{")
                                ("Acp"      "*[{")
                                ("iac"      "*[{")
                                ("Iac"      "*[{")
                                ;;
                                ("acs"      "*[{")
                                ("Acs"      "*[{")
                                ("acsp"     "*[{")
                                ("Acsp"     "*[{")
                                ("iacs"     "*[{")
                                ("Iacs"     "*[{")
                                ;;
                                ("acl"      "*[{")
                                ("Acl"      "*[{")
                                ("aclp"     "*[{")
                                ("Aclp"     "*[{")
                                ("iacl"     "*[{")
                                ("Iacl"     "*[{")
                                ;;
                                ("aca"      "*[{")
                                ("Aca"      "*[{")
                                ("acap"     "*[{")
                                ("Acap"     "*[{")
                                ("iaca"     "*[{")
                                ("Iaca"     "*[{")
                                ;;
                                ("acf"      "*[{")
                                ("Acf"      "*[{")
                                ("acfp"     "*[{")
                                ("Acfp"     "*[{")
                                ("iacf"     "*[{")
                                ("Iacf"     "*[{")
                                ;;
                                ("iacflike" "*[{")
                                ("Iacflike" "*[{")
                                ("acflike"  "*[{")
                                ("acfplike" "*[{"))
                              'reference)))
 TeX-dialect)

(defvar LaTeX-acro-package-options-list
  '(("upgrade" ("true" "false"))))

(defun LaTeX-acro-package-options ()
  "Prompt for package options for the acro package."
  (TeX-read-key-val t LaTeX-acro-package-options-list))

;;; acro.el ends here
