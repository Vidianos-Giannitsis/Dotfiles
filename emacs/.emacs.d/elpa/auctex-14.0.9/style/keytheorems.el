;;; keytheorems.el --- AUCTeX style for `keytheorems.sty' (v0.2.4)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2025-01-25
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

;; This file adds support for `keytheorems.sty' (v0.2.4) from
;; 2025-01-21.  `keytheorems.sty' is part of TeXLive.

;;; Code:

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(require 'tex)
(require 'latex)

(defvar LaTeX-keytheorems-package-options-list
  '(("overload")
    ("thmtools-compat")
    ("auto-translate" ("true" "false"))
    ("qed-symbol")
    ("restate-counters")
    ("store-all")
    ("store-sets-label"))
  "Package options for the keytheorems package.")

(defun LaTeX-keytheorems-package-options ()
  "Read the keytheorems package options from the user."
  (TeX-read-key-val t LaTeX-keytheorems-package-options-list))

;; Setup for \newkeytheorem:
(TeX-auto-add-type "keytheorems-newkeytheorem" "LaTeX")

(defvar LaTeX-keytheorems-newkeytheorem-regexp
  `(,(concat "\\\\"
             (regexp-opt '("newkeytheorem" "providekeytheorem"
                           "declarekeytheorem" "declaretheorem"))
             "[ \t\n\r%]*"
             "{\\([^}]+\\)}")
    1 LaTeX-auto-keytheorems-newkeytheorem)
  "Matches the argument of \\newkeytheorem from keytheorems package.")

(defun LaTeX-keytheorems-auto-prepare ()
  "Clear `LaTeX-auto-keytheorems-newkeytheorem' before parsing."
  (setq LaTeX-auto-keytheorems-newkeytheorem nil))

(defun LaTeX-keytheorems-auto-cleanup ()
  "Process parsed elements from keytheorems package."
  (dolist (newthm (mapcar #'car (LaTeX-keytheorems-newkeytheorem-list-clean)))
    (LaTeX-add-environments `(,newthm LaTeX-env-keytheorems-label))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-keytheorems-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-keytheorems-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-keytheorems-newkeytheorem-list-clean ()
  "Clean entries returned by function `LaTeX-keytheorems-newkeytheorem-list'.
Return an alist with the name of parsed entries as single element of
each sub-list."
  (let (envs)
    (dolist (newthm (mapcar #'car (LaTeX-keytheorems-newkeytheorem-list)))
      (if (string-match-p "," newthm)
          (setq envs (append (split-string newthm "[^[:alnum:]]+" t) envs))
        (push newthm envs)))
    (mapcar #'list (sort envs #'string<))))

(defun LaTeX-keytheorems-env-key-val-options ()
  "Return key=val options for environments defined by keytheorems.sty."
  `(;; 3.1 Keys available to theorem environments
    ("note")
    ("short-note")
    ;; We don't offer a label key in order to advertise \label:
    ;; ("label")
    ("manual-num")
    ("continue"  ,(mapcar #'car (LaTeX-label-list)))
    ("continue*" ,(mapcar #'car (LaTeX-label-list)))
    ("store")
    ("store*")
    ("restate")
    ("restate*")
    ("restate-keys")
    ("listhack" ("true" "false"))
    ("seq")))

(defun LaTeX-keytheorems-newkeytheorem-key-val-options ()
  "Return key=val options for the optional argument of \\newkeytheorem."
  (let ((counters (mapcar #'car (LaTeX-counter-list)))
        (lengths (mapcar (lambda (x) (concat TeX-esc (car x)))
                         (LaTeX-length-list))))
    `(;; 3.2 Keys also defined in thmtools
      ("name")
      ("heading")
      ("title")
      ("numbered" ("true" "false" "unless-unique"))
      ("parent" ,counters)
      ("numberwithin" ,counters)
      ("within" ,counters)
      ("sibling" ,counters)
      ("numberlike" ,counters)
      ("sharenumber" ,counters)
      ("style")
      ("preheadhook")
      ("postheadhook")
      ("prefoothook")
      ("postfoothook")
      ("qed")
      ;; 3.3 Keys added by keytheorems
      ("refname" ("{}"))
      ("Refname" ("{}"))
      ("leftmargin" ,lengths)
      ("rightmargin" ,lengths)
      ("tcolorbox")
      ("tcolorbox-no-titlebar"))))

(defun LaTeX-keytheorems-newkeytheoremstyle-key-val-options ()
  "Return key=val options for the argument of \\newkeytheoremstyle."
  (let ((lengths (mapcar (lambda (x) (concat TeX-esc (car x)))
                         (LaTeX-length-list)))
        (counters (mapcar #'car (LaTeX-counter-list)))
        (fonts (mapcar (lambda (x) (concat TeX-esc x))
                       (append LaTeX-font-size LaTeX-font-series
                               LaTeX-font-shape))))
    `(;; 3.2 Keys also defined in thmtools
      ("heading")
      ("title")
      ("numbered" ("true" "false" "unless-unique"))
      ("parent" ,counters)
      ("numberwithin" ,counters)
      ("within" ,counters)
      ("sibling" ,counters)
      ("numberlike" ,counters)
      ("sharenumber" ,counters)
      ("preheadhook")
      ("postheadhook")
      ("prefoothook")
      ("postfoothook")
      ("qed")
      ;; 3.3 Keys added by keytheorems
      ("refname" ("{}"))
      ("Refname" ("{}"))
      ("leftmargin" ,lengths)
      ("rightmargin" ,lengths)
      ("tcolorbox")
      ("tcolorbox-no-titlebar")
      ;; 4.1 Keys also defined in thmtools
      ("bodyfont" ,fonts)
      ("break")
      ("headfont" ,fonts)
      ("headformat" ("margin" "swapnumber"))
      ("headindent" ,lengths)
      ("headpunct")
      ("notebraces" ("{}{}"))
      ("notefont" ,fonts)
      ("postheadspace" ,lengths)
      ("spaceabove" ,lengths)
      ("spacebelow" ,lengths)
      ;; 4.2 Keys added by keytheorems
      ("inherit-style" ("plain" "definition" "remark"))
      ("noteseparator")
      ("numberfont" ,fonts))))

(defun LaTeX-keytheorems-listofkeytheorems-key-val-options ()
  "Return key=val options for the argument of \\keytheoremlistset."
  (let ((lengths (mapcar (lambda (x) (concat TeX-esc (car x)))
                         (LaTeX-length-list)))
        (envs (mapcar #'car (LaTeX-keytheorems-newkeytheorem-list-clean))))
    `(("ignore" ,envs)
      ("ignoreall")
      ("numwidth" ,lengths)
      ("onlynamed" ,envs)
      ("show" ,envs)
      ("showall")
      ("swapnumber" ("true" "false"))
      ("title")
      ;; 6.2 Keys added by keytheorems
      ("format-code")
      ("indent" ,lengths)
      ("no-chapter-skip" ("true" "false"))
      ("chapter-skip-length" ,lengths)
      ("no-continues" ("true" "false"))
      ("no-title" ("true" "false"))
      ("no-toc" ("true" "false"))
      ("note-code")
      ("onlynumbered" ,envs)
      ("print-body")
      ("seq")
      ("titlecode"))))

(defun LaTeX-env-keytheorems-label (environment)
  "Insert keytheorems ENVIRONMENT, query for an optional argument and label.
AUCTeX users should add ENVIRONMENT to `LaTeX-label-alist' via
customize or in init-file with:

  (add-to-list \\='LaTeX-label-alist \\='(\"theorem\" . \"thm:\"))

RefTeX users should customize or add ENVIRONMENT to both
`LaTeX-label-alist' and `reftex-label-alist', for example

  (add-to-list \\='LaTeX-label-alist \\='(\"theorem\" . \"thm:\"))
  (add-to-list \\='reftex-label-alist
               \\='(\"theorem\" ?m \"thm:\" \"~\\ref{%s}\"
                 nil (\"Theorem\" \"theorem\") nil))"
  (let* ((help-form "\
Select the content of the optional argument with a key:
'h' in order to insert a plain heading,
'k' in order to insert key=value pairs with completion,
RET in order to leave it empty.")
         (choice (read-char-choice
                  (TeX-argument-prompt
                   nil nil "Heading (h), Key=val (k), Empty (RET), Help (C-h)")
                  '(?h ?k ?\r)))
         (opthead (pcase choice
                    (?h (TeX-read-string (TeX-argument-prompt t nil "Heading")))
                    (?k (TeX-read-key-val t LaTeX-keytheorems-package-options-list))
                    ;; Clear minibuffer and don't leave the ugly ^M
                    ;; there, return an empty string:
                    (_ (message nil) ""))))
    (LaTeX-insert-environment environment
                              (when (and opthead
                                         (not (string-empty-p opthead)))
                                (format "[%s]" opthead))))
  (when (LaTeX-label environment 'environment)
    (LaTeX-newline)
    (indent-according-to-mode)))

(defun LaTeX-arg-keytheorems-newkeytheorem (optional &optional prompt)
  "Query and insert new environments defined with \\newkeytheorem."
  (let ((envs (TeX-read-string
               (TeX-argument-prompt optional prompt "Environment(s)"))))
    (LaTeX-add-keytheorems-newkeytheorems envs)
    (LaTeX-keytheorems-auto-cleanup)
    (TeX-argument-insert envs optional)))

(TeX-add-style-hook
 "keytheorems"
 (lambda ()

   ;; Add keytheorems to the parser.
   (TeX-auto-add-regexp LaTeX-keytheorems-newkeytheorem-regexp)

   (TeX-run-style-hooks "amsthm")

   (TeX-add-symbols
    ;; 2 Global options
    '("keytheoremset"
      (TeX-arg-key-val (lambda ()
                         (append '(("continues-code"))
                                 LaTeX-keytheorems-package-options-list))))

    ;; 3 Defining theorems
    '("newkeytheorem"
      LaTeX-arg-keytheorems-newkeytheorem
      [TeX-arg-key-val LaTeX-keytheorems-newkeytheorem-key-val-options])
    '("renewkeytheorem"
      (TeX-arg-completing-read LaTeX-keytheorems-newkeytheorem-list-clean)
      [TeX-arg-key-val LaTeX-keytheorems-newkeytheorem-key-val-options])
    '("providekeytheorem"
      LaTeX-arg-keytheorems-newkeytheorem
      [TeX-arg-key-val LaTeX-keytheorems-newkeytheorem-key-val-options])
    '("declarekeytheorem"
      LaTeX-arg-keytheorems-newkeytheorem
      [TeX-arg-key-val LaTeX-keytheorems-newkeytheorem-key-val-options])

    ;; 4 Theorem styles
    '("newkeytheoremstyle" "Name"
      (TeX-read-key-val LaTeX-keytheorems-newkeytheoremstyle-key-val-options))
    '("renewkeytheoremstyle" "Name"
      (TeX-read-key-val LaTeX-keytheorems-newkeytheoremstyle-key-val-options))
    '("providekeytheoremstyle" "Name"
      (TeX-read-key-val LaTeX-keytheorems-newkeytheoremstyle-key-val-options))
    '("declarekeytheoremstyle" "Name"
      (TeX-read-key-val LaTeX-keytheorems-newkeytheoremstyle-key-val-options))

    ;; 5 Restating theorems
    '("getkeytheorem"
      [TeX-arg-completing-read ("body") "Property"] "Tag")
    '("IfRestatingTF" 2)
    '("IfRestatingT"  1)
    '("IfRestatingF"  1)

    ;; 6 Listing theorems
    '("listofkeytheorems"
      [TeX-arg-key-val (LaTeX-keytheorems-listofkeytheorems-key-val-options)])
    '("keytheoremlistset"
      (TeX-arg-key-val (LaTeX-keytheorems-listofkeytheorems-key-val-options)))

    ;; 6.3 Adding code to list of theorems
    '("addtheoremcontentsline" "Level" "Text")
    '("addtotheoremcontents" "Code")

    ;; 7 Theorem hooks
    '("addtotheoremhook"
      [TeX-arg-completing-read (LaTeX-keytheorems-newkeytheorem-list-clean)
                               "Environment"]
      (TeX-arg-completing-read ("prehead" "posthead" "prefoot"
                                "postfoot" "restated")
                               "Hook name")
      t))

   ;; Managing compatibility options:
   ;; FIXME: restatable*? environments are missing, not sure how they
   ;; are invoked in the compat mode
   (when (or (LaTeX-provided-package-options-member "keytheorems"
                                                    "overload")
             (LaTeX-provided-package-options-member "keytheorems"
                                                    "thmtools-compat"))
     (TeX-add-symbols
      '("declaretheorem"
        LaTeX-arg-keytheorems-newkeytheorem
        [TeX-arg-key-val LaTeX-keytheorems-newkeytheorem-key-val-options])

      '("declaretheoremstyle" "Name"
        (TeX-read-key-val LaTeX-keytheorems-newkeytheoremstyle-key-val-options))

      '("listoftheorems"
        [TeX-arg-key-val (LaTeX-keytheorems-listofkeytheorems-key-val-options)])

      '("listtheoremname" 0))

     ;; Fontification
     (when (and (featurep 'font-latex)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("declaretheorem"      "{[")
                                  ("declaretheoremstyle" "{{")
                                  ("listoftheorems"      "["))
                                'function)
       (font-latex-add-keywords '("listtheoremname")
                                'function-noarg)))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("keytheoremset"     "{")
                                ("newkeytheorem"     "{[")
                                ("renewkeytheorem"   "{[")
                                ("providekeytheorem" "{[")
                                ("declarekeytheorem" "{[")

                                ("newkeytheoremstyle"     "{{")
                                ("renewkeytheoremstyle"   "{{")
                                ("providekeytheoremstyle" "{{")
                                ("declarekeytheoremstyle" "{{")

                                ("IfRestatingTF" "{{")
                                ("IfRestatingT " "{")
                                ("IfRestatingF"  "{")

                                ("listofkeytheorems" "[")
                                ("keytheoremlistset" "{")

                                ("addtheoremcontentsline" "{")
                                ("addtotheoremcontents"   "{")

                                ("addtotheoremhook" "[{{"))
                              'function)
     (font-latex-add-keywords '(("getkeytheorem" "[{"))
                              'reference)))
 TeX-dialect)

;;; keytheorems.el ends here
