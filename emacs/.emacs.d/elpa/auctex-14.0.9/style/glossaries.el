;;; glossaries.el - AUCTeX style for `glossaries.sty' (v4.55) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-11-11
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

;; This file adds incomplete support for `glossaries.sty' (v4.55) from
;; 2024/11/01.  `glossaries.sty' is huge, and this style covers only
;; part of the macros described in `glossaries-user.pdf'.

;; FIXME: Please make me more sophisticated!

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))

(defun LaTeX-glossaries-newentry-keyval-options ()
  "Return key=value options for glossaries defining macros."
  (append
   `(("parent" ,(mapcar #'car (LaTeX-glossaries-newentry-list))))
   (when (member "glossaries-extra" (TeX-style-list))
     '(("category")
       ("seealso")
       ("alias")
       ("group")
       ("location")))
   '(("name")
     ("description")
     ("descriptionplural")
     ("text")
     ("first")
     ("plural")
     ("firstplural")
     ("symbol")
     ("symbolplural")
     ("sort")
     ("type")
     ("user1")
     ("user2")
     ("user3")
     ("user4")
     ("user5")
     ("user6")
     ("nonumberlist" ("true" "false"))
     ("see")
     ("counter")
     ;; The following keys are reserved for \newacronym and
     ;; \newabbreviation:
     ("long")
     ("longplural")
     ("short")
     ("shortplural"))))

(defun LaTeX-glossaries-gls-keyval-options ()
  "Return key=value options for glossaries referencing macros."
  (append
   (when (member "glossaries-extra" (TeX-style-list))
     '(("noindex" ("true" "false"))
       ("hyperoutside" ("true" "false"))
       ("wrgloss" ("before" "after"))
       ("textformat")
       ("innertextformat")
       ("prefix")
       ("thevalue")
       ("theHvalue")
       ("prereset")
       ("preunset")
       ("postunset")))
   '(("hyper" ("true" "false"))
     ("format")
     ("counter")
     ("local" ("true" "false")))))

;; Setup for defining macros:
(TeX-auto-add-type "glossaries-newentry" "LaTeX" "glossaries-newentries")

(defvar LaTeX-glossaries-newentry-regexp
  `(,(concat (regexp-quote TeX-esc)
             (regexp-opt '("newglossaryentry"
                           "longnewglossaryentry"
                           "provideglossaryentry"
                           "longprovideglossaryentry"
                           "newacronym"
                           ;; These macros are defined in
                           ;; `glossaries-extra.sty', but we add them
                           ;; here in order to save some regexp groups:
                           "newabbreviation"
                           "glsxtrnewsymbol"))
             ;; Allow one level of braces inside of the optional arg:
             "\\(?:\\[[^][]*\\(?:{[^}{]*}[^}{]*\\)*\\]\\)?"
             "{\\([^}]+\\)}")
    1 LaTeX-auto-glossaries-newentry)
  "Match the label argument of glossaries macros for new labels.")

(defun LaTeX-glossaries-insert-newentry (optional &optional prompt)
  "Read and insert the new entry argument for glossaries macros."
  (let ((entry (TeX-read-string
                (TeX-argument-prompt optional prompt "New label"))))
    (LaTeX-add-glossaries-newentries entry)
    (TeX-argument-insert entry optional)))

(TeX-add-style-hook
 "glossaries"
 (lambda ()

   ;; Add regexp to the parser:
   (TeX-auto-add-regexp LaTeX-glossaries-newentry-regexp)

   (TeX-add-symbols
    '("newglossaryentry" LaTeX-glossaries-insert-newentry
      (TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s))

    '("longnewglossaryentry" LaTeX-glossaries-insert-newentry
      (TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s)
      t)

    '("provideglossaryentry" LaTeX-glossaries-insert-newentry
      (TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s))

    '("longprovideglossaryentry" LaTeX-glossaries-insert-newentry
      (TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s)
      t)

    '("newacronym"
      [TeX-arg-key-val (LaTeX-glossaries-newentry-keyval-options) nil nil ?\s]
      LaTeX-glossaries-insert-newentry
      "Short form" "Long form")

    '("setupglossaries"
      (TeX-arg-key-val
       (lambda ()
         (seq-remove (lambda (elt)
                       (member (car elt) '("xindy"
                                           "xindygloss"
                                           "xindynoglsnumbers"
                                           "makeindex"
                                           "nolong"
                                           "nosuper"
                                           "nolist"
                                           "notree"
                                           "nostyles"
                                           "nomain"
                                           "compatible-2.07"
                                           "translate"
                                           "notranslate"
                                           "languages"
                                           "acronym")))
                     LaTeX-glossaries-package-options-list)))) )

   ;; Referencing macros: Don't ask for the final optional argument
   ;; which is probably used not so often but will be annoying if
   ;; queried after every macro.  Provide fontification, though.
   (let ((macs '("gls"             "Gls"             "GLS"
                 "glspl"           "Glspl"           "GLSpl"
                 "glstext"         "Glstext"         "GLStext"
                 "glsfirst"        "Glsfirst"        "GLSfirst"
                 "glsplural"       "Glsplural"       "GLSplural"
                 "glsfirstplural"  "Glsfirstplural"  "GLSfirstplural"
                 "glsname"         "Glsname"         "GLSname"
                 "glssymbol"       "Glssymbol"       "GLSsymbol"
                 "glsdesc"         "Glsdesc"         "GLSdesc"
                 "glsuseri"        "Glsuseri"        "GLSuseri"
                 "glsuserii"       "Glsuserii"       "GLSuserii"
                 "glsuseriii"      "Glsuseriii"      "GLSuseriii"
                 "glsuseriv"       "Glsuseriv"       "GLSuseriv"
                 "glsuserv"        "Glsuserv"        "GLSuserv"
                 "glsuservi"       "Glsuservi"       "GLSuservi"))
         (args '([TeX-arg-key-val (LaTeX-glossaries-gls-keyval-options)]
                 (TeX-arg-completing-read (LaTeX-glossaries-newentry-list)
                                          "Label")))
         (macs1 '("glsdisp" "Glsdisp" "glslink" "Glslink"))
         (args1 '([TeX-arg-key-val (LaTeX-glossaries-gls-keyval-options)]
                  (TeX-arg-completing-read (LaTeX-glossaries-newentry-list)
                                           "Label")
                  "Text"))
         symbols)
     (dolist (mac macs)
       (push (cons mac args) symbols)
       (push (cons (concat mac "*") args) symbols)
       (push (cons (concat mac "+") args) symbols))
     (dolist (mac macs1)
       (push (cons mac args1) symbols)
       (push (cons (concat mac "*") args1) symbols)
       (push (cons (concat mac "+") args1) symbols))
     (apply #'TeX-add-symbols symbols)

     ;; Fontification
     (when (and (featurep 'font-latex)
                (eq TeX-install-font-lock 'font-latex-setup))
       (setq symbols nil)
       (dolist (mac macs)
         (push `(,mac "*+[{[") symbols))
       (dolist (mac macs1)
         (push `(,mac "*+[{{") symbols))
       (font-latex-add-keywords symbols 'reference)))

   ;; \setacronymstyle is not allowed when `glossaries-extra.sty' is
   ;; loaded:
   (unless (member "glossaries-extra" (TeX-style-list))
     (TeX-add-symbols
      '("setacronymstyle" ["Glossary type"]
        (TeX-arg-completing-read ("long-short" "short-long"
                                  "sc-short-long" "sm-short-long"
                                  "long-short-desc" "long-sc-short-desc"
                                  "long-sm-short-desc" "long-sp-short-desc"
                                  "short-long-desc" "sc-short-long-desc"
                                  "sm-short-long-desc"
                                  "long-sc-short" "long-sm-short" "long-sp-short"
                                  "dua" "dua-desc"
                                  "footnote" "footnote-sc" "footnote-sm"
                                  "footnote-desc" "footnote-sc-desc"
                                  "footnote-sm-desc")
                                 "Style"))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newglossaryentry"     "{{")
                                ;; Starred form is provided by
                                ;; `glossaries-extra.sty':
                                ("longnewglossaryentry" "*{{{")
                                ("provideglossaryentry" "{{")
                                ("longprovideglossaryentry" "{{{")
                                ("newacronym"           "[{{{")
                                ("setupglossaries"      "{"))
                              'function)

     (unless (member "glossaries-extra" (TeX-style-list))
       (font-latex-add-keywords '(("setacronymstyle" "[{"))
                                'function))) )
 TeX-dialect)

(defvar LaTeX-glossaries-package-options-list
  '(;; 2.1. General Options: Taken from glossaries-user.pdf:
    ("nowarn")
    ("nolangwarn")
    ("noredefwarn")
    ("debug" ("true" "false" "showtargets" "showaccsupp"))
    ("savewrites" ("true" "false"))
    ("translate" ("true" "false" "babel"))
    ("notranslate")
    ("languages")
    ("locales")
    ("hyperfirst" ("true" "false"))
    ("writeglslabels")
    ("writeglslabelnames")
    ;; 2.2. Sectioning, Headings and TOC Options
    ("toc" ("true" "false"))
    ("numberline" ("true" "false"))
    ("section" ("chapter" "section" "subsection"))
    ("ucmark" ("true" "false"))
    ("numberedsection" ("false" "nolabel" "autolabel" "nameref"))
    ;; 2.3. Glossary Appearance Options
    ("savenumberlist" ("true" "false"))
    ("entrycounter" ("true" "false"))
    ("counterwithin")
    ("subentrycounter" ("true" "false"))
    ("style")
    ("nolong")
    ("nosuper")
    ("nolist")
    ("notree")
    ("nostyles")
    ("nonumberlist")
    ("seeautonumberlist")
    ("counter")
    ("nopostdot" ("true" "false"))
    ("nogroupskip" ("true" "false"))
    ;; 2.4. Indexing Options
    ("seenoindex" ("error" "warn" "ignore"))
    ("esclocations" ("true" "false"))
    ("indexonlyfirst" ("true" "false"))
    ;; 2.5. Sorting Options
    ("sanitizesort" ("true" "false"))
    ("sort" ("none" "clear" "def" "use" "standard"))
    ("order" ("word" "letter"))
    ("makeindex")
    ("xindy" ("language" "codepage" "glsnumbers"))
    ("xindygloss")
    ("xindynoglsnumbers")
    ("automake" ("true" "false" "delayed" "immediate" "makegloss" "lite"))
    ("automakegloss")
    ("automakeglosslite")
    ("disablemakegloss")
    ("restoremakegloss")
    ;; 2.6. Glossary Type Options
    ("nohypertypes")
    ("nomain")
    ("symbols")
    ("numbers")
    ("index")
    ("noglossaryindex")
    ;; 2.7. Acronym and Abbreviation Options
    ("acronym" ("true" "false"))
    ("acronyms")
    ("acronymlists")
    ("shortcuts" ("true" "false"))
    ;; 2.9. Other Options
    ("mfirstuc" ("expanded" "unexpanded"))
    ("kernelglossredefs" ("true" "false" "nowarn")))
  "Package options for the glossaries package.")

(defvar LaTeX-glossaries-extra-package-options-list
  '(("undefaction" ("error" "warn"))
    ("docdef" ("false" "restricted" "atom" "true"))
    ("stylemods")
    ("indexcrossrefs" ("true" "false"))
    ("autoseeindex" ("true" "false"))
    ("record" ("off" "only" "nameref" "hybrid"))
    ("equation" ("true" "false"))
    ("floats" ("true" "false"))
    ("indexcounter")
    ;; 2.7. Acronym and Abbreviation Options
    ("abbreviations")
    ;; 2.9. Other Options
    ("accsupp")
    ("prefix")
    ("nomissingglstext" ("true" "false")))
  "Package options for the glossaries-extra package.")

(defun LaTeX-glossaries-package-options ()
  "Read the glossaries package options from the user."
  (TeX-read-key-val t LaTeX-glossaries-package-options-list))

;;; glossaries.el ends here
