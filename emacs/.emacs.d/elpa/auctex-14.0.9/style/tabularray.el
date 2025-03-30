;;; tabularray.el --- AUCTeX style for `tabularray.sty'   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-06-17
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

;; This file adds support for the tabularray package version 2024A from
;; 2024-02-16.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords "font-latex" (keywords class))
(declare-function reftex-compile-variables "reftex" ())

(defvar LaTeX-tabularray-outerspec-key-val
  '(("baseline" ("t" "T" "m" "b" "B"))
    ("long"   ("true" "false"))
    ("tall"   ("true" "false"))
    ("expand" ("\\expanded"))
    ("t") ("T") ("m") ("b") ("B"))
  "Key=value options for the outerspec of tabularray environments.")

(defun LaTeX-tabularray-outerspec-key-val ()
  "Return key=value options for the outerspec of tabularray environments."
  (append
   (when (member (LaTeX-current-environment) '("longtblr" "talltblr"))
     (let ((len (mapcar (lambda (x) (concat TeX-esc (car x)))
                        (LaTeX-length-list))))

       `(("headsep" ,len)
         ("footsep" ,len)
         ("presep"  ,len)
         ("postsep" ,len)
         ("theme")
         ("caption")
         ("entry" ("none"))
         ("label" ("none"))
         ("note{}")
         ("remark{}"))))
   (when (member "functional" (LaTeX-tabularray-used-library))
     '(("evalute")))
   LaTeX-tabularray-outerspec-key-val))

(defun LaTeX-tabularray-innerspec-key-val ()
  "Return key=value options for the innerspec of tabularray environments."
  (append
   (when (member (LaTeX-current-environment) '("longtblr" "talltblr"))
     '(("rowhead")
       ("rowfoot")))
   (when (member "functional" (LaTeX-tabularray-used-library))
     '(("process")))
   (let ((len (mapcar (lambda (x) (concat TeX-esc (car x)))
                      (LaTeX-length-list))) )
     `(("hlines" ("solid" "dashed" "dotted" "text"
                  "wd" "leftpos" "rightpos" "endpos" "{}{-}{}"))
       ("vlines" ("solid" "dashed" "dotted" "text"
                  "wd" "abovepos" "belowpos" "{odd}{}" "{even}{}"))
       ("hline{}" ("solid" "dashed" "dotted" "text"
                   "wd" "leftpos" "rightpos" "endpos"))
       ("vline{}" ("solid" "dashed" "dotted" "text"
                   "wd" "abovepos" "belowpos" "{odd}{}" "{even}{}"
                   "{}{}"))
       ("hborder{}" ("pagebreak" "abovespace" "belowspace"
                     "abovespace+" "belowspace+"))
       ("vborder{}" ("leftspace" "rightspace"
                     "leftspace+" "rightspace+"))

       ("cells" ("halign" "l" "c" "r" "j" "valign" "t" "m" "b" "wd"
                 "bg" "fg" "font" "mode" "$" "$$" "cmd" "preto" "appto"))
       ("cell{}{}" ("halign" "l" "c" "r" "j" "valign" "t" "m" "b" "wd"
                    "bg" "fg" "font" "mode" "$" "$$" "cmd" "preto" "appto"))

       ("rows" ("halign" "l" "c" "r" "j" "valign" "t" "m" "b" "h" "f"
                "ht" "bg" "fg" "font" "mode" "$" "$$" "cmd"
                "abovesep" "abovesep+" "belowsep" "belowsep+"
                "rowsep" "rowsep+" "preto" "appto"))
       ("columns" ("halign" "l" "c" "r" "j" "valign" "t" "m" "b" "h" "f"
                   "wd" "co" "bg" "fg" "font" "mode" "$" "$$" "cmd"
                   "leftsep" "leftsep+" "rightsep" "rightsep+"
                   "colsep" "colsep+" "preto" "appto"))

       ("row{}" ("halign" "l" "c" "r" "j" "valign" "t" "m" "b" "h" "f"
                 "ht" "bg" "fg" "font" "mode" "$" "$$" "cmd"
                 "abovesep" "abovesep+" "belowsep" "belowsep+"
                 "rowsep" "rowsep+" "preto" "appto"))
       ("column{}" ("halign" "l" "c" "r" "j" "valign" "t" "m" "b" "h" "f"
                    "wd" "co" "bg" "fg" "font" "mode" "$" "$$" "cmd"
                    "leftsep" "leftsep+" "rightsep" "rightsep+"
                    "colsep" "colsep+" "preto" "appto"))
       ("width"    ,len)
       ("colspec"  ("{}"))
       ("rulesep"  ,len)
       ("stretch")
       ("abovesep" ,len)
       ("belowsep" ,len)
       ("rowsep"   ,len)
       ("leftsep"  ,len)
       ("rightsep" ,len)
       ("colsep"   ,len)
       ("hspan"    ("default" "even" "minimal"))
       ("vspan"    ("default" "even"))
       ("baseline" ("t" "T" "m" "b" "B"))))))

(defvar LaTeX-tabularray-longtable-template-elements
  '("contfoot-text"
    "contfoot"
    "conthead-text"
    "conthead"
    "caption-tag"
    "caption-sep"
    "caption-text"
    "caption"
    "note-tag"
    "note-sep"
    "note-text"
    "note"
    "remark-tag"
    "remark-sep"
    "remark-text"
    "remark"
    "firsthead"
    "middlehead"
    "lasthead"
    "head"
    "firstfoot"
    "middlefoot"
    "lastfoot"
    "foot")
  "List of longtable template elements defined by tabularray.sty.")

(defvar LaTeX-tabularray-longtable-template-names
  '(("normal") ("empty") ("fg") ("font") ("halign") ("indent") ("hang"))
  "AList of longtable template names and keys defined by tabularray.sty.")

(TeX-auto-add-type "tabularray-NewColumnType" "LaTeX")

(defvar LaTeX-tabularray-NewColumnType-regexp
  '("\\\\NewColumnType{\\([^}]+\\)}"
    1 LaTeX-auto-tabularray-NewColumnType)
  "Matches the argument of \\NewColumnType from tabularray package.")

(defun LaTeX-tabularray-update-column-letters ()
  "Update and uniquify the local value of `LaTeX-array-column-letters'."
  (setq-local LaTeX-array-column-letters
              (let* ((newtypes (mapconcat #'car
                                          (LaTeX-tabularray-NewColumnType-list)
                                          ""))
                     (alltypes (concat LaTeX-array-column-letters newtypes)))
                (seq-concatenate 'string (seq-uniq alltypes #'=)))))

(TeX-auto-add-type "tabularray-UseTblrLibrary"
                   "LaTeX"
                   "tabularray-UseTblrLibraries")

(defvar LaTeX-tabularray-UseTblrLibrary-regexp
  '("\\\\UseTblrLibrary{\\([^}]+\\)}"
    1 LaTeX-auto-tabularray-UseTblrLibrary)
  "Matches the argument of \\UseTblrLibrary from tabularray package.")

(defun LaTeX-tabularray-used-library ()
  "Return a list of used tabularray libraries in current document."
  (when LaTeX-auto-tabularray-UseTblrLibrary
    (apply #'append
           (mapcar (lambda (x) (string-split x "," t))
                   (mapcar (lambda (y)
                             (replace-regexp-in-string "[ %\n\r\t]" "" y))
                           LaTeX-auto-tabularray-UseTblrLibrary)))))

(TeX-auto-add-type "tabularray-NewTblrEnviron" "LaTeX")

(defvar LaTeX-tabularray-NewTblrEnviron-regexp
  '("\\\\NewTblrEnviron{\\([^}]+\\)}"
    1 LaTeX-auto-tabularray-NewTblrEnviron)
  "Matches the argument of \\NewTblrEnviron from tabularray package.")

(defun LaTeX-tabularray-NewTblrEnviron-cleanup ()
  "Process user defined environments with tabularray package."
  (let (result)
    (dolist (elt (LaTeX-tabularray-NewTblrEnviron-list))
      (push (cons (car elt)
                  '(LaTeX-env-args
                    [TeX-arg-key-val LaTeX-tabularray-outerspec-key-val
                                     "Outer spec" nil ?\s]
                    (TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)
                                     "Inner spec")
                    (LaTeX-env-label-as-keyval "caption")
                    (lambda (_)
                      (LaTeX-item-tabularray t))))
            result))
    (apply #'LaTeX-add-environments result)))

(defun LaTeX-tabularray-auto-prepare ()
  "Clear `LaTeX-auto-tabularray-*' before parsing."
  (setq LaTeX-auto-tabularray-NewColumnType  nil
        LaTeX-auto-tabularray-UseTblrLibrary nil
        LaTeX-auto-tabularray-NewTblrEnviron nil))

(defun LaTeX-tabularray-auto-cleanup ()
  "Process parsed elements for tabularray package.
This function updates the value of `LaTeX-array-column-letters' with
newly defined column specifications, runs `TeX-run-style-hooks' for
loaded libraries and adds support for new environments via
`LaTeX-add-environments'."
  (LaTeX-tabularray-update-column-letters)
  (apply #'TeX-run-style-hooks (LaTeX-tabularray-used-library))
  (LaTeX-tabularray-NewTblrEnviron-cleanup))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-tabularray-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-tabularray-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defvar LaTeX-tabularray-skipping-regexp
  "[ \t]*\\(?:\\[[^]]+\\]\\)?[ \t]*"
  "Regexp matching between \\begin{tblr} and column specification.
For tblr environment only.  See `LaTeX-insert-ampersands' for detail.

This regexp assumes that the optional outer specification doesn't
contain a closing ] in any fashion.")

(defun LaTeX-tabularray-count-columns (start end)
  "Adjust START and END acc. to column spec inside the tabularray env's.
This function checks if the column specification is given as value to
the \"colspec\" key and adjusts the values of START and END before
running `LaTeX-array-count-columns' with them."
  (save-excursion
    (when (re-search-forward "colspec[ \t]*=[ \t]*" end t)
      (if (= (following-char) ?\{)
          ;; The columns spec is enclosed in a group, so handle it:
          (progn
            (forward-char)
            (setq start (point))
            (setq end (1- (TeX-find-closing-brace))))
        ;; The spec is given without a group, i.e., colspec=lcr, so we
        ;; travel until the next ',' or '}':
        (setq start (point))
        (skip-chars-forward "^,}")
        (setq end (point)))))
  (LaTeX-array-count-columns start end))

(defun LaTeX-item-tabularray (&optional suppress)
  "Insert line break macro on the last line and suitable number of &'s.
For tabularray environments.  If SUPPRESS is non-nil, do not
insert line break macro."
  (if suppress
      (save-excursion
        (goto-char TeX-exit-mark)
        (LaTeX-insert-ampersands
         LaTeX-tabularray-skipping-regexp #'LaTeX-tabularray-count-columns))
    (save-excursion
      (end-of-line 0)
      (just-one-space)
      (TeX-insert-macro "\\"))
    (LaTeX-insert-ampersands
     LaTeX-tabularray-skipping-regexp #'LaTeX-tabularray-count-columns)))

(TeX-add-style-hook
 "tabularray"
 (lambda ()

   (TeX-auto-add-regexp LaTeX-tabularray-NewColumnType-regexp)
   (TeX-auto-add-regexp LaTeX-tabularray-UseTblrLibrary-regexp)
   (TeX-auto-add-regexp LaTeX-tabularray-NewTblrEnviron-regexp)

   ;; Pre-defined environments:
   (LaTeX-add-tabularray-NewTblrEnvirons "tblr" "longtblr" "talltblr")
   ;; 5.2 Library booktabs
   (when (member "booktabs" (LaTeX-tabularray-used-library))
     (LaTeX-add-tabularray-NewTblrEnvirons "booktabs"))
   ;; Pre-defined column types:
   (LaTeX-add-tabularray-NewColumnTypes "Q" "t" "m" "b" "h" "f" "X")
   ;; Update the internals:
   (LaTeX-tabularray-NewTblrEnviron-cleanup)
   (LaTeX-tabularray-update-column-letters)

   ;; Load ninecolors.el if xcolor.el is loaded:
   (when (member "xcolor" (TeX-style-list))
     (TeX-run-style-hooks "ninecolors"))

   (TeX-add-symbols
    ;; 2.6.2 Column Types
    '("NewColumnType"
      (lambda (optional)
        (let ((col (TeX-read-string
                    (TeX-argument-prompt optional nil "Column type"))))
          (LaTeX-add-tabularray-NewColumnTypes col)
          (LaTeX-tabularray-update-column-letters)
          (TeX-argument-insert col optional)))
      [ "Number of arguments" ] [ "Default value for first argument" ] t)

    ;; 2.6.3 Row Types
    '("NewRowType" "Row type" [ "Number of arguments" ] t)

    ;; 3.3 Default Specifications
    '("SetTblrInner"
      (TeX-arg-key-val
       [TeX-arg-completing-read-multiple (LaTeX-tabularray-NewTblrEnviron-list)
                                         "Tabularray environment(s)"]
       (LaTeX-tabularray-innerspec-key-val)))

    '("SetTblrOuter"
      (TeX-arg-key-val
       [TeX-arg-completing-read-multiple (LaTeX-tabularray-NewTblrEnviron-list)
                                         "Tabularray environment(s)"]
       (LaTeX-tabularray-innerspec-key-val)))

    '("UseTblrLibrary"
      (TeX-arg-completing-read-multiple ("amsmath"
                                         "booktabs"
                                         "counter"
                                         "diagbox"
                                         "functional"
                                         "nameref"
                                         "siunitx"
                                         "varwidth"
                                         "zref")
                                        "Library")
      (lambda (_)
        (save-excursion
          (re-search-backward "\\\\UseTblrLibrary{\\([^}]+\\)}"
                              (line-beginning-position) t)
          (LaTeX-add-tabularray-UseTblrLibraries
           (match-string-no-properties 1))
          (apply #'TeX-run-style-hooks (LaTeX-tabularray-used-library)))))

    ;; 3.4 New Tabularray Environments
    '("NewTblrEnviron"
      (lambda (optional)
        (let ((env (TeX-read-string
                    (TeX-argument-prompt optional nil "New enviroment"))))
          (LaTeX-add-tabularray-NewTblrEnvirons env)
          (LaTeX-tabularray-NewTblrEnviron-cleanup)
          (TeX-argument-insert env optional))))

    ;; 4.2 Customize Templates
    '("DefTblrTemplate"
      (TeX-arg-completing-read-multiple LaTeX-tabularray-longtable-template-elements
                                        "Template element")
      (TeX-arg-key-val LaTeX-tabularray-longtable-template-names
                       "Template name")
      t)

    '("SetTblrTemplate"
      (TeX-arg-completing-read-multiple LaTeX-tabularray-longtable-template-elements
                                        "Template element")
      (TeX-arg-key-val LaTeX-tabularray-longtable-template-names
                       "Template name"))

    '("UseTblrTemplate"
      (TeX-arg-completing-read-multiple LaTeX-tabularray-longtable-template-elements
                                        "Template element")
      (TeX-arg-key-val LaTeX-tabularray-longtable-template-names
                       "Template name"))

    '("InsertTblrText"
      (TeX-arg-completing-read LaTeX-tabularray-longtable-template-elements
                               "Template element"))

    '("MapTblrNotes" t)
    "InsertTblrNoteTag"
    "InsertTblrNoteText"
    "InsertTblrRemarkTag"
    "InsertTblrRemarkText"

    ;; 4.4 Define Themes
    '("NewTblrTheme" "Theme" t) )

   ;; 5.1 Library amsmath
   (when (member "amsmath" (LaTeX-tabularray-used-library))
     (LaTeX-add-environments
      '("+array" LaTeX-env-array)
      '("+matrix" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])
      '("+bmatrix" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])
      '("+Bmatrix" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])
      '("+pmatrix" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])
      '("+vmatrix" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])
      '("+Vmatrix" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])
      '("+cases" LaTeX-env-args
        [TeX-arg-key-val (LaTeX-tabularray-innerspec-key-val)])))

   ;; 5.4 Library diagbox
   (when (member "diagbox" (LaTeX-tabularray-used-library))
     (TeX-add-symbols
      '("diagboxthree"
        [TeX-arg-key-val (LaTeX-diagbox-key-val-options)] 3)))

   ;; Add env's to `LaTeX-item-list':
   (dolist (env (mapcar #'car (LaTeX-tabularray-NewTblrEnviron-list)))
     (add-to-list 'LaTeX-item-list (cons env 'LaTeX-item-tabularray) t))

   ;; Cater for label prefix and tell RefTeX:
   (let ((envs '("longtblr" "talltblr")))
     (dolist (env envs)
       (add-to-list 'LaTeX-label-alist (cons env 'LaTeX-table-label) t)
       (when (fboundp 'reftex-add-label-environments)
         (reftex-add-label-environments
          `((,env ?t nil nil LaTeX-keyval-caption-reftex-context-function)))))
     (when (boundp 'reftex-label-regexps)
       (let ((re (concat (regexp-quote TeX-esc)
                         "begin[[:space:]]*"
                         (regexp-quote TeX-grop)
                         (regexp-opt envs)
                         (regexp-quote TeX-grcl)
                         "[[:space:]]*"
                         ;; Match the opening [ and the following chars
                         "\\[[^][]*"
                         ;; Allow nested levels of chars enclosed in braces
                         "\\(?:{[^}{]*"
                         "\\(?:{[^}{]*"
                         "\\(?:{[^}{]*}[^}{]*\\)*"
                         "}[^}{]*\\)*"
                         "}[^][]*\\)*"
                         ;; Match the label key
                         "\\<label[[:space:]]*=[[:space:]]*"
                         ;; Match the label value; braces around the value are
                         ;; optional.
                         "{?\\(?1:[^] ,}\r\n\t%]+\\)")))
         (unless (member re reftex-label-regexps)
           (add-to-list 'reftex-label-regexps re t)
           (reftex-compile-variables)))))

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("NewColumnType"    "{[[{")
                                ("NewRowType"       "{[{")
                                ("SetTblrInner"     "[{")
                                ("SetTblrOuter"     "[{")
                                ("NewTblrEnviron"   "{")
                                ("UseTblrLibrary"   "{")
                                ("DefTblrTemplate"  "{{{")
                                ("SetTblrTemplate"  "{{")
                                ("NewTblrTheme"     "{{"))
                              'function)
     (when (member "diagbox" (LaTeX-tabularray-used-library))
       (font-latex-add-keywords '(("diagboxthree"   "[{{{"))
                                'textual))) )
 TeX-dialect)

(defvar LaTeX-tabularray-package-options nil
  "Package options for the tabularray package.")

;;; tabularray.el ends here
