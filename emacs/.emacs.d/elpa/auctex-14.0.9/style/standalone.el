;;; standalone.el --- AUCTeX style for `standalone.cls|sty'   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2024-02-18
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

;; This file adds support for `standalone.sty' and `standalone.cls'
;; v1.3b from 2022/10/10.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defun LaTeX-standalone-auto-cleanup ()
  "Parse the value of \"class\" key and run the appropriate style hook."
  (let ((cls (TeX-member "\\`class="
                         (cdr (assoc "standalone" LaTeX-provided-class-options))
                         #'string-match)))
    (when cls
      (TeX-run-style-hooks (cadr (split-string cls "=" t))))))

(add-hook 'TeX-auto-cleanup-hook #'LaTeX-standalone-auto-cleanup t)

(TeX-add-style-hook
 "standalone"
 (lambda ()

   ;; General macros/env's provided by the class and the package:
   (TeX-add-symbols
    '("ifstandalonebeamer" 0)
    '("ifstandalone" 0))

   ;; Fontification:
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("standaloneconfig" "{"))
                              'function))

   ;; standalone.cls:
   (when (assoc "standalone" LaTeX-provided-class-options)
     (TeX-add-symbols
      '("standaloneconfig"
        (TeX-arg-key-val (LaTeX-standalone-class-options-list)))
      '("standaloneignore" 0) )

     (LaTeX-add-environments "standalone")

     ;; \standaloneenv is relevant in conjunction with option multi:
     (when (or (LaTeX-provided-class-options-member "standalone" "multi")
               (LaTeX-provided-class-options-member "standalone" "multi=true"))
       (TeX-add-symbols
        '("standaloneenv"
          (TeX-arg-completing-read-multiple (LaTeX-environment-list)
                                            "Enviroment(s)")))
       ;; Fontification
       (when (and (featurep 'font-latex)
                  (eq TeX-install-font-lock 'font-latex-setup))
         (font-latex-add-keywords '(("standaloneenv" "{"))
                                  'function)))

     ;; standaloneframe env is relevant in conjunction with option beamer:
     (when (or (LaTeX-provided-class-options-member "standalone" "beamer")
               (LaTeX-provided-class-options-member "standalone" "beamer=true"))
       (TeX-run-style-hooks "beamer")
       (LaTeX-add-environments '("standaloneframe")))

     ;; TikZ class option:
     (when (or (LaTeX-provided-class-options-member "standalone" "tikz")
               (LaTeX-provided-class-options-member "standalone" "tikz=true"))
       (TeX-run-style-hooks "tikz")))

   ;; standalone.sty
   (when (assoc "standalone" LaTeX-provided-package-options)
     (TeX-load-style "graphicx")
     (TeX-add-symbols
      '("standaloneconfig"
        (TeX-arg-key-val (("group" ("true" "false"))
                          ("mode" ("tex" "image" "tex|image" "build"
                                   "buildmissing" "buildnew"))
                          ("extension")
                          ("build"))))
      '("includestandalone"
        [TeX-arg-key-val (lambda ()
                           (append (LaTeX-graphicx-key-val-options)
                                   '(("group" ("true" "false"))
                                     ("mode" ("tex" "image" "tex|image" "build"
                                              "buildmissing" "buildnew"))
                                     ("extension")
                                     ("build"))))]
        TeX-arg-file-name-sans-extension))

     ;; Fontification
     (when (and (featurep 'font-latex)
                (eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("includestandalone" "{"))
                                'reference))) )
 TeX-dialect)

(defun LaTeX-standalone-class-options-list ()
  "Return an alist of class options for the standalone class."
  `(("class" ,LaTeX-global-class-files)
    ("multi" ,(append '("true" "false")
                      (mapcar #'car (LaTeX-environment-list))))
    ("crop" ("true" "false"))
    ("preview" ("true" "false"))
    ("border" ("{}"))
    ("ignorerest" ("true" "false"))
    ("multido" ("true" "false"))
    ("varwidth" ("true" "false"))
    ("tikz" ("true" "false"))
    ("pstricks" ("true" "false"))
    ("beamer" ("true" "false"))
    ("float" ("true" "false"))
    ("convert" ("true" "false"))
    ("png")))

(defun LaTeX-standalone-class-options ()
  "Prompt for the class options for the standalone class."
  (TeX-read-key-val t (LaTeX-standalone-class-options-list)))

(defvar LaTeX-standalone-package-options-list
  '(("subpreambles" ("true" "false"))
    ("sort" ("true" "false"))
    ("print" ("true" "false"))
    ("comments" ("true" "false"))
    ("nocomments")
    ("group" ("true" "false"))
    ("mode" ("tex" "image" "tex|image" "build" "buildmissing" "buildnew"))
    ("obeyclassoptions" ("true" "false"))
    ("extension")
    ("build"))
  "Package options for the standalone package.")

(defun LaTeX-standalone-package-options ()
  "Prompt for options of the standalone package."
  (TeX-arg-key-val t LaTeX-standalone-package-options-list))

;;; standalone.el ends here
