;;; psfig.el - Support for the psfig style option.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2024 Free Software Foundation, Inc.

;; Author: Marc Gemis <makke@wins.uia.ac.be>
;; Maintainer: auctex-devel@gnu.org

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

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "psfig"
 (lambda ()
   ;; probable some of the following symbols may be removed
   (TeX-add-symbols "protect" "figurepath"  "fbox"
                    "other" "letter" "other" "then" "Sine" "Cosine"
                    "psdraft" "psfull" "psscalefirst" "psrotatefirst"
                    "psnodraftbox" "psdraftbox" "pssilent" "psnoisy"
                    "minmaxtest"
                    '("psfig" TeX-arg-psfig)
                    '("psfigurepath" t)
                    )
   (LaTeX-add-environments
    '("psfigure" LaTeX-env-psfigure)))
 TeX-dialect)

(defun TeX-arg-psfig (_optional)
  "Ask for file, width and length. Insert psfig macro"
  (let ((psfile (read-file-name "PS-file: " "" "" nil))
        (figwidth (TeX-read-string "Figure width: "))
        (figheight (TeX-read-string "Figure height: "))
        )

    (insert TeX-grop "figure=" psfile)
    (if (not (zerop (length figwidth)))
        (insert ",width=" figwidth))
    (if (not (zerop (length figheight)))
        (insert ",height=" figheight))
    (insert TeX-grcl)))


(defun LaTeX-env-psfigure (_environment)
  "Create  with \\label and \\caption and \\psfig commands."
  (let* ((float (TeX-read-string "Float to: " LaTeX-float))
         (caption (TeX-read-string "Caption: "))
         (short-caption (when (>= (length caption) LaTeX-short-caption-prompt-length)
                          (TeX-read-string "(Optional) Short caption: ")))
         (label (TeX-read-string "Label: " LaTeX-figure-label))
                                        ; gf: ask if this should be centered
         (psfile (read-file-name "PS-file: " "" "" nil))
         (figwidth (TeX-read-string "Figure width: "))
         (figheight (TeX-read-string "Figure height: "))
         )

    (setq LaTeX-float (if (zerop (length float))
                          LaTeX-float
                        float))

    (LaTeX-insert-environment "figure"
                              (concat LaTeX-optop LaTeX-float LaTeX-optcl))

    (insert TeX-esc "centerline" TeX-grop TeX-esc "psfig" TeX-grop
            "figure=" psfile)
    (if (not (zerop (length figwidth)))
        (insert ",width=" figwidth))
    (if (not (zerop (length figheight)))
        (insert ",height=" figheight))
    (insert TeX-grcl TeX-grcl)
    (if (zerop (length caption))
        ()
      (newline-and-indent)
      (insert (LaTeX-compose-caption-macro caption short-caption)))
    (if (or (zerop (length label))
            (equal LaTeX-figure-label label))
        ()
      (newline-and-indent)
      (insert TeX-esc "label" TeX-grop label TeX-grcl))

    (forward-line 2)))

;;; psfig.el ends here
