;;; epsf.el - Support for the epsf style option.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2024 Free Software Foundation, Inc.

;; Author: Marc Gemis <makke@wins.uia.ac.be>

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

(TeX-add-style-hook
 "epsf"
 (lambda ()
   (TeX-add-symbols
    '("epsfsize" TeX-arg-epsfsize)
    '("epsffile" TeX-arg-file)
    '("epsfbox" TeX-arg-file)
    "epsflly" "epsfury" "testit" "epsfgetlitbb"
    "epsfnormal" "epsfgetbb" "other" "epsfsetgraph"
    "PsFragSpecialArgs" "epsfaux" "testit" "epsfgrab"
    "epsfllx" "epsflly" "epsfury" "epsfverbosetrue"))
 TeX-dialect)

(defun TeX-arg-epsfsize (_optional &optional _prompt _definition)
  "Create a line that print epsf figures at a certain percentage"
  (interactive)
  (let ((scale (TeX-read-string "Scale in percent (default 75): ")))
    (setq scale (if (zerop (length scale)) "75" scale))
    (save-excursion
      ; append #1#{scale#1}
      (insert "#1#2" TeX-grop "0." scale "#1" TeX-grcl)
      ; insert \def before \epsfsize
      (beginning-of-line 1)
      (newline)
      (insert TeX-esc "def")
      (forward-line -1)
      (insert "% From now on print figures at " scale "% of original size"))
    (end-of-line)))

;;; epsf.el ends here
