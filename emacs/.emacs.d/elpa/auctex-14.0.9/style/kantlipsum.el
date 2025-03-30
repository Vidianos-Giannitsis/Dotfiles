;;; kantlipsum.el --- AUCTeX style for `kantlipsum.sty'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2020 Free Software Foundation, Inc.

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

;; This file adds support for `kantlipsum.sty'.

;;; Code:

(require 'tex)

(TeX-add-style-hook
 "kantlipsum"
 (lambda ()
   (TeX-add-symbols
    '("kant" [ "Range of paragraph (max: 164)" ])
    '("kant*" [ "Range of paragraph (max: 164)" ])
    '("kantdef" TeX-arg-define-macro "Paragraph number"))
   (TeX-run-style-hooks "xparse"))
 TeX-dialect)

(defvar LaTeX-kantlipsum-package-options
  '("par" "nopar" "numbers" "index")
  "Package options for the kantlipsum package.")

;; kantlipsum.el ends here
