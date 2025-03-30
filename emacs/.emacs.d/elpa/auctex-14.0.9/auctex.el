;;; auctex.el --- Integrated environment for *TeX*  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2024 Free Software Foundation, Inc.

;; Version: 14.0.9
;; URL: https://www.gnu.org/software/auctex/
;; Maintainer: auctex-devel@gnu.org
;; Notifications-To: auctex-diffs@gnu.org
;; Package-Requires: ((emacs "27.1"))
;; Keywords: TeX LaTeX Texinfo ConTeXt docTeX preview-latex

;; This file is part of AUCTeX.

;; AUCTeX is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; AUCTeX is an integrated environment for writing input files for TeX,
;; LaTeX, ConTeXt, Texinfo, and docTeX.  The component preview-latex
;; provides true "WYSIWYG" experience in the sourcebuffer.

;;; Code:

;; This can be used for starting up AUCTeX, e.g., when not installed
;; from ELPA.  We have to set `no-byte-compile' to t otherwise the
;; compiler will eval the form during the compilation where
;; `load-file-name' is nil and things will go wrong.

(require 'tex-site
         (expand-file-name "tex-site.el"
                           (file-name-directory load-file-name)))

(defconst AUCTeX-version (package-get-version)
  "AUCTeX version.")

(provide 'auctex)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; auctex.el ends here
