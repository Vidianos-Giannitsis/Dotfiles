;;; auctex.el --- Integrated environment for *TeX*  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2024 Free Software Foundation, Inc.

;; Version: 13.2.4
;; URL: https://www.gnu.org/software/auctex/
;; Maintainer: auctex-devel@gnu.org
;; Notifications-To: auctex-diffs@gnu.org
;; Package-Requires: ((emacs "26.1"))
;; Keywords: TeX LaTeX Texinfo ConTeXt docTeX preview-latex

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This can be used for starting up AUCTeX.  The following somewhat
;; strange trick causes tex-site.el to be loaded in a way that can be
;; safely undone using (unload-feature 'tex-site).

;; FIXME: I don't quite understand in which way this is better than less
;; strange code such as
;;     (require 'tex-site (expand-file-name "tex-site.el"
;;                         (file-name-directory load-file-name))

;;; Code:

(autoload 'TeX-load-hack
  (expand-file-name "tex-site.el"
                    (file-name-directory load-file-name)))
(TeX-load-hack)

(provide 'auctex)

;;; auctex.el ends here
