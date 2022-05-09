;;; ox-oer-reveal.el --- Loader for oer-reveal  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2019 Jens Lechtenbörger

;;; Commentary:
;; Org export back-ends have file names starting with "ox-".
;; The prefix "ox-" is hard-coded in org.el and used to load
;; back-ends in `org-export-backends'.  With this file, you can
;; customize `org-export-backends' and add `oer-reveal'.  Then, when
;; pressing `C-c C-e', this file will be loaded, which loads
;; oer-reveal.el.

;;; Code:
(require 'oer-reveal)
(provide 'ox-oer-reveal)
;;; ox-oer-reveal.el ends here
