;;; publish.el --- Publish reveal.js presentations from Org sources
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; SPDX-FileCopyrightText: 2019,2021 Jens Lechtenbörger
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License: GPLv3

;;; Commentary:
;; Publication of Org source files to reveal.js uses Org export
;; functionality offered by org-re-reveal and oer-reveal.
;;
;; Initialization code for both is also provided by emacs-reveal, e.g.,
;; in the Docker image emacs-reveal:
;; https://gitlab.com/oer/emacs-reveal/container_registry/976295
;; Org-re-reveal and oer-reveal are also available on MELPA.
;;
;; Use this file from its parent directory with the following shell
;; command, *after* you took care to install submodules (see README.md):
;; emacs --batch --load examples/publish.el

;;; Code:
(package-initialize)

;; With `add-to-list', prefer most recent version of oer-reveal and
;; org-re-reveal from parent directories (if available) over embedded ones:
(add-to-list 'load-path
	     (expand-file-name "../"
                               (file-name-directory load-file-name)))
(add-to-list 'load-path
	     (expand-file-name "../../org-re-reveal/"
                               (file-name-directory load-file-name)))
(require 'oer-reveal-publish)

(let ((org-publish-project-alist
       (list
	(list "examples"
	      :base-directory "examples"
	      :base-extension "org"
	      :publishing-function 'oer-reveal-publish-to-reveal-and-pdf
	      :publishing-directory "./public"))))
  (oer-reveal-publish-setq-defaults)
  (oer-reveal-publish-all))

;;; publish.el ends here
