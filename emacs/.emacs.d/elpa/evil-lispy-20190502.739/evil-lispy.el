;;; evil-lispy.el --- precision Lisp editing with Evil and Lispy

;; Copyright (C) 2015 Brandon Carrell

;; Author: Brandon Carrell <brandoncarrell@gmail.com>, Mika Vilpas <mika.vilpas@gmail.com>
;; URL: https://github.com/sp3ctum/evil-lispy
;; Version: 1.1
;; Keywords: lisp
;; Package-Requires: ((lispy "0.26.0") (evil "1.2.12") (hydra "0.13.5"))


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; evil-lispy defines a minor mode and an additional Evil state for editing
;; Lisp code.  The goal is to encourage a workflow where you can hop between
;; Lispy State for making structured edits using Lispy bindings and the rest
;; of the standard Evil states for general editing.

;;; Code:
(require 'lispy)

(require 'evil-lispy-core)
(require 'evil-lispy-keybinds)
(require 'evil-lispy-help)

(provide 'evil-lispy)
;;; evil-lispy.el ends here
