(general-create-definer my-leader-def
			    :prefix "SPC")

(my-leader-def
 :states 'normal
 :keymaps 'override
  "!" 'shell-command
  "P" 'package-install
  "o" '(inferior-octave :which-key "octave")
  "D" 'dired
  "d" '(:ignore t :which-key "Dired - File operations")
  "q" '(my/quickmarks-hydra/body :which-key "Quickmarks")
  "t" '(:ignore t :which-key "Move between vterms")
  "T" 'org-babel-tangle
  "RET" 'vterm-toggle
  "<C-return>" 'named-vterm 
  "b" 'counsel-switch-buffer
  "A" 'org-agenda
  "h" 'split-window-horizontally
  "v" 'split-window-vertically
  "c" '(calc-dispatch :which-key "Open the calc dispatcher")
  "R" '(:ignore t :which-key "REPLs")
  "m" 'magit
  "B" 'ivy-bibtex
  "r" '(my/roam-ref-hydra/body :which-key "Org Roam/Ref")
  "j" '(:ignore t :which-key "Daily notes")
  "H" 'counsel-imenu
  "C" '(:ignore t :which-key "Calendar Commands")
  "l" 'ace-link
  "L" '(:ignore t :which-key "Links")
  "S" '(:ignore t :which-key "Counsel-spotify")
  "e" '(:ignore t :which-key "Evaluate Emacs-Lisp")
  "f" '(ace-window :which-key "Switch focus")
  "φ" '(ace-window :which-key "Switch focus")
  "w" '(:ignore t :which-key "Define Word")
  "J" '(:ignore t :which-key "Julia")
  "W" 'wolfram-alpha
  "n" 'winner-redo
  "u" 'winner-undo
  "p" 'projectile-commander
  "i" '(my/org-pandoc-hydra/body :which-key "Org Pandoc Import")
  "y" 'ivy-yasnippet
  "z" '(zetteldesk-main-hydra/body :which-key "Zetteldesk")
  "s" '(:ignore t :which-key "Sly Commands")
  "g" 'counsel-rg
  "E" 'eperiodic
  "ζ ι" 'zetteldesk-insert-node-contents
  "M" 'imaxima
  "β" 'counsel-switch-buffer
  "I" 'iedit-mode
  "F" '(my/flyspell-hydra/body :which-key "Flyspell Commands")
  "/" '(:ignore t :which-key "Search Engines")
  "a" '(:ignore t :which-key "Avy Commands"))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC L"
 "o" 'org-open-at-point
 "n" 'org-next-link
 "p" 'org-previous-link
 "t" 'org-toggle-link-display)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC /"
 "g" 'engine/search-google
 "y" 'engine/search-youtube
 "a" 'engine/search-archwiki
 "r" 'engine/search-reddit
 "l" 'engine/search-lutris
 "p" 'engine/search-protondb
 "L" 'engine/search-lolchess
 "w" 'engine/search-wolfram
 "s" 'engine/search-sciencedirect
 "t" 'engine/search-translate
 "B" 'engine/search-bulbapedia
 "b" 'bookmark-selector-browse-bookmark
 "G" 'engine/search-github)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC a"
 "c" 'avy-goto-char
 "C" 'avy-goto-char-2
 "t" 'avy-goto-char-timer
 "l" 'avy-goto-line
 "w" 'avy-goto-word-1
 "W" 'avy-goto-word-0
 "i" 'avy-isearch)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC R"
 "o" '(inferior-octave :which-key "Octave")
 "p" '(run-python :which-key "Python")
 "J" '(ein:jupyter-server-start :which-key "Jupyter Notebook")
 "j" '(julia-snail :which-key "Julia")
 "g" '(run-gnuplot :which-key "Gnuplot")
 "e" '(ielm :which-key "Emacs Lisp")
 "m" '(maxima :which-key "Maxima"))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC s"
 "r" '(sly :which-key "Start the REPL")
 "h" '(sly-documentation-lookup :which-key "describe-symbol")
 "d" 'sly-hyperspec-lookup
 "l" 'sly-load-file
 "c" 'sly-compile-and-load-file
 "E" 'sly-interactive-eval
 "e" 'sly-eval-last-expression
 "s" 'sly-scratch)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC C"
 "b" 'cfw:open-calendar-buffer
 "o" '(cfw:open-org-calendar :which-key "Open calendar with scheduled to-dos")
 "g" '(cfw:git-open-calendar :which-key "Open calendar with git commit history"))

(pretty-hydra-define my/flyspell-hydra (:color blue :title "Flyspell Commands")
  ("Toggles"
   (("m" flyspell-mode "Toggle the Flyspell mode")
    ("b" flyspell-buffer "Spellcheck the current buffer"))

   "Correct word"
   (("n" flyspell-correct-next "Correct next word" :exit nil)
    ("p" flyspell-correct-previous "Correct-previous word" :exit nil))))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC e"
 "b" 'eval-buffer
 "e" 'eval-expression
 "f" 'eval-defun
 "s" 'eval-last-sexp)



;; (general-define-key
;;  :states 'normal
;;  :keymaps 'override
;;  :prefix "SPC z"
;;  "b" 'zetteldesk-switch-to-buffer
;;  "a" '(:ignore t :which-key "Add to Zetteldesk")
;;  "a b" 'zetteldesk-add-to-desktop
;;  "a n" 'zetteldesk-add-node-to-desktop
;;  "a i" 'zetteldesk-add-info-node-to-desktop
;;  "a p" 'zetteldesk-add-poi-or-moc-backlink-to-desktop
;;  "r" '(:ignore t :which-key "Remove from Zetteldesk")
;;  "r b" 'zetteldesk-remove-from-desktop
;;  "r n" 'zetteldesk-remove-node-from-desktop
;;  "r i" 'zetteldesk-remove-info-node-from-desktop
;;  "n" 'zetteldesk-node-find
;;  "s" 'zetteldesk-switch-to-scratch-buffer
;;  "i" '(:ignore t :which-key "Insert to Scratch Buffer")
;;  "i n" 'zetteldesk-insert-node-contents
;;  "i N" 'zetteldesk-insert-node-contents-without-link
;;  "i o" 'zetteldesk-insert-org-file-contents
;;  "i p" 'zetteldesk-insert-link-to-pdf
;;  "i i" 'zetteldesk-insert-info-contents
;;  "i r" 'zetteldesk-insert-ref-node-contents
;;  "I" 'zetteldesk-info-goto-node
;;  "m" 'zetteldesk-switch-to-margin-notes
;;  "R" 'zetteldesk-find-ref-node)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC d"
 "f" 'counsel-find-file
 "j" '(dired-jump :which-key "Open dired in the current buffer's directory")
 "d" 'deft
 "w" 'write-file
 "o" 'mediator-open-file
 "r" 'recentf-open)

(pretty-hydra-define my/quickmarks-hydra (:color blue :title "Quickmarks")
  ("Emacs Stuff"
   (("c" (lambda() (interactive)(find-file "~/.emacs.d/README.org")) "Literate Emacs Config")
    ("k" (lambda() (interactive)(find-file "~/.emacs.d/libs/keybindings.org")) "Emacs keybindings")
    ("s" (lambda() (interactive)(dired "~/.emacs.d/snippets/org-mode")) "Org-mode snippets")
    ("r" (lambda() (interactive)(find-file "~/.emacs.d/libs/zettelkasten.org")) "Org-Roam and friends")
    ("Z" (lambda() (interactive)(find-file "~/.emacs.d/libs/zetteldesk.org")) "Zetteldesk literate config")
    ("z" (lambda() (interactive)(dired "~/Zetteldesk")) "Zetteldesk Directory")
    ("S" (lambda() (interactive)(find-file "~/org-roam-similarity/org-roam-similarity.org")) "Org Roam Similarity Config"))

   "University"
   (("p" (lambda() (interactive)(dired "~/Documents/PhD-thesis")) "PhD documents folder")
    ("B" (lambda() (interactive)(find-file "~/Sync/My_Library.bib")) "Master Bibliography file")
    ("o" (lambda() (interactive)(dired "~/Documents/Octave")) "Octave scripts directory")
    ("O" (lambda() (interactive)(dired "~/org_roam/outlines")) "Outlines")
    ("t" (lambda() (interactive)(dired "~/Documents/9o_εξάμηνο/Masters_Thesis/")) "Thesis")
    ("C" (lambda() (interactive)(dired "~/Documents/BioRural")) "Biorural Challenge"))

   "General Computer Things"
   (("h" (lambda() (interactive)(dired "~")) "Home directory")
    ("q" (lambda() (interactive)(find-file "~/.config/qtile/README.org")) "Literate Qtile config")
    ("w" (lambda() (interactive)(find-file "~/startpage/script/var.js")) "Web Start page source")
    ("a" (lambda() (interactive)(find-file "~/auth.org")) "Git auth token")
    ("j" (lambda() (interactive)(dired "~/Documents/Julia")) "Julia")
    ("d" (lambda() (interactive)(dired "~/Games/Pokemon_Draft")) "Draft")
    ("P" (lambda() (interactive)(dired "~/Documents/Personal_docs")) "Personal Docs")
    ("b" (lambda() (interactive)(dired "~/Books")) "Books Directory"))
   ))

(pretty-hydra-define my/roam-backlinks-hydra (:color blue :title "Backlink Commands")
  ("Backlinks"
   (("f" org-roam-backlinks-node-find-by-backlinks "Find node, Sorted by Backlink Count")
    ("s" org-roam-backlinks-search-from-moc-or-poi "Search for Backlinks by MOCs and POIs")
    ("S" org-roam-backlinks-search "Search for Backlinks")
    ("r" org-roam-backlinks-find-ref-nodes "Search for Reference Backlinks"))))

(pretty-hydra-define my/org-roam-similarity-hydra (:color blue :title "Org Roam Similarity")
  ("Org-roam functions"
   (("r" org-roam-similarity-node-read "Org-roam-node-read on similar nodes")
    ("f" org-roam-similarity-node-find "Org-roam-node-find on similar nodes"))

   "Others"
   (("s" org-roam-similarity-sidebuffer "Open a sidebuffer for nodes similar to the selected")
    ("S" org-roam-similarity-sidebuffer* "Open a sidebuffer for nodes similar to the current")
    ("i" org-roam-similarity-insert-list "Insert links to similar nodes in the current buffer"))))

(pretty-hydra-define my/org-roam-thesis-hydra (:color blue :title "Org Roam Thesis")
  ("Capture Functions"
   (("l" org-roam-thesis-capture-log "Capture Log File")
    ("m" org-roam-thesis-capture-measurements "Capture Measurements File"))

   "Find Node"
   (("f" org-roam-thesis-node-find "Find thesis file")
    ("L" org-roam-thesis-log-find "Find log file")
    ("M" org-roam-thesis-measurements-find "Find measurements file"))))

(pretty-hydra-define my/roam-ref-hydra (:color blue :title "Org Roam and Org Ref")
  ("Org-roam-node-find and its filters"
   (("f" org-roam-node-find "org-roam-node-find")
    ("i" (lambda () (interactive)(find-file "~/org_roam/index.org")) "Master index file for org_roam")
    ("l" ivy-bibtex-with-notes "Find Literature Note")
    ("p" org-roam-find-permanent-node "Find Permanent Note")
    ("b" my/roam-backlinks-hydra/body "Backlinks Hydra")
    ("T" org-roam-node-find-todos "Find Fleeting Note"))

   "References"
   (("a" zotra-add-entry "Get ref from Zotra")
    ("r" ebib "Launch Ebib")
    ("C" org-ref-insert-cite-link "Insert Citation (Org-ref)")
    ("c" org-cite-insert "Insert Citation (Org-Cite)")
    ("h" org-ref-insert-link-hydra/body "Org Ref Insert-Link Hydra")
    ("B" org-ref-bibtex-hydra/body "Org Ref Bibtex Hydra")
    ("R" org-roam-create-node-from-reading-list))

   "General Org Roam Commands"
   (("G" org-roam-ui-mode "Open the Org Roam UI")
    ("S" org-roam-db-sync "Sync the Org Roam db")
    ("g" counsel-rg "Search regex in the org-roam db")
    ("s" my/org-roam-similarity-hydra/body "Org-roam-similarity commands")
    ("t" my/org-roam-thesis-hydra/body "Org-roam-thesis commands")
    ("D" org-roam-buffer-display-dedicated "Dedicated Org Roam buffer"))
   ))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC j"
 "c" 'org-journal-new-entry
 "n" 'org-journal-next-entry
 "p" 'org-journal-previous-entry
 "s" 'org-journal-search
 "t" 'org-journal-open-current-journal-file
 "d" 'org-journal-new-date-entry
 "S" 'org-journal-new-scheduled-entry)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC J"
 "r" 'julia-snail
 "h" 'julia-snail/repl-history-buffer
 "s" 'julia-snail/repl-history-search-and-yank)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC S"
 "n" 'counsel-spotify-next
 "p" 'counsel-spotify-previous
 "t" 'counsel-spotify-toggle-play-pause
 "s" '(:ignore t :which-key "Search for")
 "s t" 'counsel-spotify-search-track
 "s p" 'counsel-spotify-search-playlist
 "s a" 'counsel-spotify-search-artist)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC w"
 "w" 'define-word
 "p" 'define-word-at-point
 "s" 'dictionary-search
 "d" 'dictionary-search-dwim)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC t"
 "n" 'vterm-toggle-forward
 "N" 'vterm-toggle-backward)

(pretty-hydra-define my/org-pandoc-hydra (:color blue :title "Import files to Org")
  ("Import to org file"
   (("i" org-pandoc-import-to-org "General Import")
    ("c" org-pandoc-import-csv-to-org "Import CSV")
    ("d" org-pandoc-import-docx-to-org "Import DOCX")
    ("o" org-pandoc-import-odt-to-org "Import ODT")
    ("l" org-pandoc-import-latex-to-org "Import Latex"))

   "Import to temporary org buffer"
   (("I" org-pandoc-import-as-org "General Import")
    ("C" org-pandoc-import-csv-as-org "Import CSV")
    ("D" org-pandoc-import-docx-as-org "Import DOCX")
    ("O" org-pandoc-import-odt-as-org "Import ODT")
    ("L" org-pandoc-import-latex-as-org "Import Latex"))))

(general-define-key
 :keymaps 'override
 :prefix "C-h"
 "f" 'helpful-callable
 "v" 'helpful-variable
 "k" 'helpful-key)

(general-define-key
 :states 'normal
 :keymaps 'global
 "u" 'undo-tree-undo
 "C-r" 'undo-tree-redo
 "/" 'swiper
 "M-g" 'toggle-input-method
 "M-SPC" 'org-mark-ring-goto
 "<menu>" 'elfeed
 "M-r" 'counsel-linux-app
 "C-γ" 'keyboard-quit
 "θ" 'undo-tree-undo
 "C-ρ" 'undo-tree-redo
 "ο" 'evil-open-below
 "M-λ" 'org-metaright
 "Μ-η" 'org-metaleft)

(general-define-key
 :states 'insert
 :keymaps 'override
 "<M-tab>" 'tab-jump-out
 "M-g" 'toggle-input-method)

(general-define-key
 :states 'visual
 :keymaps 'global
 "e r" 'eval-region
 "m" 'org-marginalia-mark
 "SPC r" 'replace-string)

(general-define-key
 :states 'motion
 :keymaps 'override
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "ξ" 'evil-next-visual-line
 "κ" 'evil-previous-visual-line
 "η" 'evil-backward-char
 "λ" 'evil-forward-char)

(general-define-key
 :keymaps 'override
 "M-b" 'ebuku
 "M-C-r" 'restart-emacs
 "M-m" 'man)

(general-create-definer minor-leader-def
	:prefix ",")

    (minor-leader-def
     :states 'normal
     :keymaps 'org-mode-map
     "l" 'org-latex-preview
     "λ" 'org-latex-preview
     "n" 'org-noter
     "e" 'org-export-dispatch
     "t" '(:ignore t :which-key "To-do management")
     "y" 'org-download-clipboard
     "r" '(:ignore t :which-key "Org Roam/Ref commands")
     "ρ ι" 'org-roam-node-insert
     "ζ ι" 'zetteldesk-node-insert
     "H" '(org-cycle-hide-drawers :which-key "Hide properties drawers")
     "S" 'org-store-link
     "I" 'org-insert-link
     "s" '(:ignore t :which-key "SVG commands/Inkscape")
     "i" 'org-toggle-inline-images
     "P" 'org-tree-slide-mode
     "p" '(org-plot/gnuplot :which-key "Plot table data")
     "F" 'org-footnote-action
     "f" 'org-table-edit-formulas
     "L" 'org-toggle-pretty-entities
     "R" '(lab-skeleton :which-key "Insert my lab report template")
     "h" 'hw-skeleton
     "E" 'org-table-export
     "C" 'org-table-create-or-convert-from-region
     "c" 'org-edit-special
     "u" '(uo-lab-skeleton :which-key "Unit Operations lab template")
     "T" '(toc-org-mode :which-key "Insert ToC")
     "b" 'org-beamer-select-environment
     "z" '(:ignore t :which-key "Zetteldesk")
     "j" '(:ignore t :which-key "Julia")
     "m" '(:ignore t :which-key "Org-Marginalia commands"))

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", s"
 "i" 'insert-svg
 "l" 'svglatex
 "p" 'org-svg-pdf-export)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", z"
 "i" 'zetteldesk-node-insert
 "r" 'zetteldesk-remove-backlinks-from-desktop
 "b" 'zetteldesk-add-backlinks-to-desktop
 "s" 'zetteldesk-node-insert-sort-backlinks
 "p" 'zetteldesk-node-insert-if-poi-or-moc
 "m" 'zetteldesk-remark-mark)

(general-define-key
 :states 'normal
 :keymaps 'org-marginalia-mode-map
 :prefix ", m"
 "o" 'org-marginalia-open
 "n" 'org-marginalia-next
 "p" 'org-marginalia-previous
 "r" 'org-marginalia-remove
 "t" 'org-marginalia-toggle)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", t"
 "s" 'org-schedule
 "t" 'org-todo
 "p" 'org-priority
 "v" 'org-tags-view
 "T" 'org-set-tags-command
 "e" 'org-set-effort
 "d" 'org-deadline)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", r"
 "I" 'org-roam-node-insert
 "a" 'org-roam-alias-add
 "f" 'org-roam-init-fleeting-note
 "i" 'org-roam-node-insert-permanent
 "d" 'org-id-delete-entry
 "r" 'org-roam-ref-add
 "b" 'org-roam-backlink-files)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "C-j" '(org-tree-slide-move-next-tree :which-key "Next Slide")
 "C-k" '(org-tree-slide-move-previous-tree :which-key "Previous Slide")
 "`" 'org-roam-buffer-without-latex
 "=" 'math-at-point
 "α" 'evil-append
 "ι" 'evil-insert)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", j"
 "r" 'julia-snail
 "p" 'julia-snail-package-activate
 "d" 'julia-snail-doc-lookup
 "l" 'julia-snail-send-line
 "f" 'julia-snail-send-buffer-file
 "t" 'julia-snail-send-top-level-form)

(general-define-key
 :states 'visual
 :keymaps 'org-mode-map
 :prefix ", j"
 "R" 'julia-snail-send-region)

(general-define-key
 :states 'normal
 :keymaps 'lisp-mode-map
 ", f" 'sly-compile-defun
 ", h" '(sly-documentation :which-key "Documentation at point"))

(minor-leader-def
  :states 'normal
  :keymaps 'clojure-mode-map
  "r" '(cider-jack-in-clj :which-key "Create REPL")
  "e" '(:ignore t :which-key "Evaluate Lisp")
  "l" 'cider-load-buffer
  "d" 'cider-doc
  "a" 'cider-apropos
  "s" 'xref-find-definitions
  "S" 'cider-pop-back)

(general-define-key
 :states 'normal
 :keymaps 'clojure-mode-map
 :prefix ", e"
 "s" 'cider-eval-last-sexp)

(general-define-key
 :states 'normal
 :keymaps 'hy-mode-map
 ", e" 'hy-shell-eval-current-form)

(general-define-key
 :states 'normal
 :keymaps 'cider-repl-mode-map
 ", d" 'cider-doc
 ", a" 'cider-apropos
 ", s" 'xref-find-definitions
 ", S" 'cider-pop-back)

(general-define-key
 :states 'normal
 :keymaps 'julia-mode-map
 ", r" 'julia-snail
 ", p" 'julia-snail-package-activate
 ", d" 'julia-snail-doc-lookup
 ", l" 'julia-snail-send-line
 ", f" 'julia-snail-send-buffer-file
 ", t" 'julia-snail-send-top-level-form
 ", c" 'org-edit-src-exit)

(general-define-key
 :states 'normal
 :keymaps 'gams-mode-map
 ", g" 'gams-start-menu
 ", s" 'gams-start-processor
 ", d" 'gams-view-lst
 ", i" '(:ignore t :which-key "Insertion functions")
 ", c" 'gams-insert-comment
 ", t" 'gams-template
 ", I" 'gams-show-identifier
 ", l" 'gams-show-identifier-list
 ", a" 'gams-align-block
 ", m" 'gams-view-document
 ", h" '(:ignore t :which-key "Headings functions")
 "TAB" 'gams-orglike-cycle
 "S-TAB" 'gams-orglike-global-cycle)

(general-define-key
 :states 'normal
 :keymaps 'gams-mode-map
 :prefix ", i"
 "s" 'gams-insert-statement
 "d" 'gams-insert-dollar-control
 "t" 'gams-insert-statement-extended)

(general-define-key
 :states 'normal
 :keymaps 'gams-mode-map
 :prefix ", h"
 "n" 'outline-next-visible-heading
 "p" 'outline-previous-visible-heading
 "f" 'outline-hide-leaves
 "s" 'outline-show-branches)

(general-define-key
 :states 'normal
 :keymaps 'gams-lst-mode-map
 ", i" 'gams-lst-jump-to-input-file
 ", q" 'gams-lst-kill-buffer
 ", o" 'gams-outline
 ", e" 'gams-lst-view-error
 ", f" 'gams-lst-jump-to-error-file
 ", l" 'gams-lst-jump-to-line
 ", s" 'gams-lst-solve-summary
 ", S" 'gams-lst-solve-summary-back
 ", r" 'gams-lst-report-summary
 ", R" 'gams-lst-report-summary-back
 ", v" 'gams-lst-next-var
 ", V" 'gams-lst-previous-var
 ", n e" 'gams-lst-next-equ
 ", E" 'gams-lst-previous-equ
 ", p" 'gams-lst-next-par
 ", P" 'gams-lst-previous-par
 ", t" 'gams-lst-next-set
 ", T" 'gams-lst-previous-set
 ", x" 'gams-lst-next-elt
 ", X" 'gams-lst-previous-elt
 ", c" 'gams-lst-next-clt
 ", C" 'gams-lst-previous-clt)

(general-define-key
 :states 'normal
 :keymaps 'pdf-view-mode-map
 "i" 'org-noter-insert-note
 "c" 'kill-current-buffer
 "a" '(:ignore t :which-key "Add annotation")
 "a t" 'pdf-annot-add-text-annotation
 "a m" 'pdf-annot-add-markup-annotation
 ", g" 'pdf-view-goto-page)

(require 'dired)
(define-key dired-mode-map (kbd "+") nil)
(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "C-+" 'dired-create-directory
 "+" 'dired-create-empty-file
 "h" 'dired-up-directory
 "l" 'dired-find-file
 "H" 'dired-hide-dotfiles-mode
 "g s" 'dired-toggle-sudo
 "s" '(:ignore t :which-key "Dired-subtree functions")
 "s i" 'dired-subtree-insert
 "s r" 'dired-subtree-remove
 "s n" 'dired-subtree-narrow
 "<C-return>" 'helm-dired-open)

;; (define-key calc-mode-map (kbd "/") nil)
;; (general-define-key
;;  :states 'normal
;;  :keymaps 'calc-mode-map
;;  "/" 'calc-divide)

(general-define-key
 :states 'normal
 :keymaps 'Info-mode-map
 ", c" '((lambda() (interactive) (org-roam-capture nil "i")) :which-key "org-roam-capture info-template")
 ", h" 'Info-history
 ", f" 'Info-history-forward
 ", b" 'Info-history-back)

(general-define-key
 :states 'normal
 :keymaps 'ebib-index-mode-map
 "/" 'ebib-jump-to-entry
 "C-u" 'universal-argument
 "F p" 'ebib-list-recent
 "n" 'ebib-popup-note
 "N" 'ebib-search-next
 "U" 'ebib-mark-all-entries)

(provide 'keybindings)
