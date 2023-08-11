(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(let ((default-directory  "~/.emacs.d/libs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(use-package gcmh
  :init (gcmh-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

(display-battery-mode 1)

(use-package which-key
  :init (which-key-mode 1))

(setq inhibit-startup-screen t)
(setq initial-buffer-choice "/home/vidianos")

(use-package ivy
  :init (ivy-mode 1))
(use-package counsel
  :bind ("M-x" . counsel-M-x))
(use-package swiper)
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :init (all-the-icons-ivy-setup))
(use-package marginalia
  :init (marginalia-mode 1))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package dash-functional)
(use-package helpful)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-minor-modes nil
		doom-modeline-enable-word-count t
		doom-modeline-modal-icon t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq org-hide-emphasis-markers t)

(defun set-font-faces ()
  "Needed to set up my fonts to work with the emacs daemon"
  (set-face-attribute 'default nil :font "Source Code Pro 11"))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (set-font-faces))))
  (set-font-faces))

(setq default-input-method "greek")

(setq use-dialog-box nil)

(use-package evil-collection
  :init (progn (setq evil-collection-setup-minibuffer t)
	       (setq evil-want-keybinding nil)
	       (evil-collection-init)))

(use-package evil
  :init (evil-mode 1))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config (setq evil-org-set-key-theme
		'(navigation insert textobject additional calendar)))

(use-package evil-org-agenda
  :init (evil-org-agenda-set-keys))

(use-package yasnippet
  :init (yas-global-mode))

(use-package general)
(use-package vterm)
(use-package vterm-toggle)

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)

(use-package molar-mass)
(use-package math-at-point
  :straight (math-at-point :type git :host github :repo "shankar2k/math-at-point"))

(use-package tab-jump-out
  :config (setq tab-jump-out-delimiters '(";" ")" "]" "}" "|" "'" "\"" "`" ".")))

(use-package mediator
  :straight (mediator :type git :host github :repo "dalanicolai/mediator"))

(use-package ace-window
  :init (ace-window-display-mode 1)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package pdftotext
  :straight (pdftotext :type git :host github :repo "tecosaur/pdftotext.el"))

(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package gams
  :config (add-to-list 'auto-mode-alist '("\\.gms\\'" . gams-mode)))

(defun set-font-size (SIZE)
  "Change font size of Emacs window according to SIZE. Font remains Source Code Pro."
  (interactive "MFont Size: ")
  (set-face-attribute 'default nil :font (concat "Source Code Pro " SIZE)))

(defun named-vterm (NAME)
  "Create a new vterm session with name NAME and open it in a new window"
  (interactive "sEnter Name: ")
  (vterm-other-window NAME))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(setq org-link-elisp-confirm-function nil)

(setq history-length 20)

(use-package ace-link
  :init (ace-link-setup-default))

(setq dictionary-server "dict.org")

(require 'keybindings)

(require 'dired-x)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package dired-hide-dotfile
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package helm-dired-open
  :straight (helm-dired-open :type git :host github :repo "FrostyX/helm-dired-open"))

(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(show-paren-mode 1)
(electric-pair-mode 1)

(use-package wolfram
  :config (setq wolfram-alpha-app-id "U9PERG-KTPL49AWA2"))

(use-package undo-tree
  :init (global-undo-tree-mode 1))

(use-package calfw)
(use-package calfw-org)

(use-package openwith
  :config
  (setq openwith-associations
	(list
	 (list (openwith-make-extension-regexp
		'("mpg" "mpeg" "mp3" "mp4"
		  "avi" "wmv" "wav" "mov" "flv"
		  "ogm" "ogg" "mkv"))
		"mpv"
		'(file))
	 (list (openwith-make-extension-regexp
		'("xbm" "pbm" "pgm" "ppm" "pnm"
		  "gif" "bmp" "tif"))
	       "sxiv"
	       '(file))
	 (list (openwith-make-extension-regexp
		'("mph"))
	       "comsol"
	       '(file))
	 (list (openwith-make-extension-regexp
		'("aup"))
	       "audacity"
	       '(file))
	 (list (openwith-make-extension-regexp
		'("docx" "doc" "xlsx" "xls" "ppt" "odt" "ods"))
	       "libreoffice"
	       '(file))))
	(openwith-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("M-p" . projectile-command-map)))

(use-package winner
  :init (winner-mode 1))

(use-package flyspell
  :config (setq flyspell-default-dictionary "greek"))

(use-package org-superstar
  :hook (org-superstar-mode . org-mode)
  :hook (org-superstar-configure-like-org-bullets . org-superstar-mode))

(use-package org-download
  :after org)

(use-package org-tree-slide)

(require 'ox-beamer)
(use-package ox-hugo)
(use-package ox-pandoc)

(use-package org-pandoc-import
  :straight (:host github
	     :repo "tecosaur/org-pandoc-import"
	     :files ("*.el" "filters" "preprocessors")))
(use-package org-translate)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

(setq org-odt-preferred-output-format "docx")

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'(lambda ()
								   (let ((org-confirm-babel-evaluate nil))
								     (org-babel-tangle))))
					      'run-at-end 'only-in-org-mode))

(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

(add-hook 'org-mode-hook 'visual-line-mode)

(set-face-attribute 'org-document-title nil :font "Source Code Pro" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))

(setq org-export-with-broken-links t)

(setq org-lowest-priority '69)

(setq org-format-latex-options '(:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq org-preview-latex-default-process 'dvisvgm)

(setq org-startup-with-latex-preview t)

(use-package laas)
(use-package org-fragtog)

(add-hook 'org-mode-hook #'(lambda ()
			    (turn-on-org-cdlatex)
			    (org-fragtog-mode)
			    (laas-mode)))

(setq org-latex-listings 'minted)

(setq org-latex-packages-alist '(("" "booktabs")
				 ("" "import")
				 ("LGR, T1" "fontenc")
				 ("greek, english" "babel")
				 ("" "alphabeta")
				 ("" "esint")
				 ("" "mathtools")
				 ("" "esdiff")
				 ("" "makeidx")
				 ("" "glossaries")
				 ("" "newfloat")
				 ("" "minted")
				 ("a4paper, margin=3cm" "geometry")
				 ("" "chemfig")
				 ("" "svg")))

(defun my-latex-title-page ()
  "Template for my assignment title pages.

I found a neat template for latex title pages online and decided
to start using it for my assignments. This function inserts that
template in an org document after prompting for the fields that
change from one assignment to the next."
  (interactive)
  (let ((sector (read-string "Τομέας: "))
	(lab (read-string "Εργαστήριο: "))
	(title (read-string "Τίτλος: "))
	(authors (read-string "Συγγραφείς: "))
	(numbers (read-string "Αριθμοί Μητρώου: ")))
    (insert
     "#+options: toc:nil title:nil author:nil date:nil\n"
     "#+LATEX_HEADER: \\newcommand{\\HRule}{\\rule{\\linewidth}{0.5mm}}\n"
"#+BEGIN_SRC latex
  \\renewcommand{\\contentsname}{Περιεχόμενα}
  \\begin{titlepage}

  \\begin{center}
    \\begin{minipage}{0.15\\textwidth}
      \\begin{flushleft}
	\\includegraphics[width=1\\textwidth]{~/Pictures/ntua_logo.png}\\\\[0.4cm]    
      \\end{flushleft}
    \\end{minipage}
    \\begin{minipage}{0.75\\textwidth}
      \\textsc{\\bfseries \\large ΕΘΝΙΚΟ ΜΕΤΣΟΒΙΟ ΠΟΛΥΤΕΧΝΕΙΟ}\\\\[0.2cm]
      \\textsc{\\bfseries \\large ΣΧΟΛΗ ΧΗΜΙΚΩΝ ΜΗΧΑΝΙΚΩΝ - ΤΟΜΕΑΣ " sector
      "}\\\\[0.2cm]
      \\textsc{\\bfseries \\normalsize ΕΡΓΑΣΤΗΡΙΟ " lab
      "}\\\\[0.2cm]
    \\end{minipage}
    \\\\[1.5cm]

    \\HRule \\\\[0.3cm]
    \\LARGE " title "\\\\[0.3cm]
    \\HRule \\\\[1cm]
    \\begin{minipage}{0.4\\textwidth}
      \\begin{flushleft} \\large
	\\emph{Συγγραφείς:}\\\\
	Βιδιάνος Γιαννίτσης\\\\
	" authors "
      \\end{flushleft}
    \\end{minipage}
    \\begin{minipage}{0.4\\textwidth}
      \\begin{flushright} \\large
	\\emph{Αριθμοί Μητρώου:}\\\\
	ch19113\\\\
	" numbers "
      \\end{flushright}
    \\end{minipage}\\\\[1cm]
    \\HRule \\\\[2cm]
  \\end{center}

  \\begin{abstract}

  \\end{abstract}

  \\end{titlepage}
#+END_SRC")))

(defun org-renumber-environment (orig-func &rest args)
  (let ((results '()) 
	(counter -1)
	(numberp))

    (setq results (cl-loop for (begin .  env) in 
			(org-element-map (org-element-parse-buffer) 'latex-environment
			  (lambda (env)
			    (cons
			     (org-element-property :begin env)
			     (org-element-property :value env))))
			collect
			(cond
			 ((and (string-match "\\\\begin{equation}" env)
			       (not (string-match "\\\\tag{" env)))
			  (cl-incf counter)
			  (cons begin counter))
			 ((string-match "\\\\begin{align}" env)
			  (prog2
			      (cl-incf counter)
			      (cons begin counter)                          
			    (with-temp-buffer
			      (insert env)
			      (goto-char (point-min))
			      ;; \\ is used for a new line. Each one leads to a number
			      (cl-incf counter (count-matches "\\\\$"))
			      ;; unless there are nonumbers.
			      (goto-char (point-min))
			      (cl-decf counter (count-matches "\\nonumber")))))
			 (t
			  (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
	    (concat
	     (format "\\setcounter{equation}{%s}\n" numberp)
	     (car args)))))

  (apply orig-func args))

(advice-add 'org-create-formula-image :around #'org-renumber-environment)

(setq laas-basic-snippets
  '(:cond laas-mathp
    "!="    "\\neq"
    "!>"    "\\mapsto"
    "**"    "\\cdot"
    "+-"    "\\pm"
    "-+"    "\\mp"
    "->"    "\\to"
    "..."   "\\dots"
    "<<"    "\\ll"
    "<="    "\\leq"
    "<>"    "\\diamond"
    "=<"    "\\impliedby"
    "=="    "&="
    "=>"    "\\implies"
    ">="    "\\geq"
    ">>"    "\\gg"
    "AA"    "\\forall"
    "EE"    "\\exists"
    "cb"    "^3"
    "iff"   "\\iff"
    "inn"   "\\in"
    "notin" "\\not\\in"
    "sr"    "^2"
    "xx"    "\\times"
    "|->"   "\\mapsto"
    "|="    "\\models"
    "||"    "\\mid"
    "~="    "\\approx"
    "~~"    "\\sim"

    "arccos" "\\arccos"
    "arccot" "\\arccot"
    "arccot" "\\arccot"
    "arccsc" "\\arccsc"
    "arcsec" "\\arcsec"
    "arcsin" "\\arcsin"
    "arctan" "\\arctan"
    "cos"    "\\cos"
    "cot"    "\\cot"
    "csc"    "\\csc"
    "exp"    "\\exp"
    "ln"     "\\ln"
    "log"    "\\log"
    "perp"   "\\perp"
    "sin"    "\\sin"
    "star"   "\\star"
    "gcd"    "\\gcd"
    "min"    "\\min"
    "max"    "\\max"

    "CC" "\\CC"
    "FF" "\\FF"
    "HH" "\\HH"
    "PP" "\\PP"
    "QQ" "\\QQ"
    "RR" "\\RR"
    "ZZ" "\\ZZ"

    ";a"  "\\alpha"
    ";A"  "\\forall"        ";;A" "\\aleph"
    ";b"  "\\beta"
    ";C"  "\\mathbb{C}"                            ";;;C" "\\arccos"
    ";d"  "\\dot"         ";;d" "\\partial"
    ";D"  "\\Delta"         ";;D" "\\nabla"
    ";e"  "\\mathcal{E}"       ";;e" "\\varepsilon"   ";;;e" "\\exp"
    ";E"  "\\exists"                               ";;;E" "\\ln"
    ";f"  "\\phi"           ";;f" "\\varphi"
    ";F"  "\\Phi"
    ";g"  "\\gamma"                                ";;;g" "\\lg"
    ";G"  "\\Gamma"                                ";;;G" "10^{?}"
    ";h"  "\\eta"           ";;h" "\\hbar"
    ";i"  "\\infty"            ";;i" "\\imath"
    ";I"  "\\in"          ";;I" "\\Im"
    ";;j" "\\jmath"
    ";k"  "\\kappa"
    ";l"  "\\mathcal{L}"        ";;l" "\\ell"          ";;;l" "\\log"
    ";L"  "\\Lambda"
    ";m"  "\\mu"
    ";n"  "\\nabla"         ";;n" "\\vec{\\nabla}"     ";;;n" "\\ln"
    ";N"  "\\mathbb{N}"                                ";;;N" "\\exp"
    ";o"  "\\overline"
    ";O"  "\\Omega"         ";;O" "\\mho"
    ";p"  "\\partial"            ";;p" "\\varpi"
    ";P"  "\\Pi"
    ";q"  "\\theta"         ";;q" "\\vartheta"
    ";Q"  "\\mathbb{Q}"
    ";r"  "\\rho"           ";;r" "\\varrho"
    ";R"  "\\mathbb{R}"      ";;R" "\\Re"
    ";s"  "\\sqrt"         ";;s" "\\varsigma"    ";;;s" "\\sin"
    ";S"  "\\Sigma"                               ";;;S" "\\arcsin"
    ";t"  "\\tau"                                 ";;;t" "\\tan"
    ";;;T" "\\arctan"
    ";u"  "\\upsilon"
    ";U"  "\\Upsilon"
    ";v"  "\\vec"
    ";V"  "\\Phi"
    ";w"  "\\xi"
    ";W"  "\\Xi"
    ";x"  "\\chi"
    ";y"  "\\psi"
    ";Y"  "\\Psi"
    ";z"  "\\zeta"
    ";Z"  "\\mathbb{Z}"
    ";0"  "\\emptyset"
    ";8"  "\\infty"
    ";!"  "\\neg"
    ";^"  "\\uparrow"
    ";&"  "\\wedge"
    ";~"  "\\approx"        ";;~" "\\simeq"
    ";_"  "\\downarrow"
    ";+"  "\\cup"
    ";-"  "\\leftrightarrow"";;-" "\\longleftrightarrow"
    ";*"  "\\times"
    ";/"  "\\not"
    ";|"  "\\mapsto"        ";;|" "\\longmapsto"
    ";\\" "\\setminus"
    ";="  "\\Leftrightarrow"";;=" "\\Longleftrightarrow"
    ";(" "\\langle"
    ";)" "\\rangle"
    ";[" "\\Leftarrow"     ";;[" "\\Longleftarrow"
    ";]" "\\Rightarrow"    ";;]" "\\Longrightarrow"
    ";{"  "\\subset"
    ";}"  "\\supset"
    ";<"  "\\leftarrow"    ";;<" "\\longleftarrow"  ";;;<" "\\min"
    ";>"  "\\rightarrow"   ";;>" "\\longrightarrow" ";;;>" "\\max"
    ";'"  "\\prime"
    ";."  "\\cdot"))

(use-package org-noter
  :config (setq org-noter-always-create-frame nil))

(defun org-cycle-hide-drawers (state)
  "Hide all the :PROPERTIES: drawers when called with the 'all argument. Mainly for hiding them in crammed org-noter files"
  (interactive "MEnter 'all for hiding :PROPERTIES: drawers in an org buffer: ")
  (when (and (derived-mode-p 'org-mode)
	     (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
	     (beg (if globalp
		    (point-min)
		    (point)))
	     (end (if globalp
		    (point-max)
		    (if (eq state 'children)
		      (save-excursion
			(outline-next-heading)
			(point))
		      (org-end-of-subtree t)))))
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp end t)
	  (save-excursion
	    (beginning-of-line 1)
	    (when (looking-at org-drawer-regexp)
	      (let* ((start (1- (match-beginning 0)))
		     (limit
		       (save-excursion
			 (outline-next-heading)
			   (point)))
		     (msg (format
			    (concat
			      "org-cycle-hide-drawers:  "
			      "`:END:`"
			      " line missing at position %s")
			    (1+ start))))
		(if (re-search-forward "^[ \t]*:END:" limit t)
		  (outline-flag-region start (point-at-eol) t)
		  (user-error msg))))))))))

(org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (haskell . t)
     (octave . t)
     (latex . t)
     (gnuplot . t)
     (maxima . t)
     (lisp . t)
     (clojure . t)
     (julia . t)
)
   )

(setq org-babel-lisp-eval-fn 'sly-eval)
(setq org-babel-clojure-backend 'cider)
(setq org-babel-julia-command "~/.local/bin/julia")

(define-skeleton lab-skeleton
  "A skeleton which I use for initialising my lab reports which have standard formatting"
  ""
  "#+TITLE:"str"\n"
  "glatex"-"\n"
  "ab\n\\pagebreak\n\n"

  "* Εισαγωγή\n\n"

  "* Πειραματικό Μέρος\n\n"

  "* Αποτελέσματα - Συζήτηση\n\n"

  "* Συμπεράσματα\n\n"

  "* Βιβλιογραφία\n"
  "bibliography:~/Sync/My_Library.bib\n"
  "bibliographystyle:unsrt")

(define-skeleton uo-lab-skeleton
  "My lab on unit operations has a really specific template which albeit similar is not identical to the typical structure of a lab report. This is a skeleton initialising those lab reports"
  ""
  "#+TITLE:"str"\n"
  "glatex"-"\n"
  "#+LATEX_HEADER: \\usepackage[a4paper, margin=2cm]{geometry}\n"
  "#+LATEX_CLASS_OPTIONS: [9pt]\n"
  "#+OPTIONS: toc:nil\n"
  "#+AUTHOR: \n"
  "#+DATE: Εκτέλεση: , Παράδοση: \n\n"
  "ab\n\\pagebreak\n\\tableofcontents\n\n"

  "* Εισαγωγή\n\n"

  "* Πειραματικό Μέρος\n\n"
  "** Πειραματική Διάταξη - Διάγραμμα Ροής\n\n"
  "** Πειραματική Διαδικασία\n\n"
  "** Μετρήσεις\n\n"

  "* Επεξεργασία Μετρήσεων\n\n"

  "* Συζήτηση Αποτελεσμάτων - Συμπεράσματα\n\n"

  "* Βιβλιογραφία\n"
  "bibliography:~/Sync/My_Library.bib\n"
  "bibliographystyle:unsrt")

(define-skeleton hw-skeleton
  "A skeleton for quickly adding a list of this semester's lessons to a new note which I use for tracking what I need to do for each lesson"
  ""
  "*** Διαχείριση Βιομηχανικών Αποβλήτων\n\n"
  "*** Περιβαλλοντική Βιοτεχνολογία\n\n"
  "*** Περιβαλλοντική Μηχανική\n"
  "**** Στερεά\n\n"
  "**** Υγρά\n\n"
  "**** Αέρια\n\n"
  "**** Ασκήσεις\n\n"
  "*** Πράσινη Χημεία\n\n"
  "*** Σχεδιασμός\n"
  "**** Εργασία\n\n"
  "*** Άρθρα to-read\n\n"
  "*** Other\n\n")

(require 'zettelkasten)
(use-package zetteldesk
  :straight (zetteldesk
	     :type git :host github
	     :repo "Vidianos-Giannitsis/zetteldesk.el"
	     :files "*.el")
  :init (zetteldesk-mode 1))

(defun org-inkscape-img ()
    (interactive "P")
    (setq string (read-from-minibuffer "Insert image name: "))
    ;; if images folder doesn't exist create it
    (setq dirname (concat (f-base (buffer-file-name)) "-org-img"))
    (if (not (file-directory-p dirname))
	(make-directory dirname))
     ;; if file doesn't exist create it
     (if (not (file-exists-p (concat "./" dirname "/" string ".svg")))
     (progn
	 (setq command (concat "echo " "'<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:cc=\"http://creativecommons.org/ns#\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\" xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" width=\"230mm\" height=\"110mm\" viewBox=\"0 0 164.13576 65.105995\" version=\"1.1\" id=\"svg8\" inkscape:version=\"1.0.2 (e86c8708, 2021-01-15)\" sodipodi:docname=\"disegno.svg\"> <defs id=\"defs2\" /> <sodipodi:namedview id=\"base\" pagecolor=\"#292d3e\" bordercolor=\"#666666\" borderopacity=\"1.0\" inkscape:zoom=\"1.2541194\" inkscape:cx=\"310.17781\" inkscape:cy=\"123.03495\"z inkscape:window-width=\"1440\" inkscape:window-height=\"847\" inkscape:window-x=\"1665\" inkscape:window-y=\"131\" inkscape:window-maximized=\"1\"  inkscape:current-layer=\"svg8\" /><g/></svg>' >> " dirname "/" string ".svg; inkscape " dirname "/" string ".svg"))
	    (shell-command command)
	    (concat "#+begin_export latex\n\\begin{figure}\n\\centering\n\\def\\svgwidth{0.9\\columnwidth}\n\\import{" "./" dirname "/}{" string ".pdf_tex" "}\n\\end{figure}\n#+end_export"))
	;; if file exists opens it
	(progn
	    (setq command (concat "inkscape " dirname "/" string ".svg"))
	    (shell-command command)
	    (concat "" ""))))

(add-to-list 'org-latex-packages-alist '("" "booktabs"))
(add-to-list 'org-latex-packages-alist '("" "import"))

(defun org-svg-pdf-export ()
  (interactive)
  (setq dirname (concat (f-base (buffer-file-name)) "-org-img"))
  (if (file-directory-p dirname)
      (progn
	(setq command (concat "/usr/bin/inkscape -D --export-latex --export-type=\"pdf\" " dirname "/" "*.svg"))
	(shell-command command))))

(defun svglatex (file_name)
  "Prompts for a file name (without any file prefix), takes an svg with that file name and exports the file as a latex compatible pdf file"
  (interactive "MEnter svg file name: ")
  (setq export (concat "inkscape -D --export-latex --export-pdf=" file_name ".pdf" file_name ".svg" ))
  (shell-command export))

(defun insert-svg (NAME)
  "Prompts for an svg's name (without the prefix) and inserts an
  working orglink to the svg if it is located under a directory with
  the name of the buffer suffixed by -org-img.

This is really useful for me as by default the function
`org-inkscape-img' I use extensively saves inkscape's svgs in that
directory. The problem is that that command was made with the latex
export in mind (which is perfectly fine as I use it a lot) but in my
org_roam setup I dont export files to latex so I just want to see the
svg.

For this reason, this command inserts an org link to an svg in that
directory with the prompted file name and toggles image-preview to see
it."
  (interactive "Msvg name: ")
  (insert (concat "[[" (file-name-sans-extension buffer-file-name) "-org-img/" NAME ".svg" "]]"))
  (org-toggle-inline-images))

(use-package company
  :init (global-company-mode 1))

(use-package company-maxima)
(use-package company-math)
(use-package company-bibtex)

(add-hook 'company-mode-hook #'(lambda ()
				(add-to-list 'company-backends '(company-math-symbols-latex company-bibtex company-capf))
				(add-to-list 'company-backends '(company-maxima-symbols company-maxima-libraries))
				(setq company-math-allow-latex-symbols-in-faces t)
				(setq company-bibtex-bibliography '("~/org_roam/Zotero_library.bib"))))

(use-package engine-mode)

(defengine google "https://www.google.com/search?q=%s")
(defengine youtube "https://www.youtube.com/results?search_query=%s")
(defengine archwiki "https://wiki.archlinux.org/index.php?search=%s")
(defengine reddit "https://www.reddit.com/search/?q=%s")
(defengine lutris "https://lutris.net/games?q=%s")
(defengine protondb "https://www.protondb.com/search?q=%s")
(defengine lolchess "https://lolchess.gg/search?region=EUNE&name=%s")
(defengine wolfram "https://www.wolframalpha.com/input/?i=%s")
(defengine sciencedirect "https://www.sciencedirect.com/search?qs=%s")
(defengine translate "https://translate.google.com/?sl=auto&tl=en&text=%s")
(defengine bulbapedia "https://bulbapedia.bulbagarden.net/w/index.php?title=%s")
(defengine github "https://github.com/search?ref=simplesearch&q=%s")

(use-package elfeed
  :config (setq elfeed-feeds
		'(("https://www.reddit.com/r/emacs.rss" emacs lisp reddit)
		  ("https://www.reddit.com/r/linux.rss" linux reddit)
		  ("https://www.reddit.com/r/orgmode.rss" emacs org reddit)
		  ("https://www.reddit.com/r/OrgRoam.rss" emacs org zettelkasten reddit)
		  ("https://www.reddit.com/r/Nyxt.rss" lisp reddit)
		  ("https://org-roam.discourse.group/c/how-to/6.rss" emacs org zettelkasten)
		  ("https://org-roam.discourse.group/c/dev/5.rss" emacs org zettelkasten)
		  ("https://org-roam.discourse.group/c/meta/11.rss" emacs org zettelkasten)
		  ("https://planet.emacslife.com/atom.xml" emacs news)
		  ("https://irreal.org/blog/?feed=rss2" emacs linux news)
		  ("https://sachachua.com/blog/category/emacs-news/feed/" emacs news)
		  ("https://ag91.github.io/rss.xml" emacs)
		  ("https://andreyorst.gitlab.io/feed.xml" emacs lisp)
		  ("https://magnus.therning.org/feed.xml" emacs)
		  ("https://protesilaos.com/codelog.xml" emacs lisp)
		  ("https://protesilaos.com/news.xml" news)
		  ("https://cestlaz.github.io/rss.xml" emacs news)
		  ("https://amodernist.com/all.atom" emacs lisp)
		  ("https://tilde.town/~ramin_hal9001/atom.xml" emacs lisp org)
		  ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs org)
		  ("https://karthinks.com/index.xml" emacs news)
		  ))

  (setq elfeed-search-filter "@1-months-ago +unread")
  )

(use-package elfeed-score
  :init (elfeed-score-enable))

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))

(setq-default julia-snail-executable "~/.local/bin/julia")

(setq julia-snail-multimedia-enable t)
(setq julia-snail-extensions '(ob-julia repl-history))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (pdf-tools-install))
  )

(setq pdf-view-midnight-colors '("#e9eaeb" . "#292d3e"))
(setq pdf-view-midnight-invert nil)
(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
(org-pdftools-setup-link)

(add-hook 'emacs-lisp-mode-hook #'(lambda ()
				   (eldoc-mode)
				   (lispy-mode)
				   (lispyville-mode)))
(add-hook 'ielm-mode-hook 'eldoc-mode)
