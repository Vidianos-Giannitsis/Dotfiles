(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(let ((default-directory  "~/.emacs.d/libs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:00"))

(gcmh-mode 1)

(load-theme 'doom-palenight t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

(display-battery-mode 1)

(which-key-mode 1)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice "/home/vidianos")

(ivy-mode 1)
(all-the-icons-ivy-setup)
(global-set-key (kbd "M-x") #'counsel-M-x)
(marginalia-mode 1)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(require 'dash-functional)
(require 'helpful)

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
  (set-face-attribute 'default nil :font "Source Code Pro 14"))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (set-font-faces))))
  (set-font-faces))

(setq default-input-method "greek")

(setq use-dialog-box nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq evil-collection-setup-minibuffer t)
(setq evil-want-keybinding nil)
(require 'evil-collection)
(evil-collection-init)

(require 'evil)
(evil-mode 1)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(require 'general)
(require 'vterm-toggle)

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)

(setq counsel-spotify-client-id "0df2796a793b41dc91711eb9f85c0e77")
(setq counsel-spotify-client-secret "bcdbb823795640248ff2c29eedadb800")
(setq espotify-client-id "0df2796a793b41dc91711eb9f85c0e77")
(setq espotify-client-secret "bcdbb823795640248ff2c29eedadb800")

(require 'math-at-point)
(require 'molar-mass)

(setq-default tab-jump-out-delimiters '(";" ")" "]" "}" "|" "'" "\"" "`" "."))

(require 'mediator)

(ace-window-display-mode 1)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'info+)

(require 'pdftotext)

(defun set-font-size (SIZE)
  "Change font size of Emacs window according to SIZE. Font remains Source Code Pro."
  (interactive "MFont Size: ")
  (set-face-attribute 'default nil :font (concat "Source Code Pro " SIZE)))

(defun named-vterm (NAME)
  "Create a new vterm session with name NAME and open it in a new window"
  (interactive "sEnter Name: ")
  (vterm-other-window NAME))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq org-link-elisp-confirm-function nil)

(setq ivy-youtube-key "224520591375-p6v36u3r9k8qt2k7qthb12gnjarc8c7t")

(setq counsel-linux-app-format-function 'counsel-linux-app-format-function-name-pretty)

(defun emacs-run-launcher ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Run counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame (make-frame '((name . "emacs-run-launcher")
				     (minibuffer . only)
				     (width . 120)
				     (height . 11)))
    (unwind-protect
	(counsel-linux-app)
      (delete-frame))))

(setq history-length 20)
(savehist-mode 1)

(require 'keybindings)

(require 'dired-x)

;(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package dired-hide-dotfile
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(require 'helm-dired-open)

(show-paren-mode 1)
(electric-pair-mode 1)
(setq wolfram-alpha-app-id "U9PERG-KTPL49AWA2")
(global-undo-tree-mode 1)

(require 'calfw-git)
(require 'calfw-org)

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

(setq flyspell-default-dictionary "greek")

(winner-mode 1)

(add-hook 'org-mode-hook #'(lambda ()
			       (org-superstar-mode)
			       (org-superstar-configure-like-org-bullets)))

(use-package org-download
  :after org)

(require 'org-tree-slide)

(require 'ox-beamer)
(require 'ox-hugo)
(require 'ox-pandoc)

(require 'org-marginalia-global-tracking)
(require 'org-marginalia)

(require 'org-pandoc-import)

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

(setq org-format-latex-options '(:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq org-preview-latex-default-process 'dvisvgm)

(setq org-startup-with-latex-preview t)

(require 'calctex)
(add-hook 'calc-embedded-new-formula-hook 'calctex-mode)

(add-hook 'org-mode-hook '(lambda ()
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
				 ("" "minted")))

(defun latex-filter-greek-hrefs (link backend info)
  "Change \\href to \\href{\detokenize} so that latex exports
  don't break if they see a file with greek letters"
  (when (org-export-derived-backend-p backend 'latex)
    (let* ((bracket (search "\}" link))
	   (size (length link))
	   (name
	    (concat (substring link 0 6)
		    "\\detokenize\{"
		    (substring link 6 bracket)
		    "\}"
		    (substring link bracket size))))
      (format "%s" name))))

(add-to-list 'org-export-filter-link-functions
	     'latex-filter-greek-hrefs)


(defun org-renumber-environment (orig-func &rest args)
  (let ((results '()) 
	(counter -1)
	(numberp))

    (setq results (loop for (begin .  env) in 
			(org-element-map (org-element-parse-buffer) 'latex-environment
			  (lambda (env)
			    (cons
			     (org-element-property :begin env)
			     (org-element-property :value env))))
			collect
			(cond
			 ((and (string-match "\\\\begin{equation}" env)
			       (not (string-match "\\\\tag{" env)))
			  (incf counter)
			  (cons begin counter))
			 ((string-match "\\\\begin{align}" env)
			  (prog2
			      (incf counter)
			      (cons begin counter)                          
			    (with-temp-buffer
			      (insert env)
			      (goto-char (point-min))
			      ;; \\ is used for a new line. Each one leads to a number
			      (incf counter (count-matches "\\\\$"))
			      ;; unless there are nonumbers.
			      (goto-char (point-min))
			      (decf counter (count-matches "\\nonumber")))))
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
    "NN" "\\NN"
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
    ";o"  "\\omega"
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

(setq org-noter-always-create-frame nil)

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
)
   )

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
  "*** ΜΦΔ\n\n"
  "*** Φαινόμενα Μεταφοράς\n\n"
  "*** ΗΕΔ (Υδραυλικό)\n\n"
  "*** ΗΕΔ (Ηλεκτρικό)\n\n"
  "*** Υλικά\n\n"
  "*** Προηγμένα Κεραμικά\n\n"
  "*** Βιολογία\n\n")

(require 'zettelkasten)
(require 'zetteldesk)

(zetteldesk-mode 1)

(setq bookmark-version-control t
      delete-old-versions t)

(defun org-scratchpad ()
  "Yank the entire document, delete it and save the buffer. This is very useful for my scratchpad setup"
  (interactive)
  (evil-yank-characters (point-min) (point-max))
  (delete-region (point-min) (point-max))
  (save-buffer))

(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

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

(add-hook 'after-init-hook 'global-company-mode)
(require 'company-maxima)
(add-hook 'company-mode-hook '(lambda ()
				(add-to-list 'company-backends '(company-math-symbols-latex company-bibtex company-capf))
				(add-to-list 'company-backends '(company-maxima-symbols company-maxima-libraries))
				(setq company-math-allow-latex-symbols-in-faces t)
				(setq company-bibtex-bibliography '("~/org_roam/Zotero_library.bib"))))

(setq elfeed-feeds
      '(("https://www.reddit.com/r/emacs.rss" emacs lisp reddit)
	("https://www.reddit.com/r/LaTeX.rss" latex reddit)
	("https://www.reddit.com/r/commandline.rss" linux reddit)
	("https://www.reddit.com/r/vim.rss" linux reddit)
	("https://www.reddit.com/r/linux.rss" linux reddit)
	("https://www.reddit.com/r/orgmode.rss" emacs org reddit)
	("https://www.reddit.com/r/git.rss" linux reddit)
	("https://www.reddit.com/r/OrgRoam.rss" emacs org zettelkasten reddit)
	("https://www.reddit.com/r/planetemacs.rss" emacs reddit)
	("https://www.reddit.com/r/ChemicalEngineering.rss" chemeng reddit)
	("https://www.reddit.com/r/Nyxt.rss" lisp reddit)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCQp2VLAOlvq142YN3JO3y8w" emacs org python youtube) ; John Kitchin's YT
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" linux youtube) ; DistroTube's YT
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCld68syR8Wi-GY_n4CaoJGA" linux youtube) ; Brodie Robertson's YT
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs org lisp youtube) ; SystemCrafters YT
	("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" emacs youtube) ; Protesilaos Stavrou's YT
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w" lisp youtube) ; Gavin Freeborn's YT
	("https://org-roam.discourse.group/c/how-to/6.rss" emacs org zettelkasten)
	("https://org-roam.discourse.group/c/dev/5.rss" emacs org zettelkasten)
	("https://org-roam.discourse.group/c/meta/11.rss" emacs org zettelkasten)
	("https://planet.emacslife.com/atom.xml" emacs)
	("https://irreal.org/blog/?feed=rss2" emacs linux org)
	("https://sachachua.com/blog/category/emacs-news/feed/" emacs)
	("https://www.emacswiki.org/emacs?action=rss;match=%5E%5Cd%5Cd%5Cd%5Cd-%5Cd%5Cd-%5Cd%5Cd" emacs)
	("https://ag91.github.io/rss.xml" emacs)
	("https://takeonrules.com/index.xml" emacs org)
	))

(require 'elfeed-score)
(elfeed-score-enable)

(require 'ox-word)
(require 'org-show)

(require 'scimax-autoformat-abbrev)
(add-hook 'org-mode-hook '(lambda ()
			    scimax-abbrev-mode
			    scimax-autoformat-mode))

(defun doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (require 'org-ref)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
	 (url (elfeed-entry-link elfeed-show-entry))
	 (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
	 (entry-id (elfeed-entry-id elfeed-show-entry))
	 (entry-link (elfeed-entry-link elfeed-show-entry))
	 (entry-id-str (concat (car entry-id)
			       "|"
			       (cdr entry-id)
			       "|"
			       url)))
    (if (string-match "DOI: \\(.*\\)$" content)
	(doi-add-bibtex-entry (match-string 1 content)
			      (ido-completing-read
			       "Bibfile: "
			       (append (f-entries "." (lambda (f)
							(and (not (string-match "#" f))
							     (f-ext? f "bib"))))
				       org-ref-default-bibliography)))
      (let ((dois (org-ref-url-scrape-dois url)))
	(cond
	 ;; One doi found. Assume it is what we want.
	 ((= 1 (length dois))
	  (doi-utils-add-bibtex-entry-from-doi
	   (car dois)
	   (ido-completing-read
	    "Bibfile: "
	    (append (f-entries "." (lambda (f)
				     (and (not (string-match "#" f))
					  (f-ext? f "bib"))))
		    org-ref-default-bibliography)))
	  action)
	 ;; Multiple DOIs found
	 ((> (length dois) 1)
	  (ivy-read "Select a DOI" (let ((dois '()))
				     (with-current-buffer (url-retrieve-synchronously url)
				       (loop for doi-pattern in org-ref-doi-regexps
					     do
					     (goto-char (point-min))
					     (while (re-search-forward doi-pattern nil t)
					       (pushnew
						;; Cut off the doi, sometimes
						;; false matches are long.
						(cons (format "%40s | %s"
							      (substring
							       (match-string 1)
							       0 (min
								  (length (match-string 1))
								  40))
							      doi-pattern)
						      (match-string 1))
						dois
						:test #'equal)))
				       (reverse dois)))
		    :action
		    (lambda (x)
		      (let ((bibfile (completing-read
				      "Bibfile: "
				      (append (f-entries "." (lambda (f)
							       (and (not (string-match "#" f))
								    (f-ext? f "bib"))))
					      org-ref-default-bibliography))))
			(doi-utils-add-bibtex-entry-from-doi
			 doi
			 bibfile)
			;; this removes two blank lines before each entry.
			(bibtex-beginning-of-entry)
			(delete-char -2))))
	  ;; (helm :sources
	  ;; 	`((name . "Select a DOI")
	  ;; 	  (candidates . ,(let ((dois '()))
	  ;; 			   (with-current-buffer (url-retrieve-synchronously url)
	  ;; 			     (loop for doi-pattern in org-ref-doi-regexps
	  ;; 				   do
	  ;; 				   (goto-char (point-min))
	  ;; 				   (while (re-search-forward doi-pattern nil t)
	  ;; 				     (pushnew
	  ;; 				      ;; Cut off the doi, sometimes
	  ;; 				      ;; false matches are long.
	  ;; 				      (cons (format "%40s | %s"
	  ;; 						    (substring
	  ;; 						     (match-string 1)
	  ;; 						     0 (min
	  ;; 							(length (match-string 1))
	  ;; 							40))
	  ;; 						    doi-pattern)
	  ;; 					    (match-string 1))
	  ;; 				      dois
	  ;; 				      :test #'equal)))
	  ;; 			     (reverse dois))))
	  ;; 	  (action . (lambda (candidates)
	  ;; 		      (let ((bibfile (ido-completing-read
	  ;; 				      "Bibfile: "
	  ;; 				      (append (f-entries "." (lambda (f)
	  ;; 							       (and (not (string-match "#" f))
	  ;; 								    (f-ext? f "bib"))))
	  ;; 					      org-ref-default-bibliography))))
	  ;; 			(loop for doi in (helm-marked-candidates)
	  ;; 			      do
	  ;; 			      (doi-utils-add-bibtex-entry-from-doi
	  ;; 			       doi
	  ;; 			       bibfile)
	  ;; 			      ;; this removes two blank lines before each entry.
	  ;; 			      (bibtex-beginning-of-entry)
	  ;; 			      (delete-char -2)))))))
	  ))))))

(defun org-elfeed-store-link ()
  "Store a link to an elfeed entry."
  (interactive)
  (cond
   ((eq major-mode 'elfeed-show-mode)
    (let* ((title (elfeed-entry-title elfeed-show-entry))
	   (url (elfeed-entry-link elfeed-show-entry))
	   (entry-id (elfeed-entry-id elfeed-show-entry))
	   (entry-id-str (concat (car entry-id)
				 "|"
				 (cdr entry-id)
				 "|"
				 url))
	   (org-link (concat "elfeed:entry-id:" entry-id-str)))
      (org-link-store-props
       :description title
       :type "elfeed"
       :link org-link
       :url url
       :entry-id entry-id)
      org-link))
   (t nil)))

(setq ebib-preload-bib-files '("~/Sync/My_Library.bib"))

(add-hook 'ebib-entry-mode-hook 'visual-line-mode)

(setq ebib-index-columns '(("Title" 60 t)
			   ("Author/Editor" 40 t)
			   ("Year" 6 t)
			   ("Entry Key" 40 t)
			   ("Note" 10 t)))

(setq ebib-notes-directory "~/org_roam/ref")

(use-package python-mls
  :config
  (python-mls-setup))

(setq calc-angle-mode 'rad)
;	calc-symbolic-mode t)

(require 'ebuku)
(require 'evil-collection-ebuku)

(add-hook 'ebuku-mode-hook 'evil-collection-ebuku-setup)

(use-package pdf-tools
    :mode (("\\.pdf\\'" . pdf-view-mode))
    :config
    ;(define-key pdf-view-mode-map [remap quit-window] #'kill-current-buffer)
    (progn
      (pdf-tools-install))
    )

(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(require 'eperiodic)

(require 'eaf)

(require 'eaf-evil)

(setq eaf-wm-focus-fix-wms '("qtile"))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(advice-add 'common-lisp-hyperspec
	    :around
	    (lambda (orig-fun &rest args)
	      (setq-local browse-url-browser-function 'eww-browse-url)
	      (apply orig-fun args)))

(setq deft-extensions '("org"))
(setq deft-directory "~/org_roam")
(setq deft-recursive t)

;; CUSTOM VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" default))
 '(package-selected-packages
   '(evil-collection openwith sequences cl-lib-highlight helm-system-packages async-await popup-complete helm-fuzzy-find evil-space yapfify yaml-mode ws-butler winum which-key web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit spaceline solarized-theme slim-mode scss-mode sass-mode restart-emacs request rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode pspp-mode popwin pip-requirements persp-mode pcre2el paradox org-projectile-helm org-present org-pomodoro org-mime org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint json-mode js2-refactor js-doc intero indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-flx helm-descbinds helm-css-scss helm-ag haskell-snippets gruvbox-theme google-translate golden-ratio gnuplot gh-md flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump diminish define-word cython-mode csv-mode company-ghci company-ghc column-enforce-mode coffee-mode cmm-mode clean-aindent-mode auto-highlight-symbol auto-compile auctex-latexmk anaconda-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
