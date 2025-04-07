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

;; (gcmh-mode 1)

(load-theme 'doom-palenight t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

(display-battery-mode 1)

(which-key-mode 1)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice "~")

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
  (set-face-attribute 'default nil :font "Source Code Pro 15"))

(if (daemonp)
    (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (setq doom-modeline-icon t)
		  (with-selected-frame frame
		    (set-font-faces))))
  (set-font-faces))

(setq default-input-method "greek")

(setq use-dialog-box nil)

;; (global-auto-revert-mode 1)
;; (setq global-auto-revert-non-file-buffers t)

(setq evil-collection-setup-minibuffer t)
(setq evil-want-keybinding nil)
(require 'evil-collection)
(evil-collection-init)

(require 'evil)
(evil-mode 1)

(add-hook 'org-mode-hook 'evil-org-mode)

(add-to-list 'load-path
	       "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(require 'general)
(require 'vterm-toggle)

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)

(require 'math-at-point)
(require 'molar-mass)

(setq-default tab-jump-out-delimiters '(";" ")" "]" "}" "|" "'" "\"" "`" "."))

(require 'mediator)

(ace-window-display-mode 1)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

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
(add-to-list 'auto-mode-alist '("\\.gms\\'" . gams-mode))

(setq org-link-elisp-confirm-function nil)

(setq history-length 20)

(ace-link-setup-default)

(setq dictionary-server "dict.org")

(setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles basic partial-completion))))

(setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
(add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))

(recentf-mode 1)

(defcustom bookmark-selector-web-page-alist '()
  "Alist used by `bookmark-selector-browse-bookmark' to associate
  web-links with their names. Needs to be an alist of the
  form (name . link) with both properties being
  strings. Initialised as an empty list as there is no point in
  predefining anything in it."
  :type 'alist)

(setq bookmark-selector-web-page-alist
	'(("Chemeng" . "https://www.chemeng.ntua.gr/")
	  ("Uni Submissions" . "https://www.chemeng.ntua.gr/submission/")
	  ("Wolfram Alpha" . "https://www.wolframalpha.com")
	  ("Chemeng Forum" . "https://forum.chemeng.ntua.gr/")
	  ("Science Direct" . "https://www.sciencedirect.com/")
	  ("Research Rabbit" . "https://www.researchrabbitapp.com/home")
	  ("Helios" . "https://helios.ntua.gr/my/")
	  ("Scopus" . "https://www.scopus.com/search/form.uri?display=basic#basic")
	  ("Messenger" . "https://www.messenger.com")
	  ("Twitch" . "https://www.twitch.tv")
	  ("Github" . "https://github.com/Vidianos-Giannitsis")
	  ("Youtube" . "https://www.youtube.com/")
	  ("Discord" . "https://discord.com/channels/@me")
	  ("Instagram" . "https://instagram.com/")
	  ("Org-Roam Discourse" . "https://org-roam.discourse.group/")
	  ("Qtile Docs" . "http://docs.qtile.org/en/latest/")
	  ("EXWM Docs" . "https://github.com/ch11ng/exwm/wiki")
	  ("Reddit" . "https://www.reddit.com/")
	  ("Emacs Subreddit" . "https://www.reddit.com/r/emacs/")
	  ("r/unixporn" . "https://www.reddit.com/r/unixporn/")
	  ("CompetitiveTFT Reddit" . "https://www.reddit.com/r/CompetitiveTFT/")
	  ("TFT Subreddit" . "https://www.reddit.com/r/TeamfightTactics/")
	  ("Smogon" . "https://smogon.com")
	  ("Dod" . "https://www.dod.gr")
	  ("LolChess Account" . "https://lolchess.gg/profile/eune/auroradraco")
	  ("Proton" . "https://www.protondb.com/")
	  ("DuelingBook" . "https://www.duelingbook.com/")
	  ("MC Biome Finder" . "https://www.chunkbase.com/apps/biome-finder#-3038289977291799158")
	  ("Pokemon Showdown" . "https://play.pokemonshowdown.com/")
	  ("Pokemon Damage Calc" . "https://calc.pokemonshowdown.com/index.html")
	  ("Master Duel Meta" . "https://www.masterduelmeta.com/")
	  ("YGO Pack Opener" . "https://db.ygoprodeck.com/pack-open/")
	  ("Pokemon Unbound Learnsets" . "https://github.com/Skeli789/Dynamic-Pokemon-Expansion/blob/Unbound/src/Learnsets.c")
	  ("Pokemon Unbound Egg Moves" . "https://github.com/Skeli789/Dynamic-Pokemon-Expansion/blob/Unbound/src/Egg_Moves.c")
	  ("Pokemon Unbound Evolutions" . "https://github.com/Skeli789/Dynamic-Pokemon-Expansion/blob/Unbound/src/Evolution%20Table.c")
	  ("Pokemon Unbound Bosses (Insane Difficulty)" . "https://docs.google.com/spreadsheets/d/1pLTQKQkWTnSkev4_kcjHbY0AkBujbTUDybxXFNZ_aVY/edit#gid=707456878")
	  ("Pokemon Unbound Damage Calcs" . "https://pastebin.com/raw/iyN9Ls90")
	  ("Pokemon Unbound Locations" . "https://docs.google.com/spreadsheets/d/1LFSBZuPDtJrwAz7t6ZkJ-il4j8M3qCdaKLNe6EZdPmQ/edit#gid=309549967")
	  ("EV Value Per Pokemon" . "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_effort_value_yield")
	  ("Pokemon Unbound Wild Held Items" . "https://github.com/Skeli789/Dynamic-Pokemon-Expansion/blob/Unbound/src/Base_Stats.c")
	  ("Darebee" . "https://darebee.com/")
	  ("Raindrop" . "https://app.raindrop.io/my/0")
	  ("Word Counter" . "https://wordcounter.net/")
	  ("Hex Color Codes" . "https://www.color-hex.com/")
	  ("Time Zone Converter" . "https://www.timeanddate.com/worldclock/converter.html")
	  ("Detexify" . "https://detexify.kirelabs.org/classify.html")))

(defmacro bookmark-selector-launcher (NAME WIDTH HEIGHT FUNCTION)
  "Define a launcher command.

Bookmark-selector is a package revolving around using emacs
outside of emacs to browse your bookmarks. Most of the commands
defined, consist of opening an emacs frame with only a
minibuffer, with a specified NAME, WIDTH and HEIGHT and inside it
calling FUNCTION and deleting the frame after the function
completes or is canceled."
  `(with-selected-frame (make-frame '((name . ,NAME)
					(minibuffer . only)
					(width . ,WIDTH)
					(height . ,HEIGHT)))
     (unwind-protect
	   (funcall ,FUNCTION)
	 (delete-frame))))

(setq counsel-linux-app-format-function
	'counsel-linux-app-format-function-name-pretty)

(defun emacs-run-launcher ()
  "Emacs run-launcher equivalent to dmenu.

Create and select a frame called emacs-run-launcher which
  consists only of a minibuffer and has specific dimensions.  Run
  counsel-linux-app on that frame, which is an emacs command that
  prompts you to select an app and open it in a dmenu like
  behaviour.  Delete the frame after that command has exited."
  (interactive)
  (bookmark-selector-launcher "emacs-run-launcher" 120 11 'counsel-linux-app))

(defun bookmark-selector-browse-bookmark-function ()
  "Browse a url from the list in `bookmark-selector-web-page-alist'.

This function is used as the FUNCTION argument for the
`bookmark-selector-launcher' macro to create the
`bookmark-selector-browse-bookmark' function."
  (browse-url
   (cdr (assoc (completing-read "Web-Page: " bookmark-selector-web-page-alist)
		 bookmark-selector-web-page-alist))))

(defun bookmark-selector-browse-bookmark ()
  "Create and select a frame called emacs-web-page-selector which
  consists of only a minibuffer and has specific dimensions. Inside
  that frame, run a `completing-read' prompting the user to select
  the name of a website in the `bookmark-selector-web-page-alist'. Upon selection,
  emacs will run `browse-url' opening the link associated to the
  selected name."
  (interactive)
  (bookmark-selector-launcher "emacs-web-page-selector"
				50 11 'bookmark-selector-browse-bookmark-function))

(defun bookmark-selector-add-bookmark-function ()
  "Add a web page to the `bookmark-selector-web-page-alist'.

This is the FUNCTION argument in `bookmark-selector-launcher' to
create `bookmark-selector-add-bookmark'."
  (let ((url (read-string "URL of page: "))
	  (description (read-string "Description of page: ")))
    (cl-pushnew (cons description url) bookmark-selector-web-page-alist)))

(defun bookmark-selector-add-bookmark ()
  "Create and select a new frame prompting for a new bookmark.

The bookmarks name and url are read through
`bookmark-selector-add-bookmark-function' while this function is
implemented with the `bookmark-selector-launcher' macro."
  (interactive)
  (bookmark-selector-launcher "emacs-add-bookmark"
				130 5 'bookmark-selector-add-bookmark-function))

(require 'keybindings)

(require 'dired-x)

					;(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

;;(use-package dired-collapse
;;  :hook (dired-mode . dired-collapse-mode))

(require 'helm-dired-open)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

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
		    '("xopp"))
		   "xournalpp"
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

(setq org-lowest-priority '69)

(defun org-table-import-after-n-lines (N FILE SEPARATOR)
  "Helper function for `org-table-import'.

This function moves point by N lines, checks if there is a table
in point and if there is deletes it and after that inserts an org
table from FILE using SEPARATOR using `org-table-import'. This is
a useful helper function for my thesis notebooks built in
org-mode."
  (next-line N)
  (when (org-table-p) (goto-char (org-table-begin))
	  (delete-char (- (org-table-end) (org-table-begin))))
  (org-table-import FILE SEPARATOR))

(setq org-format-latex-options '(:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq org-preview-latex-default-process 'dvisvgm)

(require 'org-auctex)
(setq org-startup-with-latex-preview t)

(add-hook 'org-mode-hook #'(lambda ()
				 (turn-on-org-cdlatex)
				 (org-fragtog-mode)
				 (laas-mode)))

(setq org-latex-listings 'minted)
(setq org-latex-minted-options '(("breaklines" "true")
				      ("breakanywhere" "true")))

(require 'tochemfig)

(setq org-latex-packages-alist '(("" "booktabs")
				      ("" "import")
				      ("LGR, T1" "fontenc")
				      ("greek, english" "babel")
				      ("" "alphabeta")
				      ("" "esint")
				      ("" "mathtools")
				      ("" "esdiff")
				      ("" "makeidx")
				      ("acronym" "glossaries")
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

(with-eval-after-load "org-tree-slide"
  (defvar my-hide-org-meta-line-p nil)
  (defun my-hide-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p t)
    (set-face-attribute 'org-meta-line nil
					    :foreground (face-attribute 'default :background)))
  (defun my-show-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p nil)
    (set-face-attribute 'org-meta-line nil :foreground nil))

  (defun my-toggle-org-meta-line ()
    (interactive)
    (if my-hide-org-meta-line-p
		(my-show-org-meta-line) (my-hide-org-meta-line)))

  (add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)
  (add-hook 'org-tree-slide-stop-hook #'my-show-org-meta-line)

  (setq org-tree-slide-modeline-display 'lighter)
  (setq org-tree-slide-activate-message "Entering Presentation")
  (setq org-tree-slide-deactivate-message "Exiting Presentation"))

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
	(maxima . t)
	(lisp . t)
	(clojure . t)
	(julia . t)
	(shell . t)
)
   )

(setq org-babel-lisp-eval-fn 'sly-eval)
(setq org-babel-clojure-backend 'cider)
(setq org-babel-julia-command "~/.julia/juliaup/julia-1.10.1+0.x64.linux.gnu/bin/julia")

(define-key org-src-mode-map (kbd ",") nil)
(general-define-key
 :states 'normal
 :keymaps 'org-src-mode-map
 ", c" 'org-edit-src-exit)

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
(require 'zetteldesk)
(zetteldesk-mode 1)
(require 'zetteldesk-kb-complete)
(require 'zetteldesk-ref)
(require 'zetteldesk-ref-citar)
(require 'zetteldesk-info)
(require 'zetteldesk-remark)
(require 'zetteldesk-saves)
(require 'org-roam-thesis)

(setq bookmark-version-control t
	delete-old-versions t)

(defun emacs-scratchpad ()
  "Create and select a frame called emacs-scratchpad with specific dimensions.

In that file, open scratchpad.org"
  (interactive)
  (with-selected-frame (make-frame '((name . "emacs-scratchpad")
					  (width . 100)
					  (height . 15)))
	(find-file "~/scratchpad.org")))

(defun org-scratchpad ()
  "Yanks the entire document, deletes it and saves the buffer
deleting the current frame. This is very useful for my scratchpad
setup, to be used with `emacs-scratchpad'."
  (interactive)
  (evil-yank-characters (point-min) (point-max))
  (delete-region (point-min) (point-max))
  (save-buffer)
  (delete-frame))

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
(add-hook 'company-mode-hook #'(lambda ()
				    (add-to-list 'company-backends '(company-math-symbols-latex company-bibtex company-capf))
				    (add-to-list 'company-backends '(company-maxima-symbols company-maxima-libraries))
				    (setq company-math-allow-latex-symbols-in-faces t)
				    (setq company-bibtex-bibliography '("~/org_roam/Zotero_library.bib"))))

(setq orderless-component-separator "[ &]")

(defun dictionary-search-dwim (&optional arg)
  "Search for definition of word at point. If region is active,
  search for contents of region instead. If called with a prefix
  argument, query for word to search."
  (interactive "P")
  (if arg
	(dictionary-search nil)
    (if (use-region-p)
	  (dictionary-search (buffer-substring-no-properties
			      (region-beginning)
			      (region-end)))
	(if (thing-at-point 'word)
	    (dictionary-lookup-definition)
	  (dictionary-search-dwim '(4))))))

(defun avy-action-define (pt)
  (save-excursion
    (goto-char pt)
    (dictionary-search-dwim))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?= avy-dispatch-alist) 'dictionary-search-dwim)

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

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

(setq mastodon-active-user "@vidianosgiannitsis")
(setq mastodon-instance-url "https://emacs.ch")

(setq elfeed-feeds
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

(require 'elfeed-score)
(elfeed-score-enable)

(require 'org-re-reveal)
(setq org-re-reveal-root "file:///home/vidianos/Cloned_Repositories/reveal.js")
(setq org-re-reveal-width 1800)
(setq org-re-reveal-height 960)

;; Add Color links for slide formatting
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
		      (progn (add-text-properties
			      0 (length path)
			      (list 'face `((t (:foreground ,path))))
			      path) path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))

(require 'ox-word)
(require 'org-show)

(require 'scimax-autoformat-abbrev)
(add-hook 'org-mode-hook #'(lambda ()
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
					 (cl-loop for doi-pattern in org-ref-doi-regexps
					       do
					       (goto-char (point-min))
					       (while (re-search-forward doi-pattern nil t)
						 (cl-pushnew
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

(defun scimax-elfeed-store-link ()
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

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))

(setq julia-snail-executable "julia")

(setq julia-snail-multimedia-enable t)
(setq julia-snail-extensions '(ob-julia repl-history))

(push "/usr/local/share/emacs/site-lisp" load-path)
(autoload 'imaxima "imaxima" "Maxima frontend" t)
(autoload 'imath "imath" "Interactive Math mode" t)

(add-to-list 'auto-mode-alist '("\\.mc\\'" . maxima-mode))

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

(setq pdf-view-midnight-colors '("#e9eaeb" . "#292d3e"))
(setq pdf-view-midnight-invert nil)
(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
(org-pdftools-setup-link)

(require 'eperiodic)

(add-hook 'eww-mode-hook #'(lambda ()
			       (texfrag-mode)
			       (texfrag-document)
			       (visual-line-mode)))

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
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75"
     default))
 '(package-selected-packages
   '(ace-jump-helm-line ace-link ace-window adaptive-wrap
			aggressive-indent anaconda-mode async-await
			auctex-latexmk auto-compile
			auto-highlight-symbol citar-org-node
			cl-lib-highlight clean-aindent-mode cmm-mode
			coffee-mode column-enforce-mode company-ghc
			company-ghci csv-mode cython-mode define-word
			diminish dumb-jump elisp-slime-nav emmet-mode
			eval-sexp-fu evil-anzu evil-args
			evil-collection evil-ediff evil-escape
			evil-exchange evil-iedit-state
			evil-indent-plus evil-lisp-state evil-matchit
			evil-mc evil-nerd-commenter evil-numbers
			evil-search-highlight-persist evil-space
			evil-surround evil-tutor evil-unimpaired
			evil-visual-mark-mode evil-visualstar
			exec-path-from-shell expand-region eyebrowse
			fancy-battery fill-column-indicator flx-ido
			gh-md gnuplot golden-ratio google-translate
			gruvbox-theme haskell-snippets helm-ag
			helm-css-scss helm-descbinds helm-flx
			helm-fuzzy-find helm-hoogle helm-make
			helm-mode-manager helm-projectile helm-pydoc
			helm-swoop helm-system-packages helm-themes
			highlight-indentation highlight-numbers
			highlight-parentheses hindent hl-todo
			hlint-refactor htmlize hungry-delete hy-mode
			indent-guide intero js-doc js2-refactor
			json-mode link-hint linum-relative
			live-py-mode livid-mode lorem-ipsum macrostep
			magit markdown-toc mmm-mode move-text neotree
			open-junk-file openwith org-bullets
			org-download org-mime org-pomodoro org-present
			org-projectile-helm paradox pcre2el persp-mode
			pip-requirements popup-complete popwin
			pspp-mode pug-mode py-isort pyenv-mode pytest
			pyvenv rainbow-delimiters request
			restart-emacs sass-mode scss-mode sequences
			slim-mode solarized-theme spaceline tagedit
			toc-org use-package uuidgen vi-tilde-fringe
			volatile-highlights vterm web-beautify
			web-mode which-key winum ws-butler yaml-mode
			yapfify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
