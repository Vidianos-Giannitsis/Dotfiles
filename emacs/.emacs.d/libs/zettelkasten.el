(add-hook 'after-init-hook 'org-roam-setup)
(setq org-roam-v2-ack t)

(use-package org-roam
  :config
  (setq org-roam-directory "~/org_roam"
	org-roam-dailies-directory "~/org_roam/daily")

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    "Access slot \"directory\" of org-roam-node struct CL-X"
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
	(format "(%s)" (car (f-split dirs)))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    "Access slot \"backlinks\" of org-roam-node struct CL-X"
    (let* ((count (caar (org-roam-db-query
			 [:select (funcall count source)
				  :from links
				  :where (= dest $s1)
				  :and (= type "id")]
			 (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
    "Access slot \"backlinks\" of org-roam-node struct CL-X. This
    is identical to `org-roam-node-backlinkscount' with the
    difference that it returns a number instead of a fromatted
    string. This is to be used in
    `org-roam-node-sort-by-backlinks'"
    (let* ((count (caar (org-roam-db-query
			 [:select (funcall count source)
				  :from links
				  :where (= dest $s1)
				  :and (= type "id")]
			 (org-roam-node-id node)))))
      count))

  (cl-defmethod org-roam-node-todostate ((node org-roam-node))
    "Modified version of org-roam-node-todo to look a bit better"
    (if-let ((state (org-roam-node-todo node)))
	(format "Status: %s" state)))

  (cl-defmethod org-roam-node-buffer ((node org-roam-node))
    "Access slot \"buffer\" of org-roam-node struct CL-X"
    (let ((buffer (get-file-buffer (org-roam-node-file node))))
      buffer))

  (setq org-roam-node-display-template "${title:100} ${backlinkscount:6} ${todostate:20} ${directories:10} ${tags:25}")

  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.40)
		 (window-height . fit-window-to-buffer))))

(defun org-roam-buffer-without-latex ()
    "Essentially `org-roam-buffer-toggle' but it ensures latex previews are turned off before toggling the buffer.

  This is useful because especially with index files, having
  latex previews on, makes opening the buffer very slow as it
  needs to load previews of many files. If you by default have
  `org-startup-with-latex-preview' set to t, you have probably
  noticed this issue before. This function solves it."
    (interactive)
    (let ((org-startup-with-latex-preview nil))
      (org-roam-buffer-toggle)))

(defun org-roam-permanent-note-p (NODE)
  "Check if NODE is at the top level org_roam directory using the
  `org-roam-node-directories' function. If it isn't,
  `org-roam-node-directories' will return a non empty string,
  therefore this expression will evaluate to nil. The way my
  notes are sorted, when a note is placed on the top level its a
  permanent note, while fleeting and reference notes are placed
  in subdirectories.

Therefore, this predicate function allows me to create a version
of `org-roam-node-find' which only shows my permanent notes,
which can be useful in some cases. That filtered function is
`org-roam-find-permanent-node'."
  (string-equal (org-roam-node-directories NODE) ""))

(defun org-roam-node-poi-or-moc-p (NODE)
  "Check if NODE has the tag POI or the tag MOC. Return t if it does"
  (or (string-equal (car (org-roam-node-tags NODE)) "POI")
      (string-equal (car (org-roam-node-tags NODE)) "MOC")))

(defun org-roam-find-permanent-node ()
  "Execute `org-roam-node-find' with the list being filtered to
only include permanent notes. In my system that is synonymous to
saying include only notes at the top level directory. The
filtering is done with the `org-roam-permanent-note-p' predicate
function."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-permanent-note-p))

(defvar-local org-roam-backlinks nil
  "Buffer local variable displaying a list of the absolute paths
  of all the files that are backlinked to current node. These are
  not added by default, and as such this variable has the value
  nil but they can be added by running the
  `org-roam-backlink-files' function on a node.")

(defvar org-roam-backlink-pdfs nil
  "After running `org-roam-export-backlinks-to-latex-pdf', to
  export a node and all its backlinks to pdf, the value of this
  variable in the original node's buffer will become a list of
  all the pdfs that were created. This is to ease the process of
  combining them as the value of this variable can then be passed
  to a program such as pdftk to combine them.")

(defun org-roam-node-sort-by-backlinks (completion-a completion-b)
  "Sorting function for org-roam that sorts the list of nodes by
the number of backlinks. This is the sorting function in
`org-roam-node-find-by-backlinks'"
  (let ((node-a (cdr completion-a))
	(node-b (cdr completion-b)))
    (>= (org-roam-node-backlinkscount-number node-a)
	(org-roam-node-backlinkscount-number node-b))))

(defun org-roam-node-find-by-backlinks ()
  "Essentially works like `org-roam-node-find' (although it uses
a combination of `find-file' and `org-roam-node-read' to
accomplish that and not `org-roam-node-find' as only
`org-roam-node-read' can take a sorting function as an argument)
but the list of nodes is sorted by the number of backlinks
instead of most recent nodes. Sorting is done with
`org-roam-node-sort-by-backlinks'"
  (interactive)
  (find-file (org-roam-node-file (org-roam-node-read nil nil #'org-roam-node-sort-by-backlinks))))

(defun org-roam-backlink-query ()
  "Simple org-roam query function that stores the IDs of all the
  files that link to the node at point. This is a modified part
  of the `org-roam-backlinks-get' function keeping only the part
  necessary for `org-roam-backlink-files' to work as this is a
  complimentary function to that"
  (org-roam-db-query
   [:select [source dest]
	    :from links
	    :where (= dest $s1)
	    :and (= type "id")]
   (org-roam-node-id (org-roam-node-at-point))))

(defun org-roam-backlink-files ()
    "Get all nodes that link to the node at point with the
    `org-roam-backlink-query' function, find their absolute path
    and save a list of those paths to the buffer local variable
    `org-roam-backlinks'.

  With the list, you can act on all those files together. This is
  exceptionally useful with index files as it allows you to do an
  action on all files linked to this index automatically."
    (interactive)
    (let ((backlinks (length (org-roam-backlink-query))))
      (dotimes (number backlinks)
	(let* ((id (car (nth number (org-roam-backlink-query))))
	       (node (org-roam-node-from-id id)))
	  (setq-local org-roam-backlinks (cons (org-roam-node-file node) org-roam-backlinks))))
      org-roam-backlinks))

(defun org-roam-export-backlinks-to-latex-pdf ()
  "Export the current buffer and every buffer that mentions it to
a pdf through the org-latex export. Makes use of the
`org-roam-backlink-files' function to find all the
backlinks. Also saves all the pdf names in a variable called
`org-roam-backlink-pdfs'. These names can then be passed to
something like pdftk to merge them into one pdf"
  (interactive)
  (save-current-buffer
    (let ((backlinks (cons (buffer-file-name) org-roam-backlinks))
	  (org-startup-with-latex-preview nil))
      (while backlinks
	(find-file (car backlinks))
	(org-latex-export-to-pdf)
	(setq org-roam-backlink-pdfs
	      (cons (concat (file-name-sans-extension (car backlinks)) ".pdf") org-roam-backlink-pdfs))
	(setq backlinks (cdr backlinks)))))
  (message "%s" "Done!"))

(setq bibtex-completion-bibliography
      '("~/Sync/My_Library.bib")
      bibtex-completion-pdf-field "File"
      bibtex-completion-library-path '("~/Sync/Zotero_pdfs"))

(setq bibtex-completion-additional-search-fields '(keywords abstract))

(setq ivy-bibtex-default-action 'ivy-bibtex-edit-notes)
(ivy-add-actions
 'ivy-bibtex
 '(("p" ivy-bibtex-open-any "Open pdf, url or DOI")))

(setq bibtex-completion-format-citation-functions
      '((org-mode . bibtex-completion-format-citation-org-title-link-to-PDF)
	(latex-mode . bibtex-completion-format-citation-cite)
	(markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	(python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
	(rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
	(default . bibtex-completion-format-citation-default)))

(require 'org-roam-bibtex)
(org-roam-bibtex-mode 1)

(setq orb-insert-interface 'ivy-bibtex
      orb-note-actions-interface 'ivy)
(setq orb-preformat-keywords '("citekey" "author" "date" "entry-type" "keywords" "url" "file"))

(require 'org-protocol)
(require 'org-roam-protocol)

(require 'websocket)
(require 'org-roam-ui)

(setq org-todo-keywords
      '((sequence "INBOX(i)"
		  "PROCESSING(p)"
		  "URGENT(u)"
		  "LOW-PRIORITY(l)"
		  "WAIT(w)"
		  "TO-READ(r)"
		  "|"
		  "DONE(d)"
		  )))

(setq org-agenda-files
      '("~/org_roam"
	"~/org_roam/daily"
	"~/org_roam/ref"))

(setq org-journal-dir "~/org_roam/daily"
      org-journal-file-format "%d-%m-%Y.org"
      org-journal-time-format "%a, %d/%m-%R")

(add-hook 'org-agenda-mode-hook 'visual-line-mode)

(define-skeleton project-skeleton
  "This skeleton inserts a link to the Current Projects file in the org-roam directory. 

Its used in my fleeting note initialization function as a means
to always make new fleeting notes point to the current projects
file, as that is that files purpose"
  ""
  "- tags :: [[id:b5e71fe5-9d76-4f7f-b58d-df6a561e6a6b][Current Projects]]")

(defun org-roam-init-fleeting-note ()
  "Prescribe an ID to the heading making it a node in org-roam, then
  add it the inbox by giving it a todo keyword. Finally, insert a new
  line and the `project-skeleton', linking the new file to the Current
  Projects file.

 This helps automate the process of creating new fleeting notes
 in combination with the `org-journal' commands"
  (interactive)
  (org-id-get-create)
  (evil-open-below 1)
  (project-skeleton)
  (org-todo))

(defun org-id-delete-entry ()
"Remove/delete an ID entry. Saves the current point and only does this if inside an org-heading."
(interactive)
  (save-excursion
    (org-back-to-heading t)
    (when (org-entry-delete (point) "ID"))))

(add-to-list 'org-after-todo-state-change-hook
	     (lambda ()
	       (when (equal org-state "DONE")
		 (org-id-delete-entry))))

(defun org-roam-node-find-todos ()
  "Filtered view of org-roam-node-find which displays only nodes
with a todo state. All my fleeting notes typically have a todo
state indicating I need to work on them so this filter helps me
out"
  (interactive)
  (org-roam-node-find nil nil #'org-roam-node-todo))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :if-new
	 (file+head "${slug}-%<%d-%m-%y>.org" "#+title: ${title}\n
- index ::  
- tags ::  ")
	 :unarrowed t
	 :jump-to-captured t)

	("o" "outline" plain "%?" :if-new
	 (file+head "outlines/${slug}-%<%d-%m-%y>.org" "#+title: ${title}\n
#+filetags: outline")
	 :unarrowed t
	 :jump-to-captured t)

	("r" "bibliography reference" plain
	 "%?"
	 :if-new
	 (file+head "ref/${citekey}.org" "#+title: ${title}\n
#+filetags: ${entry-type}
- keywords :: ${keywords}
- tags :: 

* Analysis of ${entry-type} by ${author}
:PROPERTIES:
:URL: ${url}
:NOTER_DOCUMENT: ${file}  
:NOTER_PAGE:              
:END:")
	 :unnarrowed t
	 :jump-to-captured t)

	("i" "info reference" plain
	 "%?"
	 :if-new
	 (file+head "ref/${slug}.org" "#+title: ${title}\n
#+filetags: %:type
- tags :: \n

[[elisp:(Info-goto-node \"(%:file)%:node\")][Link to Info page]]
\n
")
	 :unnarowed t)

	("e" "elfeed" plain
	 "%?"
	 :if-new
	 (file+head "ref/${slug}.org" "#+title: %:description\n
#+filetags: %:type
- keywords ::
- tags :: \n\n\n

[[%:link][Link to Elfeed Buffer]]
[[%:elfeed-entry-link][Link to Web Page]]")
	 :unnarowed t)))

(setq org-roam-capture-ref-templates 
      '(("r" "ref" entry "* %?" :target
	 (file+head "ref/${slug}.org" "#+title: ${title}\n
#+filetags: 
 - tags :: \n")
	 :unnarrowed t
	 :jump-to-captured t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?" :if-new
	 (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: daily")
	 :empty-lines 1)))

(provide 'zettelkasten)
