(add-hook 'after-init-hook 'org-roam-setup)
(setq org-roam-v2-ack t)

(use-package org-roam
  :config
  (setq org-roam-directory "~/org_roam/"
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
    difference that it returns a number instead of a formatted
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

  (setq org-roam-node-display-template "${title:115} ${backlinkscount:6} ${todostate:20} ${directories:10} ${tags:15}")

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

(setq bibtex-completion-bibliography
      '("~/org_roam/My_Library.bib" "~/org_roam/My_Library2.bib")
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

(require 'oc)
(require 'oc-csl)
(setq org-cite-global-bibliography '("~/org_roam/My_Library.bib" "~/org_roam/My_Library2.bib"))

(setq org-cite-export-processors '((t csl)))

(setq org-cite-csl-styles-dir "~/Zotero/styles")
(setq citeproc-org-default-style-file "~/Zotero/styles/american-chemical-society.csl")

(setq zotra-backend 'zotra-server
      zotra-local-server-directory "~/Cloned_Repositories/zotra-server/"
      zotra-default-bibliography "~/org_roam/My_Library2.bib"
      zotra-download-attachment-default-directory "~/Sync/Zotero_pdfs")

(setq citar-bibliography '("~/org_roam/My_Library.bib" "~/org_roam/My_Library2.bib"))
(setq citar-notes-paths '("~/org_roam/ref"))

(require 'org-roam-bibtex)
(org-roam-bibtex-mode 1)

(setq orb-insert-interface 'ivy-bibtex
      orb-note-actions-interface 'ivy)
(setq orb-preformat-keywords '("citekey" "author" "date" "entry-type" "keywords" "url" "file"))

(require 'org-protocol)
(require 'org-roam-protocol)

(require 'websocket)
(require 'org-roam-ui)

(defvar-local org-roam-backlinks-files nil
  "Buffer local variable displaying a list of the absolute paths
  of all the files that are backlinked to current node. These are
  not added by default, and as such this variable has the value
  nil but they can be added by running the
  `org-roam-backlinks-find-files' function on a node.")

(defvar org-roam-backlinks-pdfs nil
  "After running `org-roam-export-backlinks-to-latex-pdf', to
  export a node and all its backlinks to pdf, the value of this
  variable in the original node's buffer will become a list of
  all the pdfs that were created. This is to ease the process of
  combining them as the value of this variable can then be passed
  to a program such as pdftk to combine them.")

(defun org-roam-backlinks-sort-by-backlinks (completion-a completion-b)
  "Sorting function for org-roam that sorts the list of nodes by
the number of backlinks. This is the sorting function in
`org-roam-backlinks-node-find-by-backlinks'"
  (let ((node-a (cdr completion-a))
	(node-b (cdr completion-b)))
    (>= (org-roam-node-backlinkscount-number node-a)
	(org-roam-node-backlinkscount-number node-b))))

(defun org-roam-backlinks-node-find-by-backlinks ()
  "Essentially works like `org-roam-node-find' (although it uses
a combination of `find-file' and `org-roam-node-read' to
accomplish that and not `org-roam-node-find' as only
`org-roam-node-read' can take a sorting function as an argument)
but the list of nodes is sorted by the number of backlinks
instead of most recent nodes. Sorting is done with
`org-roam-backlinks-sort-by-backlinks'"
  (interactive)
  (find-file (org-roam-node-file (org-roam-node-read nil nil #'org-roam-backlinks-sort-by-backlinks))))

(defun org-roam-backlinks-query ()
  "Simple org-roam query function that stores the IDs of all the
  files that link to the node at point. This is a modified part
  of the `org-roam-backlinks-get' function keeping only the part
  necessary for `org-roam-backlinks-find-files' to work as this is a
  complimentary function to that"
  (org-roam-db-query
   [:select [source dest]
	    :from links
	    :where (= dest $s1)
	    :and (= type "id")]
   (org-roam-node-id (org-roam-node-at-point))))

(defun org-roam-backlinks-find-files ()
    "Get all nodes that link to the node at point with the
    `org-roam-backlink-query' function, find their absolute path
    and save a list of those paths to the buffer local variable
    `org-roam-backlinks'.

  With the list, you can act on all those files together. This is
  exceptionally useful with index files as it allows you to do an
  action on all files linked to this index automatically."
    (interactive)
    (let ((backlinks (length (org-roam-backlinks-query))))
      (dotimes (number backlinks)
	(let* ((id (car (nth number (org-roam-backlinks-query))))
	       (node (org-roam-node-from-id id)))
	  (setq-local org-roam-backlinks-files (cons (org-roam-node-file node) org-roam-backlinks-files))))
      org-roam-backlinks-files))

(defun org-roam-backlinks-export-to-latex-pdf ()
  "Export the current buffer and every buffer that mentions it to
a pdf through the org-latex export. Makes use of the
`org-roam-backlinks-find-files' function to find all the
backlinks. Also saves all the pdf names in a variable called
`org-roam-backlinks-pdfs'. These names can then be passed to
something like pdftk to merge them into one pdf"
  (interactive)
  (save-current-buffer
    (let ((backlinks (cons (buffer-file-name) org-roam-backlinks-files))
	  (org-startup-with-latex-preview nil))
      (while backlinks
	(find-file (car backlinks))
	(org-latex-export-to-pdf)
	(setq org-roam-backlinks-pdfs
	      (cons (concat (file-name-sans-extension (car backlinks)) ".pdf") org-roam-backlinks-pdfs))
	(setq backlinks (cdr backlinks)))))
  (message "%s" "Done!"))

(defcustom org-roam-backlinks-choices '("View Backlinks" "Go to Node" "Add to Zetteldesk" "Find Similar Nodes" "Quit")
  "List of choices for `org-roam-backlinks-node-read'.
Check that function's docstring for more info about these.")

(defun org-roam-backlinks-query* (NODE)
  "Gets the backlinks of NODE with `org-roam-db-query'."
  (org-roam-db-query
	[:select [source dest]
		 :from links
		 :where (= dest $s1)
		 :and (= type "id")]
	(org-roam-node-id NODE)))

(defun org-roam-backlinks-p (SOURCE NODE)
  "Predicate function that checks if NODE is a backlink of SOURCE."
  (let* ((source-id (org-roam-node-id SOURCE))
	 (backlinks (org-roam-backlinks-query* SOURCE))
	 (id (org-roam-node-id NODE))
	 (id-list (list id source-id)))
    (member id-list backlinks)))

(defun org-roam-backlinks-poi-or-moc-p (NODE)
  "Check if NODE has the tag POI or the tag MOC.  Return t if it does."
  (or (string-equal (car (org-roam-node-tags NODE)) "POI")
      (string-equal (car (org-roam-node-tags NODE)) "MOC")))

(defun org-roam-backlinks--read-node-backlinks (source)
  "Runs `org-roam-node-read' on the backlinks of SOURCE.
The predicate used as `org-roam-node-read''s filter-fn is
`org-roam-backlinks-p'."
  (org-roam-node-read nil (apply-partially #'org-roam-backlinks-p source)))

(defun org-roam-backlinks-node-read (node)
  "Read a NODE and run `org-roam-backlinks--read-node-backlinks'.
Upon selecting a backlink, prompt the user for what to do with
the backlink. The prompt is created with `completing-read' with
valid options being everything in the list
`org-roam-backlinks-choices'.

If the user decides to view the selected node's backlinks, the
function recursively runs itself with the selection as its
argument. If they decide they want to go to the selected node,
the function runs `find-file' and the file associated to that
node. Lastly, if they choose to quit, the function exits
silently.

There is however also the option to add the node to the current
`zetteldesk-desktop'. `zetteldesk.el' is a package I have written
to extend org-roam and naturally I wanted to include some
interaction with it in this function."
  (let* ((backlink (org-roam-backlinks--read-node-backlinks node))
	 (choice (completing-read "What to do with NODE: "
				  org-roam-backlinks-choices)))
    (cond
     ((string-equal
       choice
       (first org-roam-backlinks-choices))
      (org-roam-backlinks-node-read backlink))
     ((string-equal
       choice
       (second org-roam-backlinks-choices))
      (find-file (org-roam-node-file backlink)))
     ((string-equal
       choice
       (third org-roam-backlinks-choices))
      (zetteldesk-add-node-to-desktop backlink))
     ((string-equal
       choice
       (fourth org-roam-backlinks-choices))
      (org-roam-similarity-node-find backlink))
     ((string-equal
       choice
       (fifth org-roam-backlinks-choices))))))

(defun org-roam-backlinks-search ()
  "Select an `org-roam-node' and recursively search its backlinks.

This function is a starter function for
`org-roam-backlinks-node-read' which gets the initial node
selection from `org-roam-node-list'. For more information about
this function, check `org-roam-backlinks-node-read'."
  (interactive)
  (let ((node (org-roam-node-read)))
    (org-roam-backlinks-node-read node)))

(defun org-roam-backlinks-search-from-moc-or-poi ()
  "`org-roam-backlinks-search' with an initial selection filter.

Since nodes tagged as \"MOC\" or \"POI\" are the entry points to
my personal zettelkasten, I have this helper function which is
identical to `org-roam-backlinks-search' but filters initial
selection to only those notes. That way, they initial selection
has a point as it will be on a node that has a decent amount of
backlinks."
  (interactive)
  (let ((node (org-roam-node-read nil #'org-roam-backlinks-poi-or-moc-p)))
    (org-roam-backlinks-node-read node)))

(defvar org-roam-backlinks-selected-nodes '()
  "List of nodes selected in `org-roam-backlinks--select-nodes'.")

(defun org-roam-backlinks--select-nodes (NUM)
  "Select NUM `org-org-roam-nodes' and return a list of those."
  (setq org-roam-backlinks-selected-nodes '())
  (dotimes (i NUM)
    (let ((node (org-roam-node-read)))
      (add-to-list 'org-roam-backlinks-selected-nodes node)))
  org-roam-backlinks-selected-nodes)

(defvar org-roam-backlinks-selected-node-backlinks '()
  "List of backlinks of nodes in `org-roam-backlinks-selected-nodes'.

This list is filled using `org-roam-backlinks-get-node-backlinks'")

(defun org-roam-backlinks-get-node-backlinks (NUM)
  "Get a list of lists of backlinks of the nodes in NODE-LIST."
  (setq org-roam-backlinks-selected-node-backlinks '())
  (let ((node-list (org-roam-backlinks--select-nodes NUM)))
    (dolist (node node-list)
      (let ((backlinks (org-roam-backlinks-query* node))
	    (backlink-ids))
	(dolist (id backlinks)
	  (add-to-list 'backlink-ids (car id)))
	(add-to-list 'org-roam-backlinks-selected-node-backlinks backlink-ids)))
    org-roam-backlinks-selected-node-backlinks))

(defun org-roam-backlinks-id-intersection (list1 list2)
  "Find intersection of LIST1 and LIST2 using `cl-loop'."
  (cl-loop for id in list1
	   if (member id list2)
	   collect id into ids
	   finally (return ids)))

(defun org-roam-backlinks-get-ids (NUM)
  "Get the ids of all nodes which are backlinks of the selected nodes.

Node selection is done with the underlying function
`org-roam-backlinks--select-nodes'."
  (let* ((backlink-ids (org-roam-backlinks-get-node-backlinks NUM))
	 (result (car backlink-ids)))
    (dolist (ids backlink-ids)
      (setq result (org-roam-backlinks-id-intersection result ids)))
    result))

(defun org-roam-backlinks-roam-node-read--completions* (node-list &optional filter-fn sort-fn)
  "Run `org-roam-node-read--completions' with NODE-LIST being a list of nodes.

Typically, the function takes `org-roam-node-list' as the initial
list of nodes and creates the alist `org-roam-node-read'
uses.  However, it can be helpful to supply the list of nodes
yourself, when the predicate function used cannot be inferred
through a filter function of the form this function
takes.  FILTER-FN and SORT-FN are the same as in
`org-roam-node-read--completions'.  The resulting alist is to be
used with `org-roam-backlinks-roam-node-read*'."
  (let* ((template (org-roam-node--process-display-format org-roam-node-display-template))
	 (nodes node-list)
	 (nodes (mapcar (lambda (node)
			  (org-roam-node-read--to-candidate node template)) nodes))
	 (nodes (if filter-fn
		    (cl-remove-if-not
		     (lambda (n) (funcall filter-fn (cdr n)))
		     nodes)
		  nodes))
	 (sort-fn (or sort-fn
		      (when org-roam-node-default-sort
			(intern (concat "org-roam-node-read-sort-by-"
					(symbol-name org-roam-node-default-sort))))))
	 (nodes (if sort-fn (seq-sort sort-fn nodes)
		  nodes)))
    nodes))

(defun org-roam-backlinks-roam-node-read* (node-list &optional initial-input filter-fn sort-fn require-match prompt)
  "Run `org-roam-node-read' with the nodes supplied by NODE-LIST.

NODE-LIST is a list of nodes passed to
`org-roam-backlinks-roam-node-read--completions*', which creates an alist of
nodes with the proper formatting to be used in this
function.  This is for those cases where it is helpful to use your
own list of nodes, because a predicate function can not filter
them in the way you want easily.

INITIAL-INPUT, SORT-FN, FILTER-FN, REQUIRE-MATCH, PROMPT are the
same as in `org-roam-node-read'."
  (let* ((nodes (org-roam-backlinks-roam-node-read--completions* node-list filter-fn sort-fn))
	 (prompt (or prompt "Node: "))
	 (node (completing-read
		prompt
		(lambda (string pred action)
		  (if (eq action 'metadata)
		      `(metadata
			;; Preserve sorting in the completion UI if a sort-fn is used
			,@(when sort-fn
			    '((display-sort-function . identity)
			      (cycle-sort-function . identity)))
			(annotation-function
			 . ,(lambda (title)
			      (funcall org-roam-node-annotation-function
				       (get-text-property 0 'node title))))
			(category . org-roam-node))
		    (complete-with-action action nodes string pred)))
		nil require-match initial-input 'org-roam-node-history)))
    (or (cdr (assoc node nodes))
	(org-roam-node-create :title node))))

(defun org-roam-backlinks-multi-node-read ()
  "Read a node from intersecting backlinks of multiple nodes.

This is the low-level interactive function which is used for
collecting the nodes which are backlinks to a number of selected
nodes. This function calls `org-roam-backlinks-get-ids' for a lot
of the work, which returns a list of ids of all nodes which are
backlinked to some selected nodes. For more, check its
docstring. With this list of ids, this function runs
`org-roam-backlinks-roam-node-read*', which is a modified
`org-roam-node-read' which accepts a list of nodes as its
argument. This function returns the selected node to be used in
the higher level functions."
  (interactive)
  (let* ((num (read-number "Number of Nodes: "))
	 (ids (org-roam-backlinks-get-ids num))
	 (nodes (cl-loop for id in ids
			 collect (org-roam-node-from-id id) into nodes
			 finally (return nodes))))
    (org-roam-backlinks-roam-node-read* nodes)))

(defun org-roam-backlinks-multi-node-find ()
  "Find node from intersecting backlinks of multiple nodes.

This is a wrapper function for
`org-roam-backlinks-multi-node-read', which finds the file
associated with the selected node instead of returning it."
  (interactive)
  (let ((node (org-roam-backlinks-multi-node-read)))
    (find-file (org-roam-node-file node))))

(defun org-roam-backlinks-multi-search ()
  "Select an `org-roam-node' and recursively search its backlinks.

This function is an extension of the `org-roam-backlinks-search'
function which is used for recursively searching a node's
backlinks using `org-roam-backlinks-node-read'. Its main
difference is that it kickstarts the system not by selecting a
node but by running `org-roam-backlinks-multi-node-read'.  This
means that the selected node will be the intersection of
backlinks of a number of nodes selected from that function."
  (interactive)
  (let ((node (org-roam-backlinks-multi-node-read)))
    (org-roam-backlinks-node-read node)))

(defun org-roam-node-sort-by-atime (NODE1 NODE2)
  "Sorting function that sorts NODE1 and NODE2 by their file atime.

This is a simplified version of
`org-roam-node-read-sort-by-file-atime' which requires nodes as
its input and not something else. The above function is what
`org-roam-node-read's sorting uses and it has a special
formatting."
  (time-less-p (org-roam-node-file-atime NODE1)
	       (org-roam-node-file-atime NODE2)))

(defun org-roam-logseq-tag-function (TAG)
  "An implementation of logseq's tagging system in org-roam.

Prompt for TAG which is the name of a tag in your org-roam
repository, filter it to only contain nodes with that tag and
sort them so the most recently accessed one is the first item of
the list. Sorting is done with the custom
`org-roam-node-sort-by-atime' function. Then, check if a buffer
exists with the name *TAG-nodes* and if it doesn't create it.

In that new buffer, switch to org-mode and for every item in the
sorted-nodes list, go to `point-max', insert a new line, insert
the string #+transclude: make an org-mode id link with the node's
id and insert another newline. Once done, run
`org-transclusion-add-all' to activate the transclusion links and
view editable versions of the selected nodes.

Finally, restore the buffer from which this function was called
and insert and org-mode elisp link that runs `switch-to-buffer'
to switch to the newly-created buffer."
  (interactive "MTag: ")
  (let* ((init-list (org-roam-node-list))
	 (tagged-nodes (cl-remove-if-not (lambda (NODE)
					   (member TAG (org-roam-node-tags NODE)))
					 init-list))
	 (sorted-nodes (reverse (sort tagged-nodes #'org-roam-node-sort-by-atime)))
	 (buffer-name (concat "*" TAG "-nodes*"))
	 (buffer (get-buffer-create buffer-name)))
    (save-excursion
      (with-current-buffer buffer
	(org-mode)
	(dolist (node sorted-nodes)
	  (goto-char (point-max))
	  (newline)
	  (insert
	   "#+transclude: "
	   (org-link-make-string
	    (concat "id:" (org-roam-node-id node))))
	   (newline))
	(org-transclusion-add-all)))
    (insert
     (org-link-make-string
      (concat "elisp:(switch-to-buffer \"" buffer-name "\")")
      (concat "#" TAG)))))

(require 'org-similarity)

;; Directory to scan for possibly similar documents.
;; org-roam users might want to change it to `org-roam-directory'.
(setq org-similarity-directory org-roam-directory)

;; How many similar entries to list at the end of the buffer.
(setq org-similarity-number-of-documents 10)

;; Whether to prepend the list entries with similarity scores.
(setq org-similarity-show-scores nil)

;; Similarity score threshold. All results with a similarity score below this
;; value will be omitted from the final list.
;; Default is 0.05.
(setq org-similarity-threshold 0.05)

;; Whether the resulting list of similar documents will point to ID property or
;; filename. Default is nil.
;; However, I recommend setting it to `t' if you use `org-roam' v2.
(setq org-similarity-use-id-links t)

;; Scan for files inside `org-similarity-directory' recursively.
(setq org-similarity-recursive-search t)

;; Remove first result from the scores list. Useful if the current buffer is
;; saved in the searched directory, and you don't want to see it included
;; in the list. Default is nil."
(setq org-similarity-remove-first nil)

;; Text to show in the list heading. You can set it to "" if you
;; wish to hide the heading altogether.
(setq org-similarity-heading "** Related notes")

;; String to prepend the list items. You can set it to "* " to turn each
;; item into org headings, or "- " to turn them into an unordered org list.
;; Set the variable to "" to hide prefixes.
(setq org-similarity-prefix "- ")

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
	 (file+head "${slug}-%<%d-%m-%y>.org" "#+title: ${title}
- index ::  
- tags :: ")
	 :unnarrowed t
	 :jump-to-captured t)

	("o" "outline" plain "%?" :if-new
	 (file+head "outlines/${slug}-%<%d-%m-%y>.org" "#+title: ${title}
#+filetags: outline")
	 :unnarrowed t
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
	 :unnarowed t)

	("t" "thesis" plain "%?" :if-new
	 (file+head "thesis/${slug}-%<%d-%m-%y>.org" "#+title: ${title}
- index ::  
- tags :: ")
	 :unnarrowed t
	 :jump-to-captured t)))

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
