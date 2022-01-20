(cl-defmethod org-roam-node-buffer ((node org-roam-node))
  "Access slot \"buffer\" of org-roam-node struct CL-X"
  (let ((buffer (get-file-buffer (org-roam-node-file node))))
    buffer))

(defun org-roam-node-poi-p (NODE)
  "Check if NODE has the tag POI. Return t if it does"
  (string-equal (car (org-roam-node-tags NODE)) "POI"))

(defun org-roam-backlink-query ()
  "Simple org-roam query function that stores the IDs of all the files that link
  to the node at point. This is a modified part of the
  `org-roam-backlinks-get' function keeping only the part necessary for
  `org-roam-backlink-files' to work as this is a complimentary function to
  that"
  (org-roam-db-query
   [:select [source dest]
	    :from links
	    :where (= dest $s1)
	    :and (= type "id")]
   (org-roam-node-id (org-roam-node-at-point))))

(defvar-local zetteldesk "default"
  "Buffer local variable that determines whether a buffer is part
  of the current zetteldesk. A buffer is part of the zetteldesk
  only if the value of this variable is not its default value in
  that buffer. Its default value is default because I am not
  creative.")

(defun zetteldesk-p (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk'"
  (not (eq (default-value 'zetteldesk) (buffer-local-value 'zetteldesk (cdr BUFFER)))))

(defun zetteldesk-buffer-p (BUFFER)
  "Check if BUFFER is similtaneously part of the current
  `zetteldesk' and not a buffer for an org-roam file.

Org-roam file buffers are better viewed with `org-roam-node-file'
so this function filters down the database to non org-roam
zetteldesk buffers. This is what is used to create the filter
function `zetteldesk-switch-to-buffer'"
  (and (zetteldesk-p BUFFER) (not (org-roam-buffer-p (cdr BUFFER)))))

(defun zetteldesk-node-p (NODE)
  "Check if NODE is associated with an open buffer. If it is,
  check if that buffer is part of the current `zetteldesk'. If it
  isn't, return nil.

This function is used as a filter function to create
`zetteldesk-node-find' which is a filtered view of
`org-roam-node-find'"
  (if (org-roam-node-buffer NODE)
      (not (eq (default-value 'zetteldesk) (buffer-local-value 'zetteldesk (org-roam-node-buffer NODE))))
    nil))

(defun zetteldesk-org-buffer-p (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk' an org
  file but not one that belongs to org-roam.

This is used as the filter function for
`zetteldesk-insert-org-file-contents' which prompts for an org
file, but as `zetteldesk-insert-node-contents' is a superior
version for org-roam nodes, that function should not prompts for
those files"
  (and (zetteldesk-buffer-p BUFFER) (eq (buffer-local-value 'major-mode (cdr BUFFER)) 'org-mode)))

(defun zetteldesk-add-to-desktop (BUFFER)
  "Add BUFFER to the current `zetteldesk'"
  (interactive "b")
  (with-current-buffer BUFFER
    (setq-local zetteldesk "foo")))

(defun zetteldesk-add-node-to-desktop ()
  "Add an org-roam-node to the `zetteldesk' and if there isn't a
  buffer associated to it, create it.

The node is read through `org-roam-node-read'"
  (interactive)
  (let* ((node (org-roam-node-read))
	 (buffer (org-roam-node-buffer node))
	 (file (org-roam-node-file node))
	 (org-startup-with-latex-preview nil))
    (if (not (eq buffer nil))
	(with-current-buffer buffer
	  (setq-local zetteldesk "foo"))
      (with-current-buffer (find-file-noselect file)
	(setq-local zetteldesk "foo")))))

(defun zetteldesk-add-backlinks-to-desktop ()
  "Add the current buffer and all its backlinks to the `zetteldesk'. 

This function queries the database for all the nodes that link to
the current node with the `org-roam-backlink-query' function and
then recursively checks if there is an open buffer associated
with them, and if so adds it to the `zetteldesk'"
  (interactive)
  (setq-local zetteldesk "foo")
  (let ((backlinks (length (org-roam-backlink-query)))
	(org-startup-with-latex-preview nil))
    (dotimes (number backlinks)
      (let* ((id (car (nth number (org-roam-backlink-query))))
	      (node (org-roam-node-from-id id))
	      (buffer (org-roam-node-buffer node))
	      (file (org-roam-node-file node)))
	(if (not (eq buffer nil))
	    (with-current-buffer buffer
	      (setq-local zetteldesk "foo"))
	  (with-current-buffer (find-file-noselect file)
	    (setq-local zetteldesk "foo")))))))

(defun zetteldesk-remove-from-desktop (BUFFER)
  "Remove BUFFER from the current `zetteldesk'"
  (interactive "b")
  (with-current-buffer BUFFER
    (kill-local-variable 'zetteldesk)))

(defun zetteldesk-remove-node-from-desktop ()
  "Add an org-roam-node to the `zetteldesk' and if there isn't a
  buffer associated to it, create it.

The node is read through `org-roam-node-read'"
  (interactive)
  (let* ((node (org-roam-node-read nil #'zetteldesk-node-p))
	 (buffer (org-roam-node-buffer node)))
    (with-current-buffer buffer
      (kill-local-variable 'zetteldesk))))

(defun zetteldesk-remove-backlinks-from-desktop ()
  "Remove the current buffer and all its currently open backlinks
  from the `zetteldesk'.

This function is essentially a carbon copy of
`zetteldesk-add-backlinks-to-desktop' but instead of adding the
buffer to the desktop it removes it."
  (interactive)
  (kill-local-variable 'zetteldesk)
  (let ((backlinks (length (org-roam-backlink-query))))
    (dotimes (number backlinks)
      (let* ((id (car (nth number (org-roam-backlink-query))))
	      (node (org-roam-node-from-id id))
	      (buffer (org-roam-node-buffer node)))
	(unless (eq buffer nil)
	  (with-current-buffer buffer
	    (kill-local-variable 'zetteldesk)))))))

(defun zetteldesk-switch-to-buffer ()
  "Execute `switch-to-buffer' with the buffer list being
filtered (using `zetteldesk-buffer-p') to show only buffers that are
part of the current `zetteldesk' and not `org-roam-node's."
  (interactive)
  (switch-to-buffer (read-buffer "Zetteldesk Buffers: " nil nil #'zetteldesk-buffer-p)))

(defun zetteldesk-node-find ()
  "Execute `org-roam-node-find' with the list being
filtered (using `zetteldesk-node-p') to show only nodes that are
part of the current `zetteldesk'"
  (interactive)
  (org-roam-node-find nil nil #'zetteldesk-node-p))

(defun zetteldesk-node-insert ()
  "Execute `org-roam-node-insert' with the list being
filtered (using `zetteldesk-node-p') to show only nodes that are
part of the current `zetteldesk'"
  (interactive)
  (org-roam-node-insert #'zetteldesk-node-p))

(defun zetteldesk-create-scratch-buffer ()
  "Create the zetteldesk-scratch buffer and put it in `org-mode'"
  (interactive)
  (let ((buffer (generate-new-buffer "*zetteldesk-scratch*")))
    (with-current-buffer buffer
      (org-mode))))

(defun zetteldesk-switch-to-scratch-buffer ()
  "Open the zetteldesk-scratch buffer in a split with the current buffer"
  (interactive)
  (switch-to-buffer-other-window "*zetteldesk-scratch*"))

(defun zetteldesk-node-insert-if-poi ()
  "Filter `org-roam-node-list' to only include files in the current
`zetteldesk' that have the POI tag with `zetteldesk-node-p' and
`org-roam-node-poi-p'. Then insert a link to every one of those nodes
and seperate them with commas"
  (interactive)
  (let* ((init_list (org-roam-node-list))
	 (zetteldesk_nodes (cl-remove-if-not #'zetteldesk-node-p init_list))
	 (nodes_poi (cl-remove-if-not #'org-roam-node-poi-p zetteldesk_nodes)))
    (while nodes_poi
      (let* ((node (car nodes_poi))
	     (description (org-roam-node-formatted (car nodes_poi))))
	(insert (org-link-make-string
		 (concat "id:" (org-roam-node-id (car nodes_poi)))
		 description))
	(insert ", "))
      (setq nodes_poi (cdr nodes_poi)))))

(defun zetteldesk-insert-node-contents (&optional arg)
  "Select a node that is part of the current `zetteldesk', add a link
  to it at point and then insert its contents to the bottom of the 
  *zetteldesk-scratch* buffer after inserting a newline there. Remove
  the first 67 characters which is the properties section if it only
  contains the ID of the node as its unneeded and change the string
  #+title to a top level heading as its more practical when inserting
  the contents of multiple files.

If given a `\\[universal-argument]' also switch to the *zetteldesk-scratch* buffer
in a split.

With `\\[universal-argument]' `\\[universal-argument]' do not add a
link at point, but only insert the node's contents in the 
*zetteldesk-scratch* buffer and switch to it."
  (interactive "P")
  (let* ((node (org-roam-node-read nil #'zetteldesk-node-p))
	 (file (org-roam-node-file node))
	 (description (org-roam-node-formatted node)))
    (unless (equal arg '(16))
      (insert (org-link-make-string
	       (concat "id:" (org-roam-node-id node))
	       description)))
    (with-current-buffer "*zetteldesk-scratch*"
      (goto-char (point-max))
      (newline)
      (insert-file-contents file nil 67)
      (replace-string "#+title: " "* ")))
  (when (equal arg '(16))
    (switch-to-buffer "*zetteldesk-scratch*"))
  (when (equal arg '(4))
    (save-current-buffer
      (switch-to-buffer-other-window "*zetteldesk-scratch*"))))

(defun zetteldesk-insert-org-file-contents (&optional arg)
  "Select an org buffer (excluding org-roam files) that is part of the
  current `zetteldesk', insert its contents to the
  *zetteldesk-scratch* buffer, make its title a top level heading and
  demote all of its headings by one level (since the title now acts as
  a top level heading).

If given a `\\[universal-argument]' also switch to the
*zetteldesk-scratch* buffer in a split"
  (interactive "P")
  (let* ((buffer (set-buffer (read-buffer "Zetteldesk Buffers: " nil nil #'zetteldesk-org-buffer-p)))
	 (file (buffer-file-name buffer)))
    (set-buffer "*zetteldesk-scratch*")
    (goto-char (point-max))
    (save-excursion
      (newline)
      (insert-file-contents file))
    (save-excursion
      (while (not (org-next-visible-heading 1))
	(org-metaright)))
    (replace-string "#+title: " "* "))
  (when (equal arg '(4))
    (switch-to-buffer-other-window "*zetteldesk-scratch*")))

(provide 'zetteldesk)
