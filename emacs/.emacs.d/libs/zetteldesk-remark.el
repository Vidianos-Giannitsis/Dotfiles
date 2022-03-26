(defun zetteldesk-remark-mark (beg end)
  "Wrapper for `org-remark-mark'.

Calls the function with `org-remark-notes-file-name' being set to
zetteldesk-margin-notes.org in the `org-roam-directory'"
  (interactive (org-remark-region-or-word))
  (let ((org-remark-notes-file-name
	 (concat org-roam-directory "zetteldesk-margin-notes.org")))
    (org-remark-mark beg end)))

(defun zetteldesk-remark-highlight-advice
    (beg end &optional id mode label face properties)
  "Advice function to be used with `org-remark-highlight-mark'.

If the current buffer isn't associated to a file, find the
heading's title, associate it with an org-roam-node and find the
file associated with that node. Then run the part of
`org-remark-highlight-save' that wasn't ran if the buffer isn't
associated with a file. This part is a call to
`org-remark-highlight-save'.

This is meant to be used when taking margin notes from the
*zetteldesk-scratch* buffer. However, this will only work if the
current heading's title is associated with a node. In this case
org-remark will be able to associate the margin note with the
node its related to."
  (unless buffer-file-name
    (let* ((title (org-entry-get nil "ITEM"))
	   (node (org-roam-node-from-title-or-alias title))
	   (file (org-roam-node-file node))
	   (ov (make-overlay beg end nil :front-advance)))
      (org-remark-highlight-save file
				 beg end
				 (overlay-properties ov)
				 title))))

(advice-add 'org-remark-highlight-mark :after #'zetteldesk-remark-highlight-advice)

(defun zetteldesk-switch-to-margin-notes ()
  "Helper function which goes to the zetteldesk-margin-notes file.

If `org-remark-mark' is called through its wrapper function
`zetteldesk-remark-mark', it sets `org-remark-notes-file-name' to
a specific file, which is meant to be used with all margin notes
coming from zetteldesk-scratch. This function switches to that
file."
  (interactive)
  (pop-to-buffer (find-file (concat org-roam-directory "zetteldesk-margin-notes.org"))))
