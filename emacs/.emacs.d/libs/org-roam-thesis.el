;;; org-roam-thesis.el --- Org-roam extension functions to aid me in my thesis

;;; Commentary:

;;; This file has a few helper functions built on top of org-roam to
;;; help me manage my thesis better. The main concept is having
;;; templates for log and measurements files where I will write what I
;;; do every day and what measurements I made so that I can find them
;;; easily. Then I have some filters for viewing all thesis files,
;;; only log files or only measurements files.

;;; Code:

;; -- Templates --

(setq org-roam-thesis-templates
      '(("l" "log" plain "%?" :if-new
	 (file+head "thesis/log-%<%d-%m-%y>.org" "#+title: Log for %<%d-%m-%y>
#+filetags: LOG
- tags :: [[id:7e72e352-fffd-46f8-ab55-5adee534302a][Ενζυμική Υδρόλυση Αποβλήτων Τροφών και Παραγωγή Βιοαερίου μέσω Αναερόβιας Χώνευσης]] , ")
	 :unnarrowed t
	 :jump-to-captured t
	 :immediate-finish t)

	("m" "measurements" plain "%?" :if-new
	 (file+head "thesis/measurements-%<%d-%m-%y>.org" "#+title: Measurements for %<%d-%m-%y>
#+filetags: Measurements
- tags :: [[id:7e72e352-fffd-46f8-ab55-5adee534302a][Ενζυμική Υδρόλυση Αποβλήτων Τροφών και Παραγωγή Βιοαερίου μέσω Αναερόβιας Χώνευσης]] , ")
	 :unnarrowed t
	 :jump-to-captured t
	 :immediate-finish t)))

;; -- Org-roam-capture style functions --
(defun org-roam-thesis-capture-log ()
  "Capture a log note for the day.

The template used is the log template from
`org-roam-thesis-templates', while `org-roam-capture-' is used
for capturing it. The template does not need user input and due
to this it is immediately completed."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
		     :templates org-roam-thesis-templates
		     :keys "l"))

(defun org-roam-thesis-capture-measurements ()
  "Capture a measurements note for the day.

The template used is the measurements template from
`org-roam-thesis-templates', while `org-roam-capture-' is used
for capturing it. The template does not need user input and due
to this it is immediately completed."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
		     :templates org-roam-thesis-templates
		     :keys "m"))

;; -- Predicates for filtering --
(defun org-roam-thesis-directory-p (NODE)
  "Check if NODE is in the directory 'thesis'."
  (equal "(thesis)" (org-roam-node-directories NODE)))

(defun org-roam-thesis-log-p (NODE)
  "Check if NODE has the tag 'LOG'."
  (equal '("LOG") (org-roam-node-tags NODE)))

(defun org-roam-thesis-measurements-p (NODE)
  "Check if NODE has the tag 'Measurements'."
  (equal '("Measurements") (org-roam-node-tags NODE)))

;; -- Sorting Functions --
;; Parse the timestamp on the file names and create a sorting function
;; that will sort according to the creation date, not the date in
;; which it was last edited.

(defun org-roam-thesis-parse-log-time (node)
  "Parse the creation date of NODE if it has the tage 'LOG'.

This function is a helper function for
`org-roam-thesis-sort-by-log-time', the default sorting function
of `org-roam-thesis-log-find'. For more details refer to its
docstring."
  (when (org-roam-thesis-log-p node)
    (let ((day (string-to-number (substring (org-roam-node-title node) 8 10)))
	  (month (string-to-number (substring (org-roam-node-title node) 11 13)))
	  (year (string-to-number (substring (org-roam-node-title node) 14 16))))
      (encode-time (list 0 0 0 day month year)))))

(defun org-roam-thesis-parse-measurements-time (node)
  "Parse the creation date of NODE if it has the tag 'Measurements'.

This function is a helper function for
`org-roam-thesis-sort-by-measurements-time', the default sorting function
of `org-roam-thesis-measurements-find'. For more details refer to its
docstring."
  (when (org-roam-thesis-measurements-p node)
    (let ((day (string-to-number (substring (org-roam-node-title node) 17 19)))
	  (month (string-to-number (substring (org-roam-node-title node) 20 22)))
	  (year (string-to-number (substring (org-roam-node-title node) 23 25))))
      (encode-time (list 0 0 0 day month year)))))

(defun org-roam-thesis-sort-by-log-time (completion-a completion-b)
  "Sort by 'LOG' creation time.

This function operates on nodes with the tag 'LOG' created using
`org-roam-thesis-capture-log'. Using its helper function
`org-roam-thesis-parse-log-time', it parses the creation date of
each file from the file name and sorts them according to that
timestring. This is useful because this system acts like a
journal, so we don't want a file moving to the top of the
completion menu because it was modified recently but made a while
back."
  (let ((node-a (cdr completion-a))
	(node-b (cdr completion-b)))
    (if (time-less-p (org-roam-thesis-parse-log-time node-a)
		     (org-roam-thesis-parse-log-time node-b))
	nil
      t)))

(defun org-roam-thesis-sort-by-measurements-time (completion-a completion-b)
  "Sort by 'Measurements' creation time.

This function operates on nodes with the tag 'Measurements' created using
`org-roam-thesis-capture-measurements'. Using its helper function
`org-roam-thesis-parse-measurements-time', it parses the creation date of
each file from the file name and sorts them according to that
timestring. This is useful because this system acts like a
journal, so we don't want a file moving to the top of the
completion menu because it was modified recently but made a while
back."
  (let ((node-a (cdr completion-a))
	(node-b (cdr completion-b)))
    (if (time-less-p (org-roam-thesis-parse-measurements-time node-a)
		     (org-roam-thesis-parse-measurements-time node-b))
	nil
      t)))

;; -- Filter Functions --
(defun org-roam-thesis-node-find ()
  "Run `org-roam-node-find' in the 'thesis' directory."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-thesis-directory-p))

(defun org-roam-thesis-log-find ()
  "Run `org-roam-node-find' for thesis log files.

This function uses `org-roam-thesis-log-p' to filter only the
correct files and sorts them according to
`org-roam-thesis-sort-by-log-time'."
  (interactive)
  (find-file (org-roam-node-file
	      (org-roam-node-read nil #'org-roam-thesis-log-p
				  #'org-roam-thesis-sort-by-log-time))))

(defun org-roam-thesis-measurements-find ()
  "Run `org-roam-node-find' for thesis measurements files.

This function uses `org-roam-thesis-measurements-p' to filter only the
correct files and sorts them according to
`org-roam-thesis-sort-by-measurements-time'."
  (interactive)
  (find-file (org-roam-node-file
	      (org-roam-node-read nil #'org-roam-thesis-measurements-p
				  #'org-roam-thesis-sort-by-measurements-time))))

(defun org-roam-thesis-log-find* ()
  "Run `org-roam-node-find' for thesis log files.

The difference between this and `org-roam-thesis-log-find' is
that this function uses the default org-roam sorting."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-thesis-log-p))

(defun org-roam-thesis-measurements-find* ()
  "Run `org-roam-node-find' for thesis measurements files.

The difference between this and `org-roam-thesis-measurements-find' is
that this function uses the default org-roam sorting."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-thesis-measurements-p))

(provide 'org-roam-thesis)
;;; org-roam-thesis.el ends here
