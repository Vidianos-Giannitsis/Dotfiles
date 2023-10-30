;;; org-roam-thesis.el --- Org-roam extension functions to aid me in my thesis

;;; Commentary:

;;; This file has a few helper functions built on top of org-roam to
;;; help me manage my thesis better. The main concept is having
;;; templates for log and measurements files where I will write what I
;;; do every day and what measurements I made so that I can find them
;;; easily. Then I have some filters for viewing all thesis files,
;;; only log files or only measurements files.

;;; Code:

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

;; -- Filter Functions --
(defun org-roam-thesis-node-find ()
  "Run `org-roam-node-find' in the 'thesis' directory."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-thesis-directory-p))

(defun org-roam-thesis-log-find ()
  "Run `org-roam-node-find' for thesis log files."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-thesis-log-p))

(defun org-roam-thesis-measurements-find ()
  "Run `org-roam-node-find' for thesis measurements files."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-thesis-measurements-p))

(provide 'org-roam-thesis)
;;; org-roam-thesis.el ends here
