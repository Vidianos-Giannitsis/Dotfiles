;;; citar-org-roam.el --- Citar/org-roam integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: May 22, 2022
;; Modified: May 22, 2022
;; Version: 0.1
;; Homepage: https://github.com/bdarcus/citar-org-roam
;; Package-Requires: ((emacs "27.1") (org-roam "2.2") (citar "0.9.6"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A tiny minor-mode to integrate 'citar' and 'org-roam'.
;;
;;  Provides functions for:
;;  
;;  1. updating the 'citar' UI from the 'org-roam' database
;;  2. using org-roam to open or create notes
;;
;;; Code:

(require 'org-roam)
(require 'citar)

(defcustom citar-org-roam-subdir "references"
  "Org-roam subdirectory to place reference notes."
  :group 'citar
  :group 'citar-org-roam
  :type 'string)

;; REVEIW experimental config
(defvar citar-org-roam-notes-config
  `(:name "Org-Roam Ref Notes"
    :category org-roam-node
    :hasnote ,#'citar-org-roam-has-notes
    :action ,#'citar-org-roam-open-note-from-id
    :annotate ,#'citar-org-roam--annotate
    :items ,#'citar-org-roam--get-candidates))

(defvar citar-notes-source)
(defvar citar-notes-sources)

;;; Functions

(defun citar-org-roam--has-note-p (key &optional _entry)
  "Return non-nil if a KEY has an associated org-roam ref note."
  (let ((ref-node (org-roam-node-from-ref (concat "@" key))))
    (when ref-node t)))

(defun citar-org-roam-has-notes (&optional _entries)
  "Return function to check for notes.
When given a citekey, return non-nil if there's an associated
note."
  ;; Return predicate that queries hash table for given key
  (lambda (citekey)
    (org-roam-db-query
     [:select ref :from refs :where (= ref $s1)] citekey)))

(defun citar-org-roam-keys-with-notes ()
  "Return a list of keys with associated note(s)."
  (mapcar #'car (org-roam-db-query
                 [:select ref :from refs :where (= type "cite")])))

(defun citar-org-roam-cited (reference)
  "Return a list of notes that cite the REFERENCE."
  (interactive (list (citar-select-ref
                      :filter (citar-has-notes))))
  (let* ((ids
         (org-roam-db-query [:select * :from citations
                             :where (= cite-key $s1)]
                            (car reference)))
         ;; TODO candidates need to be more useful
         (note
          (if ids
              (completing-read "Note: " ids)
            (message "No notes cite this reference."))))
    ;; TODO need to open the note.
    note))

(defun citar-org-roam-open-note (key &optional _entry)
  "Open org-roam node for KEY."
  (when-let ((ref-node-ids
               (flatten-list
                (org-roam-db-query
                 [:select node-id :from refs
                  :where (= ref $s1)] key))))
    (dolist (id ref-node-ids)
      (let ((ref-node (org-roam-node-from-id id)))
        (org-roam-node-open ref-node)))))

(defun citar-org-roam-open-note-from-id (node-id)
  "Open note from NODE-ID."
  (let ((ref-node (org-roam-node-from-id node-id)))
    (org-roam-node-open ref-node)))

(defun citar-org-roam-ref-add (reference)
  "Add a roam_ref for REFERENCE to the node at point.

This is just a wrapper for 'org-roam-ref-add'."
  (interactive (list (citar-select-ref)))
  (let ((key (car reference)))
    (org-roam-ref-add (concat "@" key))))

(defun citar-org-roam--get-ref-nodes-for-key (key)
  "Return ref node ids for KEY."
  (when-let ((ref-node-ids
              (org-roam-db-query
               [:select [node-id ref] :from refs
                :where (= ref $s1)] key)))
    ref-node-ids))

(defun citar-org-roam--get-ref-nodes ()
  "Return all ref nodes as id and ref pair."
  (when-let ((ref-nodes
              (org-roam-db-query
               [:select [node-id ref] :from refs])))
    ref-nodes))

(defun citar-org-roam--annotate (candidate)
  "Annotate the CANDIDATE."
  (let* ((node (org-roam-node-from-id candidate))
         (ref (org-roam-db-query
               [:select ref :from refs
                :where (= node-id $s1)] candidate)))
    (concat
     "          " ; not thrilled with this
     (truncate-string-to-width
      (propertize (caar ref) 'face 'citar-highlight) 15 nil 32)
     (propertize (org-roam-node-title node) 'face 'citar))))

(defun citar-org-roam--get-candidates (&optional keys)
  "Return ref node candidate list, optionally filtered by KEYS.

Each candidate is a 'node-id' string."
  ;; REVIEW experimental
  (let ((refs nil))
    (progn
      (if keys
          (dolist (key keys)
            (let ((nodes (citar-org-roam--get-ref-nodes-for-key key)))
              (dolist (node nodes)
                (push node refs))))
        (setq refs (citar-org-roam--get-ref-nodes)))
      (mapcar
       (lambda (ref) (car ref)) refs))))

(defun citar-org-roam-select-ref ()
  "Return org-roam node-id for ref candidate."
  ;; TODO just a demo ATM
  (completing-read "ref:" (citar-org-roam--get-candidates)))

(defun citar-org-roam--create-note (key entry)
  "Create org-roam node for KEY and ENTRY."
  ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
  ;; FIX doesn't work ATM.
  (let ((title (citar-format--entry
                "${author editor} :: ${title}" entry)))
    (org-roam-capture-
     :templates
     '(("r" "reference" plain "%?" :if-new
        (file+head
         "%(concat citar-org-roam-subdir \"/\" \"${key}.org\")"
         ":PROPERTIES:
:ROAM_REFS: @${key}
:END:
#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey key
                                   :node (org-roam-node-create :title title)
                                   :props '(:finalize find-file)))))

(defvar citar-org-roam--orig-source citar-notes-source)

(defun citar-org-roam-setup ()
  "Setup 'citar-org-roam-mode'."
  ;; REVIEW if I use add-to-list here, it will run both functions.
  (citar-register-notes-source
   'citar-org-roam citar-org-roam-notes-config)
  (setq citar-notes-source 'citar-org-roam))

(defun citar-org-roam-reset ()
  "Reset 'citar-org-roam-mode' to default."
  ;; TODO this should be smarter.
  (setq citar-notes-source citar-org-roam--orig-source)
  (citar-remove-notes-source 'citar-org-roam))

;;;###autoload
(define-minor-mode citar-org-roam-mode
  "Toggle citar-org-roam-mode."
  :global t
  :group 'citar
  :lighter " citar"
  (if citar-org-roam-mode (citar-org-roam-setup)
    (citar-org-roam-reset)))

(provide 'citar-org-roam)
;;; citar-org-roam.el ends here
