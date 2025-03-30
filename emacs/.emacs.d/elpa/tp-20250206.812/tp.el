;;; tp.el ---  Utilities for transient menus that POST to an API -*- lexical-binding: t; -*-

;; Author: Marty Hiatt <mousebot@disroot.org>
;; Copyright (C) 2024 Marty Hiatt <mousebot@disroot.org>
;; Package-Version: 20250206.812
;; Package-Revision: cce2dfe0ec2b
;; Package-Requires: ((emacs "28.1") (transient "0.5.0"))
;; Keywords: convenience, api, requests
;; URL: https://codeberg.org/martianh/tp.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Some functions, classes and methods to make it easier to create
;; transient menus that send complex POST, PUT, or PATCH requests to JSON
;; APIs.

;; A typical use-case is where you have a single endpoint that takes many
;; different parameters. It's handy for a user to be able to set all the
;; options, then make a single request to change all the settings on the
;; server. It's also expected that they'll be able to view all the current
;; settings on the server, and make modifications to them for sending.

;; The classes and methods define some transient behaviours that make
;; sense for dealing with APIs:

;; - We handle fetching and saving current server settings, and initialize
;; our transients with the values fetched
;; - We compare the state of a current transient option against the server
;; values
;; - We distinguish the formatting of server settings and current transient
;; setting
;; - When reading a string from the user, we provide the server setting as
;; default input
;; - We can seemlessly handle making requests with arrays, i.e. =source[key]=,
;; or =field[1][name]= parameters
;; - We can easily fetch an infix's allowed values for =completing-read=
;; - We (ususally) only send changed values to the server.


;;; Code:

(require 'transient)
(require 'json)

;;; VARIABLES

(defvar tp-choice-booleans '("true" "false")
  "Boolean strings for sending non-JSON requests.")

(defvar tp-transient-settings nil
  "Settings for the current transient prefix.
This can hold either the default settings data (editable) as
returned by a server, or alternatively the transient's current
settings if it is only stored when completed, and may be edited
again before being sent.")

;;; UTILS

(defun tp-remove-not-editable (alist var)
  "Remove non-editable fields from ALIST.
Check against the fields in VAR, which should be a list of strings."
  (cl-remove-if-not
   (lambda (x)
     (member (symbol-name (car x))
             var))
   alist))

(defun tp-return-data (fetch-fun &optional editable-var field)
  "Return data to populate current settings.
Call FETCH-FUN with zero arguments to GET the data. Cull the data
with `tp-remove-not-editable', bind the result to
`tp-transient-settings' and return it.
EDITABLE-VAR is a variable containing a list of strings
corresponding to the editable fields of the JSON data returned.
See `tp-remove-not-editable'.
FIELD is a JSON field to set `tp-transient-settings' to, fetched
with `alist-get'."
  (let* ((data (funcall fetch-fun))
         (editable (if editable-var
                       (tp-remove-not-editable data editable-var)
                     data)))
    (setq tp-transient-settings
          (if field
              (alist-get field editable)
            editable))))

(defun tp-only-changed-args (alist)
  "Remove elts from ALIST if value is changed.
Values are considered changed if they do not match those in
`tp-transient-settings'. Nil values are also removed if they
match the empty string.
If ALIST contains dotted.notation keys, we drill down into
`tp-transient-settings' to check the value."
  (prog1
      (cl-remove-if
       (lambda (x)
         (let* ((split (split-string (symbol-name (car x)) "\\."))
                (server-val
                 (if (< 1 (length split))
                     ;; FIXME: handle arbitrary nesting:
                     (alist-get (intern (cadr split))
                                (alist-get (intern (car split))
                                           tp-transient-settings))
                   (alist-get (car x)
                              tp-transient-settings))))
           (cond ((not (cdr x)) (equal "" server-val))
                 (t (equal (cdr x) server-val)))))
       alist)
    ;; unset our vars (comment if need to inspect):
    (setq tp-transient-settings nil)))

(defun tp-bool-to-str (cons)
  "Convert CONS, into a string boolean if it is either t or :json-false.
Otherwise just return CONS."
  (if (not (consp cons))
      cons
    (cons (car cons)
          (cond ((eq :json-false (cdr cons)) "false")
                ((eq t (cdr cons)) "true")
                (t (cdr cons))))))

(defun tp-bool-str-to-json (cons)
  "Convert CONS, into a string boolean if it is either t or :json-false.
Otherwise just return CONS."
  ;; do nothing if not a cons: `-tree-map' doesn't handle parsed JSON well
  (if (not (consp cons))
      cons
    (cons (car cons)
          (cond ((equal "false" (cdr cons)) :json-false)
                ((equal "true" (cdr cons)) t)
                (t (cdr cons))))))

(defun tp-tree-map (fn tree)
  "Apply FN to each element of TREE while preserving the tree structure.
This is just `-tree-map' from the dash library."
  ;; (declare (important-return-value t)) ;; commented for older emacsen
  (cond
   ((null tree) ())
   ;; the smallest element this operates on is not a list item, but a cons
   ;; pair, e.g. (a . b):
   ((nlistp (cdr-safe tree)) ;; ie `-cons-pair?'
    (funcall fn tree))
   ((consp tree)
    (mapcar (lambda (x) (tp-tree-map fn x)) tree))
   ((funcall fn tree))))

(defun tp-bools-to-strs (alist)
  "Convert values in ALIST to string booleans if they are JSON booleans."
  (tp-tree-map
   #'tp-bool-to-str alist))

(defun tp-bool-strs-to-json (alist)
  "Convert values in ALIST to string booleans if they are JSON booleans."
  (tp-tree-map
   #'tp-bool-str-to-json alist))

(defun tp-dots-to-arrays (alist &optional only-last-two parent-suffix)
  "Convert keys in ALIST tp dot annotation to array[key] annotation.
If ONLY-LAST-TWO is non-nil, return only secondlast[last] from
KEY.
PARENT-SUFFIX is appended to the first element."
  ;; FIXME: handle multi dots?
  (cl-loop for a in alist
           collect (cons (tp-dot-to-array (symbol-name (car a))
                                          only-last-two parent-suffix)
                         (cdr a))))

(defun tp-dot-to-array (key &optional only-last-two parent-suffix)
  "Convert KEY from tp dot annotation to array[key] annotation.
Handles multiple values by calling `tp-dot-to-array-multi', which
see.
If ONLY-LAST-TWO is non-nil, return only secondlast[last] from
KEY.
PARENT-SUFFIX is appended to the first element."
  (let* ((split (split-string key "\\.")))
    (cond ((not key) nil)
          ((= 1 (length split)) key)
          (only-last-two
           (concat ;; but last item
            (car (last split 2))
            ;; last item
            "[" (car (last split)) "]"))
          (t (tp-dot-to-array-multi split parent-suffix)))))

(defun tp-dot-to-array-multi (list parent-suffix)
  "Wrap all elements of LIST in [square][brackets] save the first.
Concatenate the results together.
PARENT-SUFFIX is appended to the first element."
  (mapconcat
   (lambda (x)
     (if (equal x (car list))
         (if parent-suffix
             (concat x parent-suffix)
           x)
       (concat"[" x "]")))
   list))

(defun tp--get-choices (obj)
  "Return the value contained in OBJ's choices-slot.
It might be a symbol, in which case evaluate it, a function, in
which case call it. else just return it."
  (let ((slot (oref obj choices)))
    (cond ((functionp slot)
           (funcall slot))
          ((symbolp slot)
           (symbol-value slot))
          (t slot))))

(defun tp-parse-args-for-send (args &optional strings)
  "Parse ARGS, an alist of args for sending.
If STRINGS is non-nil, convert elisp JSON booleans into string
booleans."
  (let* ((changed (tp-only-changed-args args))
         (arrays (tp-dots-to-arrays changed)))
    (if strings
        (tp-bools-to-strs arrays)
      arrays)))

;; CLASSES

(defclass tp-option (transient-option)
  ((always-read :initarg :always-read :initform t)
   (alist-key :initarg :alist-key)) ;; :key slot already taken!
  "An infix option class for our options.
We always read.")

(defclass tp-option-str (tp-option)
  ((format :initform " %k %d %v"))
  "An infix option class for our option strings.
We always read, and our reader provides initial input from
default/current values.")

(defclass tp-bool (tp-option)
  ((format :initform " %k %d %v")
   (choices :initarg :choices :initform
            (lambda () tp-choice-booleans)))
  "An option class for our choice booleans.
We implement this as an option because we need to be able to
explicitly send true/false values to the server, whereas
transient ignores false/nil values.")

(defclass tp-cycle (tp-bool)
  ()
  "A class to cycle through options.
Like `tp-bool', but supports arbitrary multiple options.
Values to send will not be JSON booleans, but plain strings.")

;;; TRANSIENT METHODS
;; for `tp-bool' we define our own infix option that displays
;; [true|false] like exclusive switches. activating the infix
;; moves to the next option.

(cl-defmethod transient-infix-value ((obj tp-option))
  "Return the infix value of OBJ as a cons cell."
  (cons (oref obj alist-key)
        (oref obj value)))

(cl-defmethod transient-init-value ((obj tp-option))
  "Initialize the value of OBJ."
  (let* ((prefix-val (oref transient--prefix value)))
    (oset obj value
          (tp-get-server-val obj prefix-val))))

(cl-defmethod transient-format-value ((obj tp-option))
  "Format the value of OBJ.
Format should just be a string, highlighted green if it has been
changed from the server value."
  (let* ((cons (transient-infix-value obj))
         (value (when cons (cdr cons))))
    (if (not value)
        ""
      (propertize value
                  'face (if (tp-arg-changed-p obj cons)
                            'transient-value
                          'transient-inactive-value)))))

(defvar tp-json-bool-alist
  '((t . "true")
    (:json-false . "false"))
  "An alist for JSON booleans and boolean strings.")

(defun tp-get-bool-str (bool)
  "Given a JSON BOOL, return its string boolean."
  (alist-get bool tp-json-bool-alist))

(defun tp-active-face-maybe (obj cons)
  "Return a face spec based on OBJ's value and CONS."
  (let* ((value (oref obj value))
         (active-p (equal (cdr cons)
                          (if (symbolp value)
                              (tp-get-bool-str value)
                            value)))
         (changed-p (tp-arg-changed-p obj cons)))
    ;; FIXME: differentiate init server value
    ;; from switching to other value then switching
    ;; back to server value?
    (cond ((and active-p changed-p)
           'transient-value)
          ((and active-p (not changed-p))
           '(:inherit transient-value :underline t))
          ((and (not active-p) (not changed-p))
           '(:inherit transient-inactive-value :underline t))
          (t
           'transient-inactive-value))))

(cl-defmethod transient-format-value ((obj tp-bool))
  "Format the value of OBJ.
Format should be like \"[opt1|op2]\", with the active option highlighted.
The value currently on the server should be underlined."
  (let* ((key (oref obj alist-key))
         (choices (tp--get-choices obj)))
    (concat
     (propertize "["
                 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (let ((cons (cons key choice)))
          (propertize
           choice
           'face (tp-active-face-maybe obj cons))))
      choices
      (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-read ((obj tp-bool))
  "Cycle through the possible values of OBJ."
  (let* ((cons (transient-infix-value obj))
         (val (tp-get-bool-str (cdr cons)))
         (choices (tp--get-choices obj))
         (str (if (or (not val) ;; handle nil value
                      (equal val (car (last choices))))
                  (car choices)
                (cadr (member val choices)))))
    ;; return JSON bool:
    (car (rassoc str tp-json-bool-alist))))

(cl-defmethod transient-infix-read ((obj tp-cycle))
  "Cycle through the possible values of OBJ."
  (let* ((cons (transient-infix-value obj))
         (val (cdr cons))
         (choices (tp--get-choices obj)))
    (if (or (not val) ;; handle nil value
            (equal val (car (last choices))))
        (car choices)
      (cadr (member val choices)))))

;; FIXME: see the `transient-infix-read' method's docstring:
;; we should preserve history, follow it. maybe just mod it.
(cl-defmethod transient-infix-read ((obj tp-option-str))
  "Reader function for OBJ.
We add the current value as initial input."
  (let* ((value (transient-infix-value obj))
         (prompt (transient-prompt obj)))
    (read-string prompt (cdr value))))

(cl-defmethod transient-prompt ((obj tp-option))
  "Format a prompt for OBJ."
  (let* ((key (oref obj alist-key))
         (split (split-string (symbol-name key) "\\."))
         (str (string-replace "_" " " (car (last split)))))
    (format "%s: " str)))

(cl-defmethod transient-infix-read ((obj tp-option))
  "Cycle through the possible values of OBJ."
  (let* ((prompt (transient-prompt obj))
         (choices (tp--get-choices obj)))
    (completing-read prompt choices nil :match)))

;;; OUR METHODS

(cl-defmethod tp-arg-changed-p ((obj tp-option) cons)
  "T if value of CONS is different to the value in `tp-transient-settings'.
The format of the value is a transient pair as a string, ie \"key=val\".
Nil values will also match the empty string.
OBJ is the object whose args are being checked."
  (let* ((data (oref transient--prefix value))
         (server-val (tp-get-server-val obj data))
         (server-str (if (and server-val
                              (symbolp server-val))
                         (tp-get-bool-str server-val)
                       server-val)))
    (not (equal (cdr cons) server-str))))

(cl-defmethod tp-get-server-val ((obj tp-option) data)
  "Return the server value for OBJ from DATA.
If OBJ's key has dotted notation, drill down into the alist. Currently
only one level of nesting is supported."
  ;; TODO: handle nested alist keys
  (let* ((key (oref obj alist-key))
         (split (split-string (symbol-name key) "\\.")))
    (cond ((= 1 (length split))
           (alist-get key data))
          ((= 2 (length split))
           (alist-get (intern (cadr split))
                      (alist-get (intern (car split)) data))))))

;;; VARIABLE CLASS

;; a class used to implement `transient-init-value' in a way that doesn't
;; fetch from a server, but merely gets data from `tp-transient-settings'.
;; this hopefully means you can multi-inherit from this class and another,
;; and not have to implement `transient-init-value' for each of your
;; class(es).

;; it seems like for this to work right, tp-option-var should be the
;; second, not first, superclass. e.g. (defclass your-tp-option (tp-option
;; tp-option-var) (()))

(defclass tp-option-var (tp-option)
  (()))

(cl-defmethod transient-init-value ((obj tp-option-var))
  "Initialize OBJ, an option.
Pull value from `tp-transient-settings' if possible.'"
  (let ((key (oref obj alist-key)))
    (oset obj value
          (alist-get key tp-transient-settings))))

(provide 'tp)
;;; tp.el ends here
