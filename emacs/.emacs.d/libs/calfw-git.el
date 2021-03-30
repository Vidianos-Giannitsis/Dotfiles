;;; calfw-git.el --- calendar view for git-log

;; Copyright (C) 2014  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x cfw:git-open-calendar, in your git project.

;;; Code:

(require 'calfw)

(defvar cfw:git-command "git" "git command")

(defun cfw:git-schedule-period-to-calendar (begin end)
  (let ((begin-date (cfw:strtime begin))
        (end-date (cfw:strtime end)))
    (with-temp-buffer 
      (call-process cfw:git-command nil (current-buffer) nil 
                    "--no-pager" "log" 
                    "--after" begin-date "--before" end-date 
                    "--date" "iso" "--stat" 
                    "--pretty=format:%x00%ad%n%H%n%an%n%s%n%b%n")
      ;; 0 date / hash / author name / subject / body
      (goto-char (point-min))
      (let (entries (cont t) pps ps)
        (while cont
          (setq ps (re-search-forward "\x0" nil t))
          (unless pps (setq pps ps))
          (cond
           ((and ps (< pps ps))
            (let* ((lines (split-string (buffer-substring pps (1- ps)) "\n"))
                   (datestr (nth 0 lines)) (date (date-to-time datestr))
                   (hash (nth 1 lines))
                   (author (nth 2 lines))
                   (subject (nth 3 lines))
                   (body (mapconcat 'identity (nthcdr 4 lines) "\n"))
                   (e (make-cfw:event :title subject :start-date (cfw:emacs-to-calendar date)
                                      :description (format "Author: %s\nHash:%s\n%s" author hash body))))
              (push e entries))
            (setq pps ps))
           (ps nil) ; try next search
           (t (setq cont nil))))
        entries))))

;; (cfw:git-command-execute (cfw:date 1 1 2011) (cfw:date 4 1 2011))

(defun cfw:git-create-source (&optional color)
  "Create a git log source."
  (make-cfw:source
   :name "git log"
   :color (or color "SteelBlue")
   :update 'cfw:git-schedule-cache-clear
   :data 'cfw:git-schedule-period-to-calendar))

(defun cfw:git-open-calendar ()
  "Open git log calendar."
  (interactive)
  (let ((cp (cfw:create-calendar-component-buffer
             :view 'month
             :contents-sources (list (cfw:git-create-source)))))
    (switch-to-buffer (cfw:cp-get-buffer cp))))

;; (cfw:git-open-calendar)

(provide 'calfw-git)
;;; calfw-git.el ends here
