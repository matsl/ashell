;;; hib-ashell.el --- run hyperbole shell actions using ashell    -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2025 Free Software Foundation, Inc.

;; Author: Mats Lidell <matsl@gnu.org>
;; Keywords: processes, hypermedia

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

;; Fun with background jobs with Hyperbole.

;;; Code:

(require 'hyperbole)
(require 'ashell)

(defact exec-shell-cmd (shell-cmd &optional internal-cmd _kill-prev)
  "Executes a SHELL-CMD string asynchronously.
 Optional non-nil second argument INTERNAL-CMD means do not display the shell
 command line executed."
  (interactive
   (let ((default  (car hargs:defaults))
 	 (default1 (nth 1 hargs:defaults)))
     (list (hargs:read "Shell cmd: "
		       '(lambda (cmd) (not (string= cmd "")))
		       default "Enter a shell command." 'string)
	   (y-or-n-p (format "Omit cmd from output (default = %s)? "
			     default1))
	   nil)))
  (let ((owind (selected-window))
        (cmd (concat "cd " (shell-quote-argument (expand-file-name default-directory) t) "; " shell-cmd)))
    (unwind-protect
 	(ashell-command cmd internal-cmd)
      (select-window owind))))

(defact exec-window-cmd (shell-cmd)
  "Executes an external window-based SHELL-CMD string asynchronously."
  (interactive
   (let ((default (car hargs:defaults)))
     (list (hargs:read "Shell cmd: "
		       '(lambda (cmd) (not (string= cmd "")))
		       default "Enter a shell command." 'string))))
  (let ((cmd (concat "cd " (shell-quote-argument (expand-file-name default-directory) t) "; " shell-cmd)))
    (save-excursion
      (ashell-command cmd t t)
      (message "Executing: %s" shell-cmd))))

(provide 'hib-ashell)

;;; hib-ashell.el ends here
