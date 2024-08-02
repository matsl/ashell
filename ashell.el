;;; ashell.el --- background shell command with history  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

;; Author: Mats Lidell <matsl@gnu.org>
;; Keywords: tools

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

;; Keep a number of shell buffers with their output.

;;; Code:

(require 'ibuffer)
(require 'dired)

(defcustom ashell-output-buffer-history-size 20
  "Number of old shell buffers to keep."
  :type 'integer
  :group 'shell)

(defvar ashell-output-buffer-name-number 0)
(defvar ashell-output-buffer-name-prefix "*AShell")
(defvar ashell-command ""
  "Store a buffer local value of the command executed for display purposes.")

(defun ashell-output-buffer-name-generator ()
  "Generate a unique buffer name."
  (setq ashell-output-buffer-name-number (1+ ashell-output-buffer-name-number))
  (concat ashell-output-buffer-name-prefix "<" (number-to-string ashell-output-buffer-name-number) ">*"))

(defun ashell-garbage-collect-old-buffers ()
  "Limit number of ashell buffers by removing old buffers.
Number of old buffers that are kept is controlled by
`ashell-output-buffer-history-size'.  Buffers with running processes or
buffers marked as read only are not up for garbage collection and are
kept."
  (let* ((buffers (buffer-list))
         (buffer-name-regexp
          (concat
           (replace-regexp-in-string "\\*" "\\\\*" ashell-output-buffer-name-prefix)
           "<[[:digit:]]+>\\\\*"))
	 (shell-buffers
	  (sort
	   (delq nil
		 (mapcar
		  (lambda (x) (and (string-match buffer-name-regexp
						 (buffer-name x))
				   (not (get-buffer-process x))
                                   (with-current-buffer x
                                     (not buffer-read-only))
				   (buffer-name x)))
		  buffers))
	   (lambda (s1 s2) (string-version-lessp s1 s2)))))
    (dolist (buff (butlast shell-buffers (1- ashell-output-buffer-history-size)))
      (kill-buffer buff))))

(defun ashell-command (command &optional hide-command hide-window)
  "Run asynchronous shell COMMAND in a new buffer.
Set HIDE-COMMAND to t for not showing the command in the output buffer.
With optional HIDE-WINDOW do not display the shell output buffer.  Keep
`ashell-output-buffer-history-size' buffers as history."
  (interactive
   (list
    (read-shell-command (concat ashell-output-buffer-name-prefix ": ") nil nil
			(let ((filename
			       (cond
				(buffer-file-name)
				((eq major-mode 'dired-mode)
				 (dired-get-filename nil t)))))
			  (and filename (file-relative-name filename))))))
  (let ((buffer (get-buffer-create (ashell-output-buffer-name-generator)))
	(shell-command-dont-erase-buffer t)
        (async-shell-command-display-buffer (not hide-window)))
    (when (not hide-command)
      (with-current-buffer buffer
        (insert "$ " command "\n")))
    (async-shell-command command buffer nil)
    (with-current-buffer buffer
      (setq-local ashell-command command))
    (ashell-garbage-collect-old-buffers)))

(defun ashell-rerun ()
  "Rerun the command."
  (interactive)
  (ashell-command ashell-command))

(define-ibuffer-column ashell-cmd (:name "Cmd")
  "The command executed in the async shell."
  (when (string-prefix-p ashell-output-buffer-name-prefix (buffer-name))
    ashell-command))

(define-ibuffer-column ashell-number (:name "Nr")
  "The ashell number to display."
  (when (string-prefix-p ashell-output-buffer-name-prefix (buffer-name))
    (progn
      (string-match "<\\(.*\\)>" (buffer-name))
      (match-string 1 (buffer-name)))))

(define-ibuffer-column ashell-process-active (:name "P")
  "Show a P for buffers with active processes."
  (if (get-buffer-process buffer) "P" " "))

(defun ashell-ibuffer ()
  "Show all ashell buffers in ibuffer."
  (interactive)
  (let ((format
         '((mark (ashell-process-active) read-only " "
                 (ashell-number 2 3 :left)
                 " "
                 (size 6 -1 :right)
                 " "
                 (filename 35 35 :left :elide)
                 " > "
                 ashell-cmd)
           (mark (ashell-process-active) read-only " "
                 (ashell-number 2 3 :left)
                 " "
                 (size 6 -1 :right)
                 " "
                 (filename 35 -1 :left :elide)
                 " > "
                 ashell-cmd))))
    (ibuffer nil (concat ashell-output-buffer-name-prefix "-IBuffer*") nil nil nil nil format)
    (ibuffer-filter-by-name
     (concat
      (replace-regexp-in-string "\\*" "\\\\*" ashell-output-buffer-name-prefix)
      "<[[:digit:]]+>\\\\*"))))

(provide 'ashell)
;;; ashell.el ends here
