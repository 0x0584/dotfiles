;;; defuns.el ---- Summary: Some Useful Emacs Functions
;;
;; Filename: defuns.el
;; Description: Emacs Configuration
;; Author: Anas (0x0584)
;; Created: Nov 20, 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Some Useful Emacs Functions
;;
;; Summary:
;;
;;   Describe loaded files.
;;   Describe loaded modes.
;;   Describe load time.
;;
;; This is configuration is done after working with a messy
;; Emacs configuration for three years.	 Now after dealing with
;; Emacs Lisp for a while, I think I can handle this, as they said:
;;
;;   - You would need to configure your Emacs all over someday.
;;
;;  User options defined here:
;;
;;  Commands defined here:
;;
;;  interactive functions defined here:
;;
;;  Non-interactive functions defined here:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun insert-time ()
  "Insert the current time as H:M:S."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-date ()
  "Insert today's date as mm/dd/yyyy."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))

;; TODO: find a way to call this regularly after a specific delay
(defun create-tags (dir-name)
  "Create tags file based on source code found in DIR-NAME."
  (interactive "DDirectory: ")
  (eshell-command
   (concat "find . -name '*.c' -print -or -name '*.h' -print " "|"
	   " xargs etags --append" dir-name)))

(defun save-buffer-if-modified (&optional buffer)
  "Return t if the BUFFER was modified and saved."
  (interactive)
  (and (buffer-modified-p buffer)
       (not (ding))
       (y-or-n-p (concat (buffer-file-name buffer) " is modified, save it? "))
       (save-buffer buffer)))

(defun c-format-buffer ()
  "Format C buffer using indent command."
  (interactive)
  (when (save-buffer-if-modified)
    ;; (mark-whole-buffer)
    (universal-argument)
    (shell-command-on-region
     (point-min)
     (point-max)
     (concat "indent -nbad -bap -bbo -nbc -br -brs "
	     "-brf -c33 -cd33 -ncdb -ce -ci4 -cli0 "
	     "-cp33 -cs -d0 -di4 -nfc1 -nfca -hnl "
	     "-i4 -ip0 -l75 -lp -npcs -nprs -npsl "
	     "-saf -sai -saw -nsc -nsob -nss -ppi2 ")
     (buffer-name))
    (save-buffer)))

(defun code-buffer (direction)
  "Simply, avoiding buffers that start with asterisk.  if DIRECTION is t we call `next-buffer' otherwise we call `previous-buffer'."
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (if (eq direction 'nil) (next-buffer) (previous-buffer))
    (while (and (string-match-p "^\*" (buffer-name))
		(not (equal bread-crumb (buffer-name))))
      (if (eq direction 'nil) (next-buffer) (previous-buffer)))))

(defun next-code-buffer ()
  "See `code-buffer'."
  (interactive)
  (code-buffer nil))

(defun previous-code-buffer ()
  "See `code-buffer'."
  (interactive)
  (code-buffer t))

(defun safe-revert-buffer ()
  "Reverts a buffer, and only ask to save the buffer if it was modified."
  (interactive)
  (when (save-buffer-if-modified)
    (revert-buffer t t t)
    (message "buffer is reverted")))

(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file
       (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.  If some buffer are modified, ask to save them."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer))
		 (not (buffer-file-name buffer)))
      (if (save-buffer-if-modified buffer)
	  (kill-buffer buffer)))))

;;; defuns.el ends here
