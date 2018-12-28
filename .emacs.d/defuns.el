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
;;	 Some Useful Emacs Functions
;;
;;; Summary:
;;
;;	  All function used elsewhere are defined here.
;;
;;; Interactive functions defined here:
;;	   `create-tags', `next-code-buffer', `named-term',
;;	   `named-term-below', `move-line', `move-line-up',
;;	   `move-line-down', `kill-other-buffers',
;;	   `find-overlays-specifying', `emacs-quit-confirm',
;;	   `de/highlight-line', `delete-nl-spaces',
;;	   `delete-nl-spaces-find-file-hook', `code-buffer',
;;	   `browse-file-directory', `save-buffer-if-modified',
;;	   `safe-revert-buffer', `switch-window-orientation',
;;	   `split-current-window', `select-line', `swap-buffer',
;;	   `xah-syntax-color-hex', `remove-highlight',
;;	   `previous-code-buffer'
;;
;;; Non-interactive functions defined here:
;;
;;	   `insert-time', `insert-date'
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
;; MERCHANT ABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
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

(require 'desktop+)

(defun insert-time ()
  "Insert the current time as H:M:S."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-date ()
  "Insert today's date as mm/dd/yyyy."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))

;; TODO: find a way to call this regularly after a specific delay
(defun create-etags ()
  "Create TAGS file based on source code found in the current directory."
  (interactive)
  (eshell-command
   (concat "find . -name '*.c' -print -or -name '*.h' -print"
		   " |	xargs etags")))

(defun save-buffer-if-modified (&optional buffer)
  "Return t if the BUFFER was modified and saved."
  (interactive)
  (if (not (buffer-modified-p buffer))
	  t
	(and (not (ding))
		 (y-or-n-p (concat (buffer-name buffer)
						   " is modified, save it? "))
		 (not (save-buffer buffer)))))

(defun code-buffer (next)
  "Simply, avoiding buffers that start with asterisk.
If NEXT is t we call `next-buffer' otherwise we call `previous-buffer'."
  (interactive)
  (let ((bread-crumb (buffer-name)))
	(if (eq next 't) (next-buffer) (previous-buffer))
	(while (and (string-match-p "^\*" (buffer-name))
				(not (equal bread-crumb (buffer-name))))
	  (if (eq next 't) (next-buffer) (previous-buffer)))))

(defun next-code-buffer ()
  "See `code-buffer'."
  (interactive)
  (code-buffer t))

(defun previous-code-buffer ()
  "See `code-buffer'."
  (interactive)
  (code-buffer nil))

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
  "Kill all buffers but the current one.
If some buffer are modified, ask to save them."
  (interactive)
  (dolist (buffer (buffer-list))
	(unless (or (eql buffer (current-buffer))
				(not (buffer-file-name buffer)))
	  (if (save-buffer-if-modified buffer)
		  (kill-buffer buffer)))))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column)) (start '()) (end '()))
	(beginning-of-line)
	(setq start (point))
	(end-of-line)
	(forward-char)			; get the '\n' too
	(setq end (point))
	(let ((line-text (delete-and-extract-region start end)))
	  (forward-line n)
	  (insert line-text)
	  (forward-line -1)
	  (forward-char col))))

(defun move-line-up (&optional n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (or (- n) -1)))

(defun move-line-down (&optional n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (or n 1)))

(defun indent-c-buffer ()
  "Call `indent' on the whole current buffer.
Ask to say the buffer if modified."
  (interactive)
  (let ((current (point)))
	(goto-char (point-min))
	(push-mark (point-max) nil t)
	(when (save-buffer-if-modified)
	  (universal-argument)
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   (concat "indent -nbad -bap -bbo -nbc -br -brs "
			   "-brf -c33 -cd33 -ncdb -ce -ci4 -cli0 "
			   "-cp33 -cs -d0 -di4 -nfc1 -nfca -hnl "
			   "-i4 -ip0 -l75 -lp -npcs -nprs -npsl "
			   "-saf -sai -saw -nsc -nsob -nss -ppi2 "
			   "-pmt ")
	   (buffer-name))
	  (indent-region (point-min) (point-max))
	  (save-buffer-if-modified)
	  (goto-char current))))

(defun tst-mark ()
  "."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max) nil t))

(defun switch-window-orientation ()
  "Switch the orientation of two windows between horizontal and vertical split."
  (interactive)
  (if (> (length (window-list)) 2)
	  (error "Can't toggle with more than 2 windows!")
	(let ((func (if (window-full-height-p)
					#'split-window-vertically
				  #'split-window-horizontally)))
	  (delete-other-windows)
	  (funcall func)
	  (save-selected-window
		(other-window 1)
		;;(delete-window)
		(switch-to-buffer (other-buffer))))))

(defun split-current-window (size direction)
  "Split the current window with SIZE in DIRECTION."
  (split-window (frame-root-window)
				(and size (prefix-numeric-value size))
				direction))

(defun split-current-window-below (&optional size)
  "Splits the current window below with SIZE."
  (interactive "P")
  (split-current-window size 'below))

(defun split-current-window-right (&optional size)
  "Splits the current window right with SIZE."
  (interactive "P")
  (split-current-window size 'right))

(defun split-current-window-above (&optional size)
  "Splits the current window above with SIZE."
  (interactive "P")
  (split-current-window size 'above))

(defun split-current-window-left (&optional size)
  "Splits the current window left with SIZE."
  (interactive "P")
  (split-current-window size 'left))

(defun named-term (&optional name)
  "Open a terminal with NAME in the current window."
  (interactive "sName: ")
  (ansi-term SHELL-ZSH name))

(defun named-term-below ()
  "Open a terminal in a new window on the bottom."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (named-term))

(defun find-overlays-specifying (prop pos)
  "Find the proposition PROP at position POS."
  (let ((overlays (overlays-at pos))
		found)
	(while overlays
	  (let ((overlay (car overlays)))
		(if (overlay-get overlay prop)
			(setq found (cons overlay found))))
	  (setq overlays (cdr overlays)))
	found))

(defun de/highlight-line (&optional face-background)
  "Set the backgroud of the current line into FACE-BACKGROUND."
  (interactive)
  ;; remove the highlight if line was alrady highlighted
  (if (find-overlays-specifying
	   'line-highlight-overlay-marker
	   (line-beginning-position))
	  (remove-overlays
	   (line-beginning-position)
	   (+ 1 (line-end-position)))
	;; or highlight it instead
	(let ((face-bg (or face-background '(:background "#2f4f4f")))
		  (overlay-highlight
		   (make-overlay
			(line-beginning-position)
			(+ 1 (line-end-position)))))
	  (overlay-put overlay-highlight 'face face-bg)
	  (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))

(defun remove-highlight ()
  "Remove all highlight."
  (interactive)
  (remove-overlays (point-min)) (point-max))

(defun select-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
	  (0 (put-text-property
		  (match-beginning 0)
		  (match-end 0)
		  'face
		  (list :background
				(let* ((ms (match-string-no-properties 0))
					   (r (substring ms 1 2))
					   (g (substring ms 2 3))
					   (b (substring ms 3 4)))
				  (concat "#" r r g g b b))))))
	 ("#[[:xdigit:]]\\{6\\}"
	  (0 (put-text-property
		  (match-beginning 0)
		  (match-end 0)
		  'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

(defun emacs-quit-confirm ()
  "Quit Emacs after confirmation."
  (interactive)
  (if (y-or-n-p-with-timeout
	   "Do you really want to exit Emacs ? " 10 nil)
	  (progn
		(if window-system
			(progn
			  (if (fboundp 'uptime) (uptime))
			  (sleep-for 1)))
		(save-buffers-kill-emacs)))
  (message "emacs quit aborted"))

(defun swap-buffer ()
  "Swap the current buffer with the previous buffer in the list."
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
		((let* ((buffer-a (current-buffer))
				(window-b (cadr (window-list)))
				(buffer-b (window-buffer window-b)))
		   (set-window-buffer window-b buffer-a)
		   (switch-to-buffer buffer-b)
		   (other-window 1)))))

(defun generate-tab-stops (&optional max width)
  "Return a sequence of MAX elements suitable for `tab-stop-list' of tabs size WIDTH."
  (let* ((max-column (or max 200))
		 (tab-width (or width tab-width))
		 (count (/ max-column tab-width)))
	(number-sequence tab-width (* tab-width count) tab-width)))

;;; defuns.el ends here
