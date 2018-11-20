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
;;; Summary:
;;
;;   Describe loaded files.
;;   Describe loaded modes.
;;   Describe load time.
;;
;;; User options defined here:
;;
;;; Commands defined here:
;;
;;; Interactive functions defined here:
;;
;;; Non-interactive functions defined here:
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
    (beginning-of-line) (setq start (point))
    (end-of-line) (setq end (point))
    (forward-char)
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun indent-c-buffer ()
  "Call `indent' on the whole current buffer.
Ask to say the buffer if modified."
  (interactive)
  ;; setting the mark
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
	     "-saf -sai -saw -nsc -nsob -nss -ppi2 ")
     (buffer-name))
    (save-buffer)))

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
  (ansi-term "/bin/bash" name))

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

(defun org-turn-on-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (iimage-mode)
  (set-face-underline 'org-link nil))

;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline 'org-link)
      (set-face-underline 'org-link nil)
    (set-face-underline 'org-link t))
  (call-interactively 'iimage-mode))

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

(defun org-html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string
	     (concat
	      "xclip -o -t TARGETS    | "
	      "grep -q text/html     && "
	      "(xclip -o -t text/html | "
	      "pandoc -f html -t json | "
	      "pandoc -f json -t org) || "
	      "xclip -o")))
  (yank))

;;; defuns.el ends here
