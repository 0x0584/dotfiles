;;; modes.el ---- Summary: Emacs mode configurations
;;
;; Filename: modes.el
;; Description: Emacs mode configurations
;; Author: Anas (0x0584)
;; Created: Nov 20, 2018
;; Updated: <2019-01-02 Wed 22:35:12>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	  Some Useful Emacs mode configurations
;;
;;; Summary:
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

;; Setting the initial frame size
(when (window-system)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(if (not window-system)
	t
  (set-frame-height (selected-frame) 35)
  (set-frame-width (selected-frame) 82))

(when (eq system-type 'darwin)		; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char))

(which-key-mode 1)
(winner-mode 1)
(hl-sexp-mode 1)
(global-hl-line-mode 1);; line selection
(global-undo-tree-mode)
(show-paren-mode 1)

;; Who needs a mouse inside Emacs?

(define-minor-mode delete-nl-spaces-mode
  "Toggle deleting needless spaces (Delete Needless Spaces mode).
With a prefix argument ARG, enable Delete Needless Spaces mode if ARG is
positive, and disable it otherwise.	 If called from Lisp, enable
the mode if ARG is omitted or nil.
If Delete Needless Spaces mode is enable, before a buffer is saved to its file:
- delete initial blank lines;
- change spaces on tabs or vice versa depending on `indent-tabs-mode';
- delete the trailing whitespaces and empty last lines;
- delete also latest blank line if `require-final-newline' is nil;"
  :init-value t
  :lighter " dns")
(defun delete-nl-spaces ()
  "Execute `delete-nl-spaces'."
  (if (delete-nl-spaces-mode)
	  (save-excursion
	;; Delete initial blank lines
	(goto-char (point-min))
	(skip-chars-forward " \n\t")
	(skip-chars-backward " \t")
	(if (> (point) 0)
		(delete-char (- (- (point) 1))))

	;; Change spaces on tabs or tabs on spaces
	(if indent-tabs-mode
		(tabify (point-min) (point-max))
	  (untabify (point-min) (point-max)))

	;; Delete the trailing whitespaces and all blank lines
	(let ((delete-trailing-lines t))
	  (delete-trailing-whitespace))

	;; Delete the latest newline
	(unless require-final-newline
	  (goto-char (point-max))
	  (let ((trailnewlines (skip-chars-backward "\n\t")))
		(if (< trailnewlines 0)
		(delete-char (abs trailnewlines))))))))

(defun delete-nl-spaces-find-file-hook ()
  "Check whether to disable `delete-nl-spaces'."
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
	(let ((buffer (current-buffer))
	  (final-newline require-final-newline)
	  (tabs-mode indent-tabs-mode))
	  (with-temp-buffer
	(setq-local require-final-newline final-newline)
	(setq indent-tabs-mode tabs-mode)
	(insert-buffer-substring buffer)
	(delete-nl-spaces)
	(unless (= (compare-buffer-substrings buffer nil nil nil nil nil) 0)
	  (set-buffer buffer)
	  (delete-nl-spaces-mode -1)
	  (message "delete-nl-spaces-mode disabed for %s"
		   (buffer-name buffer)))))))

(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " 🐭"
  :keymap (make-sparse-keymap))

(dolist (type '(mouse down-mouse drag-mouse
			  double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
	;; Yes, I actually HAD to go up to 7 here.
	(dotimes (n 7)
	  (let ((k (format "%s%s-%s" prefix type n)))
	(define-key disable-mouse-mode-map
	  (vector (intern k)) #'ignore)))))

(disable-mouse-mode 1)

(add-hook 'after-init-hook #'global-emojify-mode)

(require 'telephone-line)
(telephone-line-mode)

(electric-pair-mode)

(require 'speedbar)
(setq speedbar-use-images nil)

(add-hook 'find-file-hook 'delete-nl-spaces-find-file-hook)
(add-hook 'before-save-hook 'delete-nl-spaces)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; modes.el ends here
