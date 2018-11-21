;;; modes.el ---- Summary: Emacs mode configurations
;;
;; Filename: modes.el
;; Description: Emacs mode configurations
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

;; Winner mode to easily switch between windows configs

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
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
If Delete Needless Spaces mode is enable, before a buffer is saved to its file:
- delete initial blank lines;
- change spaces on tabs or vice versa depending on `indent-tabs-mode';
- delete the trailing whitespaces and empty last lines;
- delete also latest blank line if `require-final-newline' is nil;"
  :init-value t
  :lighter " dns")

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

(add-hook 'find-file-hook 'delete-nl-spaces-find-file-hook)
(add-hook 'before-save-hook 'delete-nl-spaces)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; modes.el ends here
