;;; configs.el ---- Summary: Some additional Emacs configurations
;;
;; Filename: modes.el
;; Description: Some additional Emacs configurations
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

(setenv "SHELL" (expand-file-name "/bin/bash"))

;; Personal Information
(setq user-full-name "Anas Rchid"
      user-mail-address "rchid.anas@gmail.com")

;; Set standard indent to 4 rather that 8
(setq standard-indent 4)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Using text-mode as the default mode for new buffers
(setq major-mode 'text-mode)

;; Set backup configuration
(setq backup-by-copying t		; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.autosaves"))		; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)		; use versioned backups

;; Create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Use lexical binding when evaluating Lisp
(setq lexical-binding t)

;; Better that a sound, right?
(setq visible-bell t)

;; Org-mode Configuration
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(s!)" "EXAM" "TO-BUY(b!)"
		  "MEETING(m!)" "|" "PENDING(p!)" "DONE(d!/@)"
		  "CANCELED(c!/!)" "UNDER-REVISING(u)" "|" "HOLY-DAY")))
(setq org-agenda-skip-scheduled-if-done t)

(setq helm-M-x-fuzzy-match t)

(ac-config-default)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; configs.el ends here
