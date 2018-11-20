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

(require 'package)
(require 'rich-minority)
(require 'smex) ; Not needed if you use package.el
(require 'rainbow-delimiters)
(require 'helm-config)
(require 'fzf)

;; Winner mode to easily switch between windows configs
(package-initialize)

(helm-mode 1)
(helm-autoresize-mode t)


;; Setting the initial frame size
(when (window-system)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-frame-height (selected-frame) 35)
  (set-frame-width (selected-frame) 82))

(auto-complete-mode 1)
(auto-complete)

(winner-mode 1)
(rainbow-delimiters-mode 1)

(hl-sexp-mode 1)
(global-hl-line-mode 1);; line selection
(global-undo-tree-mode)
(show-paren-mode 1)

;; Who needs a mouse inside Emacs?
(disable-mouse-mode 1)

(rich-minority-mode 1)
(setf rm-blacklist "")

;; Can be omitted. This might cause a (minimal) delay
(smex-initialize) ; when Smex is auto-initialized on its first run.

;; -- Display images in org mode
;; enable image mode first
(iimage-mode)
;; add the org file link format to the iimage mode regex
(add-to-list 'iimage-mode-image-regex-alist
	     (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))
;;  add a hook so we can display images on load
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))
;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))
;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))
(setq org-image-actual-width (/ (display-pixel-width) 3))

;; c-eldoc
(load "c-eldoc")

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(setq c-eldoc-includes "-I./include -I../include -I./ -I../ ")
(setq c-eldoc-buffer-regenerate-time 60)

;;; modes.el ends here
