;;; custom.el ---- Summary: Emacs Configuration
;;
;; Filename: init.el
;; Description: Emacs Configuration
;; Author: Anas (0x0584)
;; Created: Nov 20, 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Emacs Configuration
;;
;;; Summary:
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(cperl-autoindent-on-semi t)
 '(cperl-brace-offset 0)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-brace-offset 0)
 '(cperl-continued-statement-offset 0)
 '(cperl-electric-backspace-untabify t)
 '(cperl-electric-parens-mark nil)
 '(cperl-highlight-variables-indiscriminately nil)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(custom-enabled-themes (quote (clues)))
 '(custom-safe-themes
   (quote
    ("f11e219c9d043cbd5f4b2e01713c2c24a948a98bed48828dc670bd64ae771aa1" default)))
 '(flycheck-clang-include-path
   (quote
    ("../../include" "../include" "../util" "../utils" "../lib" "../libs")))
 '(flycheck-clang-includes nil)
 '(line-number-mode nil)
 '(linum-format (quote dynamic))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal :size 15))))
 '(bm-face ((t (:background "orange" :foreground "Black"))))
 '(bm-fringe-face ((t (:background "gold" :foreground "Black"))))
 '(bold ((t (:slant normal :weight bold))))
 '(cperl-array-face ((t (:foreground "goldenrod1" :weight bold))))
 '(cperl-hash-face ((t (:foreground "sea green" :slant italic :weight normal))))
 '(diff-refine-removed ((t (:inherit diff-refine-change :background "#aa2222"))))
 '(font-lock-function-name-face ((t (:foreground "#BFC3A9"))))
 '(font-lock-variable-name-face ((t (:foreground "#BDBA9F" :slant italic))))
 '(git-commit-summary ((t (:inherit font-lock-type-face :weight bold))))
 '(helm-ff-directory ((t (:weight bold))))
 '(helm-ff-dotted-directory ((t (:background "DimGray" :foreground "white smoke"))))
 '(helm-ff-dotted-symlink-directory ((t (:background "white smoke" :foreground "gray20"))))
 '(helm-selection ((t (:distant-foreground "black" :box (:line-width 2 :color "grey75" :style released-button) :weight bold))))
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :height 1.3))))
 '(magit-section-highlight ((t (:background "grey20" :weight bold))))
 '(mode-line ((t (:inherit variable-pitch :background "#111111" :foreground "#777777" :box nil :family "DejaVu Sans Mono"))))
 '(org-block ((t nil)))
 '(org-checkbox ((t (:inherit bold))))
 '(org-document-title ((t (:foreground "pale turquoise" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :slant italic))))
 '(org-tag ((t (:box (:line-width 2 :color "grey75" :style pressed-button) :slant normal :weight bold))))
 '(term-color-blue ((t (:background "dark slate blue" :foreground "dark slate blue")))))

;;; custom.el ends here
