;;; kbds.el ---- Summary: Some Useful Emacs Keybindings
;;
;; Filename: keybindings.el
;; Description: Some Useful Emacs Keybindings
;; Author: Anas (0x0584)
;; Created: Nov 20, 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Some Useful Emacs Keybindings
;;
;;; Summary:
;;
;;	 Describe loaded files.
;;	 Describe loaded modes.
;;	 Describe load time.
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

;; Global key sets

(require 'dired)

(define-key dired-mode-map (kbd ",") 'create-etags)

(global-set-key (kbd "<f12>") 'info)
;; (global-set-key (kbd "C-;") 'helm-etags-select)
(global-set-key (kbd "C-c /") 'tags-query-replace)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c C-f") 'fzf)

(global-set-key (kbd "C-c -") 'compile)
(global-set-key (kbd "C-c =") 'recompile)
(global-set-key (kbd "C-c z") 'occur)

(global-set-key (kbd "C-.") 'quickrun)
(global-set-key (kbd "C-,") 'helm-git-grep-from-here)

(global-set-key (kbd "C-c y") 'popup-kill-ring)

(global-set-key (kbd "C-c m") 'man)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(global-set-key (kbd "C-c ;") 'make-directory)

(global-set-key (kbd "C-x !") 'restart-emacs)

(global-set-key (kbd "C-c h") 'highlight-blocks-mode)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-m") 'smex-major-mode-commands)

(global-set-key (kbd "C-x M-d") 'browse-file-directory)
(global-set-key (kbd "C-c k") 'kill-other-buffers)

(global-set-key (kbd "C-x b") 'helm-mini);
(global-set-key (kbd "C-x f") 'helm-find-files)

(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)

(global-set-key (kbd "C-c w <down>") 'wind-below)
(global-set-key (kbd "C-c w <up>") 'wind-above)
(global-set-key (kbd "C-c w <left>") 'wind-left)
(global-set-key (kbd "C-c w <right>") 'wind-right)
(global-set-key (kbd "C-c w o") 'switch-window-orientation)
(global-set-key (kbd "C-c w s") 'swap-buffer)

(global-set-key (kbd "S-M-<down>") 'windmove-down)
(global-set-key (kbd "S-M-<left>") 'windmove-left)
(global-set-key (kbd "S-M-<right>") 'windmove-right)
(global-set-key (kbd "S-M-<up>") 'windmove-up)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-x <left>") 'previous-code-buffer)
(global-set-key (kbd "C-x <right>") 'next-code-buffer)

(global-set-key (kbd "C-c l l") 'linum-mode)
(global-set-key (kbd "C-c l r") 'linum-relative-mode)

(global-set-key (kbd "C-c 0") 'calculator)
(global-set-key (kbd "C-c $") 'calendar)

(global-set-key (kbd "C-c r") 'popup-ruler)

(global-set-key (kbd "C-c t t") 'named-term)
(global-set-key (kbd "C-c t b") 'named-term-below)

(global-set-key (kbd "C-c b R") 'bm-remove-all-current-buffer)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b p") 'bm-previous)
(global-set-key (kbd "C-c b k") 'bm-next)
(global-set-key (kbd "C-c b h") 'de/highlight-line)
(global-set-key (kbd "C-c b r") 'recover-this-file)
(global-set-key (kbd "C-c b v") 'safe-revert-buffer)


(global-set-key (kbd "C-h z") 'zeal-at-point)

(add-hook 'c-mode-common-hook
		  (lambda()
			(local-set-key (kbd "C-c <down>") 'hs-show-block)
			(local-set-key (kbd "C-c <up>")	 'hs-hide-block)
			(local-set-key (kbd "C-c a <up>")	 'hs-hide-all)
			(local-set-key (kbd "C-c a <down>")	 'hs-show-all)
			(hs-minor-mode t)
			(local-set-key (kbd "C-c f") 'indent-c-buffer)))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook
			(lambda ()
			  (local-set-key (kbd "C-h d")
							 (lambda ()
							   (interactive)
							   (manual-entry (current-word)))))))

(global-set-key (kbd "M-[") 'select-line)
(global-set-key (kbd "C-c h") 'highlight-blocks-mode)
(global-set-key (kbd "C-x C-c") 'emacs-quit-confirm)

(global-set-key (kbd "C-c g") 'auto-complete)

(global-set-key (kbd "C-x \\") 'wordnut-lookup-current-word)
(global-set-key (kbd "C-x <f12>") 'helm-tldr)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-h !") 'devdocs-search)

(global-set-key (kbd "C-c [") 'sr-speedbar-close)
(global-set-key (kbd "C-c ]") 'sr-speedbar-open)
(global-set-key (kbd "C-c \'") 'sr-speedbar-select-window)

(global-set-key (kbd "C-x %") 'find-grep)
(global-set-key (kbd "C-x &") 'find-grep-dired)
(global-set-key (kbd "C-x ^") 'find-name-dired)
(global-set-key (kbd "C-x 4 k") 'kill-this-buffer)
(global-set-key (kbd "C-{") 'backward-page)
(global-set-key (kbd "C-}") 'forward-page)

(global-set-key (kbd "C-c \\") 'hide/show-comments-toggle)

(global-set-key (kbd "M-C-I") 'ac-fuzzy-complete)

(global-set-key (kbd "C-c o") 'switch-to-minibuffer)

(global-set-key (kbd "C-c d c") 'desktop+-create)
(global-set-key (kbd "C-c d l") 'desktop+-load)
(global-set-key (kbd "C-c d d") 'disaster)
(global-set-key (kbd "C-x d") 'delete-region)
(global-set-key (kbd "C-c i") 'change-indent-tabs)
(global-set-key (kbd "C-c w w") 'whitespace-mode)
(global-set-key (kbd "C-c w w") 'whitespace-mode)

(global-set-key (kbd "C-c q q") 'quickrun)
(global-set-key (kbd "C-c q c") 'quickrun-compile-only)

(defun goto-scratch-buffer ()
  "Just go to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "C-c *") 'goto-scratch-buffer)

;;; kbds.el ends here
