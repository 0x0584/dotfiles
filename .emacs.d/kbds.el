;;; keybindings.el ---- Summary: Some Useful Emacs Keybindings
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
;;   Some Useful Emacs Keybindings
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
(global-set-key (kbd "C-;") 'helm-etags-select)
(global-set-key (kbd "C-c /") 'tags-query-replace)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-f") 'fzf)

(global-set-key (kbd "C-c -") 'compile)
(global-set-key (kbd "C-c =") 'recompile)
(global-set-key (kbd "C-c z") 'occur)

(global-set-key (kbd "C-.") 'quickrun)
(global-set-key (kbd "C-,") 'helm-git-grep-from-here)

(global-set-key (kbd "C-c y") 'popup-kill-ring)

(global-set-key (kbd "C-c b") 'recover-this-file)
(global-set-key (kbd "C-c x") 'man)

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

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "C-c f") 'indent-c-buffer)

(global-set-key (kbd "C-c w <down>") 'wind-below)
(global-set-key (kbd "C-c w <up>") 'wind-above)
(global-set-key (kbd "C-c w <left>") 'wind-left)
(global-set-key (kbd "C-c w <right>") 'wind-right)


;;
(global-set-key (kbd "S-M-<down>") 'windmove-down)

(global-set-key (kbd "S-M-<left>") 'windmove-left)
(global-set-key (kbd "S-M-<right>") 'windmove-right)
(global-set-key (kbd "S-M-<up>") 'windmove-up)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "S-C-l") 'linum-mode)
(global-set-key (kbd "C-:") 'linum-relative-mode)

(global-set-key (kbd "C-c C-d") 'named-term)

(global-set-key (kbd "<f4>") 'bm-remove-all-current-buffer)
(global-set-key (kbd "<f5>") 'bm-toggle)
(global-set-key (kbd "<f6>") 'bm-previous)
(global-set-key (kbd "<f7>") 'bm-next)

(global-set-key (kbd "<f8>") 'de/highlight-line)

(global-set-key (kbd "C-h z") 'zeal-at-point)

(add-hook 'c-mode-common-hook
	  (lambda()
	    (local-set-key (kbd "C-c <down>") 'hs-show-block)
	    (local-set-key (kbd "C-c <up>")  'hs-hide-block)
	    (local-set-key (kbd "C-c a <up>")	 'hs-hide-all)
	    (local-set-key (kbd "C-c a <down>")	 'hs-show-all)
	    (hs-minor-mode t)))

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
(global-set-key (kbd "C-c s") 'swap-buffer)

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

;;; kbds.el ends here
