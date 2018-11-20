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

;; Global key sets
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

;;; keybindings.el ends here
