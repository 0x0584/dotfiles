;;; init.el ---- Summary: Emacs Configuration
;;
;; Filename: init.el
;; Description: Emacs Configuration
;; Author: Anas (0x0584)
;; Created: <2018-11-20 Mon 13:07:00>
;; Updated: <2018-12-30 Sun 13:44:06>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	  Emacs Configuration
;;
;;; Summary:
;;
;;	  Load modes.el -- Modes configurations
;;	  Load defuns.el -- Useful Emacs Lisp functions
;;	  Load configs.el -- Some addtional Emacs configurations
;;	  Load keybindings.el -- My Emacs keybindings
;;	  Load custom.el -- customized variables and faces
;;	  Load beta.el -- Anything that I would test
;;
;; This is configuration is done after working with a messy
;; Emacs configuration for three years.	 Now after dealing with
;; Emacs Lisp for a while, I think I can handle this, as they said:
;;
;;	 - You would need to configure your Emacs all over someday.
;;
;;; Variables defined here:
;;
;;; Functions defined here:
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

;; Load packages

;; Added by Package.el.	 This must come before configurations of
;; installed packages.	Don't delete this line.	 If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file "~/.emacs.d/pkgs.el")

;; Load Emacs modes configurations
(load-file "~/.emacs.d/modes.el")

;; Load custom functions
(load-file "~/.emacs.d/defuns.el")

;; Load some additional configurations
(load-file "~/.emacs.d/configs.el")

;; Load personal keybindings
(load-file "~/.emacs.d/kbds.el")

;; Load custom variables and faces
(load-file "~/.emacs.d/custom.el")

;; Load beta configurations
(load-file "~/.emacs.d/beta.el")

(require 'benchmark)

(let ((lisp-dir "~/.emacs.d/lisp"))
  (add-to-list 'load-path lisp-dir)
  (mapc (lambda (fname)
		  (let ((feat (intern (file-name-base fname))))
			(message "Feature '%s' loaded in %.2fs" feat
					 (benchmark-elapse (require feat fname)))
			(require feat fname)))
		(directory-files lisp-dir t "\\.el")))

(message "end of the road")

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
