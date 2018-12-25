;;; init-auto.el ---- Summary: Auto configurations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Auto configurations
;;
;;; Summary:
;;
;;	  Loads Auto Complete globally, also set auto completion after
;;	  typing 1 character, also setting auto insert templates
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'auto-complete)
(require 'autoinsert)

(setq ac-auto-start 3)
(setq ac-delay 0.5)
(setq ac-dwim t)
(setq ac-use-menu-map t)

(define-key ac-completing-map "\C-m" nil)
(define-key ac-menu-map "\C-m" 'ac-complete)

(auto-complete-mode 1)
(auto-complete)

(ac-config-default)

(custom-set-variables
 '(ac-etags-requires 1))

(eval-after-load "etags"
  '(progn
	 (ac-etags-setup)))

(add-hook 'c-mode-common-hook 'ac-etags-ac-setup)
(add-hook 'ruby-mode-common-hook 'ac-etags-ac-setup)

(defvar auto-insert-copyright "
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.	If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth
Floor, Boston, MA 02110-1301, USA."
  "GPL v2 Copyright.")

(setq auto-insert-alist
	  ;; Ruby mode
	  '(((ruby-mode . "Ruby program") nil
		 "#!/usr/bin/env ruby\n\n"
		 "# File: " (file-name-nondirectory buffer-file-name) "\n"
		 "# Updated: <>\n"
		 "# Copyright (C) " (substring (current-time-string) -4) " "
		 (shell-command-to-string (concat "echo -e '"
										  auto-insert-copyright
										  "' | perl -pe 's/^/ # /'"))
		 "\n" "# Description: " _ "\n\n")

		;; Emacs mode
		((emacs-lisp-mode . "Emacs lisp mode") nil
		 ";;; "
		 (file-name-nondirectory buffer-file-name)
		 " ---- Summary:
;;
;; Filename: "
		 (file-name-nondirectory buffer-file-name)
		 "
;; Description:
;; Author: "
		 (user-full-name)
		 "
;; Created: "
		 (format-time-string "<%Y-%02m-%02d %3a %02H:%02M:%02S>")
		 "
;; Updated: <>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Summary:
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


;;; "
		 (file-name-nondirectory buffer-file-name) " ends here")

		;; C mode
		((c-mode . "C program") nil
		 "/*\n"
		 " * File: " (file-name-nondirectory buffer-file-name) "\n"
		 " * Author: " (user-full-name) "\n"
		 " * Description: \n"
		 " * Created: " (format-time-string "<%Y-%02m-%02d %3a %02H:%02M:%02S>") "\n"
		 " * Updated: <> \n *\n *\n"
		 " * Copyright (C) " (substring (current-time-string) -4) "\n"
		 (shell-command-to-string (concat "echo -e '"
										  auto-insert-copyright
										  "' | perl -pe 's/^/ * /'"))
		 " */")

		;; Shell Mode
		((shell-mode . "Shell script") nil
		 "#!/bin/bash\n\n"
		 " # File: " (file-name-nondirectory buffer-file-name) "\n"
		 " # Updated: <>\n"
		 " # Copyright (C) "
		 (substring (current-time-string) -4) " "
		 (shell-command-to-string (concat "echo -e '"
										  auto-insert-copyright
										  "' | perl -pe 's/^/ # /'"))

		  "\n"
		  " # Description: " _ "\n\n")

		;; Perl Mode
		((cperl-mode . "Perl") nil
		 "#!/usr/bin/perl"
		 "\n#======================================================="
		 "========================"
		 "\n#"
		 "\n#		  FILE: " (file-name-nondirectory buffer-file-name)
		 "\n#"
		 "\n#		 USAGE: ./" (file-name-nondirectory buffer-file-name)
		 "\n#"
		 "\n#  DESCRIPTION: ---"
		 "\n#"
		 "\n#	   OPTIONS: ---"
		 "\n# REQUIREMENTS: ---"
		 "\n#		  BUGS: ---"
		 "\n#		 NOTES: ---"
		 "\n#		AUTHOR: Anas Rchid (0x0584) <rchid.anas@gmail.com>"
		 "\n# ORGANIZATION: ---"
		 "\n#	   VERSION: 1.0"
		 "\n#	   CREATED: " (insert-date)
		 "\n#	  REVISION: ---"
		 "\n#========================================================"
		 "=======================")))

(provide 'init-auto)
;;; init-auto.el ends here
