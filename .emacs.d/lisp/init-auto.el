;;; init-auto.el ---- Summary: Auto configurations
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

(setq auto-insert-alist
	  '(((ruby-mode . "Ruby program") nil
		 "#!/usr/bin/env ruby\n\n"
		 "# File: " (file-name-nondirectory buffer-file-name) "\n"
		 "# Updated: <>\n"
		 "# Copyright (C) " (substring (current-time-string) -4) " "
		 auto-insert-copyright "\n" "# Description: " _ "\n\n")
		((emacs-lisp-mode . "Emacs lisp mode") nil
		 ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
		 ";; Time-stamp: <>\n"
		 ";; Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n\n"
		 ";;; Commentary:\n\n"
		 ";;; Code:\n\n"
		 ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")

		((c-mode . "C program") nil
		 "/*		   File: \t" (file-name-nondirectory buffer-file-name) "\n"
		 " *		 Author: \tANAS RCHID\n"
		 " *	Description: \tNULL\n"
		 " * \n"
		 " *	  Created: " (format-time-string "<%Y-%02m-%02d %3a %02H:%02M:%02S>") "\n"
		 " *	  Updated: <>\n */\n\n")

		((shell-mode . "Shell script") nil
		 "#!/bin/bash\n\n"
		 " # File: " (file-name-nondirectory buffer-file-name) "\n"
		 " # Updated: <>\n"
		 " # Copyright (C) " (substring
							  (current-time-string) -4) " "
							  auto-insert-copyright "\n"
							  " # Description: " _ "\n\n")
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
