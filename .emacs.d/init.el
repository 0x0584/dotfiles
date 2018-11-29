;;; init.el ---- Summary: Emacs Configuration
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
;;    Emacs Configuration
;;
;;; Summary:
;;
;;    Load modes.el -- Modes configurations
;;    Load defuns.el -- Useful Emacs Lisp functions
;;    Load configs.el -- Some addtional Emacs configurations
;;    Load keybindings.el -- My Emacs keybindings
;;    Load custom.el -- customized variables and faces
;;    Load beta.el -- Anything that I would test
;;
;; This is configuration is done after working with a messy
;; Emacs configuration for three years.	 Now after dealing with
;; Emacs Lisp for a while, I think I can handle this, as they said:
;;
;;   - You would need to configure your Emacs all over someday.
;;
;;; Variables defined here:
;;
;;     `*emacs-load-start*'
;;
;;; Functions defined here:
;;
;;     `display-loading-time', `time-to-ms', `ensure-package-installed'
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
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed PACKAGES or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-refresh-contents)
       (package-install package)))
   packages))

(ensure-package-installed
 'ac-etags
 'ac-helm
 'ac-inf-ruby
 'ac-ispell
 'ac-php
 'ac-php-core
 'ac-slime
 'ace-popup-menu
 'alert
 'anti-zenburn-theme
 'anything
 'async
 'auctex
 'auto-complete
 'auto-complete-auctex
 'auto-complete-c-headers
 'auto-complete-clang
 'awk-it
 'basic-c-compile
 'benchmark-init
 'bm
 'boxquote
 'c-eldoc
 'checkbox
 'chess
 'clues-theme
 'csharp-mode
 'css-eldoc
 'devdocs
 'disaster
 'djvu
 'doom
 'edit-server
 'eldoc-eval
 'eldoc-overlay
 'electric-operator
 'emojify
 'emojify-logos
 'epl
 'flycheck
 'flycheck
 'flycheck-clangcheck
 'flycheck-css-colorguard
 'flycheck-cstyle
 'flycheck-julia
 'flycheck-perl6
 'flylisp
 'flymake-css
 'flymake-easy
 'flymake-jshint
 'flymake-jslint
 'flymake-json
 'flymake-less
 'flymake-ruby
 'function-args
 'fuzzy
 'fzf
 'gitignore-mode
 'git-timemachine
 'gnu-apl-mode
 'gnuplot
 'god-mode
 'gotham-theme
 'graphql
 'helm
 'helm-bibtex
 'helm-company
 'helm-core
 'helm-flymake
 'helm-git
 'helm-git-files
 'helm-git-grep
 'helm-perldoc
 'helm-robe
 'helm-rubygems-local
 'helm-rubygems-org
 'helm-themes
 'hexrgb
 'highlight
 'highlight-blocks
 'highlight-escape-sequences
 'highlight-operators
 'highlight-quoted
 'hlinum
 'hl-sexp
 'hl-todo
 'impatient-mode
 'inf-ruby
 'inkpot-theme
 'lang-refactor-perl
 'langtool
 'latexdiff
 'latex-preview-pane
 'linum-relative
 'magit
 'magit-annex
 'magit-filenotify
 'magit-find-file
 'magithub
 'magithub
 'magit-org-todos
 'magit-popup
 'magit-rockstar
 'magit-todos
 'markdown-mode+
 'memory-usage
 'nasm-mode
 'nhexl-mode
 'org
 'org-alert
 'org-beautify-theme
 'org-commentary
 'org-doing
 'org-download
 'org-page
 'org-readme
 'org-ref
 'orgtbl-ascii-plot
 'org-webpage
 'org-web-tools
 'parsebib
 'pcre2el
 'pdf-tools
 'perl6-mode
 'persp-mode
 'php-eldoc
 'php+-mode
 'pkg-info
 'plantuml-mode
 'plsense
 'plsql
 'popup
 'popup-complete
 'popup-kill-ring
 'popup-switcher
 'popwin
 'pos-tip
 'quack
 'quickrun
 'rainbow-blocks
 'rainbow-delimiters
 'restart-emacs
 'rich-minority
 'rubocop
 'ruby-block
 'ruby-compilation
 'ruby-dev
 'ruby-electric
 'ruby-factory
 'ruby-hash-syntax
 'ruby-refactor
 'ruby-tools
 'runtests
 'rvm
 'show-css
 'showkey
 'simple-httpd
 'slime
 'smex
 'sotlisp
 'spaces
 'sr-speedbar
 'ssh
 'super-save
 'svg
 'svg-clock
 'tablist
 'telephone-line
 'tldr
 'tuareg
 'undo-tree
 'web-mode
 'websocket
 'which-key
 'wordnut
 'xkcd
 'yaoddmuse
 'zeal-at-point
 'zenburn-theme)


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

(provide 'init)
;;; init.el ends here
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
