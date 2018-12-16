;;; pkgs.el ---- Summary: Maintaining installed must-have-installed packages
;;
;; Filename: pkgs.el
;; Description: Emacs Configuration
;; Author: Anas (0x0584)
;; Created: <2018-12-16 Mon 02:30:00>
;; Updated: <2018-12-16 Sun 02:56:06>
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
;;	   `ensure-package-installed'
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
 'ac-c-headers
 'ac-clang
 'ac-etags
 'ac-helm
 'ac-html
 'ac-html-bootstrap
 'ac-html-csswatcher
 'ac-inf-ruby
 'ac-ispell
 'ac-php
 'ac-php-core
 'ac-slime
 'ace-popup-menu
 'alert
 'ample-theme
 'anti-zenburn-theme
 'anything
 'apiwrap
 'ascii-art-to-unicode
 'async
 'auctex
 'auto-complete
 'auto-complete-auctex
 'auto-complete-c-headers
 'auto-complete-clang
 'avy
 'avy-menu
 'awk-it
 'basic-c-compile
 'benchmark-init
 'biblio
 'biblio-core
 'bind-key
 'bm
 'boxquote
 'c-eldoc
 'caml
 'checkbox
 'chess
 'clues-theme
 'company
 'company-inf-ruby
 'csharp-mode
 'css-eldoc
 'dash
 'dash-functional
 'deferred
 'define-word
 'desktop+
 'desktop-menu
 'devdocs
 'diff-hl
 'diffview
 'diminish
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
 'esxml
 'exwm
 'f
 'flycheck
 'flycheck-clangcheck
 'flycheck-css-colorguard
 'flycheck-cstyle
 'flycheck-grammalecte
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
 'font-lock+
 'function-args
 'fuzzy
 'fzf
 'geben
 'ghub
 'ghub+
 'git
 'git-commit
 'git-timemachine
 'gited
 'gitignore-mode
 'gnome-calendar
 'gntp
 'gnu-apl-mode
 'gnuplot
 'god-mode
 'gotham-theme
 'grandshell-theme
 'graphql
 'header2
 'helm
 'helm-bibtex
 'helm-c-yasnippet
 'helm-company
 'helm-core
 'helm-css-scss
 'helm-dash
 'helm-dictionary
 'helm-flymake
 'helm-git
 'helm-git-files
 'helm-git-grep
 'helm-gitignore
 'helm-gtags
 'helm-make
 'helm-perldoc
 'helm-robe
 'helm-rubygems-local
 'helm-rubygems-org
 'helm-spotify-plus
 'helm-themes
 'hexrgb
 'hide-comnt
 'highlight
 'highlight-blocks
 'highlight-escape-sequences
 'highlight-operators
 'highlight-quoted
 'hl-sexp
 'hl-todo
 'hlinum
 'ht
 'htmlize
 'http-post-simple
 'hydra
 'iedit
 'impatient-mode
 'inf-ruby
 'inflections
 'inkpot-theme
 'inline-docs
 'ivy
 'java-snippets
 'julia-mode
 'julia-repl
 'julia-shell
 'key-chord
 'lang-refactor-perl
 'langtool
 'latex-preview-pane
 'latexdiff
 'less-css-mode
 'let-alist
 'lib-requires
 'linum-relative
 'log4e
 'macrostep
 'magic-latex-buffer
 'magit
 'magit-annex
 'magit-filenotify
 'magit-find-file
 'magit-org-todos
 'magit-popup
 'magit-rockstar
 'magit-todos
 'magithub
 'map-regexp
 'markdown-mode
 'markdown-mode+
 'memory-usage
 'minor-mode-hack
 'multi
 'mustache
 'mysql-to-org
 'names
 'nasm-mode
 'nhexl-mode
 'nimbus-theme
 'omniref
 'org
 'org-alert
 'org-beautify-theme
 'org-commentary
 'org-doing
 'org-download
 'org-gnome
 'org-gnome-calendar
 'org-page
 'org-pandoc
 'org-pomodoro
 'org-preview-html
 'org-readme
 'org-ref
 'org-web-tools
 'org-webpage
 'orgtbl-ascii-plot
 'ox-pandoc
 'pacmacs
 'parsebib
 'pcre2el
 'pdf-tools
 'perl6-mode
 'persp-mode
 'php+-mode
 'php-eldoc
 'php-mode
 'pkg-info
 'plantuml-mode
 'plsense
 'plsql
 'pomidor
 'popup
 'popup-complete
 'popup-kill-ring
 'popup-switcher
 'popwin
 'pos-tip
 'projectile
 'quack
 'quick-peek
 'quickrun
 'rainbow-blocks
 'rainbow-delimiters
 'request
 'restart-emacs
 'rich-minority
 'robe
 'rotate
 'rsense
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
 's
 'seq
 'show-css
 'showkey
 'simple-httpd
 'slime
 'smart-mode-line
 'smartparens
 'smex
 'sotlisp
 'spacemacs-theme
 'spaces
 'sr-speedbar
 'ssh
 'super-save
 'svg
 'svg-clock
 'swap-buffers
 'swiper
 'sx
 'symon
 'tablist
 'tango-2-theme
 'tango-plus-theme
 'tangotango-theme
 'tao-theme
 'telepathy
 'telephone-line
 'tldr
 'treepy
 'tuareg
 'undo-tree
 'visual-regexp
 'vkill
 'vline
 'volatile-highlights
 'web-beautify
 'web-completion-data
 'web-mode
 'websocket
 'which-key
 'with-editor
 'wordnut
 'xbm-life
 'xcscope
 'xelb
 'xkcd
 'yaoddmuse
 'yasnippet
 'yasnippet-snippets
 'yaxception
 'zeal-at-point
 'zenburn-theme
 'zone-nyan
 'zone-rainbow
 'zone-select
 'zweilight-theme)

(provide 'pkgs)

;;; pkgs.el ends here
