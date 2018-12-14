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
;;	 Emacs Configuration
;;
;;; Summary:
;;
;;	 Describe loaded files.
;;	 Describe loaded modes.
;;	 Describe load time.
;;
;; This is configuration is done after working with a messy
;; Emacs configuration for three years.	 Now after dealing with
;; Emacs Lisp for a while, I think I can handle this, as they said:
;;
;;	 - You would need to configure your Emacs all over someday.
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
 '(ac-delay 0.1)
 '(ac-etags-requires 1)
 '(ansi-color-names-vector
   ["#c0c0c0" "#336c6c" "#806080" "#0f2050" "#732f2c" "#23733c" "#6c1f1c" "#232333"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(blink-cursor-mode t)
 '(blink-matching-paren (quote jump))
 '(c-basic-offset (quote set-from-style))
 '(chess-stockfish-path /usr/games/stockfish)
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
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
 '(debug-on-error nil)
 '(fci-rule-color "#c7c7c7")
 '(find-grep-options "-q --color=auto")
 '(find-ls-option (quote ("-ls" . "-dilsbF")))
 '(find-ls-subdir-switches "-albF")
 '(flycheck-clang-include-path
   (quote
	("../../include" "../include" "../util" "../utils" "../lib" "../libs")))
 '(flycheck-clang-includes nil)
 '(idle-update-delay 0.5)
 '(indicate-buffer-boundaries (quote left))
 '(line-number-mode nil)
 '(linum-format (quote dynamic))
 '(magit-diff-refine-hunk t)
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
	("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
	(org-pomodoro pomidor flycheck-grammalecte helm-c-yasnippet java-snippets yasnippet-snippets xbm-life exwm diff-hl chess zone-nyan zone-rainbow zone-select zweilight-theme zenburn-theme zeal-at-point xkcd wordnut which-key websocket web-mode web-beautify volatile-highlights vline vkill visual-regexp undo-tree tuareg tldr telephone-line tao-theme tangotango-theme tango-plus-theme tango-2-theme symon sx swiper swap-buffers svg-clock super-save ssh sr-speedbar spaces spacemacs-theme sotlisp smex smartparens showkey show-css rvm runtests ruby-tools ruby-refactor ruby-hash-syntax ruby-factory ruby-electric ruby-dev ruby-compilation ruby-block rubocop rsense rotate robe restart-emacs rainbow-delimiters rainbow-blocks quickrun quack popwin popup-switcher popup-kill-ring popup-complete plsql plsense plantuml-mode php-eldoc php+-mode persp-mode perl6-mode pacmacs ox-pandoc orgtbl-ascii-plot org-webpage org-web-tools org-ref org-readme org-preview-html org-pandoc org-page org-gnome-calendar org-gnome org-download org-doing org-commentary org-beautify-theme org-alert omniref nimbus-theme nhexl-mode nasm-mode names mysql-to-org minor-mode-hack memory-usage markdown-mode+ map-regexp magithub magit-todos magit-rockstar magit-org-todos magit-find-file magit-filenotify magit-annex magic-latex-buffer linum-relative latexdiff latex-preview-pane langtool lang-refactor-perl julia-shell julia-repl inkpot-theme impatient-mode iedit hl-sexp highlight-quoted highlight-operators highlight-escape-sequences highlight-blocks highlight hide-comnt hexrgb helm-themes helm-spotify-plus helm-rubygems-org helm-rubygems-local helm-robe helm-perldoc helm-make helm-gtags helm-gitignore helm-git-grep helm-git-files helm-git helm-flymake helm-dictionary helm-dash helm-css-scss helm-company grandshell-theme gotham-theme god-mode gnuplot gnu-apl-mode gited git-timemachine geben fzf fuzzy function-args font-lock+ flymake-ruby flymake-less flymake-json flymake-jslint flymake-jshint flymake-css flylisp flycheck-perl6 flycheck-julia flycheck-cstyle flycheck-css-colorguard flycheck-clangcheck emojify-logos electric-operator eldoc-overlay eldoc-eval edit-server djvu disaster diminish diffview devdocs desktop-menu desktop+ define-word css-eldoc csharp-mode company-inf-ruby clues-theme checkbox c-eldoc boxquote bm bind-key benchmark-init awk-it auto-complete-clang auto-complete-c-headers auto-complete-auctex auctex ascii-art-to-unicode anything anti-zenburn-theme ample-theme ace-popup-menu ac-slime ac-php ac-ispell ac-inf-ruby ac-html-csswatcher ac-html-bootstrap ac-html ac-helm ac-etags ac-clang ac-c-headers)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(quack-programs
   (quote
	("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((nameless-current-name . "rm"))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
   (quote
	((20 . "#437c7c")
	 (40 . "#336c6c")
	 (60 . "#205070")
	 (80 . "#2f4070")
	 (100 . "#1f3060")
	 (120 . "#0f2050")
	 (140 . "#a080a0")
	 (160 . "#806080")
	 (180 . "#704d70")
	 (200 . "#603a60")
	 (220 . "#502750")
	 (240 . "#401440")
	 (260 . "#6c1f1c")
	 (280 . "#935f5c")
	 (300 . "#834744")
	 (320 . "#732f2c")
	 (340 . "#6b400c")
	 (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c")
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
 '(helm-visible-mark ((t (:background "DarkOrange1" :foreground "black" :weight bold))))
 '(magit-section-highlight ((t (:background "grey20" :weight bold))))
 '(mode-line ((t (:inherit variable-pitch :background "#111111" :foreground "#777777" :box nil :family "DejaVu Sans Mono"))))
 '(org-block ((t nil)))
 '(org-checkbox ((t (:inherit bold))))
 '(org-document-title ((t (:foreground "pale turquoise" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :slant italic))))
 '(org-tag ((t (:box (:line-width 2 :color "grey75" :style pressed-button) :slant normal :weight bold))))
 '(speedbar-selected-face ((t (:foreground "orange" :slant italic))))
 '(term-color-blue ((t (:background "dark slate blue" :foreground "dark slate blue")))))

;;; custom.el ends here
