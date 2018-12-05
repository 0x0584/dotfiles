;;; init-prog.el ---- Summary: Programming configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Programming modes configuration, including C Language and Perl.
;;
;;; Summary:
;;
;;	  Set c-eldoc and programming hooks, some cperl configs and Tex.
;;	  Also enabling coloring delimiters.
;;
;;; Functions defined here:
;;
;;	  `cperl-eldoc'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'rainbow-delimiters)
(require 'c-eldoc)
(require 'cperl-mode)
(require 'cc-mode)
(require 'diff-hl)

(diff-hl-mode 1)

(setq c-eldoc-includes "-I./include -I../include -I./ -I../ ")
(setq c-eldoc-buffer-regenerate-time 60)
(setq c-default-style "cc-mode")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

(setq cperl-hairy nil) ;; Turns off most of the CPerlMode options

;; (set-face-attribute 'linum nil :height 100)

(defalias 'perl-mode 'cperl-mode)

(defun cperl-eldoc ()
  "Return meaningful doc string for eldoc mode."
  (car (let ((cperl-message-on-help-error nil))
		 (cperl-get-help))))

(rainbow-delimiters-mode 1)
(save-place-mode 1)

;; Hooks
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'php-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'cperl-mode-hook
		  (lambda ()
			(set (make-local-variable 'eldoc-documentation-function)
				 'cperl-eldoc)))

;; (add-hook 'cperl-mode-hook 'auto-insert)

(defun generate-tab-stops (&optional max width)
  "Return a sequence of MAX elements suitable for `tab-stop-list' of tabs size WIDTH."
  (let* ((max-column (or max 200))
		 (tab-width (or width tab-width))
		 (count (/ max-column tab-width)))
	(number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-width 4)
(setq tab-stop-list (generate-tab-stops))
(setq indent-tabs-mode nil)

(provide 'init-prog)
;;; init-prog.el ends here
