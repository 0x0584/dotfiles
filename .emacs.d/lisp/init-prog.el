;;; init-prog.el ---- Summary: Programming configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Auto Complete mode configurations
;;
;;; Summary:
;;
;;    Set c-eldoc and programming hooks, some cperl configs and Tex.
;;    Also enabling coloring delimiters.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(load "c-eldoc")

(setq c-eldoc-includes "-I./include -I../include -I./ -I../ ")
(setq c-eldoc-buffer-regenerate-time 60)

;; (set-face-attribute 'linum nil :height 100)

;; Hooks
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'php-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)

(defalias 'perl-mode 'cperl-mode)

(setq cperl-hairy nil) ;; Turns off most of the CPerlMode options

(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car (let ((cperl-message-on-help-error nil))
	 (cperl-get-help))))

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (set (make-local-variable 'eldoc-documentation-function)
		 'my-cperl-eldoc-documentation-function)))

;; (add-hook 'cperl-mode-hook 'auto-insert)

(setq c-default-style "cc-mode")
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-flycheck-mode)

(provide 'init-prog)
;;; init-prog.el ends here
