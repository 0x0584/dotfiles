;;; init-helm.el ---- Summary: Helm mode configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Helm mode configurations
;;
;;; Summary:
;;
;;	  Loads Helm mode globally, enabling helm resizing and fuzzy
;;	  matching
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'helm)
(require 'helm-config)
(require 'helm-c-yasnippet)

(setq helm-M-x-fuzzy-match t)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c TAB") 'helm-yas-complete)

(helm-mode 1)
(helm-autoresize-mode t)

(provide 'init-helm)
;;; init-helm.el ends here
