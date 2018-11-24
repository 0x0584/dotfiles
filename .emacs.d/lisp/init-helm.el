;;; init-helm.el ---- Summary: Helm mode configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Helm mode configurations
;;
;;; Summary:
;;
;;    Loads Helm mode globally, enabling helm resizing and fuzzy
;;    matching
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'helm)
(require 'helm-config)

(setq helm-M-x-fuzzy-match t)

(helm-mode 1)
(helm-autoresize-mode t)

(provide 'init-helm)
;;; init-helm.el ends here
