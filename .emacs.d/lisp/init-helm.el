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

(require 'helm-config)

(helm-mode 1)
(helm-autoresize-mode t)

(setq helm-M-x-fuzzy-match t)

(provide 'init-helm)
;;; init-helm.el ends here
