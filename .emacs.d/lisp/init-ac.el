;;; init-ac.el ---- Summary: Auto Complete mode configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Auto Complete mode configurations
;;
;;; Summary:
;;
;;    Loads Auto Complete globally, also set auto completion after
;;    typing 3 characters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(setq ac-auto-start 3)
(setq ac-dwim t)

(auto-complete-mode 1)
(auto-complete)

(ac-config-default)

(provide 'init-ac)
;;; init-ac.el ends here
