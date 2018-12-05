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

(require 'auto-complete)

(setq ac-auto-start 3)
(setq ac-dwim t)

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

(provide 'init-ac)
;;; init-ac.el ends here
