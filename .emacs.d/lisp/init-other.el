;;; init-other.el ---- Summary: Other configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Other mode configurations
;;
;;; Summary:
;;
;;    Showing less minor mods, activating smex and god mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'rich-minority)
(rich-minority-mode 1)
(setf rm-blacklist "")

(require 'smex)			    ; Not needed if you use package.el
(smex-initialize) ; when Smex is auto-initialized on its first run.

(require 'fzf)
(require 'bm)

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defun god-toggle-on-overwrite ()
  "Toggle `god-mode` on `overwrite-mode`."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)

(require 'god-mode-isearch)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)

(require 'recentf)
(recentf-mode 1)
(savehist-mode 1)

(require 'magit-todos)
(magit-todos-mode)

(provide 'init-other)
;;; init-other.el ends here
