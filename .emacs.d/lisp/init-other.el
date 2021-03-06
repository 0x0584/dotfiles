;;; init-other.el ---- Summary: Other configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Other mode configurations
;;
;;; Summary:
;;
;;	  Showing less minor mods, activating smex and god mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'rich-minority)
(require 'smex)				; Not needed if you use package.el
(require 'fzf)
(require 'bm)
(require 'god-mode)
(require 'recentf)
(require 'magit-todos)
(require 'god-mode-isearch)
(require 'desktop+)

(setf rm-blacklist "")

(defun god-toggle-on-overwrite ()
  "Toggle `god-mode` on `overwrite-mode`."
  (if (bound-and-true-p overwrite-mode)
	  (god-local-mode-pause)
	(god-local-mode-resume)))

(recentf-mode 1)
(savehist-mode 1)
(magit-todos-mode)
(rich-minority-mode 1)
(smex-initialize) ; when Smex is auto-initialized on its first run.

(require 'zone)
(zone-when-idle 900)

(zone-select-add-program 'zone-nyan-preview)
(zone-select-add-program 'zone-rainbow)

(defun svg-clock-when-zone ()
  "Use `svg-clock' with zone."
  (svg-clock)
  (previous-buffer)
  (backward-char))

(defun zone-svg-clock ()
  "Zone out with svg clock."
  (interactive)
  (let ((zone-programs [svg-clock-when-zone]))
	(zone)))

;; TODO: this needs to be fixed
;; (zone-select-add-program 'zone-svg-clock)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)

(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)
(add-hook 'text-mode 'toggle-truncate-lines)

;; (defun my/desktop-frame-title-function (desktop-name)
;;	 "Set the frame title to the current DESKTOP-NAME."
;;	 (list (concat "%b - Emacs [" desktop-name "]")))

;; (setq desktop+-frame-title-function
;;	  'my/desktop-frame-title-function)

;; (setq desktop+-special-buffer-handlers
;;	  '(term-mode
;;		compilation-mode
;;		org-agenda-mode
;;		indirect-buffer
;;		Man-mode
;;		shell-mode))

(provide 'init-other)
;;; init-other.el ends here
