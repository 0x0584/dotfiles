;;; init-org.el ---- Summary: Org mode configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Org mode configurations
;;
;;; Summary:
;;
;;	  Everything related to Org mode, like enabling images and also
;;	  setting notification to agenda event via org-alert
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'iimage)
(require 'org-alert)
(require 'faces)
(require 'org-clock)

(setq alert-default-style 'libnotify)
(setq org-alert-interval (* 60 60))	; each hour
(setq org-todo-keywords
	  '((sequence "TODO(t)" "IN-PROGRESS(s!)" "EXAM" "TO-BUY(b!)"
				  "MEETING(m!)" "|" "PENDING(p!)" "DONE(d!/@)"
				  "CANCELED(c!/!)" "UNDER-REVISING(u)" "|" "HOLY-DAY")))
(setq org-agenda-files (list "~/orged/agenda"))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-format-latex-options
	  (plist-put org-format-latex-options
				 :scale 1.3))
(setq org-return-follows-link t)
(setq org-src-fontify-natively t)
(setq image-file-name-extensions
	  (quote
	   ("png" "jpeg" "jpg" "gif" "tiff" "tif"
		"xbm" "xpm" "pbm" "pgm" "ppm" "pnm"
		"svg" "pdf" "bmp")))
(setq org-image-actual-width 400)
(defvar org-imagemagick-display-command
  "convert -density 600 \"%s\" -thumbnail \"%sx%s>\" \"%s\"")
(setq org-image-actual-width (/ (display-pixel-width) 3))
(setq org-agenda-custom-commands
	  '(("x" "Exams"
		 ;; agenda with only items tagged event
		 ((agenda "" ((org-agenda-ndays 45)
					  (org-agenda-tag-filter-preset '("+exam"))
					  (org-deadline-warning-days 0)))))))

(setq org-clock-persist 'history)
(setq org-agenda-clockreport-parameter-plist
	  '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))
(setq org-clock-idle-time 15)
(setq org-global-properties
	  '(("Effort_ALL" .
		 "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
;;	  1	   2	3	 4	  5	   6	7	 8	  9	   0
(setq org-columns-default-format
	  "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
(defvar org-latex-listings 'minted)
(defvar org-latex-pdf-process
  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun org-turn-on-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (iimage-mode)
  (set-face-underline 'org-link nil))

;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline 'org-link)
	  (set-face-underline 'org-link nil)
	(set-face-underline 'org-link t))
  (call-interactively 'iimage-mode))

(defun org-html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string
			 (concat
			  "xclip -o -t TARGETS	  | "
			  "grep -q text/html	 && "
			  "(xclip -o -t text/html | "
			  "pandoc -f html -t json | "
			  "pandoc -f json -t org) || "
			  "xclip -o")))
  (yank))

(defun plantuml-render-buffer ()
  "."
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/.emacs.d/plantuml/plantuml.jar "
						 buffer-file-name))
  (message (concat "PLANTUML Rendered:	" (buffer-name))))

;; Image reloading
(defun reload-image-at-point ()
  "."
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  "."
  (interactive)
  (message "resizing image at point in the current buffer123...")
  (let* ((image-spec (get-text-property (point) 'display))
		 (file (cadr (member :file image-spec))))
	(message (concat "resizing image..." file))
	(shell-command
	 (format "convert -resize %d %s %s "
			 (* (window-width (selected-window)) (frame-char-width))
			 file file))
	(reload-image-at-point)))

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.	 When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This
can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (unless refresh
	(org-remove-inline-images)
	(if (fboundp 'clear-image-cache) (clear-image-cache)))
  (save-excursion
	(save-restriction
	  (widen)
	  (setq beg (or beg (point-min)) end (or end (point-max)))
	  (goto-char beg)
	  (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
						(substring (image-file-name-regexp) 0 -2)
						"\\)\\]" (if include-linked "" "\\]")))
			old file ov img)
		(while (re-search-forward re end t)
		  (setq old (get-char-property-and-overlay (match-beginning 1)
												   'org-image-overlay)
				file (expand-file-name
					  (concat (or (match-string 3) "")
							  (match-string 4))))
		  (when (file-exists-p file)
			(let ((file-thumb (format "%s%s_thumb.png"
									  (file-name-directory file)
									  (file-name-base file))))
			  (if (file-exists-p file-thumb)
				  (let ((thumb-time (nth 5 (file-attributes file-thumb
															'string)))
						(file-time (nth 5 (file-attributes file 'string))))
					(if (time-less-p thumb-time file-time)
						(shell-command (format org-imagemagick-display-command
											   file org-image-actual-width
											   org-image-actual-width
											   file-thumb)
									   nil nil)))
				(shell-command (format org-imagemagick-display-command
									   file
									   org-image-actual-width
									   org-image-actual-width file-thumb)
							   nil nil))
			  (if (and (car-safe old) refresh)
				  (image-refresh (overlay-get (cdr old) 'display))
				(setq img (save-match-data (create-image file-thumb)))
				(when img
				  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
				  (overlay-put ov 'display img)
				  (overlay-put ov 'face 'default)
				  (overlay-put ov 'org-image-overlay t)
				  (overlay-put ov 'modification-hooks
							   (list 'org-display-inline-remove-overlay))
				  (push ov org-inline-image-overlays))))))))))

(iimage-mode)
(org-alert-enable)
(org-clock-persistence-insinuate)

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
		 (java . t)
		 (dot . t)
		 (ditaa . t)
		 (R . t)
		 (python . t)
		 (ruby . t)
		 (gnuplot . t)
		 (clojure . t)
		 (sh . t)
		 (ledger . t)
		 (org . t)
		 (plantuml . t)
		 (latex . t))))

(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-agenda-custom-commands
			 '("W" "Weekend" ((agenda "" ))
			   ((org-agenda-overriding-header "20 days")
				(org-agenda-span 20)))
			 t)

(add-to-list 'iimage-mode-image-regex-alist
			 (cons (concat "\\[\\[file:\\(~?"
						   iimage-mode-image-filename-regex
						   "\\)\\]")
				   1))

(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))

(provide 'init-org)
;;; init-org.el ends here
