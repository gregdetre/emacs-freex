;;; freex-mode.el --- minor mode

;; Copyright (C) 2007 Greg Detre, Per B. Sederberg

;; This file is part of Emacs Freex.  It is not part of GNU Emacs.

;; Freex is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation;
;; either version 2, or (at your option) any later version.

;; Freex is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public
;; License along with Freex; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Contributors:

;;; Code:




(require 'freex)
(require 'freex-embed)
(require 'freex-fontify)
(require 'freex-meta)
(require 'freex-timestamp)

(require 'font-lock)

(defun turn-on-freex-mode ()
  "Turn on FREEX-MODE."
  (interactive)
  (freex-mode 1))


(defun turn-off-freex-mode ()
  "Turn off FREEX-MODE."
  (interactive)
  (freex-mode -1))


(defun freexify ()
  (let ((filename (buffer-file-name)))
    ;; we only want to go into freex mode if there is a filename and
    (if (not filename)
	;; can't start freex unless there is a buffer file name
	(progn
	  (freex-mode -1)
	  (message "You can not enter freex mode if the buffer has no filename."))
      ;; is in the proper freex directory
      (if (not (equal (file-name-directory filename) freex-mode-dir))
	  ;; we have a filename, but it is in the wrong directory
	  ;; don't go into freex mode
	  (progn
	    (freex-mode -1)
	    (message "You can not enter freex mode when not in the proper directory."))
	;; we are either visiting a valid freex file or a scratch buffer
	(progn
	  ;; if the buffer is visiting a file (i.e. not scratch),
	  ;; and that file exists, then we can try and get its id
	  (if (file-exists-p filename)
	      (progn
		;; need to tell it to remove overlays before, and re-embed
		;; them after, reverting the buffer
		;;
		;; the final two arguments say to prepend it (rather than
		;; append - we don't really care) and to make this a
		;; buffer-local variable (this is important, so that our
		;; revert modifications only affect freex-mode
		(add-hook 'before-revert-hook
			  'freex-embed-before-revert nil t)
		(add-hook 'after-revert-hook
			  'freex-embed-after-revert nil t)
		(setq freex-embed-ov-props
		      (plist-put
		       freex-embed-ov-props 'id
		       (number-to-string
			(freex-sqlalchemy-get-nugid-from-filename
			 (file-name-nondirectory (buffer-file-name)))))))
	    ;; This is the case when we have a scratch/temporary buffer that
	    ;; contains embedded nuggets.  In such a case we would like to
	    ;; save the nuggets, but not save the scratch buffer holding
	    ;; them.  If you would like to save the scratch buffer to file,
	    ;; it is necessary to save as.
            ;;
            ;; Bleurgh. This breaks if you try and create a
            ;; new nugget by editing newnugget.freex, then
            ;; trying to save.
	    (progn
	      (setq freex-embed-ov-props
		    (plist-put freex-embed-ov-props 'id nil))
	      ;; set the save function to null so that the base file will
	      ;; not be saved to disk (and is hence a throwaway buffer)
              ;;
              ;; xxx
              ;; 	      (setq freex-embed-ov-props
              ;; 		    (plist-put freex-embed-ov-props 'save-funct 
              ;; 			       'freex-embed-overlay-save-null))
              ))
          
	  (dolist (hook freex-mode-hook)
	    (eval (list hook)))
	  ;; start using freex custom save
	  (add-hook 'write-contents-hooks 'freex-embed-save nil t)

	  ;; build the regex if this is the first time freex-mode has run
          (if (boundp 'freex-mode-has-run-already)
              nil ; do nothing
              (progn
                (setq freex-mode-has-run-already t)
                (freex-fontify-update-implicit-link-regexp)))
      
	  (if (eq major-mode 'muse-mode)
	      ;; add our fontification hook to the muse-colors-region-hook
	      ;; the last argument tells it to be buffer local
	      (add-hook 'muse-colors-buffer-hook 'freex-fontify-region nil t)
	    ;; do our own font-locking
	    (progn
	      (set (make-local-variable 'font-lock-defaults)
		   `(nil t nil nil beginning-of-line
			 (font-lock-fontify-region-function . freex-fontify-region)
			 (font-lock-unfontify-region-function
			  . freex-unhighlight-region)))
	      (set (make-local-variable 'font-lock-fontify-region-function)
		   'freex-fontify-region)
	      (set (make-local-variable 'font-lock-unfontify-region-function)
		   'freex-unhighlight-region)
	      (font-lock-mode 1)))
	  ;; now do all the embedding (and fontifying)
	  (freex-embed-buffer))))))


(defun defreexify ()
  ;; save all the overlays before closing them
  (freex-embed-remove-all-freex-embed-overlays t)
  ;; stop using freex custom save
  (remove-hook 'write-contents-hooks 'freex-embed-save t)
  ;; unfontify the buffer
  (when (not (eq major-mode 'muse-mode))
    (kill-local-variable 'font-lock-defaults)
    (kill-local-variable 'font-lock-fontify-region-function)
    (kill-local-variable 'font-lock-unfontify-region-function))
  (freex-unhighlight-region (point-min) (point-max)))


(define-minor-mode freex-mode
  "Freex minor mode for editing."
   :group 'freex
   :global nil
   :init-value nil
   :lighter " Freex"
   :keymap freex-mode-map
   (when (not freex-embed-saving-p)
     (if freex-mode
	 (freexify)
       (defreexify))))


(defun freex-mode-list-files ()
  "Returns a list of freex files in the current directory."
  (mapcar 'list (directory-files
                 freex-mode-dir ;; where to look
                 nil ;; full file names?
                 freex-mode-dir-filter ;; file match regex
                 nil))) ;; nosort


;; re-evals freex-meta.el, along with a few other files, to
;; give Freex a kick-start in case pymacs crashes
(defun freex-restart ()
  (interactive)
  (load "freex.el")
  (load "freex-mode.el")
  (load "freex-embed.el")
  (load "freex-fontify.el")
  (load "freex-timestamp.el")
  (load "freex-meta.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'freex-mode)

