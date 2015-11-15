;;; freex.el --- Base info used in emacs freex

;; Copyright (C) 2007 Per B. Sederberg, Greg Detre

;; Author: Per B. Sederberg, Greg Detre
;; Keywords: hypermedia
;; Date: 

;; This file is part of Emacs Freex.  It is not part of GNU
;; Emacs.

;; Emacs Freex is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any
;; later version.

;; Emacs Freex is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public
;; License along with Emacs Freex; see the file COPYING.  If
;; not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:


;;; Code:

;; set the required files

;; 0.0.3 since extricating from muse
(defconst freex-version "0.0.3")


;; [^.#]+\\.freex$"
(defvar freex-mode-dir-filter
"[^.#]+\\.freex$"
  "Get all the .freex files in the directory. I think we were
  trying to exclude files beginning with a dot, or something
  here. However, this doesn't allow any files that have extra
  dots in now, so we need to change it.")


(defvar freex-mode-dir "~/"
  "This is the directory that freex will look inside for the
files that it will use to update the index. In the future,
you'll be able to define multiple projects, but for now it's
just a single global variable specifying a single
directory." )


(defvar freex-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [(control -)] 'freex-embed-remove-overlay-at-point)
    ;; all our predefined keybindings should follow the \C-cx format,
    ;; which is prescribed by Emacs for minor modes.  End users can
    ;; certainly remap these to whatever they like.
    (define-key km "\C-cm" 'freex-meta-insert-metadata-here)
    (define-key km "\C-cf" 'freex-meta-find)
    (define-key km "\C-ct" 'freex-meta-edit-tag-parents-in-minibuffer)
    (define-key km "\C-ca" 'freex-meta-edit-aliases-in-minibuffer)
    km)
  "The keymap used in `freex-mode'."
  )



(defvar freex-mode-hook nil
  "Set of hooks that get run at the end of freexify.")




;; ;; This could have issues if two links are right next to one another
;; (defun freex-link-info-at (&optional pos)
;;   "Return the beg, end, and link text as a list if a link is at
;;  pos [defaults to point].  Otherwise return nil."
;;   (let ((here (or pos (point)))
;; 	(beg nil)
;; 	(end nil)
;; 	(link-text nil)
;; 	(ret-list nil))
;;     (when (get-text-property here 'freex-link)
;;       (save-excursion
;; 	;; find the beginning of the link
;; 	(goto-char here)
;; 	(when (and (not (bobp))
;; 		   (get-text-property (1- here) 'freex-link))
;; 	  (goto-char (or (previous-single-property-change here 'freex-link)
;; 			 (point-min))))
;; 	(setq beg (point))

;; 	;; find the end of the link
;; 	(goto-char here)
;; 	(when (and (not (eobp))
;; 		   (get-text-property (1+ here) 'freex-link))
;; 	  (goto-char (or (next-single-property-change here 'freex-link)
;; 			 (point-min))))
;; 	(setq end (point)))
;;       ;; get the link text
;;       (setq link-text (get-text-property here 'link-text))
;;       (setq ret-list (list beg end link-text)))))

(defun freex-follow-link-at-point (&optional other-window)
  "Visit the link at point."
  (interactive "P")
  ;; see if we are on a link. don't count the last character
  ;; of a link as being a link, otherwise it's too hard to
  ;; add carriage returns
  (if (get-text-property (point) 'freex-link)
      (let ((link-file (get-text-property (point) 'link-file))
	    (link-url (get-text-property (point) 'link-url)))
	(if link-file
	    (let ((base-buffer nil))
	      (setq base-buffer (get-buffer link-file))
	      (if (and base-buffer (not (buffer-file-name base-buffer)))
		  ;; If file is temporary (no associated file), just switch to
		  ;; the buffer
		  (if other-window
		      (switch-to-buffer-other-window base-buffer)
		    (switch-to-buffer base-buffer))
		;; find the file
		(if other-window
		    (find-file-other-window link-file)
		  (find-file link-file))))
;; 	    ;; open file in buffer
;; 	    (message (format "I would open this file: %s" 
;; 			     link-file))
	  (when link-url
	    ;; open url in browser
	    (browse-url link-url))))
;; 	    (message (format "I would open this url: %s" 
;; 			     link-url)))))
    ;; not a link
    (message "No link at point.")))

(defun freex-follow-link-at-point-other-window ()
  "Visit the link at point in another window."
  (interactive)
  (freex-follow-link-at-point t))


(defun freex-make-marker-at (pos)
  "Creates a marker and sets it to POS, then returns it."
  (set-marker (make-marker) pos))


(provide 'freex)

