;;; freex-xml.el --- Interface for dealing with xml

;; Copyright (C) 2007, Per B. Sederberg, Greg Detre
;; Inc.

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
(require 'freex)


(defcustom freex-regexp-blank
  (if t   ;; FIX THIS
      "[:blank:]"
    " \t")
  "Regexp to use in place of \"[:blank:]\".
This should be something that matches spaces and tabs.

It is like a regexp, but should be embeddable inside brackets.
Freex will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:blank:]" " \t")
  :group 'freex-xml)


(defcustom freex-element-regexp
  (concat "<\\([^/" freex-regexp-blank "\n][^" freex-regexp-blank
          "</>\n]*\\)\\(\\s-+[^<>\n]+[^</>\n]\\)?\\(/\\)?>")
  "A regexp used to find XML-style elements within a buffer.
Group 1 should be the element name, group 2 the properties, and
group 3 the optional immediate ending slash."
  :type 'regexp
  :group 'freex-xml)


(defun freex-xml-goto-element-end (element nested)
  "Move forward past the end of ELEMENT.

If NESTED is non-nil, look for other instances of this element that
may be nested inside of this element, and skip past them."
  (if (not nested)
      (search-forward (concat "</" element ">") nil t)
    (let ((nesting 1)
          (element-regexp (concat "^\\(<\\(/?\\)" element ">\\)"))
          (match-found nil))
      (while (and (> nesting 0)
                  (setq match-found (re-search-forward element-regexp nil t)))
        (if (string-equal (match-string 2) "/")
            (setq nesting (1- nesting))
          (setq nesting (1+ nesting))))
      match-found)))


(if (fboundp 'looking-back)
    (defalias 'freex-looking-back 'looking-back)
  (defun freex-looking-back (regexp &optional limit &rest ignored)
    (save-excursion
      (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t))))



(defsubst freex-xml-element-info (elementlist elementname &rest args)
  (assoc elementname elementlist))


(defun freex-xml-process-element (elementlist)
  "Highlight `freex-embed-elements'."
  ;; set the match data
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at freex-element-regexp))
  ;; see if we matched a element that we are supposed to process
  (let ((element-info (freex-xml-element-info elementlist (match-string 1))))
    (when element-info
      ;; we matched a element, process its options
      (let ((closed-element (match-string 3))
            (start (match-beginning 0))
            end attrs)
	;; see if we parse the attributes
        (when (nth 2 element-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match (concat "\\([^"
                                              freex-regexp-blank
                                              "=\n]+\\)\\(=\""
                                              "\\([^\"]+\\)\"\\)?")
                                      attrstr))
              (let ((attr (cons (downcase
                                 (match-string-no-properties 1 attrstr))
                                (match-string-no-properties 3 attrstr))))
                (setq attrstr (replace-match "" t t attrstr))
                (if attrs
		    ;; append the new attribute
                    (nconc attrs (list attr))
		  ;; create new attribute list
                  (setq attrs (list attr)))))))
	;; see if we expect a closing element
        (if (and (cadr element-info) (not closed-element))
            (if (freex-xml-goto-element-end (car element-info) (nth 3 element-info))
                (setq end (match-end 0))
              (setq element-info nil)))
        (when element-info
          (let ((args (list start end)))
            (if (nth 2 element-info)
                (nconc args (list attrs)))
	    ;; run the desired function with the args
            (apply (nth 4 element-info) args)))))
    ;; set the return value for if found a element
    (if element-info
	t
      nil)))

;; (defvar freex-fontify-regexp-list
;;   '((freex-element-regexp t freex-xml-process-element 
;; 			  (freex-embed-elements))
;;     (freex-implicit-link-regexp t '(freex-fontify-implicit-link))))

;; (setq cmd (nth 2 (car freex-fontify-regexp-list)))
;; (setq args (mapcar 'eval (nth 3 (car freex-fontify-regexp-list))))
;; (apply cmd args)

(defun freex-xml-process-region (beg end regexp-list &optional verbose)
  "Parse the region and handle any matching regexps."
  (let ((modified-p (buffer-modified-p))
	(new-end nil))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (widen)
	    ;; loop over each regexp in the list, processing the region
	    (dolist (regexp-def regexp-list)
	      (let ((current-regexp (eval (nth 0 regexp-def)))
		    (case-fold-search (nth 1 regexp-def))
		    (found-cmd (nth 2 regexp-def))
		    (found-args (if (nth 3 regexp-def)
				    (mapcar 'eval (nth 3 regexp-def))
				  nil)) ;; there must be a better way to do this
		    (len (float (- end beg)))
		    (buflen (point-max)))
		(goto-char beg)
		(while (and (< (point) end)
			    (re-search-forward current-regexp end t))
		  (if verbose
		      (message "Highlighting buffer...%d%%"
			       (* (/ (float (- (point) beg)) len) 100)))
		  ;; set that we matched something
		  (setq element-found t)
		  ;; handle the match
		  (apply found-cmd found-args)
		  ;; see if update end position
		  (when (not (eq buflen (point-max)))
		    ;; must update end
		    (setq end (+ end (- (point-max) buflen)))
		    (setq len (float (- end beg)))
		    (setq buflen (point-max)))
		  ;; set the new-end val to show that we updated something
		  (setq new-end end))
		(if verbose (message "Highlighting buffer...done")))))))
    (set-buffer-modified-p modified-p)
    ;; return whether we matched a regexp. for simple regexps, this
    ;; does not guarantee that we actually processed anything
    ;; new-end will be nil if we did not match anything
    new-end))

(defun freex-xml-insert-element-at-point (element attrs text &optional postfun)
  "Insert an element at point, optionally processing the region
with postfun afterwords.  postfun takes the args: beg, end,
verbose."
  (let ((insert-text (format "<%s%s>%s</%s>" element 
			     (if attrs (concat " " attrs) "") text element))
	(before-element (freex-make-marker-at (point))))
    ;; use markers to get beg and end
    (insert insert-text)
    ;; if postfun, call it with apply
    (when postfun
      (apply postfun (list before-element (point) nil)))))

(defun freex-xml-replace-element-at-point (&optional element attrs postfun)
  "Replace [or remove if element is nil] the element at point."
  (interactive)
  ;; eventually must check to see if element is nestable
  (let ((starting-point (point))
	(nested nil))
    ;; search backwards for the innermost element
    (save-excursion
      (when (re-search-backward freex-element-regexp nil t)
	;; we found an element, so see if the ending element is after
	;; the start (it will be a singleton element if the third
	;; match string is not empty
	(let ((has-closing-element (not (match-string 3)))
	      (starting-element (match-string 1))
	      (do-replace t))
	  (if has-closing-element
	      (save-excursion
		(save-match-data
		  ;; search forward for the end
		  (freex-xml-goto-element-end starting-element nested)
		  ;; see if point is after the start
		  (when (< (match-beginning 0) starting-point )
		    ;; not within element, so ignore
		    (setq do-replace nil))))
	    ;; make sure end of single element is after start
	    (when (< (match-beginning 2) starting-point)
	      ;; not within, so ignore
	      (setq do-replace nil)))
	  ;; process replace if desired
	  (when do-replace
	    (if element
		;; replace
		(let ((beg (match-beginning 0))
		      (end (match-end 0))
		      (text-beg (match-end 0))
		      (text-end nil)
		      (element-text ""))
		  ;; extract the text
		  (when has-closing-element
		    ;; get end from close
		    (freex-xml-goto-element-end starting-element nested)
		    (setq end (match-end 0))
		    (setq text-end (match-beginning 0))
		    ;; set the text
		    (setq element-text (buffer-substring-no-properties 
					text-beg text-end)))
		  ;; delete the entire element
		  (delete-region beg end)
		  ;; add in new element
		  (freex-xml-insert-element-at-point element attrs 
						     element-text postfun))
	      ;; remove
	      (progn 
		;; remove the beginning element
		(delete-region (match-beginning 0) (match-end 0))
		(when has-closing-element
		  ;; remove the ending element
		  (freex-xml-goto-element-end starting-element nested)
		  (delete-region (match-beginning 0) (match-end 0)))))))))))

(provide 'freex-xml)
