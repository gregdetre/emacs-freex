;;; freex-hiert.el --- miscellaneous functions for dealing
;;; with hierarchical indenting
;;
;; Copyright (C) 2007 Per B. Sederberg, Greg Detre
;;
;; Author: Per B. Sederberg, Greg Detre
;; Keywords: hypermedia
;; Date: 
;;
;; This file is part of Emacs Freex.  It is not part of GNU
;; Emacs.
;;
;; Emacs Freex is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any
;; later version.
;;
;; Emacs Freex is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Emacs Freex; see the file COPYING.  If
;; not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;; Contributors:


;;; Code:


;; the idea is that you press M-. (i.e. alt->) to indent a
;; paragraph, and M-, (i.e. alt-<) to outdent the
;; paragraph. the paragraph does not have to be selected in
;; order for this to work
;;
;; if you want to in/outdent multiple paragraphs at once,
;; just select them. you don't have to select the entire
;; paragraph - just make sure the start and end regions
;; include at least some of the paragraphs you're interested
;; in
;;
;; everything here is designed for indenting 2 spaces. i
;; haven't attempted to generalize things for n spaces.


;; just for freex-hiert-is-mark-active
(require 'version-info)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.dotemacs.de/dotfiles/SarirKhamsi.emacs.html
;;
;; i'm not convinced these are doing anything, but they were
;; in my .emacs file...
(setq tab-width 2)
(setq tab-interval 2)
(setq adaptive-fill-mode t)


(defun freex-hiert-in2 ()
  (interactive)
  (freex-hiert-indent-rigidly 2))


(defun freex-hiert-out2 ()
  (interactive)
  (let ((dent-size 2))
    (if (or
         (> (current-column) 0)
         (not (freex-hiert-on-blank-line)))
        (freex-hiert-indent-rigidly (* dent-size -1)))))


(defun freex-hiert-mark-paragraph ()
  (interactive)
  (freex-hiert-mark-multiple-paragraphs (point) (point)))


;; this works, but the recentering after narrowing gets
;; screwy if you change the font size
(defun freex-hiert-mark-multiple-paragraphs (start end)
  "According to the help for save-restriction, you're supposed to
use the save-excursion outside the save-restriction, which might
explain why i get weird recentering issues when i call this in a
frame with small font on a mac..."
  (interactive "r")
  (save-restriction
    (narrow-to-region
     (save-excursion
       (goto-char start)
       (freex-hiert-backward-paragraph))
     (save-excursion
       (goto-char end)
       (freex-hiert-forward-paragraph)))
    (mark-whole-buffer)))


    

;; (defun freex-hiert-mark-multiple-paragraphs (start end)
;;   (interactive "r")
;;   (let ((beg-par
;;          (save-excursion
;;            (goto-char start)
;;            (freex-hiert-backward-paragraph)))
;;         (end-par
;;          (save-excursion
;;            (goto-char end)
;;            (freex-hiert-forward-paragraph))))
;;       (push-mark beg-par)
;;       (push-mark end-par)))


; based on http://xahlee.org/emacs/examples.html
(defun freex-hiert-indent-rigidly-paragraphs-ncols(start end ncols)
  (freex-hiert-mark-multiple-paragraphs start end)
  (save-restriction
    (narrow-to-region (point) (mark))
    (indent-rigidly (point-min) (point-max) ncols)))


(defun freex-hiert-indent-rigidly (ncols)
  "If the mark is inactive, run indent-rigidly on the (whole
of the) current paragraph. If the mark is active, then run
it on (the entirety of) all the paragraphs in the region. In
other words, make sure that you indent entire paragraphs at
a time."
  (if (and (freex-hiert-on-blank-line) (not (freex-hiert-is-mark-active)))
      (if (= ncols 2)
          (insert "  ")
        (delete-char -2))
    (progn
      (save-excursion
       (if (freex-hiert-is-mark-active)
           (progn 
             (if (< (mark) (point))
                 (exchange-point-and-mark))
             (freex-hiert-indent-rigidly-paragraphs-ncols
              (point) (mark) ncols))
         (progn
           (freex-hiert-mark-paragraph)
           (indent-rigidly (point) (mark) ncols))))
  ; if the point is at the beginning of the line, don't let
  ; it stay there - move it forward to the beginning of the
  ; paragraph
     (if (= 0 (current-column))
         (skip-chars-forward " \t"))
     (if (and (freex-hiert-at-end-of-paragraph)
              (< (skip-chars-backward " ") 0))
         (progn
           (freex-hiert-fill-paragraph)
           (insert " "))
       (progn
         (freex-hiert-fill-paragraph))))))


;; ;; worked fairly well for a while, but was frustrating
;; (defun freex-hiert-newline-and-indent ()
;;   (interactive)
;;   (if (freex-hiert-on-blank-line)
;;       (newline)
;;     (progn
;;       (newline-and-indent)
;;       (newline-and-indent))))


;; ;; new attempt at newline-and-indent, 070222
;; (defun freex-hiert-newline-and-indent ()
;;   (interactive)
;;   ;; ;; need to check if we're at the leftmost margin
;;   ;; (if (equal (current-column) 0)
;;   (let ((length-of-line
;;          (save-excursion
;;            (end-of-line)
;;            (current-column))))
;;     ;; need to check whether we're on a completely blank line
;;     ;; (i.e. no whitespace at all)
;;     (if (> length-of-line 0)
;;         ;; if so
;;         (newline-and-indent)
;;       ;; otherwise, just add a newline
;;       (newline))))


(defun freex-hiert-newline-and-indent ()
  (interactive)
  (let ((length-of-line (save-excursion (end-of-line) (current-column)))
        (ncols (current-column)))
    (if (> length-of-line 0)
        (if (looking-back "^\\([    ]\\)*") ;; blank up to point
            (progn
              (newline)
              (dotimes (i ncols) (insert " ")))
          (newline-and-indent))
      (newline))))


; based on muse-freex-hiert-on-blank-line, but muse-freex-hiert-on-blank-line
; doesn't deal well with the last line of the buffer
(defun freex-hiert-on-blank-line ()
  "See if point is on a blank line (i.e. containing nothing,
or nothing-but-whitespace"
  (interactive)
  (let ((isblank))
    (save-excursion
      (beginning-of-line)
      (setq isblank (looking-at "\\([    ]\\)*$")))
    isblank))


(defun freex-hiert-add-two-spaces ()
  (interactive)
  (insert "  "))

  

; from http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el
(defun freex-hiert-is-mark-active ()
  "True if the selection is displayed."
  (cond
    (version-emacs
      mark-active)           ; (mark) causes error if inactive under regular emacs
    (version-xemacs
      (not (not (mark))))    ; but nil with xemacs, and mark-active doesn't exist
    (t t)
  ))


(defun freex-hiert-backward-paragraph ()
  (interactive)
  (if (<= (count-lines 1 (point)) 1)
      ;; if we're on the first line, then just go to
      ;; (point-min) and return it
      ;;
      ;; N.B. count-lines returns 0 if at point-min, 1 if
      ;; you're on the first line but not at point-min, and
      ;; otherwise returns a 1-indexed integer
      (goto-char (point-min))
    ;; otherwise, go back up a para
    (progn
      (re-search-backward "^\\( \\)*$")
      (forward-line)
      (point))))


;; ;; trying to make this function more robust (e.g. for
;; ;; indenting paragraphs at the very end of the buffer)
;; (defun freex-hiert-forward-paragraph ()
;;   (interactive)
;;   (condition-case err ;; unneeded variable argument
;;       (progn ;; try block
;;         (re-search-forward "^\\( \\)*$")
;;         (backward-char))
;;     ;; catch block
;;     ;; (search-failed (message "hello")))
;;     (search-failed nil))
;;     (point))


;; (defun freex-hiert-forward-paragraph ()
;;   (interactive)
;;   (when (not (= (point) (point-max)))
;;     (when
;;         ;; try and scoot ahead to the end of this
;;         ;; paragraph
;;         (re-search-forward "^\\( \\)*$" nil t)
;;       ;; and if that worked, then move back one
;;       (backward-char)))
;;   (point))

(defun freex-hiert-forward-paragraph ()
  (interactive)
  (when (not (= (point) (point-max)))
    ;; if you can't scoot ahead to the end of this
    ;; paragraph
    (when (not (re-search-forward "^\\( \\)*$" nil t))
      (save-excursion
        (goto-char (point-max))
        (insert "\n"))
      ;; and now try again
      (re-search-forward "^\\( \\)*$" nil t))
    ;; now you're ready to move back one
    (backward-char))
  (point))


;; I don't want to allow hanging indents. So the first-line
;; indent should set the indentation for the entire paragraph,
;; block-quote style. The easiest way to do this is to unfill
;; the paragraph, turning it into a single long line, and then
;; fill it again.
(defun freex-hiert-fill-paragraph ()
  (interactive)
  (freex-hiert-unfill-paragraph)
  (fill-paragraph nil))

;; (local-set-key "\M-q" 'freex-hiert-fill-paragraph)
(define-key freex-mode-map "\M-q" 'freex-hiert-fill-paragraph)


; http://www.emacswiki.org/cgi-bin/wiki/UnfillParagraph
;
;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph Takes a multi-line paragraph and makes it
;;; into a single line of text.
(defun freex-hiert-unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))



(defun freex-hiert-at-beginning-of-paragraph ()
  (interactive)
  (let ((cur (point))
        (at-beg nil))
    (save-excursion
      (freex-hiert-backward-paragraph)
      (setq at-beg (= cur (point))))
    at-beg))


(defun freex-hiert-at-end-of-paragraph ()
  (interactive)
  (let ((cur (point))
        (at-end nil))
    (save-excursion
      (freex-hiert-forward-paragraph)
      (setq at-end (= cur (point))))
    at-end))


(defun freex-hiert-at-beginning-of-line ()
  (= (point) (point-at-bol)))

(defun freex-hiert-at-end-of-line ()
  (= (point) (point-at-eol)))


(provide 'freex-hiert)
