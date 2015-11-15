;;; freex-tests.el --- emacs lisp unit tests
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


;; if you want to run this, run 'emacs -q' to avoid loading
;; your .emacs file, and eval this, and then run
;; (freex-tests-run-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initiate things

;; this is where all the tests are going to write their
;; files, and where the freex.db will live. DO NOT SET IT TO
;; YOUR MAIN FREEX DOCS DIRECTORY
(setq freex-mode-dir "/home/greg/elisp/freex")

(add-to-list 'load-path freex-mode-dir)

(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
;; this needs to be made general, but i'm not sure how to
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path freex-mode-dir))

;; at the moment, this the only content-storage type we're
;; testing
(setq freex-content-storage "files")

(load "/Users/greg/Dropbox/elisp/freex/freex-mode.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defuns

(defun freex-tests-run-all ()
  "This calls the main pyunit unittest function, which manages
the running of all the tests."
)
