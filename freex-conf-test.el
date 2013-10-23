;; Copyright 2007, Greg Detre, Per Sederberg.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test version of freex-conf. modify then run this with 
; 'emacs -q -l freex-conf-test.el' if you want to load this automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'freex)
(require 'freex-hiert)

;; Add freex scripts (lisp and python files) to the load path ; xxx
(add-to-list 'load-path "/Users/greg/elisp/freex/")

;; Load pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "/Users/greg/elisp/freex/")) ;; xxx

;; Specify where the freex .db file and its .freex (or .muse) friends
;; will live.  (Make sure this directory exists.)
(setq freex-mode-dir "/Users/greg/elisp/freex/testdocs/") ; xxx

;; Set the file extension that identifies freex files
(setq freex-mode-ext "freex")
;; Muse(XXX): Change "freex" to "muse" if you are using Muse with Freex
;; (see below)

;; this is the regex for finding files in the directory that
;; should be inserted into the db. the caret at the
;; beginning tells it to ignore files that have a dot in
;; front (useful for excluding emacs temp files), but it
;; also excludes any files with dots in them after the first
;; character, which annoys per. since my filenames don't
;; have dots in them, this is fine for me
(setq freex-mode-dir-filter "^[^.#]+\\.freex$")
;; Muse (XXX): Change freex to muse if you are using Muse and Freex together.


;; Set it to load freex-mode when it sees a .freex
;; file
(setq auto-mode-alist
      (cons '("\\.freex\\'" . freex-mode)
            auto-mode-alist))

;;; ;; Muse (XXX): To use Freex with Muse, comment out the above
;;; ;; auto-mode-alist entry and uncomment the following
;;; (defun muse-freex-mode ()
;;;   (when (not freex-embed-saving-p)
;;;     ;; must load muse first
;;;     (muse-mode)
;;;     ;; load freex
;;;     ;; edit so that will only go into freex if file is in the freex dir
;;;     ;;(freex-mode)))
;;;     (when (equal (file-name-directory (buffer-file-name)) freex-mode-dir)
;;;       (freex-mode))))
;;;
;;; (setq auto-mode-alist
;;;       (cons '("\\.muse\\'" . muse-freex-mode)
;;;             auto-mode-alist))


;; Specify the way to store data (we recommend that you mirror your
;; files to the database so you can make use of the full-text search
;; capabilities).
(setq freex-content-storage "mirror-files-to-db")

;; Get Freex mode ready to go
(load "/Users/greg/elisp/freex/freex-mode.el") ; xxx

;; These next lines tell the database to update itself automatically
;; if there are any new files in the freex data directory, every time
;; you save
;;
;; Both these processes are resource intensive for large databases (>
;; 1000 nuggets), so you may want to comment these out and run them
;; manually in such cases.
(add-hook 'freex-embed-save-hook 'freex-meta-update-index)
(add-hook 'freex-embed-save-hook 'freex-fontify-update-implicit-link-regexp)


;; This hook automatically tokenizes the filename by hyphens, and adds
;; all the parts as tags (very handy)
(add-hook 'freex-meta-add-nugget-hooks
          'freex-meta-parse-alias-into-tag-parents)


;; Otherwise implicit links will be turned off
(setq freex-enable-implicit-links t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional keyboard shortcuts that could interfere with
;; other emacs modes ; xxx

;; i have C-l for find-file and M-l for freex-find-file
(global-set-key [(meta l)] 'freex-meta-find)
(global-set-key [(alt l)] 'freex-meta-find)
(global-set-key [(hyper l)] 'freex-meta-find)

;; Otherwise implicit links will be turned off
(setq freex-enable-implicit-links t)

(define-key freex-mode-map [(control =)] 'freex-embed-all-tag-children)

(define-key freex-mode-map "\M-s" 'save-buffer)

(define-key freex-mode-map "\M-DEL" 'backward-kill-word)

;; make it a little more visible than the default 20
(setq freex-embed-color-step 25)

;; makes find-alias etc. case-insensitive
(setq completion-ignore-case t)

(define-key minibuffer-local-isearch-map (quote [67108921])
   (lookup-key minibuffer-local-isearch-map "\C-s"))

(define-key freex-mode-map (kbd "RET") 'freex-hiert-newline-and-indent)
(define-key freex-mode-map [(shift return)] 'newline)

(define-key freex-mode-map [(meta return)]
  'freex-meta-define-new-or-insert-metadata)

(define-key freex-mode-map [(meta shift return)]
  'freex-meta-edit-tag-parents-in-minibuffer)

(define-key freex-mode-map "\M-." 'freex-hiert-in2)
(define-key freex-mode-map "\M-," 'freex-hiert-out2)

(define-key freex-mode-map
  (quote [201326641]) ;; [(control meta 1)]
  'freex-fontify-insert-sect-element-1)
(define-key freex-mode-map
  (quote [201326642]) ;; [(control meta 2)]
  'freex-fontify-insert-sect-element-2)
(define-key freex-mode-map
  (quote [201326643]) ;; [(control meta 3)]
  'freex-fontify-insert-sect-element-3)
