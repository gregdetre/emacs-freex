(require 'muse-mode)	     ; load authoring mode
(require 'muse-html) ; load publishing styles I use
;; (load "load_latex.el")
;; (require 'muse-latex)
;; (require 'muse-texinfo)
;; (require 'muse-docbook)
(require 'muse-wiki)
(require 'muse-colors)
;; (require 'muse-convert) ; renamed to muse-convert-latex
;; (require 'muse-journal)
;; ;; my pages
(require 'muse-project)


; see http://www.mwolson.org/projects/emacs-config/muse-init.el.html muse-init.el for more info

(setq muse-project-alist
      '(
        ("testmuse"
	 ("~/elisp/freex/testmuse" :default "index" :author "Greg Detre")
	  (:base "html" :path "~/elisp/freex/testmuse"))

	))






