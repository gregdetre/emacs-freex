;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; from http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ------------ what version of emacs? -----------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; true if we're under NT
(setq version-os-nt     (equal (getenv "OS") "Windows_NT"))

; true if we're under XEmacs (www.xemacs.org)
(setq version-xemacs    (string-match "XEmacs\\|Lucid" emacs-version))

; true if we're under regular emacs
(setq version-emacs     (not version-xemacs))

; true if we're running under anything other than a text terminal
(setq version-not-term  (not (not window-system)))

; true under regular emacs and X windows
(setq version-emacs-x   (and (not version-xemacs) (equal window-system 'x)))


(provide 'version-info)
