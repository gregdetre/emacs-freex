;;; freex-embed.el --- embedding of info in overlays used by Freex
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

;; set the required files
(require 'freex)
(require 'freex-xml)
(require 'freex-fontify)

(defvar freex-embed-color-step 20
  "This is the global color step size used to determine how
much to increment the color depth based on an overlay's
priority. The default (get-color-from-priority) makes
overlays' red, green and blue values darker by a stepsize of
20 (out of 256).")


(defvar freex-content-storage "files"
  "This determines how the content of your embeddings will
be stored.

If set to 'files' (the default), then freex will write the
contents of embedded overlays out to flat files, as you'd
expect.

If set to 'mirror-files-to-db', then freex
will write the contents of embedded overlays out to files,
but also to a contents field in the database. When reading,
it will read from files. This means you get all the
convenience of flat files, but all the indexing capabilities
of the db. As long as you don't manually edit the database,
things should stay perfectly in sync.

If set to 'db', freex will write the contents of embedded
overlays to the database only, i.e. there will be no flat
files. This requires you to have defined a freex-homepage,
and you will only be able to view your nuggets as embedded
overlays (since there's no way for a database record to
exist as a buffer without being associated with a file. This
hasn't been tested, and probably isn't a good idea.

'mirror-db-to-files - NOT IMPLEMENTED. If you can think of a
really good reason why you'd want this, email us.")


(defvar freex-embed-ov-props nil
  "This is the buffer-local property list containing more or
less the same information stored in the overlay property
list, which tells buffers (such as the nugget-buffers
created by freex-embed-load-nugget-alias how to save themselves
etc.")
(make-variable-buffer-local 'freex-embed-ov-props)


(defvar freex-embed-save-hook nil
  "Gets run at freex-embed-save.")


(defvar freex-embed-saving-p nil
  "Variable to hold whether we are currently saving an overlay.")

(defvar freex-embed-embedding-p nil
  "Variable to hold whether we are currently embedding an
  overlay.  We need this to prevent the modification hooks from
  marking other overlays as modified just because we are
  embedding an overlay.")


;; add our cool hook (now to freex-mode-hook)
;;(add-hook 'freex-mode-hook 'freex-embed-buffer)

;; used to be to colors-buffer-hook
;(add-hook 'freex-colors-buffer-hook 'freex-embed-region)


(defun freex-embed-buffer ()
  "Re-embed the entire Freex buffer."
  (interactive)
  ;; you don't want to do this when saving
  (when (not freex-embed-saving-p)
    (freex-embed-region (point-min) (point-max) t)))

;; (defvar freex-fontify-regexp-list
;;   '((freex-element-regexp t freex-xml-process-element 
;; 			  (freex-embed-elements))
;;     (freex-implicit-link-regexp t '(freex-fontify-implicit-link))))

(defvar freex-embed-regexp-list
  '((freex-element-regexp t freex-xml-process-element
			  (freex-embed-elements)))
    "List of regexps to search for and process for freex-embed.")

(defun freex-embed-region (beg end &optional verbose)

  ;; process the tags in the region
  (let* ((freex-embed-embedding-p t)
	 (new-end (freex-xml-process-region beg 
					   end 
					   freex-embed-regexp-list 
					   verbose)))
    ;; see if we matched anything and should use the new end
    ;;
    ;; it turns out that we don't need this. or at least, we
    ;; don't think we do, but it may be that this is here to
    ;; fix a bug... :(
    ;;     (when (and new-end
    ;; 	       (> new-end beg))
    ;;       ;; fontify that region
    ;;       ;; (freex-fontify-region beg new-end verbose)))
    ))

(defcustom freex-embed-elements
  '(("file" t t nil freex-embed-file-element))
  "A list of element specifications for embeding text.

For each entry, the name of the element is given, whether it
expects a closing element and/or an optional set of attributes,
whether it is nestable, and a function that performs whatever
action is desired within the delimited region.

The function is called with three arguments, the beginning and
end of the region surrounded by the elements. If properties are
allowed, they are passed as a third argument in the form of an
alist. The `end' argument to the function is the last character
of the enclosed element or region.

Functions should not modify the contents of the buffer."
  :type '(repeat (list (string :tag "Markup element")
                       (boolean :tag "Expect closing element" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'freex-embed)


;; add our custom save function that handles overlays
(defun freex-embed-use-custom-save ()
  (interactive)
  (make-local-hook 'write-contents-hooks)
  (add-hook 'write-contents-hooks 'freex-embed-save nil t))
;(add-hook 'freex-mode-hook 'freex-embed-use-custom-save)


;; define how to handle an embedded file
(defun freex-embed-file-element (beg end attr)
    ;(freex-unhighlight-region beg end)
    (let ((beg-file nil) 
          (end-file nil)
	  (fileroot nil)
          (filename nil)
	  (full-filename nil)
	  (text-tween-elements nil)
	  ;;(freex-embed-embedding-p t)
          ;; decide which function to use for reading
          ;; content from
          (insert-funct
           (cond
            ((equal freex-content-storage "files")
             'freex-embed-overlay-insert-file)
            ((equal freex-content-storage "db")
             'freex-embed-overlay-insert-db)
            ((equal freex-content-storage "mirror-files-to-db")
             'freex-embed-overlay-insert-file)
            (t
             (error "Unknown freex-content-storage option"))))

          ;; decide which function to use for writing
          ;; content to
          (save-funct
           (cond
            ((equal freex-content-storage "files")
             'freex-embed-overlay-save-file)
            ((equal freex-content-storage "db")
             'freex-embed-overlay-save-db)
            ((equal freex-content-storage "mirror-files-to-db")
             'freex-embed-overlay-mirror-files-to-db)
            (t
             (error "Unknown freex-content-storage option"))))
          (priority (+ (freex-embed-get-highest-priority-at beg) 1)))
      (save-match-data
        (goto-char beg)
        (setq beg-file (and (looking-at "<[^>]+>")
                            (match-end 0)))
        (goto-char end)
        (setq end-file (and (freex-looking-back "</[^>]+>")
                            (match-beginning 0))))
      ;; insert file as overlay
      (setq text-tween-elements (buffer-substring-no-properties 
                             beg-file 
                             end-file))

      ;; set the filename and filename with full path
      (setq filename text-tween-elements)
      (setq full-filename (concat (file-name-as-directory freex-mode-dir) filename))
      ;; only insert if the file exists
      (when (file-exists-p full-filename)
	;; insert the file
	(freex-embed-create-overlay beg end priority
				    insert-funct
				    save-funct
				    (list 'filename filename
					  'full-filename full-filename))
	;; goto the beginning of the region
	(goto-char beg))))


(defun freex-embed-overlay-insert-file (ov)
  "This is attached to an overlay upon its creation as the
INSERT-FUNCT property, that gets called after this
overlay has been created that fills it with text."

  (let ((filename (overlay-get ov 'filename))
	(full-filename (overlay-get ov 'full-filename)))
    (unless filename
      (error "Filename is nil"))
    ;; this seemed to insert ^M in windows
    ;; (insert-file-contents-literally full-filename)
    (insert-file full-filename)
    (overlay-put ov 'id
                 (number-to-string
                  (freex-sqlalchemy-get-nugid-from-filename
                   filename))))
  ov)


(defun freex-embed-overlay-save-file ()
  (write-file (plist-get freex-embed-ov-props 'full-filename) nil))


(defun freex-embed-overlay-mirror-files-to-db ()
  ;; untested
  (freex-embed-overlay-save-file)
  (freex-embed-overlay-save-db))


(defun freex-embed-overlay-save-db ()
  ;; untested
  (freex-sqlalchemy-put-contents-a 
   (freex-sqlalchemy-remove-ext
    (plist-get freex-embed-ov-props 'filename))
   (buffer-string)))
  

(defun freex-embed-create-overlay
  (beg end priority insert-funct save-funct
             &optional properties)
  "Creates an overlay, cuts out and stores the link text
within BEG and END, sets the default properties, runs and
appends the INSERT-FUNCT function to fill the text, stores
the SAVE-FUNCT function, adds the optional PROPERTIES, and
then runs the INSERT-FUNCT to fill it in. PROPERTIES is a
list containing an even number of arguments, in PROP VALUE
form (e.g. (list table table-name col column-name identifier
idno). It needs to include any identifying information that
the SAVE-FUNCT will need when saving. To make it easy for
the INSERT-FUNCT function to insert the text into the
overlay, we create the overlay first (containing a single
newline), birth the file's contents into the overlay, and
then shrink it by a character's worth at the end of the
function."
  (let ((ov nil)
        ;; stext = source text
	(stext (freex-embed-cut-region beg end))
	(modified-p (buffer-modified-p)))
    (save-excursion
      ;; make the overlay consist of a single newline, to
      ;; begin with
      (goto-char beg)
      (setq ov (make-overlay beg beg nil nil t))
      ;; add the default properties
      (overlay-put ov 'is-modified nil)
      (overlay-put ov 'is-freex-embed t)
      (overlay-put ov 'priority priority)
      (overlay-put ov 'evaporate nil)
      (overlay-put ov 'rear-nonsticky t)
      ;; set the color of the text
      (overlay-put ov 'face
                   (cons 'background-color
                         (freex-embed-get-color-from-priority priority)))
      (overlay-put ov 'source-text stext)
      ;; add any optional properties to it, unless the
      ;; PROPERTIES list has an even number of items
      (unless (equal (mod (length properties) 2) 0)
        (error "Properties list must be even in length"))
      (while properties
        (overlay-put ov (pop properties) (pop properties)))
      ;; append the SAVE-FUNCT
      (overlay-put ov 'save-funct save-funct)
      ;; now we're ready to run the INSERT-FUNCT, to get the
      ;; actual text contents, e.g. from a file, database
      ;; query etc
      (eval (list insert-funct 'ov))
      ;; append the INSERT-FUNCT for good measure
      ;;
      ;; is this right??? why "insert-function" rather than
      ;; "insert-funct"??? and why is insert-funct quoted???
      (overlay-put ov 'insert-funct insert-funct)
      ;; add the modification hooks (after we have inserted)
      (overlay-put ov 'modification-hooks
                   '(overlay-mark-as-modified))
      ;; make sure to use custom save
;;       (when (not freex-embed-saving-p)
;; 	(freex-embed-use-custom-save))
      ;; set buffer modified to nil if it was not already modified
      (set-buffer-modified-p modified-p)
      ;; force overlay to be not modified
      (overlay-put ov 'is-modified nil)
      ov)))
      


(defun overlay-mark-as-modified 
  (overlay is-post start end &optional replaced-length)
  "This is the function that gets called by the overlay's
modified hook. Must have these arguments."
  (when (and is-post
	     (not freex-embed-saving-p)
	     (not freex-embed-embedding-p)
	     (freex-embed-is-max-overlay-at overlay))
  ;;(when is-post
  ;;  (message (format "P(%i)" (point)))
    (overlay-put overlay 'is-modified t)))
;; PBS: Deleted the following from the above fun because it's no
;; longer needed now that we have rear-sticky overlays.
;;(freex-ensure-whitespace-at-overlay-end overlay)))


(defun freex-embed-cut-region (beg end)
  "Grabs and returns the text between BEG and END, having
deleted it from the buffer."
  ;; i think i might have reinvented
  ;; delete-and-extract-region unwittingly
  (let ((txt (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    txt))


(defun freex-embed-pick-pastel-colors (&optional min-sum max-sum)
  "Returns a list of color names, all of whose RGB values
sum to greater than MIN-SUM (optional, default 400), less
than MAX-SUM (optional, default 600) and do not contain
either 'gray' or 'grey' in their name - these tend to be
fairly pastel"
  (when (not min-sum)
    (setq min-sum 400))
  (when (not max-sum)
    (setq max-sum 600))
  (let ((pastels))
    (dolist (col color-name-rgb-alist)
      (when (and
             (> (apply '+ (cdr col)) min-sum)
             (< (apply '+ (cdr col)) max-sum)
             (not (or
                   (string-match "grey" (car col))
                   (string-match "gray" (car col)))))
        (push (car col) pastels)))
    pastels))


(defun freex-embed-pick-random-color (&optional cols)
  "Pick a random color from COLS, or from the complete list
of defined colors if COLS isn't supplied."
  (when (not cols)
    (setq cols (x-defined-colors)))
  (nth (random (length cols))
       cols))


(defun freex-embed-get-RGB-from-systemlist (color-name)
  "Find a COLOR-NAME by using `color-values' (Emacs) or
`color-rgb-components' (Xemacs)."
  (let ((color-name (downcase color-name))
        colors-list-to-return)
    (setq colors-list-to-return
          (if (featurep 'xemacs)
              (color-rgb-components (make-color-specifier color-name))
            (color-values color-name)))
    ;; color intensities take two bits, and we want them to take one
    (setq colors-list-to-return
          (mapcar '(lambda (elem) (round (* (/ elem 65535.0) 255)))
                  colors-list-to-return))
    (add-to-list 'colors-list-to-return color-name)
    colors-list-to-return))


(defsubst freex-embed-hex-from-RGB (red green blue)
  "Build a color like #00FF00 from given RED, GREEN and BLUE.
For example: 0 255 0 will result in #00FF00."
  (format "#%02X%02X%02X" (round red) (round green) (round blue)))

(defsubst freex-embed-color-in-hex-format (color)
  "Find out if COLOR is in hex format or not."
  (string-equal (if (featurep 'xemacs)
                    (replace-in-string color
                                       "#[0-9a-fA-F]\\{6\\}"
                                       "")
                  (replace-regexp-in-string "#[0-9a-fA-F]\\{6\\}"
                                            ""
                                            color))
                ""))


(defsubst freex-embed-hex-from-colorname (color)
  "Build a color like #00FF00 from \"green\" or return COLOR if already in hex"
  (let ((return-color
         (if (freex-embed-color-in-hex-format color)
             color
           (let ((freex-embed-color-from-system-list
                  (freex-embed-get-RGB-from-systemlist color)))
             (freex-embed-hex-from-RGB
              (nth 1 freex-embed-color-from-system-list)
              (nth 2 freex-embed-color-from-system-list)
              (nth 3 freex-embed-color-from-system-list))))))
    return-color))


(defun freex-embed-get-color-from-priority (priority)
  "Return the proper color for an overlay based on the desired
  priority level.  It is based on the default background color
  for the frame."
  (let* ((bg-color-name
	  (if (featurep 'xemacs)
	      (face-background-name 'default)
	    (cdr (assoc 'background-color (frame-parameters)))))
	 (bg-color-hex (freex-embed-hex-from-colorname bg-color-name))
	 (bg-color-hex-red (string-to-number (substring bg-color-hex 1 3) 16))
	 (bg-color-hex-green (string-to-number (substring bg-color-hex 3 5) 16))
	 (bg-color-hex-blue (string-to-number (substring bg-color-hex 5 7) 16 ))
	 (new-red)
	 (new-green)
	 (new_blue)
	 (pri (+ priority 1)))
    ;; set the new color values from the priority
    (setq new-red (- bg-color-hex-red (* pri freex-embed-color-step))
	  new-green (- bg-color-hex-green (* pri freex-embed-color-step))
	  new-blue (- bg-color-hex-blue (* pri freex-embed-color-step)))

    ;; check the values
    (if (< new-red 0)
	(setq new-red 0))
    (if (< new-green 0)
	(setq new-green 0))
    (if (< new-blue 0)
	(setq new-blue 0))

    ;; return the hex
    (freex-embed-hex-from-RGB new-red new-green new-blue)))


;;; this should share code with freex-embed-get-max-overlay-at
;;; somehow. when i tried to amalgamate them, i realized
;;; that they need each other... see comment for
;;; freex-embed-get-max-overlay-at
(defun freex-embed-get-highest-priority-at (&optional pos)
  "Return the priority value of the highest-priority overlay
at POS (defaults to POINT)."
  (when (not pos) (setq pos (point)))
  (let ((overlays (overlays-at pos))
	(max-so-far -1))
    (dolist (overlay overlays)
      (setq overlay-priority (overlay-get overlay 'priority))
      (when (and
	     (> overlay-priority max-so-far)
	     (overlay-get overlay 'is-freex-embed))
	(setq max-so-far overlay-priority)))

;;     (while overlays
;;       (let ((overlay (car overlays))
;; 	    (overlay-priority))
;; 	(setq overlay-priority (overlay-get overlay 'priority))
;; 	(when (and
;;                (> overlay-priority max-so-far)
;;                (overlay-get overlay 'is-freex-embed))
;; 	  (setq max-so-far overlay-priority))
;; 	(setq overlays (cdr overlays))))

    max-so-far))


(defun freex-embed-get-overlay-of-priority (pri &optional freex-only)
  "Returns the overlay with priority PRI at point optionally only
checking overlays where the 'is-freex-embed property is true."
  (let ((ovs (overlays-at (point)))
        (found-ov nil))
    (dolist (try-ov ovs)
      (when (and
             (equal (overlay-get try-ov 'priority) pri)
             (overlay-get try-ov 'is-freex-embed))
        (setq found-ov try-ov)))
    found-ov))


;;; it would be better if this ran get-overlay-of-pri
;;; recursively from PRI 0 upwards, terminating when that
;;; fails and returning the highest PRI that succeeded
(defun freex-embed-get-max-overlay-at (&optional pos)
  "Return the overlay with the highest priority at pos [defaults
to point]."
  (when (not pos)
    (setq pos (point)))
  (let ((overlays (overlays-at pos))
	(max-so-far -1)
	(max-overlay)
	(overlay-priority))
    (dolist (overlay overlays)
      (setq overlay-priority (overlay-get overlay 'priority))
      (when (and
	     (> overlay-priority max-so-far)
	     (overlay-get overlay 'is-freex-embed))
	(setq max-so-far overlay-priority)
	(setq max-overlay overlay)))
    
;;     (while overlays
;;       (let ((overlay (car overlays))
;; 	    (overlay-priority))
;; 	(setq overlay-priority (overlay-get overlay 'priority))
;; 	(when (and
;;                (> overlay-priority max-so-far)
;;                (overlay-get overlay 'is-freex-embed))
;; 	  (setq max-so-far overlay-priority)
;; 	  (setq max-overlay overlay))
;; 	(setq overlays (cdr overlays))))

    max-overlay))

(defun freex-embed-is-max-overlay-at (ov &optional pos)
  "Return whether the provided overlay is the maximum overlay at
position.  Defaults to point."
  (when (not pos)
    (setq pos (point)))
  (let ((ov-pri (overlay-get ov 'priority))
	(max-pri (freex-embed-get-highest-priority-at pos)))
    (if (eq ov-pri max-pri)
	t
      nil)))

(defun freex-embed-remove-overlay-at-point ()
  "Remove the highest-priority overlay at the current point. You
currently must be inside the contents and not the link."
  (interactive)
  (let ((ov (freex-embed-get-max-overlay-at (point))))
    (when ov
      ;; remove the overlay, saving as you collapse
      (freex-embed-remove-overlay ov t))))


(defun freex-embed-remove-overlay-at-point-without-saving ()
  "Remove the highest-priority overlay at the current point. You
currently must be inside the contents and not the link."
  (interactive)
  (let ((ov (freex-embed-get-max-overlay-at (point))))
    (when ov
      ;; remove the overlay, saving as you collapse
      (freex-embed-remove-overlay ov nil))))


(defun freex-embed-revert-overlay-at-point ()
  "Removes the overlay at point without saving, which should
leave the source text, and then reinserts it. N.B. This won't
reinsert metadata overlays. Any ideas???"
  (interactive)
  ;; need to get the overlay at point, then store its start
  ;; and end, because by the time we need them, the overlay
  ;; won't exist any more
  (let* ((ov (freex-embed-get-max-overlay-at (point)))
         (start (overlay-start ov))
         (end (overlay-end ov)))
    (freex-embed-remove-overlay-at-point-without-saving)
    ;; removing the overlay will shrink the document, so we
    ;; may only need to embed as far as point-max
    (freex-embed-region start (min (point-max) end))))


(defun overlays-within (start end)
  "Find all the overlays that lie wholly within the region
BEG and END. Requires that the entire overlay lie within the
region, which is a much stricter criterion than OVERLAYS-IN
which includes any overlays that contain even a single
character within the region."

  (let ((ovs-in (overlays-in start end))
        (ovs-within))
    (dolist (ov ovs-in)
      (when (and
             (>= (overlay-start ov) start)
             (<= (overlay-end ov) end))
        (push ov ovs-within)))
    ovs-within))


(defun freex-embed-delete-outside-region (beg end &optional buf)
  "Deletes everything before BEG and after END, i.e. crops
the region. Think of it as a destructive NARROW-TO-REGION."
  (interactive "r")
  (when (not buf)
    (setq buf (current-buffer)))
  (save-excursion
    (set-buffer buf)
    (let ((m-beg (freex-make-marker-at (min beg end)))
          (m-end (freex-make-marker-at (max beg end))))
      (delete-region (point-min) m-beg)
      (delete-region m-end (point-max)))))


(defun freex-embed-overlays-in-or-within (start end within)
  "If WITHIN = t, calls OVERLAYS-WITHIN, otherwise it calls
OVERLAYS-IN."
  (if within
      (overlays-within start end)
    (overlays-in start end)))


(defun freex-embed-overlays-with-properties-in
  (start end properties &optional within)
  "Return a list of overlays within the range that match the set
of properties. Properties should be a list of property-value
lists, e.g. ((prop1 val1) (prop2 val2))."
  (let ((overlays (freex-embed-overlays-in-or-within start end within))
	(overlays-to-keep))
    ;; loop over possible overlays
    (dolist (overlay overlays)
      (let ((all-match t)
	    (test-properties properties))
	;; test overlay properties
	(while test-properties
          ;; cur-prop-tv = a (type val) property pair
	  (let ((cur-prop-tv (pop test-properties)))
	    (when (not (equal
                        ;; compare actual with desired property
                        (overlay-get overlay (pop cur-prop-tv))
                        (pop cur-prop-tv)))
              ;; one does not match, so skip remaining
              (setq all-match nil)
              (setq test-properties nil))))
	;; see if pass all tests
	(when all-match
          ;; all properties matched so add to overlays list
          (push overlay overlays-to-keep))))
      ;; advance the overlay
      ;; return the overlays that matched
      overlays-to-keep))


(defun freex-embed-delete-overlay-and-properties (overlay)
  "The DELETE-OVERLAY function is amazingly lame, since the
overlay continues to exist, including all of its
properties. For some reason, one of these 'deleted' overlays
seem to be hanging around, and getting passed to
freex-embed-copy-overlay. The best I can do right now is to get rid of
all an overlay's properties, and create an is-deleted
property set to t."
  ;; only do it if there is an overlay passed in. this handles the
  ;; case when a nil link-overlay is passed
  (when overlay
    (dolist (prop (overlay-properties overlay))
      (overlay-put overlay prop nil))
    (overlay-put overlay 'is-deleted t)
    (delete-overlay overlay)))


(defun freex-embed-set-freex-overlays-properties (props)
  "Set the properties of all freex overlays in the buffer to the
specified props."
  (let ((overlays
         (freex-embed-overlays-with-properties-in
          (point-min) ; start
          (point-max) ; end
          (list ; properties
           (list 'is-freex-embed t)))))
    (dolist (ov overlays)
      (dolist (prop props)
	(overlay-put ov (nth 0 prop) (nth 1 prop))))))
	    

(defun freex-embed-remove-overlay (ov save-each &optional leave-no-trace)
  "Remove the specified overlay, cleaning out all child
overlays. N.B. Since this just calls DELETE-OVERLAY, the overlay
actually continues to exist, but is no longer anchored to its
buffer, so it might as well be gone. If SAVE-EACH is non-nil,
then it will save each overlay as it collapses it. If
LEAVE-NO-TRACE, it will simply remove the overlay, but
default (nil) is to insert <file>blah.freex</file> in place of
the removed overlay."
  ;; find all the freex-embed overlays within OV's region that
  ;; have priority 1 higher than OV's priority, and then
  ;; squash them
  ;;
  ;; N.B. we don't need to specify overlays *within*, since
  ;; the (+ priority 1) takes care of that for us
  (let ((overlays
         (freex-embed-overlays-with-properties-in
          (overlay-start ov) ; start
          (overlay-end ov) ; end
          (list ; properties
           (list 'priority (+ (overlay-get ov 'priority) 1))
           (list 'is-freex-embed t))))
        (ov-beg (overlay-start ov))
        (source-text (overlay-get ov 'source-text))
        (modified-p (buffer-modified-p)))
    ;; remove all the overlays we found
    (dolist (child-ov overlays)
      (freex-embed-remove-overlay child-ov save-each))
    ;; eventually, check if it was modified and save it,
    ;; using the overlay's save function
    (when (and save-each (overlay-get ov 'is-modified))
      ;;(message (format "Saving %s" (overlay-get ov 'filename)))
      (freex-embed-save-overlay ov))
    ;; clear the region text
    (delete-region (overlay-start ov) (overlay-end ov))
    ;; remove the inserted text overlay
    (freex-embed-delete-overlay-and-properties ov)
    ;; re-insert the source-text
    (if leave-no-trace
        nil
      ;; by default, add <file>blah.freex</file>
      (save-excursion
        (goto-char ov-beg)
        (insert source-text)))
    ;; set buffer modified to nil if it was not already modified
    (set-buffer-modified-p modified-p)))



(defun freex-embed-remove-all-freex-embed-overlays (&optional save-each)
  "Remove all the freex-embed overlays from the buffer by
finding all the priority==0 overlays and removing them
one-by-one. If SAVE-EACH is true, will save each as it
closes them. Defaults to false."
  (interactive)
  ;; get the freex-embed overlays
  (let* ((overlays (freex-embed-overlays-with-properties-in
                    (point-min) (point-max)
                    '((is-freex-embed t) (priority 0))))
	 (modified-p (buffer-modified-p)))
    (dolist (ov overlays)
      ;; remove the text, saving as you go
      (freex-embed-remove-overlay ov save-each))
    ;; set buffer modified to nil if it was not already modified
    (set-buffer-modified-p modified-p)))


(defun freex-embed-copy-overlay (ov new-buf)
  "Return a copy of overlay OV that will live in buffer
NEW-BUF."
  (when (overlay-get ov 'is-deleted)
   (error "This overlay has been deleted"))
  (when (> (overlay-end ov) (+ (buffer-size new-buf) 1))
   (error "Destination buffer too small for this overlay"))
  (let ((ov-new (make-overlay
              (overlay-start ov) (overlay-end ov) new-buf))
        (props (overlay-properties ov)))
    (while props
        (overlay-put ov-new (pop props) (pop props)))
    ov-new))


(defun freex-embed-copy-content-overlays (src-buf dest-buf beg end)
  "Copy all the IS-FREEX-EMBED from SRC-BUF that are completely
contained within the region defined by BEG and END into the
current buffer."
  (save-current-buffer
    (set-buffer src-buf)
    (let ((freex-embed-file-overlays
           (freex-embed-overlays-with-properties-in
            beg end '((is-freex-embed t)) t)))
      (dolist (ff-ov freex-embed-file-overlays)
        (freex-embed-copy-overlay ff-ov dest-buf)))))


(defun freex-embed-clone-region-and-overlays
  (beg end ov-props)
  "This inserts the contents of the region of the current
buffer between BEG and END into a new buffer called NEW-NAME,
as well as all its freex-embed overlays. We can't use CLONE-BUFFER
since it irritatingly refuses to work on the current
buffer. Returns the new buffer. By default, sets the buffer
invisibly."
  ;; remember which buffer we're in now, and unwind back to
  ;; it at the end
  (save-current-buffer
    (let* ((old-buf (current-buffer))
          (new-buf (generate-new-buffer
                    (concat (buffer-name old-buf) ".save"))))
      (set-buffer new-buf)
      (insert-buffer old-buf)
      (freex-embed-copy-content-overlays old-buf new-buf beg end)
      (freex-embed-delete-outside-region beg end)
      ;; store the properties of the overlay that created
      ;; this as buffer properties
      (setq freex-embed-ov-props ov-props)
      new-buf)))


(defun freex-embed-clone-whole-buffer-and-freex-embed-overlays ()
  (interactive)
  (let ((visited-file-name (buffer-file-name))
        (new-buf
         (freex-embed-clone-region-and-overlays
          (point-min) (point-max)
          freex-embed-ov-props)))
    (save-current-buffer
      (set-buffer new-buf)
      (set-visited-file-name visited-file-name t))
    new-buf))


(defun freex-embed-clone-overlay (ov)
  "Clones the buffer (with overlays), then crops the region
outside the range of OV, leaving you with a new buffer
consisting solely of OV"
  (freex-embed-clone-region-and-overlays
   (overlay-start ov)
   (overlay-end ov)
   (overlay-properties ov)))


(defun freex-embed-save ()
   "Save the buffer, along with any embedded freex-embed
overlays. Designed to be added to write-contents-hook when
in freex-embed-mode. This is the highest-level save
function."
   (interactive)
   (let ((freex-embed-saving-p t)
         (id (plist-get freex-embed-ov-props 'id))
         (save-funct (plist-get freex-embed-ov-props 'save-funct)))
     (save-current-buffer
       ;; do all the saving etc. on the temporary buffer
       (set-buffer
	(freex-embed-clone-whole-buffer-and-freex-embed-overlays))
       ;; this will remove all the lowest-priority overlays,
       ;; recursively removing any overlays embedded in them,
       ;; and saving them as it goes along
       (freex-embed-remove-all-freex-embed-overlays t)
       ;; now we need to deal with our top-level temporary
       ;; buffer
       (freex-embed-write-buffer))

     ;; add a timestamp to say we saved this nugget
     (freex-timestamp-save)
     
     ;; it's possible that this buffer had just been
     ;; created. until it has been saved for the first time,
     ;; it wouldn't have an id in the database, and so it
     ;; won't have an id in freex-embed-ov-props.  now that
     ;; it has been saved and the index has been updated,
     ;; there will be an id for it, and we can add it to
     ;; freex-embed-ov-props
     ;;
     ;; however, it's possible that this is a temporary
     ;; buffer (e.g. created for embedded-tag-children) that
     ;; we don't want to save to file, and that doesn't have
     ;; an id. so, if it's using the null save-funct, don't
     ;; bother trying to get a filename for it
     (when (and (not id)
                (not (eq save-funct 'freex-embed-overlay-save-null)))

       ;; set the buffer's embed-ov-props id
       (setq freex-embed-ov-props
             (plist-put
              freex-embed-ov-props 'id
              ;; to the string nugid for this filename
              (number-to-string
               (freex-sqlalchemy-get-nugid-from-filename
                (file-name-nondirectory (buffer-file-name)))))))
       
     ;; update the implicit link regexp
     (freex-fontify-update-implicit-link-regexp)

     (dolist (hook freex-embed-save-hook)
       (eval (list hook)))

     ;; now deal with the user's actual buffer - we saved the
     ;; temp-buffer duplicate version of this
     ;; buffer, so we don't need to save it again here. just
     ;; mark it unmodified
     (clear-visited-file-modtime)
     (set-buffer-modified-p nil)
     (freex-embed-set-freex-overlays-properties 
      (list (list 'is-modified nil)))

     ;; has to return non-nil, so that write-contents-hook
     ;; knows that freex-embed-save has taken care of saving
     t))
   

(defun freex-embed-write-buffer ()
  "If the buffer is visiting a file, then save to that
file. otherwise, just call the save-funct stored in the
buffer-local freex-embed-ov-props, and it can figure out how to
save. it's up to the save-funct to declare the buffer
unmodified."
  ;; if the buffer's visiting a file, just save to that
  (let* ((save-funct (plist-get freex-embed-ov-props 'save-funct))
         (id (freex-embed-get-id-at-point)))
    (if save-funct
	;; run the save funct for that buffer
	(eval (list save-funct))

      ;; need to deal with the possibility that the
      ;; nugget doesn't yet exist in the db
      (when (not id)
        (let* ((fname (file-name-nondirectory (buffer-file-name)))
               (fname-no-ext (freex-sqlalchemy-remove-ext fname)))
          ;; create a new nugget, and get its id
          (setq id
                (freex-sqlalchemy-add-nugget
                 fname (buffer-string)))
          (dolist (hook freex-meta-add-nugget-hooks)
            (eval (list hook fname-no-ext)))))
      
      ;; decide which function to use for writing
      ;; content to
      (cond
       ((equal freex-content-storage "files")
        ;; run the standard save funct
        (save-buffer))

       ((equal freex-content-storage "db")
        ;; save the whole buffer to the db
        (freex-sqlalchemy-put-contents
         (freex-embed-get-id-at-point) (buffer-string)))

       ((equal freex-content-storage "mirror-files-to-db")
        ;; save to both the hd and the db
        (progn
          (save-buffer)
          (freex-sqlalchemy-put-contents
           (freex-embed-get-number-from id)
           ;;           (buffer-string))))
           (buffer-substring-no-properties (point-min) (point-max)))))

      (t
       (error "Unknown freex-content-storage option")))))

  ;; and kill this buffer (since we're always running
  ;; this on a cloned buffer, rather than the one the
  ;; user is editing)
  (kill-this-buffer))


(defun freex-embed-get-number-from (in)
  "string-to-number is really annoying, because it will error if
you feed it a number. This will run string-to-number if IN is a
string, otherwise it just returns IN. Either way, you get a
number back, without having to check what you started with. Oh,
and if IN is nil, it'll just return that."

  ;; this is what we're going to return
  (let ((ret nil))

    ;; if it's nil, just return nil
    (cond
     ((not in)
      (setq ret nil))
     
     ((stringp in)
      (setq ret (string-to-number in)))
     
     ((numberp in)
      (setq ret in)))
  
    ret))


(defun freex-embed-kill-buffer-no-query (&optional buf)
  "Kill buffer BUF without asking, even if it's modified. If
no BUF is specified, then assume current buffer."
  (interactive)
  (when (not buf)
    (setq buf (current-buffer)))
  (set-buffer buf)
  (set-buffer-modified-p nil)
  (kill-this-buffer))


(defun freex-embed-save-overlay (ov)
  "Run OV's save-funct."
  (let ((freex-embed-saving-p t))
    (save-current-buffer
      ;; clone the overlay, and then set the new cloned buffer
      ;; to be active
      (set-buffer (freex-embed-clone-overlay ov))
      ;; the cloned buffer (containing just our overlay OV)
      ;; knows how to save itself, since OV's properties are
      ;; stored in it in FREEX-EMBED-OV-PROPS
      (freex-embed-write-buffer))))


(defun freex-embed-save-overlay-at ()
  "Save the highest overlay at point."
  (interactive)
  (freex-embed-save-overlay (freex-embed-get-max-overlay-at)))



(defun freex-embed-overlay-save-null ()
  "Doesn't actually save anything, but it returns t, so the
write-contents-hooks knows that saving has been dealt
with. Set this as the save-funct for throwaway overlays."
  (set-buffer-modified-p nil)
  t)


(defun freex-has-extension (filename)
  "Returns a boolean for whether or not FILENAME has an
  extension. N.B. this uses the freex_sqlalchemy.py FSQA class
  variable file_ext to store/determine what the extension
  is. Defaults to 'freex'."
  (freex-sqlalchemy-has-ext filename))


(defun freex-remove-extension (filename)
  "Removes the filename extension from a filename if it's there,
otherwise just returns the filename."
  (if (freex-has-extension filename)
      (freex-sqlalchemy-remove-ext filename)
    filename))
  


(defun freex-embed-file-here ()
  "Embeds a file at point (completes to aliases), which
should get automatically fontified."
  (interactive)
  ;; ask the user to choose an alias (requires match)
  (let ((filename
         (freex-meta-complete-alias-filename "Embed: " t)))

    ;; don't prepend the freex-mode-dir, because i think
    ;; somehow the insert-element-at-point function deals
    ;; with that
    ;;
    ;;     (setq filename
    ;;           (concat (file-name-as-directory freex-mode-dir)
    ;;                   filename))


    ;; insert it the new way
    (freex-xml-insert-element-at-point "file" nil filename 
                                       'freex-embed-region)))
  
;; (let ((before-elements (freex-make-marker-at (point))))
;;     (insert (format "<file>%s</file>" filename))
;;     (freex-embed-file-element before-elements (point) filename)))


(defun freex-embed-before-revert ()
  "Gets added to the before-revert hook. Gets rid of all the
embedded overlays without saving, and unfontifies."

  ;; ;; turning off freex-mode caused all the overlays to be
  ;;   ;; saved before closing
  ;;   (turn-off-freex-mode))

  (freex-embed-remove-all-freex-embed-overlays nil)
  (freex-unhighlight-buffer))



(defun freex-embed-after-revert ()
  "Gets added to the after-revert hook."
;;  (freex-embed-buffer))
  (turn-on-freex-mode))


;; do we ever use this now that we have the
;; freex-embed-id-from-overlays??? 070217
;;
;; (defun freex-embed-get-filename-at-point ()
;;   "Returns the filename of the highest-priority overlay at
;; point. If we're not on an overlay, get the filename being
;; visited by the buffer (which may be nil)."
;;   ;; try and get the filename from the highest-priority
;;   ;; overlay at point
;;   (let ((filename (freex-embed-get-filename-from-overlays
;;                  (freex-embed-get-highest-priority-at))))
;;     ;; if we got a filename, then we're done - return it
;;     (if filename
;;         filename
;;       ;; if not, then try to get the filename from the
;;       ;; buffer's freex-embed-ov-props - return it, whether
;;       ;; it's nil or not
;;       (file-name-nondirectory (buffer-file-name)))))


(defun freex-embed-get-id-at-point ()
  "Returns the id of the highest-priority overlay at
point. If we're not on an overlay, get the id from the
buffer's freex-embed-ov-props (which may be nil for a scratch
buffer)."
  ;; try and get the id from the highest-priority
  ;; overlay at point
  (let ((id (freex-embed-get-id-from-overlays
                 (freex-embed-get-highest-priority-at))))
    ;; if we got an id, then we're done - return it
    (if id
        id
      ;; if not, then try to get the id from the buffer's
      ;; freex-embed-ov-props - return it, whether it's nil
      ;; or not
      (plist-get freex-embed-ov-props 'id))))

;; do we ever use this now that we have the
;; freex-embed-id-from-overlays??? 070217
;;
;; (defun freex-embed-get-filename-from-overlays (pri)
;;   "Try to get the filename from the overlay at point with
;; priority PRI or lower. If we succeed with the overlay of
;; priority PRI, return that. Otherwise, recursively keep
;; trying with lower and lower priorities. Terminate if we get
;; to the overlay of priority 0, in which case return nil."
;;   ;; if pri<0, then give up and just return nil, otherwise
;;   ;; try and get a filename from the overlay of priority PRI
;;   (if (< pri 0)
;;       nil
;;     (let* ((ov (freex-embed-get-overlay-of-priority pri))
;;            (filename (overlay-get ov 'filename)))
;;       ;; if we got a filename, then we're done - return it
;;       (if filename
;;           filename
;;         ;; otherwise, try getting a filename from the overlay
;;         ;; below - return that
;;         (freex-embed-get-filename-from-overlays (- pri 1))))))


(defun freex-embed-get-id-from-overlays (pri)
  "Try to get the id from the overlay at point with
priority PRI or lower. If we succeed with the overlay of
priority PRI, return that. Otherwise, recursively keep
trying with lower and lower priorities. Terminate if we get
to the overlay of priority 0, in which case return nil."
  ;; if pri<0, then give up and just return nil, otherwise
  ;; try and get a id from the overlay of priority PRI
  (if (< pri 0)
      nil
    (let* ((ov (freex-embed-get-overlay-of-priority pri))
           (id (overlay-get ov 'id)))
      ;; if we got a id, then we're done - return it
      (if id
          id
        ;; otherwise, try getting a id from the overlay
        ;; below - return that
        (freex-embed-get-id-from-overlays (- pri 1))))))


(defun freex-embed-define-region-as-nugget (beg end)
  "Create a new nugget, using BEG and END to define its
contents. Return the new overlay. The new nugget should inherit
the tag parents of its container nugget (i.e. the overlay or
buffer containing it)."
  (interactive "r")

  ;; save before trying to define the region as a nugget if
  ;; it's a brand new nugget without an entry in the
  ;; database
  (when (not (freex-embed-get-id-at-point))
    (save-buffer))

  ;; needs to be a let*, so it will prompt for the filename
  ;; before cutting the text
  (let* (
;;         (id (plist-get freex-embed-ov-props 'id))
         (id (freex-embed-get-id-at-point))         
         (cur-alias (freex-sqlalchemy-get-alias-from-nugid id))
         ;; get a new nugget name - will fail if the user
         ;; tries to reuse an existing name
         (new-filename (freex-meta-complete-alias-new nil nil cur-alias)))
    (freex-embed-define-named-region-as-nugget new-filename beg end)))


(defun freex-embed-define-named-region-as-nugget (new-filename beg end)
  "The meat of defining a new nugget.  This is called by
define-region-as-nugget once it has determined the filename."

  ;; needs to be a let*, so it will prompt for the filename
  ;; before cutting the text
  (let* (
        ;; will store the full path to the new filename
        (full-new-filename nil)

        ;; the contents of the new nugget
        (txt (freex-embed-cut-region beg end))

        ;; depending on your freex-content-storage settings,
        ;; you may or may not want to store TXT in your db,
        ;; so this will either be empty or equal to TXT
        (txt-to-store "")

        ;; get the nugid for whatever contains the region
        ;; we're about to define
        (container-nugid (freex-embed-get-id-at-point)))

    ;; if the user left the filename blank, create a random
    ;; string for the new filename
    (when (equal (length new-filename) 0)
      (setq new-filename (make-temp-name "")))

    ;; make sure the new filename is not already taken
    (when (freex-sqlalchemy-exist-nugget-a new-filename)
      (error "A nugget of that name already exists"))

    ;; get the full path and extension
    (setq full-new-filename
          (concat (file-name-as-directory freex-mode-dir)
                  new-filename "." freex-mode-ext))

    ;; create a temp buffer into which we can paste the new
    ;; nugget's contents, so that we can save it out to a
    ;; new file
    (with-temp-buffer
      (insert txt)

      ;; depending on how you're choosing to store things,
      ;; you may or may not want the content added to the db
      (cond
       ((equal freex-content-storage "files")
        (setq txt-to-store ""))
       ((equal freex-content-storage "db")
        (setq txt-to-store txt))
       ((equal freex-content-storage "mirror-files-to-db")
        (setq txt-to-store txt))
       (t
        (error "Unknown freex-content-storage option")))        
      (freex-sqlalchemy-add-nugget
        (concat new-filename "." freex-mode-ext) txt-to-store)

      ;; the new nugget we're creating should inherit the
      ;; tag parents of its container nugget
      (freex-sqlalchemy-put-tag-parents-delim-a
       ;; put into the new nugget
       new-filename
       ;; the tag-parents of the old one
       (freex-sqlalchemy-get-tag-parents-delim
        container-nugid))

      ;; in case there are any functions that the user
      ;; wants to run whenever a new nugget gets created
      (dolist (hook freex-meta-add-nugget-hooks)
        (eval (list hook new-filename)))

      (let ((freex-embed-saving-p t))
        ;; use the full path for writing the file
        (write-file full-new-filename)))

    ;; so now we're ready to add an embed element for our
    ;; new nugget in the containing nugget
    (freex-xml-insert-element-at-point
     "file" nil (concat new-filename "." freex-mode-ext)
     'freex-embed-region))

  ;; btw, no need to call
  ;; freex_fontify_update_implicit_link_regexp here because
  ;; freex-sqlalchemy does that in add-nugget

  (freex-embed-get-max-overlay-at beg))

;; i wanted to be able to say 'ask the user for a location,
;; and then create a link to whatever nugget is there', but
;; i couldn't figure out how to tell emacs to ask the user
;; interactively to move the point somewhere
;; (defun freex-embed-get-alias-at-point (p m)
;;   (interactive "r")
;;   (message (format "%i %i" p m)))
;; ;;  (freex-sqlalchemy-get-alias-from-nugid
;; ;;   (freex-embed-get-id-at-point)))
;;   (interactive)
;;   ;; (message "Put the mouse somewhere and press RET")
;;   ;; (wait-for-user-input "b"))
;;   ;; (read-char)
;;   (read-from-minibuffer "Put the mouse somewhere and press RET")
;;   (message (buffer-name)))


(defun freex-embed-all-tag-children ()
  "Hit C-= (i.e. Ctrl and + to create a temp buffer with all the
tag-(grand)children of this nugget (plus this nugget)
embedded. Closes this buffer."
  (interactive)
  (let* ((id (freex-embed-get-id-at-point))
         (alias (freex-sqlalchemy-get-alias-from-nugid id)))

    ;; kill the current buffer, saving first if necessary
    (when (buffer-modified-p)
      (freex-embed-save))
    (kill-this-buffer)

    (freex-meta-insert-embedded-tag-children-in-temp-buffer 
     (concat alias "//*"))

    ;; now scroll up to the top of our temporary buffer, and
    ;; embed the tag-parent itself
    ;;
    ;; xxx - i think i'd actually prefer this if it embedded
    ;; them in a temporary overlay in the same buffer... but
    ;; i don't remember whether we have code for creating
    ;; temporary buffers, and we haven't got any safeguards
    ;; for avoiding infinite regresses, so this is safer for
    ;; now
    (beginning-of-buffer)
    ;; e.g. <file>blah.freex</file>
    ;;
    ;; i think the right way to do this is with
    ;; freex-xml-insert-element-at-point...
    (insert (concat "<file>" alias "." freex-mode-ext "</file>\n"))
    (freex-embed-buffer))
    (set-buffer-modified-p nil))


(defun freex-embed-merge-into-parent ()
  "Cuts the text in the embedded nugget, pastes it into the
embedder, and deletes the embedded nugget/overlay. Only works for
embedded nuggets. May not work well if the embedded nugget itself
embeds a nugget."

  ;; todo
  ;;
  ;; when i remove the overlay, get rid of the <file>blah.freex</file>
  ;;
  ;; make it fail if the embedded overlay itself contains
  ;; any embedded overlays. in the future, it should just
  ;; merge these in first recursively

  (interactive)
  (let* ((id (freex-embed-get-id-at-point))
         (nugname (freex-sqlalchemy-get-alias-from-nugid id))
         (fname (freex-sqlalchemy-get-filename id))
         (ov (freex-embed-get-max-overlay-at))
         (beg (overlay-start ov))
         (end (overlay-end ov))
         (txt (buffer-substring-no-properties beg end)))
    ;; it doesn't matter whether we save or not, since we're
    ;; about to delete this nugget, but we might as well,
    ;; just in case
    (freex-embed-remove-overlay ov t t) ; don't save, leave no trace
    (message (format "Removing %s" nugname))
    (freex-sqlalchemy-remove-nugget id)
    (delete-file fname)
    (insert txt)))


(provide 'freex-embed)


