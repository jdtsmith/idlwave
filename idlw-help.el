;;; idlw-help.el --- Help code and topics for IDLWAVE
;; Copyright (c) 2000 Carsten Dominik
;; Copyright (c) 2001 J.D.Smith
;;
;; Author: Carsten Dominik <dominik@astro.uva.nl>
;; Maintainer: J.D. Smith <jdsmith@alum.mit.edu>
;; Version: 4.10

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The constants which contain the topics information for IDLWAVE's
;; online help feature.  This information is extracted automatically from
;; the IDL documentation.
;;
;; Created by get_rinfo on Tue Nov 27 16:14:14 2001
;; IDL version: 5.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(defvar idlwave-completion-help-info)
(defvar idlwave-help-use-dedicated-frame)
(defvar idlwave-help-frame-parameters)

(defvar idlwave-help-frame nil
  "The frame for display of IDL online help.")
(defvar idlwave-help-frame-width 102
  "The default width of the help frame.")

(defvar idlwave-help-file nil
  "The file containing the ASCII help for IDLWAVE.")

(defvar idlwave-help-topics nil
  "List of helptopics and byte positions in `idlw-help.txt'.")

(defvar idlwave-help-current-topic nil
  "The topic currently loaded into the IDLWAVE Help buffer.")

(defvar idlwave-help-mode-line-indicator ""
  "Used for the special mode line in the idlwave-help-mode.")

(defvar idlwave-help-window-configuration nil)
(defvar idlwave-help-name-translations nil)   ; defined by get_rinfo
(defvar idlwave-help-alt-names nil)           ; defined by get_rinfo
(defvar idlwave-help-special-topic-words)     ; defined by get_rinfo

(defvar idlwave-help-stack-back nil
  "Help topic stack for backwards motion.")
(defvar idlwave-help-stack-forward nil
  "Help topic stack for forward motion. 
Only gets populated when moving back.")


;; Define the key bindings for the Help application

(defvar idlwave-help-mode-map (make-sparse-keymap)
  "The keymap used in idlwave-help-mode.")

(define-key idlwave-help-mode-map "q" 'idlwave-help-quit)
(define-key idlwave-help-mode-map "w" 'widen)
(define-key idlwave-help-mode-map "\C-m" (lambda (arg)
					   (interactive "p")
					   (scroll-up arg)))
(define-key idlwave-help-mode-map "n" 'idlwave-help-next-topic)
(define-key idlwave-help-mode-map "p" 'idlwave-help-previous-topic)
(define-key idlwave-help-mode-map " " 'scroll-up)
(define-key idlwave-help-mode-map [delete] 'scroll-down)
(define-key idlwave-help-mode-map "b" 'idlwave-help-back)
(define-key idlwave-help-mode-map "f" 'idlwave-help-forward)
(define-key idlwave-help-mode-map "c" 'idlwave-help-clear-history)
(define-key idlwave-help-mode-map "o" 'idlwave-online-help)
(define-key idlwave-help-mode-map "*" 'idlwave-help-load-entire-file)
(define-key idlwave-help-mode-map "h" 'idlwave-help-find-header)
(define-key idlwave-help-mode-map "H" 'idlwave-help-find-first-header)
(define-key idlwave-help-mode-map "L" 'idlwave-help-activate-aggressively)
(define-key idlwave-help-mode-map "." 'idlwave-help-toggle-header-match-and-def)
(define-key idlwave-help-mode-map "F" 'idlwave-help-fontify)
(define-key idlwave-help-mode-map "\M-?" 'idlwave-help-return-to-calling-frame)
(define-key idlwave-help-mode-map "x" 'idlwave-help-return-to-calling-frame)

;; Define the menu for the Help application

(easy-menu-define
 idlwave-help-menu idlwave-help-mode-map
 "Menu for Help IDLWAVE system"
 '("IDLHelp"
   ["Open topic" idlwave-online-help t]
   ["History: Backward" idlwave-help-back t]
   ["History: Forward" idlwave-help-forward t]
   ["History: Clear" idlwave-help-clear-history t]
   "---"
   ["Follow Link" idlwave-help-follow-link (not idlwave-help-is-source)]
   ["Browse: Next Topic" idlwave-help-next-topic (not idlwave-help-is-source)]
   ["Browse: Previous Topic" idlwave-help-previous-topic
    (not idlwave-help-is-source)]
   ["Load Entire Help File" idlwave-help-load-entire-file t]
   "---"
   ["Definition <-> Help Text" idlwave-help-toggle-header-match-and-def
    idlwave-help-is-source]
   ["Find DocLib Header" idlwave-help-find-header idlwave-help-is-source]
   ["Find First DocLib Header" idlwave-help-find-first-header
    idlwave-help-is-source]
   ["Fontify help buffer" idlwave-help-fontify idlwave-help-is-source]
   "--"
   ["Quit" idlwave-help-quit t]))

(defun idlwave-help-mode ()
  "Major mode for displaying IDL Help.

This is a VIEW mode for the ASCII version of IDL Help files,
with some extras.  Its main purpose is speed - so don't
expect a fully hyper-linked help.

Scrolling:          SPC  DEL  RET
Topic Histrory:     [b]ackward   [f]orward
Topic Browsing:     [n]ext       [p]revious
Choose new Topic:   [o]pen
Follow Link:        Mouse button 2 finds help on word at point
Text Searches:      Inside Topic: Use Emacs search functions
                    Global:  Press `*' to load entire help file
Exit:               [q]uit or mouse button 3 will kill the frame

When the hep text is a source file, the following commands are available

Fontification:      [F]ontify the buffer like source code
Jump:               [h] to function doclib header
                    [H] to file doclib header
                    [.] back and forward between header and definition

Here are all keybindings.
\\{idlwave-help-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'idlwave-help-mode
	mode-name "IDLWAVE Help")
  (use-local-map idlwave-help-mode-map)
  (easy-menu-add idlwave-help-menu idlwave-help-mode-map)
  (setq truncate-lines t)
  (setq case-fold-search t)
  (setq mode-line-format
	(list ""
	      'mode-line-modified
	      'mode-line-buffer-identification
	      ":  " 'idlwave-help-mode-line-indicator
	      " -%-"))
  (setq buffer-read-only t)
  (set (make-local-variable 'idlwave-help-def-pos) nil)
  (set (make-local-variable 'idlwave-help-args) nil)
  (set (make-local-variable 'idlwave-help-in-header) nil)
  (set (make-local-variable 'idlwave-help-is-source) nil)
  (run-hooks 'idlwave-help-mode-hook))

(defvar idlwave-current-obj_new-class)
(defvar idlwave-help-diagnostics)
(defvar idlwave-experimental)
(defvar idlwave-last-context-help-pos)
(defun idlwave-do-context-help (&optional arg)
  "Wrapper around the call to idlwave-context-help1.
It collects and pronts the diagnostics messages."
  (let ((marker (list (current-buffer) (point)))
	(idlwave-help-diagnostics nil))
    ;; Check for frame switching.  When the command is invoked twice
    ;; at the same position, we try to switch to the help frame
    ;; FIXME:  Frame switching works only on XEmacs
    (if (and idlwave-experimental
	     (equal last-command this-command)
	     (equal idlwave-last-context-help-pos marker))
	(idlwave-help-select-help-frame)
      ;; Do the real thing.
      (setq idlwave-last-context-help-pos marker)
      (idlwave-do-context-help1 arg)
      (if idlwave-help-diagnostics
	  (message "%s" (mapconcat 'identity 
				   (nreverse idlwave-help-diagnostics)
				   "; "))))))


(defun idlwave-do-context-help1 (&optional arg)
  "The work-horse version of `idlwave-context-help', which see."
  (save-excursion
    (if (equal (char-after) ?/) 
	(forward-char 1)
      (if (equal (char-before) ?=)
	  (backward-char 1)))
    (let* ((idlwave-query-class nil)
	   (idlwave-force-class-query (equal arg '(4)))
	   (chars "a-zA-Z0-9_$.!")
	   (beg (save-excursion (skip-chars-backward chars) (point)))
	   (end (save-excursion (skip-chars-forward chars) (point)))
	   (this-word (buffer-substring beg end))
	   (st-ass (assoc (downcase this-word) idlwave-help-special-topic-words))
	   module keyword cw mod1 mod2 mod3)
      (if (or arg 
	      (and (not (member (string-to-char this-word) '(?! ?.)))
		   (not st-ass)))
	  ;; Need the module information
	  (progn
	    (setq module (idlwave-what-module-find-class)
		  cw (nth 2 (idlwave-where)))
	    ;; Correct for OBJ_NEW, we may need an INIT method instead.
	    (if (equal (idlwave-downcase-safe (car module)) "obj_new")
		(let* ((bos (save-excursion (idlwave-beginning-of-statement)
					    (point)))
		       (str (buffer-substring bos (point))))
		  (if (string-match "OBJ_NEW([ \t]*['\"]\\([a-zA-Z][a-zA-Z0-9$_]+\\)['\"]"
				    str)
		      (setq module (list "init" 'fun (match-string 1 str))
			    idlwave-current-obj_new-class (match-string 1 str))
		    )))))
      (cond (arg (setq mod1 module))
	    (st-ass (setq mod1 (list (or (cdr st-ass) (car st-ass)) 
				     nil nil nil)))
	    ((string-match "\\`![a-zA-Z0-9_]+" this-word)
	     ;; A system variable
	     (setq mod1 (list "system variables" nil nil
			      (match-string 0 this-word))))
	    ((string-match "^\\." this-word)
	     ;; An executive command
	     (setq mod1 (list this-word nil nil nil)))
	    ((and (eq cw 'class)
		  (or (idlwave-in-quote)  ; e.g. obj_new
		      (re-search-backward "\\<inherits[ \t]+[A-Za-z0-9_]*\\="
					  (max (point-min) (- (point) 40)) t))
		  )
	     ;; Class completion insite string delimiters should be
	     ;; the class inside OBJ_NEW.
	     ;; FIXME: Do we really need the in-quotes restriction?
	     ;; This is why it does not work after INHERITS.
	     (setq mod1 (list nil nil this-word nil)))
	    ((and (memq cw '(function-keyword procedure-keyword))
		  (stringp this-word)
		  (string-match "\\S-" this-word)
		  (not (string-match "!" this-word)))
	     (cond ((or (= (char-before beg) ?/)
			(save-excursion (goto-char end)
					(looking-at "[ \t]*=")))
		    ;; Certainly a keyword. Check for abbreviation etc.
		    (setq keyword (idlwave-expand-keyword this-word module))
		    (cond
		     ((null keyword)
		      (idlwave-help-diagnostics
		       (format "%s does not accept `%s' kwd"
			       (idlwave-make-full-name (nth 2 module)
						       (car module))
			       (upcase this-word))
		       'ding))
		     ((consp keyword)
		      (idlwave-help-diagnostics
		       (format "%d matches for kwd abbrev `%s'"
			       (length keyword) this-word)
		       'ding)
		      ;; We continue anyway with the first match...
		      (setq keyword (car keyword))))
		    (setq mod1 (append module (list keyword)))
		    (setq mod2 module))
		   ((equal (char-after end) ?\()
		    ;; A function - what-module will have caught this
		    (setq mod1 module))
		   (t
		    ;; undecided - try function, keyword, then enclosing mod.
		    ;; Check for keyword abbreviations, but do not report
		    ;; errors, because it might something else.
		    ;; FIXME: is this a good way to handle this?
		    (setq keyword (idlwave-expand-keyword this-word module))
		    (if (consp keyword) (setq keyword (car keyword)))
		    (setq mod1 (append module (list keyword))
			  mod2 (list this-word 'fun nil)
			  mod3 module))))
	    (t
	     (setq mod1 module)))
      (if mod3
	  (condition-case nil
	      (apply 'idlwave-online-help nil mod1)
	    (error (condition-case nil
		       (apply 'idlwave-online-help nil mod2)
		     (error (apply 'idlwave-online-help nil mod3)))))
	(if mod2
	    (condition-case nil
		(apply 'idlwave-online-help nil mod1)
	      (error (apply 'idlwave-online-help nil mod2)))
	  (if mod1
	      (apply 'idlwave-online-help nil mod1)
	    (error "Don't know which routine to show help for.")))))))

(defvar idlwave-extra-help-function)
(defun idlwave-do-mouse-completion-help (ev)
  "Display online help on n item in the *Completions* buffer.
Need additional info stored in `idlwave-completion-help-info'."
  (let* ((cw (selected-window))
	 (info idlwave-completion-help-info)
	 (what (nth 0 info))
	 (name (nth 1 info))
	 (type (nth 2 info))
	 (class (nth 3 info))
	 (need-class class)
	 (kwd (nth 4 info))
	 (sclasses (nth 5 info))
	 word)
    (mouse-set-point ev)
    (setq word (idlwave-this-word))
    (select-window cw)
    (cond ((memq what '(procedure function routine))
	   (setq name word)
	   (if (or (eq class t)
		   (and (stringp class) sclasses))
	       (let* ((classes (idlwave-all-method-classes
			       (idlwave-sintern-method name)
			       type)))
		 (if sclasses
		     (setq classes (idlwave-members-only 
				    classes (cons class sclasses))))
		 (if (not idlwave-extra-help-function)
		     (setq classes (idlwave-grep-help-topics classes)))
		 (setq class (idlwave-help-popup-select ev classes))))
	   (if (stringp class)
	       (setq class (idlwave-find-inherited-class
			    (idlwave-sintern-routine-or-method name class)
			    type (idlwave-sintern-class class)))))
	  ((eq what 'keyword)
	   (setq kwd word)
	   (if (or (eq class t)
		   (and (stringp class) sclasses))
	       (let ((classes  (idlwave-all-method-keyword-classes
				(idlwave-sintern-method name)
				(idlwave-sintern-keyword kwd)
				type)))
		 (if sclasses
		     (setq classes (idlwave-members-only 
				    classes (cons class sclasses))))
		 (if (not idlwave-extra-help-function)
		     (setq classes (idlwave-grep-help-topics classes)))
		 (setq class (idlwave-help-popup-select ev classes))))
	   (if (stringp class)
	       (setq class (idlwave-find-inherited-class
			    (idlwave-sintern-routine-or-method name class)
			    type (idlwave-sintern-class class)))))
	  ((eq what 'class)
	   (setq class word))
	  ((and (symbolp what)  ;; FIXME:  document this.
		(fboundp what))
	   (funcall what 'set word))
	  (t (error "Cannot help with this item")))
    (if (and need-class (not class))
	(error "Cannot help with this item"))
    (idlwave-online-help nil name type class kwd)))

(defvar idlwave-highlight-help-links-in-completion)
(defun idlwave-highlight-linked-completions ()
  "Highlight all completions for which help is available.
`idlwave-help-link-face' is used for this."
  (if idlwave-highlight-help-links-in-completion      
      (save-excursion
	(set-buffer (get-buffer "*Completions*"))
	(save-excursion
	  (let* ((buffer-read-only nil)
		 (case-fold-search t)
		 (props (list 'face 'idlwave-help-link-face))
		 (info idlwave-completion-help-info)
		 (what (nth 0 info))
		 (name (nth 1 info))
		 (type (nth 2 info))
		 (class (nth 3 info))
		 ;; (kwd (nth 4 info))
		 (sclasses (nth 5 info))
		 (kwd-doit
		  (and (eq what 'keyword)
		       (if (equal (idlwave-downcase-safe name) "obj_new")
			   (idlwave-is-help-topic
			    (idlwave-make-full-name
			     idlwave-current-obj_new-class "Init"))
			 (idlwave-is-help-topic
			  (idlwave-make-full-name class name)))))
		 word beg end doit)
	    (goto-char (point-min))
	    (re-search-forward "possible completions are:" nil t)
	    (while (re-search-forward "\\s-\\([A-Za-z0-9_]+\\)\\(\\s-\\|\\'\\)"
				      nil t)
	      (setq beg (match-beginning 1) end (match-end 1)
		    word (match-string 1) doit nil)
	      (cond
	       ((eq what 'class)
		(setq doit (idlwave-is-help-topic word)))
	       ((memq what '(procedure function routine))
		(if (eq class t)
		    (setq doit (idlwave-any-help-topic 
				(idlwave-all-method-classes
				 (idlwave-sintern-method word) type)))
		  (if sclasses
		      (setq doit (idlwave-any-help-topic
				  (mapcar (lambda (x)
					    (idlwave-make-full-name x word))
					  (idlwave-members-only
					   (idlwave-all-method-classes
					    (idlwave-sintern-method word) type)
					   (cons class sclasses)))))
		    (setq doit (idlwave-is-help-topic
				(idlwave-make-full-name class word))))))
	       ((eq what 'keyword)
		(if (eq class t)
		    (setq doit (idlwave-any-help-topic
				(idlwave-all-method-classes
				 (idlwave-sintern-method name) type)))
		  (if sclasses
		      (setq doit (idlwave-any-help-topic
				  (mapcar 
				   (lambda (x)
				     (idlwave-make-full-name x name))
				   (idlwave-members-only
				    (idlwave-all-method-keyword-classes
				     (idlwave-sintern-method name)
				     (idlwave-sintern-keyword word)
				     type)
				    (cons class sclasses)))))
		    (setq doit kwd-doit))))
	       ((and (symbolp what) ; FIXME: document this.
		     (fboundp what))
		(setq doit (funcall what 'test word))))
	      (if doit
		  (add-text-properties beg end props))
	      (goto-char end)))))))

;; Arrange for this function to be called after completion
(add-hook 'idlwave-completion-setup-hook
	  'idlwave-highlight-linked-completions)

(defvar idlwave-help-return-frame nil
  "The frame to return to from the help frame.")

(defun idlwave-help-quit ()
  "Exit IDLWAVE Help buffer.  Kill the dedicated frame if any."
  (interactive)
  (cond ((and idlwave-help-use-dedicated-frame
	      (eq (selected-frame) idlwave-help-frame))
	 (if (and idlwave-experimental
		  (frame-live-p idlwave-help-return-frame))
	     ;; Try to select the return frame.
	     ;; This can crash on slow network connections, obviously when
	     ;; we kill the help frame before the return-frame is selected.
	     ;; To protect the workings, we wait for up to one second 
	     ;; and check if the return-frame *is* now selected.
	     ;; This is marked "eperimental" since we are not sure when its OK.
	     (let ((maxtime 1.0) (time 0.) (step 0.1))
	       (select-frame idlwave-help-return-frame)
	       (while (and (sit-for step)
			   (not (eq (selected-frame) idlwave-help-return-frame))
			   (< (setq time (+ time step)) maxtime)))))
	 (delete-frame idlwave-help-frame))
	((window-configuration-p idlwave-help-window-configuration)
	 (set-window-configuration idlwave-help-window-configuration)
	 (select-window (previous-window)))
	(t (kill-buffer (idlwave-help-get-help-buffer)))))

(defun idlwave-help-follow-link (ev)
  "Try the word at point as a help topic.  If positive, display topic."
  (interactive "e")
  (mouse-set-point ev)
  (let* ((beg (or (previous-single-property-change (1+ (point))
						   'idlwave-help-link)
		  (point-min)))
	 (end (or (next-single-property-change (point) 'idlwave-help-link)
		  (point-max)))
	 (this-word (downcase (buffer-substring beg end)))
	 (ass (assoc this-word idlwave-help-special-topic-words))
	 (topic (if ass (or (cdr ass) (car ass)) this-word)))
    (cond ((idlwave-is-help-topic topic)
	   (idlwave-online-help
	    (idlwave-help-maybe-translate topic)))
	  ((string-match "::" this-word)
	   (let* ((l (split-string this-word "::"))
		  (class (car l))
		  (method (nth 1 l)))
	     (idlwave-online-help nil method nil class)))
	  (t
	   (error "Cannot find help for \"%s\"" this-word)))))

(defun idlwave-help-next-topic ()
  "Select next topic in the physical sequence in the Help file."
  (interactive)
  (if (stringp idlwave-help-current-topic)
      (let* ((topic (car (car (cdr (memq (assoc idlwave-help-current-topic 
						idlwave-help-topics)
					 idlwave-help-topics))))))
	(if topic
	    (idlwave-online-help topic)
	  (error "Already in last topic")))
    (error "No \"next\" topic")))

(defun idlwave-help-previous-topic ()
  "Select previous topic in the physical sequence in the Help file."
  (interactive)
  (if (stringp idlwave-help-current-topic)
      (let* ((topic (car (nth (- (length idlwave-help-topics)
				 (length (memq (assoc idlwave-help-current-topic 
						      idlwave-help-topics)
					       idlwave-help-topics))
				 1)
			      idlwave-help-topics))))
	(if topic
	    (idlwave-online-help topic)
	  (error "Already in first topic")))
    (error "No \"previous\" topic")))

(defun idlwave-help-back ()
  "Select previous topic as given by help history stack."
  (interactive)
  (if idlwave-help-stack-back
      (let* ((back idlwave-help-stack-back)
	     (fwd idlwave-help-stack-forward)
	     (goto (car back)))
	(setq back (cdr back))
	(setq fwd (cons (cons idlwave-help-current-topic (window-start)) fwd))
	(if (consp (car goto))
	    (apply 'idlwave-online-help nil (car goto))
	  (idlwave-online-help (car goto)))
	(set-window-start (selected-window) (cdr goto))
	(setq idlwave-help-stack-forward fwd
	      idlwave-help-stack-back back))
    (error "Cannot go back any further in history")))

(defun idlwave-help-forward ()
  "Select next topic as given by help history stack.
Only accessible if you have walked back with `idlwave-help-back' first."
  (interactive)
  (if idlwave-help-stack-forward
      (let* ((back idlwave-help-stack-back)
	     (fwd idlwave-help-stack-forward)
	     (goto (car fwd)))
	(setq fwd (cdr fwd))
	(setq back (cons (cons idlwave-help-current-topic (window-start)) back))
	(if (consp (car goto))
	    (apply 'idlwave-online-help nil (car goto))
	  (idlwave-online-help (car goto)))
	(set-window-start (selected-window) (cdr goto))
	(setq idlwave-help-stack-forward fwd
	      idlwave-help-stack-back back))
    (error "Cannot go forward any further in history")))

(defun idlwave-help-clear-history ()
  "Clear the history."
  (interactive)
  (setq idlwave-help-stack-back nil
	idlwave-help-stack-forward nil))

(defun idlwave-help-load-entire-file ()
  "Load the entire help file for global searches."
  (interactive)
  (let ((buffer-read-only nil))
    (idlwave-help-load-topic "***")
    (message "Entire Help file loaded")))

(defun idlwave-find-help (class1 routine1 keyword1)
  "Find help corresponding to the arguments."
  (let ((search-list (idlwave-help-make-search-list class1 routine1 keyword1))
	class routine keyword topic
	entry pre-re pos-re found kwd-re
	pos-p not-first)

    (save-excursion
      (set-buffer (idlwave-help-get-help-buffer))
      ;; Loop over all possible search compinations
      (while (and (not found)
		  (setq entry (car search-list)))
	(setq search-list (cdr search-list))
	(catch 'next
	  (setq class (nth 0 entry)
		routine (nth 1 entry)
		keyword (nth 2 entry))
	
	  ;; The [XYZ] keywords need a special search strategy
	  (if (and keyword (string-match "^[xyz]" keyword))
	      (setq kwd-re (format "\\(%s\\|\\[[xyz]+\\]\\)%s"
				   (substring keyword 0 1)
				   (substring keyword 1)))
	    (setq kwd-re keyword))
	
	  ;; Determine the topic, and the regular expressions for narrowing and
	  ;; window start during display.
	  (setq topic (if class
			  (if routine (concat class "::" routine) class)
			routine))
	  (setq pre-re nil pos-re nil found nil)
	  (setq pos-p nil)
	  (cond ((and (stringp keyword) (string-match "^!" keyword))
		 ;; A system keyword
		 (setq pos-re (concat "^[ \t]*"
				      "\\(![a-zA-Z0-9_]+ *, *\\)*"
				      keyword
				      "\\( *, *![a-zA-Z0-9_]+ *\\)*"
				      " *\\([sS]ystem +[vV]ariables?\\)?"
				      "[ \t]*$")))
		((and class routine)
		 ;; A class method
		 (if keyword 
		     (setq pos-re (concat
				   "^ *"
				   kwd-re
				   " *\\(( *\\(get *, *set\\|get\\|set\\) *)\\)?"
				   " *$"))))
		(routine
		 ;; A normal routine
		 (if keyword 
		     (setq pre-re "^ *keywords *$"
			   pos-re (concat
				   "^ *"
				   kwd-re
				   " *$"))))
		(class
		 ;; Just a class
		 (if keyword 
		     (setq pre-re "^ *keywords *$"
			   pos-re (concat
				   "^ *"
				   kwd-re
				   " *\\(( *\\(get *, *set\\|get\\|set\\) *)\\)?"
				   " *$")))))
	  ;; Load the correct help topic into this buffer
	  (widen)
	  (if (not (equal topic idlwave-help-current-topic))
	      ;; The last topic was different - load the new one.
	      (let ((buffer-read-only nil))
		(or (idlwave-help-load-topic topic)
		    (throw 'next nil))))
	  (goto-char (point-min))

	  ;; Position cursor and window start.
	  (if pre-re
	      (re-search-forward pre-re nil t))
	  (if (and pos-re
		   (setq pos-p (re-search-forward pos-re nil t)))
	      (progn (goto-char (match-beginning 0))))
	  ;; Determine if we found what we wanted
	  (setq found (if pos-re
			  pos-p
			(not not-first)))
	  (setq not-first t)))
      (if found
	  (point)
	(or idlwave-help-use-dedicated-frame
	    (idlwave-help-quit))
	nil))))

(defvar default-toolbar-visible-p)
(defvar idlwave-help-activate-links-aggressively)
(defun idlwave-help-display-help-window (pos &optional nolinks)
  "Display the help window and move window start to POS.
See `idlwave-help-use-dedicated-frame'."
  (let ((cw (selected-window))
	(buf (idlwave-help-get-help-buffer)))
    (if (and window-system idlwave-help-use-dedicated-frame)
	(progn
	  ;; Use a special frame for this
	  (if (frame-live-p idlwave-help-frame)
	      nil
	    (setq idlwave-help-frame
		  (make-frame idlwave-help-frame-parameters))
	    ;; Strip menubar (?) and toolbar from the Help frame.
	    (if (fboundp 'set-specifier)
		(progn
		  ;; XEmacs
		  (let ((sval (cons idlwave-help-frame nil)))
		    ;; (set-specifier menubar-visible-p sval)
		    (set-specifier default-toolbar-visible-p sval)))
	      ;; Emacs
	      (modify-frame-parameters idlwave-help-frame
				       '(;;(menu-bar-lines . 0)
					 (tool-bar-lines . 0)))))
	  ;; We should use display-buffer here, but there are problems on Emacs
	  (select-frame idlwave-help-frame)
	  (switch-to-buffer buf))
      ;; Do it in this frame and save the window configuration
      (if (not (get-buffer-window buf nil))
	  (setq idlwave-help-window-configuration 
		(current-window-configuration)))
      (display-buffer buf nil (selected-frame))
      (select-window (get-buffer-window buf)))
    (raise-frame)
    (goto-char pos)
    (recenter 0)
    (if nolinks
	nil
      (idlwave-help-activate-see-also)
      (idlwave-help-activate-methods)
      (idlwave-help-activate-class)
      (if idlwave-help-activate-links-aggressively
	  (idlwave-help-activate-aggressively)))
    (select-window cw)))


(defun idlwave-help-select-help-frame ()
  "Select the help frame."
  (if (and (frame-live-p idlwave-help-frame)
	   (not (eq (selected-frame) idlwave-help-frame)))
      (progn
	(setq idlwave-help-return-frame (selected-frame))
	(select-frame idlwave-help-frame))))
(defun idlwave-help-return-to-calling-frame ()
  "Select the frame from which the help frame was selected."
  (interactive)
  (if (and (frame-live-p idlwave-help-return-frame)
	   (not (eq (selected-frame) idlwave-help-return-frame)))
      (select-frame idlwave-help-return-frame)))

(defvar idlwave-help-is-source)
(defun idlwave-help-load-topic (topic)
  "Load topic TOPIC into the current buffer."
  (setq idlwave-help-is-source nil)
  (let* ((entry (assoc topic idlwave-help-topics))
	 beg end)
    (if (equal topic "***")
	;; Make it load the whole file
	(setq entry (cons t nil)))
    (if entry
	(progn
	  (setq beg (cdr entry)
		end (cdr (car (cdr (memq entry idlwave-help-topics)))))
	  (erase-buffer)
	  (setq idlwave-help-current-topic topic)
	  (setq idlwave-help-mode-line-indicator (upcase topic))
	  (insert-file-contents idlwave-help-file nil beg end)
	  (set-buffer-modified-p nil)
	  t)
      nil)))

(defvar idlwave-extra-help-function)
(defun idlwave-online-help (topic &optional name type class keyword)
  "Display help on a certain topic.
Note that the topics are the section headings in the IDL documentation.
Thus the right topic may not always be easy to guess."
  (interactive (list (completing-read "Topic: " idlwave-help-topics)))
  (let ((last-topic idlwave-help-current-topic)
	(last-ws (window-start (get-buffer-window "*IDLWAVE Help*" t))))
    ;; Push the current topic on the history stack
    (if last-topic
	(progn
	  (if (equal last-topic (car (car idlwave-help-stack-back)))
	      (setcdr (car idlwave-help-stack-back) (or last-ws 1))
	    (setq idlwave-help-stack-back
		  (cons (cons last-topic (or last-ws 1))
			idlwave-help-stack-back)))))
    (if (> (length idlwave-help-stack-back) 20)
	(setcdr (nthcdr 17 idlwave-help-stack-back) nil))
    (setq idlwave-help-stack-forward nil)
    (if topic
	;; A specific topic
	(progn
	  (save-excursion
	    (set-buffer (idlwave-help-get-help-buffer))
	    (let ((buffer-read-only nil))
	      (idlwave-help-load-topic (downcase topic))))
	  (idlwave-help-display-help-window 0))
      ;; Find the right topic and place
      (if idlwave-extra-help-function
	  (condition-case nil
	      (idlwave-routine-info-help name type class keyword)
	    (error
	     (idlwave-help-get-special-help name type class keyword)))
	(idlwave-routine-info-help name type class keyword)))))

(defun idlwave-routine-info-help (routine type class &optional keyword)
  "Show help about KEYWORD of ROUTINE in CLASS.  TYPE is currently ignored.
When CLASS is nil, look for a normal routine.
When ROUTINE is nil, display the info about the entire class.
When KEYWORD is non-nil, position window start at the description of that
keyword, but still have the whole topic in the buffer."
  (let ((cw (selected-window))
	(help-pos (idlwave-find-help class routine keyword)))
    (if help-pos
	(idlwave-help-display-help-window help-pos)
      (idlwave-help-error routine type class keyword))
    (select-window cw)))

(defun idlwave-help-get-special-help (name type class keyword)
  "Call the function given by `idlwave-extra-help-function'."
  (let* ((cw (selected-window))
	 (idlwave-min-frame-width nil)
	 (help-pos (save-excursion
		     (set-buffer (idlwave-help-get-help-buffer))
		     (let ((buffer-read-only nil))
		       (funcall idlwave-extra-help-function 
				name type class keyword)))))
    (if help-pos
	(progn
	  (setq idlwave-help-current-topic (list name type class keyword))
	  (idlwave-help-display-help-window help-pos 'no-links)
	  ;; Check if we have to widen the frame
	  (if (and (integerp idlwave-min-frame-width)
		   (framep idlwave-help-frame)
		   (< (frame-width idlwave-help-frame) idlwave-min-frame-width))
	      (set-frame-width idlwave-help-frame  idlwave-min-frame-width)))
      (setq idlwave-help-current-topic nil)
      (idlwave-help-error name type class keyword))
    (select-window cw)))

(defvar idlwave-min-frame-width)
(defvar idlwave-help-def-pos)
(defvar idlwave-help-args)
(defvar idlwave-help-in-header)
(defvar idlwave-help-is-source)
(defvar idlwave-help-fontify-source-code)
(defvar idlwave-help-source-try-header)

(defun idlwave-help-with-source (name type class keyword)
  "Provide help for routines not documented in the IDL manual.
Works by loading the routine source file into the help buffer.
Depending on the value of `idlwave-help-source-try-header', it shows
the routine definition or the header description.

This function can be used as `idlwave-extra-help-function'."
  (let* ((entry (idlwave-best-rinfo-assoc
		 name (or type t) class (idlwave-routines)))
	 (case-fold-search t)
	 (file (cdr (nth 3 entry)))
	 header-pos def-pos)
    (setq idlwave-help-def-pos nil)
    (setq idlwave-help-args (list name type class keyword))
    (setq idlwave-help-in-header nil)
    (setq idlwave-help-is-source t)
    (if (stringp file)
	(progn
	  (setq file (idlwave-expand-lib-file-name file))
	  (if (and (> (buffer-size) 3)
		   (equal file (get-text-property 1 :idlwave-file))
		   nil)   ;; force reloading the file each time
	                  ;; FIXME: store and check the modification time?
	      nil ; Do nothing: this is already the correct file
	    ;; load the file
	    (if (file-exists-p file)
		(progn
		  (erase-buffer)
		  (insert-file-contents file nil nil nil 'replace))
	      (idlwave-help-error name type class keyword))
	    (if idlwave-help-fontify-source-code
		(idlwave-help-fontify))))
      (idlwave-help-error name type class keyword))
    (setq idlwave-help-mode-line-indicator file)
    (put-text-property 1 2 :idlwave-file file)

    ;; Try to find a good place to display
    ;; First, find the definition
    (setq def-pos (idlwave-help-find-routine-definition
		   name type class keyword))
    (setq idlwave-help-def-pos def-pos)

    (if idlwave-help-source-try-header
	;; Check if we can find the header
	(save-excursion
	  (goto-char (or def-pos (point-max)))
	  (setq header-pos (idlwave-help-find-in-doc-header
			    name type class keyword 'exact)
		idlwave-help-in-header header-pos)))

    (if (or header-pos def-pos)
	(progn 
	  (if (boundp 'idlwave-min-frame-width)
	      (setq idlwave-min-frame-width 80))
	  (goto-char (or header-pos def-pos)))
      (idlwave-help-error name type class keyword))

    (point)))


;; FIXME: Should use type here.
(defun idlwave-help-find-routine-definition (name type class keyword)
  "Find the definition of routine CLASS::NAME in current buffer.
TYPE and KEYWORD are ignored.
Returns hte point of match if successful, nil otherwise."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward 
	 (concat "^[ \t]*\\(pro\\|function\\)[ \t]+"
		 (regexp-quote (downcase (idlwave-make-full-name class name)))
		 "[, \t\r\n]")
	 nil t)
	(match-beginning 0)
      nil)))

(defvar idlwave-doclib-start)
(defvar idlwave-doclib-end)

(defun idlwave-help-find-in-doc-header (name type class keyword
					     &optional exact)
  "Find the requested help in the doc-header above point.
First checks if there is a doc-lib header which describes the correct routine.
Then tries to find the KEYWORDS section and the KEYWORD, if given.
Returns the point which should be window start of the help window.
If EXACT is non-nil, the full help position must be found - down to the
keyword requested.  This setting is for context help, if the exact
spot is needed.
If EXACT is nil, the position of the header is returned if it
describes the correct routine - even if the keyword description cannot
be found.
TYPE is ignored.

This function expects a more or less standard routine header.  In
particlar it looks for the `NAME:' tag, either with a colon, or alone
on a line.  Then `NAME:' must be followed by the routine name on the
same or the next line.  
When KEYWORD is non-nil, looks first for a `KEYWORDS' section.  It is
amazing how inconsisten this is through some IDL libraries I have
seen.  We settle for a line containing an upper case \"KEYWORD\"
string.  If this line is not fould we search for the keyword anyway to
increase the hit-rate

When one of these sections exists we check for a line starting with any of

  /KEYWORD  KEYWORD-  KEYWORD=  KEYWORD

with spaces allowed between the keyword and the following dash or equal sign.
If there is a match, we assume it is the keyword description."
  (let* ((case-fold-search t)
	 ;; NAME tag plus the routine name.  The new version is from JD.
	 (name-re (concat 
		   "\\(^;+\\*?[ \t]*name\\([ \t]*:\\|[ \t]*$\\)[ \t]*\\(\n;+[ \t]*\\)*"
		   (if (stringp class)
		       (concat "\\(" (regexp-quote (downcase class))
			       "::\\)?")
		     "")
		   (regexp-quote (downcase name))
		   "\\>\\)"
		   "\\|"
		   "\\(^;+[ \t]*"
		   (regexp-quote (downcase name))
		   ":[ \t]*$\\)"))
;	 (name-re (concat 
;		   "\\(^;+\\*?[ \t]*name\\([ \t]*:\\|[ \t]*$\\)[ \t]*\\(\n;+[ \t]*\\)?"
;		   (if (stringp class)
;		       (concat "\\(" (regexp-quote (downcase class))
;			       "::\\)?")
;		     "")
;		   (regexp-quote (downcase name))
;		   "\\>"))
	 ;; Header start plus name
	 (header-re (concat "\\(" idlwave-doclib-start "\\).*\n"
			    "\\(^;+.*\n\\)*"
			    "\\(" name-re "\\)"))
	 ;; A keywords section
	 (kwds-re "^;+[ \t]+KEYWORD PARAMETERS:[ \t]*$")    ; hard
	 (kwds-re2 (concat		                    ; forgiving
		    "^;+\\*?[ \t]*"
		    "\\([-A-Z_ ]*KEYWORD[-A-Z_ ]*\\)"
		    "\\(:\\|[ \t]*\n\\)"))
	 ;; The keyword description line.
	 (kwd-re (if keyword                                ; hard (well...)
		     (concat
		      "^;+[ \t]+"
		      "\\(/" (regexp-quote (upcase keyword))
		      "\\|"  (regexp-quote (upcase keyword)) "[ \t]*[-=:\n]"
		      "\\)")))
	 (kwd-re2 (if keyword                               ; forgiving
		      (concat
		       "^;+[ \t]+"
		       (regexp-quote (upcase keyword))
		      "\\>")))
	 dstart dend name-pos kwds-pos kwd-pos)
    (catch 'exit 
      (save-excursion
	(goto-char (point-min))
	(while (and (setq dstart (re-search-forward idlwave-doclib-start nil t))
		    (setq dend (re-search-forward idlwave-doclib-end nil t)))
	  ;; found a routine header
	  (goto-char dstart)
	  (if (setq name-pos (re-search-forward name-re dend t))
	      (progn 
		(if keyword
		    ;; We do need a keyword
		    (progn
		      ;; Try to find a keyword section, but don't force it.
		      (goto-char name-pos)
		      (if (let ((case-fold-search nil))
			    (or (re-search-forward kwds-re dend t)
				(re-search-forward kwds-re2 dend t)))
			  (setq kwds-pos (match-beginning 0)))
		      ;; Find the keyword description
		      (if (or (let ((case-fold-search nil))
				(re-search-forward kwd-re dend t))
			      (re-search-forward kwd-re dend t)
			      (let ((case-fold-search nil))
				(re-search-forward kwd-re2 dend t))
			      (re-search-forward kwd-re2 dend t))
			  (setq kwd-pos (match-beginning 0))
			(if exact
			    (progn
			      (idlwave-help-diagnostics
			       (format "Could not find description of kwd %s"
				       (upcase keyword)))
			      (throw 'exit nil))))))
		;; Return the best position we got
		(throw 'exit (or kwd-pos kwds-pos name-pos dstart)))
	    (goto-char dend))))
      (idlwave-help-diagnostics "Could not find doclib header")
      (throw 'exit nil))))

(defun idlwave-help-diagnostics (string &optional ding)
  "Add a diagnostics string to the list.
When DING is non-nil, ring the bell as well."
  (if (boundp 'idlwave-help-diagnostics)
      (progn
	(setq idlwave-help-diagnostics
	      (cons string idlwave-help-diagnostics))
	(if ding (ding)))))

(defun idlwave-help-toggle-header-top-and-def (arg)
  (interactive "P")
  (if (not idlwave-help-is-source)
      (error "This is not a source file"))
  (let (pos)
    (if idlwave-help-in-header
	;; Header was the last thing displayed
	(progn
	  (setq idlwave-help-in-header nil)
	  (setq pos idlwave-help-def-pos))
      ;; Try to display header
      (setq pos (idlwave-help-find-in-doc-header
		 (nth 0 idlwave-help-args)
		 (nth 1 idlwave-help-args)
		 (nth 2 idlwave-help-args)
		 nil))
      (if pos
	  (setq idlwave-help-in-header t)
	(error "Cannot find doclib header for routine %s"
	       (idlwave-make-full-name (nth 2 idlwave-help-args)
				       (nth 0 idlwave-help-args)))))
    (if pos
	(progn
	  (goto-char pos)
	  (recenter 0)))))

(defun idlwave-help-find-first-header (arg)
  (interactive "P")
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward idlwave-doclib-start nil t)
	  (setq pos (match-beginning 0))))
    (if pos
	(progn
	  (goto-char pos)
	  (recenter 0))
      (error "No DocLib Header in current file"))))

(defun idlwave-help-find-header (arg)
  "Jump to the DocLib Header."
  (interactive "P")
  (if arg
      (idlwave-help-find-first-header nil)
    (setq idlwave-help-in-header nil)
    (idlwave-help-toggle-header-match-and-def arg 'top)))
  
(defun idlwave-help-toggle-header-match-and-def (arg &optional top)
  (interactive "P")
  (if (not idlwave-help-is-source)
      (error "This is not a source file"))
  (let ((args idlwave-help-args)
	pos)
    (if idlwave-help-in-header
	;; Header was the last thing displayed
	(progn
	  (setq idlwave-help-in-header nil)
	  (setq pos idlwave-help-def-pos))
      ;; Try to display header
      (setq pos (apply 'idlwave-help-find-in-doc-header
		       (if top 
			   (list (car args) (nth 1 args) (nth 2 args) nil)
			 args)))
      (if pos
	  (setq idlwave-help-in-header t)
	(error "Cannot find doclib header for routine %s"
	       (idlwave-make-full-name (nth 2 idlwave-help-args)
				       (nth 0 idlwave-help-args)))))
    (if pos
	(progn
	  (goto-char pos)
	  (recenter 0)))))

(defvar font-lock-verbose)
(defvar idlwave-mode-syntax-table)
(defvar idlwave-font-lock-defaults)
(defun idlwave-help-fontify ()
  "Fontify the Help buffer as source code.
Useful when source code is displayed as help.  See the option
`idlwave-help-fontify-source-code'."
  (interactive)
  (if (not idlwave-help-is-source)
      (error "Fontification only for source files...")
    (if (and (featurep 'font-lock)
	     idlwave-help-is-source)
	(let ((major-mode 'idlwave-mode)
	      (font-lock-verbose
	       (if (interactive-p) font-lock-verbose nil))
	      (syntax-table (syntax-table)))
	  (unwind-protect
	      (progn
		(set-syntax-table idlwave-mode-syntax-table)
		(set (make-local-variable 'font-lock-defaults)
		     idlwave-font-lock-defaults)
		(font-lock-fontify-buffer))
	    (set-syntax-table syntax-table))))))

(defun idlwave-help-error (name type class keyword)
  (error "Cannot find help on %s%s"
	 (idlwave-make-full-name class name)
	 (if keyword (format ", keyword %s" (upcase keyword)) "")))

(defun idlwave-help-get-help-buffer ()
  "Return the IDLWAVE Help buffer.  Make it first if necessary."
  (let ((buf (get-buffer "*IDLWAVE Help*")))
    (if buf
	nil
      (setq buf (get-buffer-create "*IDLWAVE Help*"))
      (save-excursion
	(set-buffer buf)
	(idlwave-help-mode)))
    buf))

(defun idlwave-help-make-search-list (class routine keyword)
  "Return a list of all possible search compinations.
For some routines, keywords are described under a different topic or routine.
This function returns a list of entries (class routine keyword) to be
searched.  It also makes everything downcase, to make sure the regexp
searches will work properly with `case-fold-search'"
  (let (routines list)
    (setq routine (idlwave-downcase-safe routine)
	  class (idlwave-downcase-safe class)
	  keyword (idlwave-downcase-safe keyword))
    (setq routine (or (cdr (assoc routine idlwave-help-name-translations))
		      routine))
    (setq routines (append (cdr (assoc routine idlwave-help-alt-names))
			   (list routine)))
    (if (equal routine "obj_new")
	(setq routines (cons (list (idlwave-downcase-safe
				    idlwave-current-obj_new-class)
				   "init" keyword)
			      routines)))
    (while routines
      (if (consp (car routines))
	  (setq list (cons (car routines) list))
	(setq list (cons (list class (car routines) keyword) list)))
      (setq routines (cdr routines)))
    list))

(defvar idlwave-help-link-map (copy-keymap idlwave-help-mode-map)
  "The keymap for activated stuff in the Help application.")

(define-key idlwave-help-link-map (if (featurep 'xemacs) [button1] [mouse-1])
  'idlwave-help-follow-link)
(define-key idlwave-help-link-map (if (featurep 'xemacs) [button2] [mouse-2])
  'idlwave-help-follow-link)
(define-key idlwave-help-link-map (if (featurep 'xemacs) [button3] [mouse-3])
  'idlwave-help-follow-link)

(defun idlwave-help-activate-see-also ()
  "Highlight the items under `See Also' in indicate they may be used as links."
  (save-excursion
    (if (re-search-forward "^ *See Also *$" nil t)
	(let ((lim (+ (point) 500))
	      (case-fold-search nil)
	      (props (list 'face 'idlwave-help-link-face
			   'idlwave-help-link t
			   (if (featurep 'xemacs) 'keymap 'local-map)
			   idlwave-help-link-map
			   'mouse-face 'highlight))
	      (buffer-read-only nil))
	  (while (re-search-forward "\\(\\.?[A-Z][A-Z0-9_]+\\)" lim t)
	    (if (idlwave-is-help-topic (match-string 1))
		(add-text-properties (match-beginning 1) (match-end 1) props)))))))

(defun idlwave-help-activate-methods ()
  "Highlight the items under `See Also' in indicate they may be used as links."
  (save-excursion
    (if (re-search-forward "^ *Methods *$" nil t)
	(let ((lim (+ (point) 1000))
	      (case-fold-search t)
	      (props (list 'face 'idlwave-help-link-face
			   'idlwave-help-link t
			   (if (featurep 'xemacs) 'keymap 'local-map)
			   idlwave-help-link-map
			   'mouse-face 'highlight))
	      (buffer-read-only nil))
	  (while (re-search-forward 
		  "^ *\\* +\"?\\([A-Z][A-Z0-9_]+::[A-Z][A-Z0-9_]+\\)\"?\\( *on +page +[0-9]*\\)? *" lim t)
	    (add-text-properties (match-beginning 1) (match-end 1) props))))))

(defun idlwave-help-activate-class ()
  "Highlight the items under `See Also' in indicate they may be used as links."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "\\([A-Z][A-Z0-9_]+\\)::[A-Z][A-Z0-9_]+ *$")
	(let ((props (list 'face 'idlwave-help-link-face
			   'idlwave-help-link t
			   (if (featurep 'xemacs) 'keymap 'local-map)
			   idlwave-help-link-map
			   'mouse-face 'highlight))
	      (buffer-read-only nil))
	  (add-text-properties (match-beginning 1) (match-end 1) props)))))

(defun idlwave-help-activate-aggressively ()
  (interactive)
  (let ((props (list 'face 'idlwave-help-link-face
		     'idlwave-help-link t
		     (if (featurep 'xemacs) 'keymap 'local-map)
		     idlwave-help-link-map
		     'mouse-face 'highlight))
	(except
	 '("For" "If" "Example" "Wait" "Do" "Events" "Fonts" "Device"
	   "Reference" "Guide" "Routines" "Return" "Print" "Reverse"
	   "Function" "Pro" "Where" "Plot"))
	(case-fold-search nil)
	(buffer-read-only nil)
	b e s bc ac)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\.?[A-Z][a-zA-Z0-9_:]+" nil t)
	(setq b (match-beginning 0) e (match-end 0) s (match-string 0)
	      bc (char-before b) ac (char-after e))
	(if (and (idlwave-is-help-topic s)
		 (not (member s except))
		 (not (eq bc ?/)) (not (eq ac ?=))
		 (string-match "[A-Z]" (substring s 1)) ; 2nd UPPER char
		 (not (equal (downcase s) idlwave-help-current-topic)))
	    (add-text-properties b e props)))
      (goto-char (point-min))
      (while (re-search-forward "\"" nil t)
	(when (looking-at "\\([^\"]+\\)\"")
	  (setq b (match-beginning 1) e (match-end 1) s (match-string 1))
	  (when (< (length s) 100)
	    (while (string-match "\\s-\\s-+" s)
	      (setq s (replace-match " " t t s)))
	  (if (idlwave-is-help-topic s)
	      (add-text-properties b e props))))))))

(defvar idlwave-max-popup-menu-items)
(defun idlwave-help-popup-select (ev list)
  "Selected an item in LIST with a popup menu."
  (let ((maxpopup idlwave-max-popup-menu-items)
	rtn menu resp)
    (cond ((null list))
	  ((= 1 (length list))
	   (setq rtn (car list)))
	  ((featurep 'xemacs)
	   (setq list (sort list (lambda (a b) 
				   (string< (upcase a) (upcase b)))))
	   (setq menu
		 (append (list "Select Class")
			 (mapcar (lambda (x) (vector x (list 'idlwave-help-pset
							     x)))
				 list)))
	   (setq menu (idlwave-help-split-menu-xemacs menu maxpopup))
	   (setq resp (get-popup-menu-response menu))
	   (funcall (event-function resp) (event-object resp)))
	  (t
	   (setq list (sort list (lambda (a b) 
				   (string< (upcase a) (upcase b)))))
	   (setq menu (cons "Select Class"
			    (list
			     (append (list "")
				     (mapcar (lambda(x) (cons x x)) list)))))
	   (setq menu (idlwave-help-split-menu-emacs menu maxpopup))
	   (setq rtn (x-popup-menu ev menu))))
    rtn))

(defun idlwave-help-split-menu-xemacs (menu N)
  "Split the MENU into submenus of maximum length N."
  (if (<= (length menu) (1+ N))
      ;; No splitting needed
      menu
    (let* ((title (car menu))
	   (entries (cdr menu))
	   (menu (list title))
	   (cnt 0)
	   (nextmenu nil))
      (while entries
	(while (and entries (< cnt N))
	  (setq cnt (1+ cnt)
		nextmenu (cons (car entries) nextmenu)
		entries (cdr entries)))
	(setq nextmenu (nreverse nextmenu))
	(setq nextmenu (cons (format "%s...%s"
				     (aref (car nextmenu) 0)
				     (aref (nth (1- cnt) nextmenu) 0))
			     nextmenu))
	(setq menu (cons nextmenu menu)
	      nextmenu nil
	      cnt 0))
      (nreverse menu))))

(defun idlwave-help-split-menu-emacs (menu N)
  "Split the MENU into submenus of maximum length N."
  (if (<= (length (nth 1 menu)) (1+ N))
      ;; No splitting needed
      menu
    (let* ((title (car menu))
	   (entries (cdr (nth 1 menu)))
	   (menu nil)
	   (cnt 0)
	   (nextmenu nil))
      (while entries
	(while (and entries (< cnt N))
	  (setq cnt (1+ cnt)
		nextmenu (cons (car entries) nextmenu)
		entries (cdr entries)))
	(setq nextmenu (nreverse nextmenu))
	(prin1 nextmenu)
	(setq nextmenu (cons (format "%s...%s"
				     (car (car nextmenu))
				     (car (nth (1- cnt) nextmenu)))
			     nextmenu))
	(setq menu (cons nextmenu menu)
	      nextmenu nil
	      cnt 0))
      (setq menu (nreverse menu))
      (setq menu (cons title menu))
      menu)))

(defvar rtn)
(defun idlwave-help-pset (item)
  (set 'rtn item))

(defun idlwave-grep (regexp list)
  (let (rtn)
    (while list
      (if (string-match regexp (car list))
	  (setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-is-help-topic (word)
  "Try if this could be a help topic.
Also checks special translation lists."
  (setq word (downcase word))
  (car
   (or (assoc word idlwave-help-topics)
       (assoc word idlwave-help-name-translations)
       (assoc word idlwave-help-special-topic-words))))

(defun idlwave-help-maybe-translate (word)
  "Return the real topic assiciated with WORD."
  (setq word (downcase word))
  (or (car (assoc word idlwave-help-topics))
      (cdr (assoc word idlwave-help-name-translations))
      (cdr (assoc word idlwave-help-special-topic-words))))

(defun idlwave-grep-help-topics (list)
  "Return only the classis in LIST which are also help topics."
  (delq nil (mapcar 'idlwave-is-help-topic list)))

(defun idlwave-any-help-topic (list)
  "Return the first member in LIST which is also a help topic."
  (catch 'exit
    (while list
      (if (idlwave-is-help-topic (car list))
	  (throw 'exit (car list))
	(setq list (cdr list))))))

(setq idlwave-help-topics
 '(
  ("### idlw-help.txt --- help file for idlwave" . 0)
  ("reference" . 539)
  ("idl syntax" . 1476)
  (".compile" . 13151)
  (".continue" . 14546)
  (".edit" . 15456)
  (".full_reset_session" . 15933)
  (".go" . 16861)
  (".out" . 17128)
  (".reset_session" . 17405)
  (".return" . 19747)
  (".rnew" . 20271)
  (".run" . 22240)
  (".skip" . 25132)
  (".step" . 26334)
  (".stepover" . 26878)
  (".trace" . 27702)
  ("a_correlate" . 28021)
  ("abs" . 30635)
  ("acos" . 31528)
  ("adapt_hist_equal" . 32394)
  ("alog" . 35609)
  ("alog10" . 36338)
  ("amoeba" . 36743)
  ("annotate" . 43240)
  ("arg_present" . 46034)
  ("array_equal" . 47890)
  ("arrow" . 49769)
  ("ascii_template" . 51834)
  ("asin" . 53421)
  ("assoc" . 54001)
  ("atan" . 58059)
  ("axis" . 59363)
  ("bar_plot" . 66135)
  ("begin...end" . 71836)
  ("beseli" . 74670)
  ("beselj" . 76451)
  ("beselk" . 78203)
  ("besely" . 79986)
  ("beta" . 81765)
  ("bilinear" . 82615)
  ("bin_date" . 85473)
  ("binary_template" . 86583)
  ("bindgen" . 89643)
  ("binomial" . 90872)
  ("blas_axpy" . 93400)
  ("blk_con" . 95902)
  ("box_cursor" . 97920)
  ("break" . 99831)
  ("breakpoint" . 100414)
  ("broyden" . 104899)
  ("bytarr" . 108580)
  ("byte" . 109510)
  ("byteorder" . 111185)
  ("bytscl" . 117619)
  ("c_correlate" . 120044)
  ("caldat" . 123221)
  ("calendar" . 126586)
  ("call_external" . 127601)
  ("call_function" . 159898)
  ("call_method" . 161156)
  ("call_procedure" . 162362)
  ("case" . 163576)
  ("catch" . 165841)
  ("cd" . 169263)
  ("cdf routines" . 173587)
  ("chebyshev" . 174915)
  ("check_math" . 175629)
  ("chisqr_cvf" . 184014)
  ("chisqr_pdf" . 185254)
  ("choldc" . 187218)
  ("cholsol" . 188231)
  ("cindgen" . 189830)
  ("cir_3pnt" . 190900)
  ("close" . 192364)
  ("clust_wts" . 194324)
  ("cluster" . 196126)
  ("color_convert" . 198920)
  ("color_quan" . 200953)
  ("colormap_applicable" . 207862)
  ("comfit" . 209163)
  ("common" . 212996)
  ("compile_opt" . 213258)
  ("complex" . 217712)
  ("complexarr" . 221100)
  ("complexround" . 222043)
  ("compute_mesh_normals" . 222770)
  ("cond" . 223390)
  ("congrid" . 225107)
  ("conj" . 229814)
  ("constrained_min" . 230572)
  ("continue" . 242230)
  ("contour" . 243235)
  ("graphics keywords accepted" . 265243)
  ("examples" . 265807)
  ("convert_coord" . 268043)
  ("convol" . 271194)
  ("coord2to3" . 278263)
  ("correlate" . 279857)
  ("cos" . 282137)
  ("cosh" . 283026)
  ("cramer" . 283850)
  ("create_struct" . 285316)
  ("create_view" . 287070)
  ("crossp" . 291434)
  ("crvlength" . 291768)
  ("ct_luminance" . 293389)
  ("cti_test" . 295087)
  ("cursor" . 299084)
  ("curvefit" . 303379)
  ("cv_coord" . 309235)
  ("cvttobm" . 312199)
  ("cw_animate" . 314583)
  ("cw_animate_getp" . 321153)
  ("cw_animate_load" . 322921)
  ("cw_animate_run" . 325962)
  ("cw_arcball" . 327860)
  ("cw_bgroup" . 333892)
  ("keywords to widget_control and widget_info" . 340474)
  ("widget events returned by the cw_bgroup widget" . 341511)
  ("see also" . 341887)
  ("cw_clr_index" . 341924)
  ("cw_colorsel" . 345764)
  ("cw_defroi" . 348356)
  ("cw_field" . 352966)
  ("cw_filesel" . 360253)
  ("cw_form" . 366331)
  ("cw_fslider" . 381044)
  ("cw_light_editor" . 387213)
  ("cw_light_editor_get" . 394348)
  ("cw_light_editor_set" . 397610)
  ("cw_orient" . 400553)
  ("cw_palette_editor" . 403310)
  ("cw_palette_editor_get" . 414553)
  ("cw_palette_editor_set" . 415150)
  ("cw_pdmenu" . 416116)
  ("cw_rgbslider" . 428334)
  ("cw_tmpl" . 433135)
  ("cw_zoom" . 434297)
  ("dblarr" . 442470)
  ("dcindgen" . 443400)
  ("dcomplex" . 444479)
  ("dcomplexarr" . 447730)
  ("define_key" . 448670)
  ("defroi" . 464670)
  ("defsysv" . 467290)
  ("delete_symbol" . 469065)
  ("dellog" . 469705)
  ("delvar" . 470218)
  ("deriv" . 470930)
  ("derivsig" . 471604)
  ("determ" . 472470)
  ("device" . 474245)
  ("dfpmin" . 480834)
  ("dialog_message" . 484843)
  ("dialog_pickfile" . 489330)
  ("example" . 494511)
  ("see also" . 494785)
  ("dialog_printersetup" . 494806)
  ("dialog_read_image" . 498062)
  ("dialog_write_image" . 501416)
  ("digital_filter" . 504234)
  ("dilate" . 505914)
  ("dindgen" . 515299)
  ("dissolve" . 516319)
  ("dist" . 517468)
  ("dlm_load" . 518253)
  ("dlm_register" . 518898)
  ("do_apple_script" . 519958)
  ("doc_library" . 521718)
  ("double" . 525028)
  ("draw_roi" . 526853)
  ("efont" . 529379)
  ("eigenql" . 531066)
  ("eigenvec" . 534404)
  ("elmhes" . 537476)
  ("empty" . 538968)
  ("enable_sysrtn" . 539564)
  ("eof" . 542412)
  ("eos_* routines" . 544036)
  ("erase" . 544144)
  ("erode" . 545428)
  ("errorf" . 552879)
  ("errplot" . 553730)
  ("execute" . 555232)
  ("exit" . 556704)
  ("exp" . 557588)
  ("expand" . 558537)
  ("expand_path" . 559578)
  ("expint" . 565896)
  ("extrac" . 567726)
  ("extract_slice" . 571222)
  ("f_cvf" . 577220)
  ("f_pdf" . 578652)
  ("factorial" . 580496)
  ("fft" . 582069)
  ("file_chmod" . 589542)
  ("file_delete" . 596495)
  ("file_expand_path" . 598719)
  ("file_mkdir" . 600247)
  ("file_test" . 601350)
  ("filepath" . 609294)
  ("findfile" . 611760)
  ("findgen" . 614640)
  ("finite" . 615721)
  ("fix" . 618881)
  ("flick" . 622088)
  ("float" . 622647)
  ("floor" . 624341)
  ("see also" . 625451)
  ("flow3" . 625489)
  ("fltarr" . 627319)
  ("flush" . 628261)
  ("for" . 628761)
  ("format_axis_values" . 629536)
  ("forward_function" . 630472)
  ("free_lun" . 630902)
  ("fstat" . 632515)
  ("fulstr" . 638089)
  ("funct" . 639514)
  ("function" . 640675)
  ("fv_test" . 640957)
  ("fx_root" . 643632)
  ("fz_roots" . 646470)
  ("gamma" . 648790)
  ("gamma_ct" . 649666)
  ("gauss_cvf" . 650962)
  ("gauss_pdf" . 651990)
  ("gauss2dfit" . 653634)
  ("gaussfit" . 658177)
  ("gaussint" . 662099)
  ("get_drive_list" . 663150)
  ("get_kbrd" . 664616)
  ("get_lun" . 666431)
  ("get_screen_size" . 667817)
  ("get_symbol" . 669100)
  ("getenv" . 669803)
  ("goto" . 675538)
  ("grid_tps" . 677060)
  ("grid3" . 681991)
  ("gs_iter" . 686735)
  ("h_eq_ct" . 690230)
  ("h_eq_int" . 691213)
  ("hanning" . 692750)
  ("hdf_* routines" . 694141)
  ("hdf_browser" . 694346)
  ("hdf_read" . 699634)
  ("heap_gc" . 705089)
  ("help" . 707124)
  ("hilbert" . 720419)
  ("hist_2d" . 722017)
  ("hist_equal" . 724002)
  ("histogram" . 727689)
  ("hls" . 736981)
  ("hqr" . 751232)
  ("hsv" . 753176)
  ("ibeta" . 754500)
  ("identity" . 757310)
  ("idl_container object class" . 758480)
  ("idlanroi object class" . 758569)
  ("idlanroigroup object class" . 758653)
  ("idlffdicom object class" . 758741)
  ("idlffdxf object class" . 758826)
  ("idlfflanguagecat object class" . 758909)
  ("idlffshape object class" . 759000)
  ("idlgr* object classes" . 759085)
  ("if...then...else" . 759802)
  ("igamma" . 760885)
  ("image_cont" . 764051)
  ("image_statistics" . 765217)
  ("imaginary" . 769760)
  ("indgen" . 770942)
  ("int_2d" . 773086)
  ("int_3d" . 777479)
  ("int_tabulated" . 781441)
  ("intarr" . 783829)
  ("interpol" . 784580)
  ("interpolate" . 787302)
  ("invert" . 794048)
  ("ioctl" . 795877)
  ("ishft" . 800968)
  ("isocontour" . 801733)
  ("isosurface" . 808148)
  ("journal" . 811126)
  ("julday" . 812520)
  ("keyword_set" . 816170)
  ("krig2d" . 817214)
  ("kurtosis" . 824187)
  ("kw_test" . 825466)
  ("l64indgen" . 829630)
  ("label_date" . 830639)
  ("label_region" . 836110)
  ("ladfit" . 839140)
  ("laguerre" . 841287)
  ("leefilt" . 844013)
  ("legendre" . 845719)
  ("linbcg" . 849843)
  ("lindgen" . 853043)
  ("linfit" . 854060)
  ("linkimage" . 857803)
  ("syntax" . 860192)
  ("arguments" . 860361)
  ("live_contour" . 872683)
  ("live_control" . 886512)
  ("live_destroy" . 889737)
  ("live_export" . 892115)
  ("live_image" . 896410)
  ("live_info" . 906943)
  ("live_line" . 923909)
  ("live_load" . 929605)
  ("live_oplot" . 930025)
  ("live_plot" . 937828)
  ("live_print" . 951321)
  ("live_rect" . 952783)
  ("live_style" . 957611)
  ("live_surface" . 968053)
  ("ljlct" . 987683)
  ("ll_arc_distance" . 988762)
  ("lmfit" . 990480)
  ("lmgr" . 998235)
  ("lngamma" . 1002618)
  ("lnp_test" . 1003252)
  ("loadct" . 1007006)
  ("locale_get" . 1008907)
  ("lon64arr" . 1009110)
  ("lonarr" . 1009875)
  ("long" . 1010766)
  ("long64" . 1012333)
  ("lsode" . 1013888)
  ("lu_complex" . 1021415)
  ("ludc" . 1023934)
  ("lumprove" . 1025270)
  ("lusol" . 1027568)
  ("m_correlate" . 1029584)
  ("machar" . 1032694)
  ("make_array" . 1035844)
  ("make_dll" . 1039398)
  ("map_2points" . 1050855)
  ("map_continents" . 1055987)
  ("map_grid" . 1062274)
  ("map_image" . 1071631)
  ("map_patch" . 1077816)
  ("map_proj_info" . 1083755)
  ("map_set" . 1086754)
  ("matrix_multiply" . 1105252)
  ("max" . 1107939)
  ("md_test" . 1111034)
  ("mean" . 1113420)
  ("meanabsdev" . 1114543)
  ("median" . 1116152)
  ("memory" . 1118932)
  ("mesh_clip" . 1123109)
  ("mesh_decimate" . 1124905)
  ("mesh_issolid" . 1128828)
  ("mesh_merge" . 1130007)
  ("mesh_numtriangles" . 1131498)
  ("mesh_obj" . 1132019)
  ("mesh_smooth" . 1144726)
  ("mesh_surfacearea" . 1146419)
  ("mesh_validate" . 1148134)
  ("mesh_volume" . 1149925)
  ("message" . 1150808)
  ("min" . 1154160)
  ("min_curve_surf" . 1156644)
  ("mk_html_help" . 1164695)
  ("modifyct" . 1169596)
  ("moment" . 1171124)
  ("morph_close" . 1174930)
  ("morph_distance" . 1178982)
  ("morph_gradient" . 1183244)
  ("morph_hitormiss" . 1186369)
  ("morph_open" . 1189972)
  ("morph_thin" . 1194184)
  ("morph_tophat" . 1195884)
  ("mpeg_close" . 1198617)
  ("mpeg_open" . 1199478)
  ("mpeg_put" . 1205330)
  ("mpeg_save" . 1207271)
  ("msg_cat_close" . 1208502)
  ("msg_cat_compile" . 1208810)
  ("msg_cat_open" . 1210560)
  ("multi" . 1212268)
  ("n_elements" . 1213039)
  ("n_params" . 1214401)
  ("n_tags" . 1215128)
  ("ncdf_* routines" . 1216822)
  ("newton" . 1216930)
  ("norm" . 1220602)
  ("obj_class" . 1223196)
  ("obj_destroy" . 1224824)
  ("obj_isa" . 1226343)
  ("obj_new" . 1227154)
  ("obj_valid" . 1231207)
  ("objarr" . 1234562)
  ("on_error" . 1235753)
  ("on_ioerror" . 1236746)
  ("online_help" . 1238136)
  ("open" . 1242442)
  ("oplot" . 1265115)
  ("oploterr" . 1268777)
  ("p_correlate" . 1269727)
  ("particle_trace" . 1271476)
  ("pcomp" . 1275345)
  ("plot" . 1281098)
  ("plot_3dbox" . 1287958)
  ("plot_field" . 1292191)
  ("ploterr" . 1293670)
  ("plots" . 1294675)
  ("pnt_line" . 1298221)
  ("point_lun" . 1299827)
  ("polar_contour" . 1303040)
  ("polar_surface" . 1305820)
  ("poly" . 1308526)
  ("poly_2d" . 1309107)
  ("poly_area" . 1315252)
  ("poly_fit" . 1316715)
  ("polyfill" . 1322078)
  ("polyfillv" . 1329135)
  ("polyshade" . 1332078)
  ("polywarp" . 1338568)
  ("popd" . 1341804)
  ("powell" . 1342505)
  ("primes" . 1346107)
  ("print/printf" . 1346857)
  ("printd" . 1350561)
  ("pro" . 1351083)
  ("profile" . 1351908)
  ("profiler" . 1353971)
  ("profiles" . 1356801)
  ("project_vol" . 1358942)
  ("ps_show_fonts" . 1363308)
  ("psafm" . 1364164)
  ("pseudo" . 1365014)
  ("ptr_free" . 1366492)
  ("ptr_new" . 1367695)
  ("ptr_valid" . 1369385)
  ("ptrarr" . 1372574)
  ("pushd" . 1374196)
  ("qromb" . 1375035)
  ("qromo" . 1377635)
  ("qsimp" . 1381290)
  ("query_* routines" . 1384103)
  ("query_bmp" . 1387048)
  ("query_dicom" . 1387923)
  ("query_image" . 1389595)
  ("query_jpeg" . 1393658)
  ("query_pict" . 1394505)
  ("query_png" . 1395357)
  ("query_ppm" . 1397153)
  ("query_srf" . 1398215)
  ("query_tiff" . 1399059)
  ("query_wav" . 1401221)
  ("r_correlate" . 1402349)
  ("r_test" . 1405781)
  ("radon" . 1408097)
  ("randomn" . 1423183)
  ("randomu" . 1431232)
  ("ranks" . 1438871)
  ("rdpix" . 1440851)
  ("read/readf" . 1441907)
  ("read_ascii" . 1446842)
  ("read_binary" . 1451566)
  ("read_bmp" . 1454707)
  ("read_dicom" . 1457750)
  ("read_image" . 1459055)
  ("read_interfile" . 1460420)
  ("read_jpeg" . 1461759)
  ("read_pict" . 1468134)
  ("read_png" . 1469562)
  ("read_ppm" . 1473071)
  ("read_spr" . 1474867)
  ("read_srf" . 1475879)
  ("read_sylk" . 1477377)
  ("read_tiff" . 1482497)
  ("read_wav" . 1495578)
  ("read_wave" . 1496407)
  ("read_x11_bitmap" . 1498639)
  ("read_xwd" . 1500317)
  ("reads" . 1501691)
  ("readu" . 1504088)
  ("rebin" . 1507551)
  ("recall_commands" . 1511517)
  ("recon3" . 1512141)
  ("reduce_colors" . 1522784)
  ("reform" . 1524189)
  ("regress" . 1526447)
  ("repeat...until" . 1532830)
  ("replicate" . 1533747)
  ("replicate_inplace" . 1535015)
  ("resolve_all" . 1537221)
  ("resolve_routine" . 1539481)
  ("restore" . 1541229)
  ("retall" . 1544696)
  ("return" . 1544989)
  ("reverse" . 1547544)
  ("rewind" . 1549360)
  ("rk4" . 1549893)
  ("roberts" . 1552163)
  ("rot" . 1553757)
  ("rotate" . 1557733)
  ("round" . 1561187)
  ("routine_info" . 1562480)
  ("rs_test" . 1567144)
  ("s_test" . 1570562)
  ("save" . 1572502)
  ("savgol" . 1576238)
  ("scale3" . 1581599)
  ("scale3d" . 1583203)
  ("search2d" . 1583966)
  ("search3d" . 1588960)
  ("set_plot" . 1594341)
  ("set_shading" . 1597397)
  ("set_symbol" . 1600228)
  ("setenv" . 1600959)
  ("setlog" . 1601936)
  ("setup_keys" . 1603640)
  ("sfit" . 1607596)
  ("shade_surf" . 1609555)
  ("shade_surf_irr" . 1618863)
  ("shade_volume" . 1621614)
  ("shift" . 1627244)
  ("show3" . 1629295)
  ("showfont" . 1632001)
  ("sin" . 1633654)
  ("sindgen" . 1634782)
  ("sinh" . 1635827)
  ("size" . 1636929)
  ("skewness" . 1643430)
  ("skipf" . 1644680)
  ("slicer3" . 1646193)
  ("slide_image" . 1670427)
  ("smooth" . 1677045)
  ("sobel" . 1682373)
  ("socket" . 1684206)
  ("sort" . 1691492)
  ("spawn" . 1693572)
  ("sph_4pnt" . 1708332)
  ("sph_scat" . 1709830)
  ("spher_harm" . 1713264)
  ("spl_init" . 1717507)
  ("spl_interp" . 1720122)
  ("spline" . 1722390)
  ("spline_p" . 1723643)
  ("sprsab" . 1726475)
  ("sprsax" . 1729282)
  ("sprsin" . 1730553)
  ("sprstp" . 1734235)
  ("sqrt" . 1735028)
  ("standardize" . 1736038)
  ("stddev" . 1738715)
  ("stop" . 1739750)
  ("strarr" . 1740545)
  ("strcmp" . 1741217)
  ("strcompress" . 1742965)
  ("streamline" . 1743963)
  ("stregex" . 1746201)
  ("stretch" . 1752291)
  ("string" . 1754329)
  ("strjoin" . 1759071)
  ("strlen" . 1760131)
  ("strlowcase" . 1760963)
  ("strmatch" . 1761806)
  ("strmessage" . 1765092)
  ("strmid" . 1766490)
  ("strpos" . 1769186)
  ("strput" . 1772360)
  ("strsplit" . 1774066)
  ("strtrim" . 1780209)
  ("struct_assign" . 1781645)
  ("struct_hide" . 1785296)
  ("strupcase" . 1787991)
  ("surface" . 1788784)
  ("surfr" . 1798798)
  ("svdc" . 1799522)
  ("svdfit" . 1802698)
  ("svsol" . 1809912)
  ("swap_endian" . 1812366)
  ("switch" . 1813284)
  ("systime" . 1815708)
  ("t_cvf" . 1819880)
  ("t_pdf" . 1821113)
  ("t3d" . 1822728)
  ("tag_names" . 1826610)
  ("tan" . 1828054)
  ("tanh" . 1828792)
  ("taprd" . 1829888)
  ("tapwrt" . 1831485)
  ("tek_color" . 1832817)
  ("temporary" . 1833609)
  ("tetra_clip" . 1834681)
  ("tetra_surface" . 1836566)
  ("tetra_volume" . 1837231)
  ("thin" . 1838875)
  ("threed" . 1841198)
  ("time_test2" . 1842212)
  ("timegen" . 1843093)
  ("tm_test" . 1852126)
  ("total" . 1855266)
  ("trace" . 1858663)
  ("trackball object" . 1859519)
  ("transpose" . 1859597)
  ("tri_surf" . 1861451)
  ("triangulate" . 1867138)
  ("trigrid" . 1873332)
  ("triql" . 1886194)
  ("trired" . 1888806)
  ("trisol" . 1889914)
  ("trnlog" . 1892304)
  ("ts_coef" . 1895740)
  ("ts_diff" . 1897273)
  ("ts_fcast" . 1898696)
  ("ts_smooth" . 1901134)
  ("tv" . 1904249)
  ("tvcrs" . 1911137)
  ("tvlct" . 1913415)
  ("tvrd" . 1917417)
  ("tvscl" . 1923810)
  ("uindgen" . 1926876)
  ("uint" . 1927890)
  ("uintarr" . 1929577)
  ("ul64indgen" . 1930482)
  ("ulindgen" . 1931515)
  ("ulon64arr" . 1932531)
  ("ulonarr" . 1933463)
  ("ulong" . 1934397)
  ("ulong64" . 1935996)
  ("uniq" . 1937726)
  ("usersym" . 1939644)
  ("value_locate" . 1941536)
  ("variance" . 1943922)
  ("vax_float" . 1945216)
  ("vector_field" . 1947687)
  ("vel" . 1949494)
  ("velovect" . 1951100)
  ("vert_t3d" . 1953823)
  ("voigt" . 1956595)
  ("voronoi" . 1958855)
  ("voxel_proj" . 1961304)
  ("wait" . 1971041)
  ("warp_tri" . 1971656)
  ("watershed" . 1973921)
  ("wdelete" . 1977624)
  ("weof" . 1978496)
  ("wf_draw" . 1979122)
  ("where" . 1983517)
  ("while...do" . 1989020)
  ("widget_base" . 1989752)
  ("widget_button" . 2037186)
  ("widget_control" . 2056882)
  ("widget_draw" . 2117983)
  ("widget_droplist" . 2143969)
  ("widget_event" . 2158751)
  ("widget_info" . 2165038)
  ("widget_label" . 2186521)
  ("widget_list" . 2198925)
  ("widget_slider" . 2214980)
  ("widget_table" . 2231579)
  ("widget_text" . 2262181)
  ("window" . 2281635)
  ("write_bmp" . 2287417)
  ("write_image" . 2290729)
  ("write_jpeg" . 2292100)
  ("write_nrif" . 2297705)
  ("write_pict" . 2299868)
  ("write_png" . 2301220)
  ("write_ppm" . 2304585)
  ("write_spr" . 2305834)
  ("write_srf" . 2306627)
  ("write_sylk" . 2308689)
  ("write_tiff" . 2310633)
  ("write_wav" . 2322235)
  ("write_wave" . 2323025)
  ("writeu" . 2324623)
  ("wset" . 2327042)
  ("wshow" . 2328751)
  ("wtn" . 2329900)
  ("xbm_edit" . 2336392)
  ("xdisplayfile" . 2339109)
  ("xdxf" . 2342873)
  ("xfont" . 2346783)
  ("xinteranimate" . 2347872)
  ("xloadct" . 2360122)
  ("xmanager" . 2363823)
  ("xmng_tmpl" . 2381473)
  ("xmtool" . 2383196)
  ("xobjview" . 2384540)
  ("xpalette" . 2398767)
  ("xpcolor" . 2405599)
  ("xplot3d" . 2407026)
  ("xregistered" . 2416315)
  ("xroi" . 2417903)
  ("xsq_test" . 2435729)
  ("xsurface" . 2439394)
  ("xvaredit" . 2441301)
  ("xvolume" . 2442767)
  ("xvolume_rotate" . 2452633)
  ("xvolume_write_image" . 2455476)
  ("xyouts" . 2456629)
  ("zoom" . 2462694)
  ("zoom_24" . 2465276)
  ("idl_container" . 2473579)
  ("idl_container::add" . 2474368)
  ("idl_container::cleanup" . 2475117)
  ("idl_container::count" . 2475832)
  ("idl_container::get" . 2476084)
  ("idl_container::init" . 2477564)
  ("idl_container::iscontained" . 2478300)
  ("idl_container::move" . 2478960)
  ("idl_container::remove" . 2480051)
  ("idlanroi" . 2480878)
  ("idlanroi::appenddata" . 2482033)
  ("idlanroi::cleanup" . 2484901)
  ("idlanroi::computegeometry" . 2485632)
  ("idlanroi::computemask" . 2488343)
  ("idlanroi::containspoints" . 2492185)
  ("idlanroi::getproperty" . 2493876)
  ("idlanroi::init" . 2495800)
  ("idlanroi::removedata" . 2500371)
  ("idlanroi::replacedata" . 2502248)
  ("idlanroi::rotate" . 2506896)
  ("idlanroi::scale" . 2507615)
  ("idlanroi::setproperty" . 2508420)
  ("idlanroi::translate" . 2508769)
  ("idlanroigroup" . 2509614)
  ("idlanroigroup::add" . 2510564)
  ("idlanroigroup::cleanup" . 2511145)
  ("idlanroigroup::computemask" . 2511889)
  ("idlanroigroup::computemesh" . 2515601)
  ("idlanroigroup::containspoints" . 2518753)
  ("idlanroigroup::getproperty" . 2521247)
  ("idlanroigroup::init" . 2522971)
  ("idlanroigroup::rotate" . 2523700)
  ("idlanroigroup::scale" . 2524465)
  ("idlanroigroup::translate" . 2525285)
  ("idlffdicom" . 2526159)
  ("idlffdicom::cleanup" . 2535570)
  ("idlffdicom::dumpelements" . 2536674)
  ("idlffdicom::getchildren" . 2537837)
  ("idlffdicom::getdescription" . 2539569)
  ("idlffdicom::getelement" . 2541514)
  ("idlffdicom::getgroup" . 2543468)
  ("idlffdicom::getlength" . 2545373)
  ("idlffdicom::getparent" . 2547149)
  ("idlffdicom::getpreamble" . 2548660)
  ("idlffdicom::getreference" . 2549425)
  ("idlffdicom::getvalue" . 2552040)
  ("idlffdicom::getvr" . 2555933)
  ("idlffdicom::init" . 2557719)
  ("idlffdicom::read" . 2559008)
  ("idlffdicom::reset" . 2559975)
  ("idlffdxf" . 2560544)
  ("idlffdxf::cleanup" . 2563525)
  ("idlffdxf::getcontents" . 2564229)
  ("idlffdxf::getentity" . 2567642)
  ("idlffdxf::getpalette" . 2581797)
  ("idlffdxf::init" . 2582258)
  ("idlffdxf::putentity" . 2583140)
  ("idlffdxf::read" . 2584062)
  ("idlffdxf::removeentity" . 2584865)
  ("idlffdxf::reset" . 2585549)
  ("idlffdxf::setpalette" . 2585740)
  ("idlffdxf::write" . 2586218)
  ("idlfflanguagecat" . 2587739)
  ("idlfflanguagecat::isvalid" . 2588456)
  ("idlfflanguagecat::query" . 2588719)
  ("idlfflanguagecat::setcatalog" . 2589473)
  ("idlffshape" . 2590454)
  ("idlffshape::addattribute" . 2607158)
  ("idlffshape::cleanup" . 2609732)
  ("idlffshape::close" . 2610879)
  ("idlffshape::destroyentity" . 2611530)
  ("idlffshape::getattributes" . 2612797)
  ("idlffshape::getentity" . 2614554)
  ("idlffshape::getproperty" . 2617165)
  ("idlffshape::init" . 2621861)
  ("idlffshape::open" . 2623701)
  ("idlffshape::putentity" . 2624923)
  ("idlffshape::setattributes" . 2626928)
  ("idlgraxis" . 2630338)
  ("idlgraxis::cleanup" . 2631054)
  ("idlgraxis::getctm" . 2631770)
  ("idlgraxis::getproperty" . 2633875)
  ("idlgraxis::init" . 2636320)
  ("idlgraxis::setproperty" . 2659052)
  ("idlgrbuffer" . 2659404)
  ("idlgrbuffer::cleanup" . 2660648)
  ("idlgrbuffer::draw" . 2661388)
  ("idlgrbuffer::erase" . 2662776)
  ("idlgrbuffer::getcontiguouspixels" . 2663174)
  ("idlgrbuffer::getdeviceinfo" . 2664347)
  ("idlgrbuffer::getfontnames" . 2666797)
  ("idlgrbuffer::getproperty" . 2668759)
  ("idlgrbuffer::gettextdimensions" . 2670406)
  ("idlgrbuffer::init" . 2672532)
  ("idlgrbuffer::pickdata" . 2676554)
  ("idlgrbuffer::read" . 2680010)
  ("idlgrbuffer::select" . 2680304)
  ("idlgrbuffer::setproperty" . 2682729)
  ("idlgrclipboard" . 2683099)
  ("idlgrclipboard::cleanup" . 2684212)
  ("idlgrclipboard::draw" . 2684930)
  ("idlgrclipboard::getcontiguouspixels" . 2687166)
  ("idlgrclipboard::getdeviceinfo" . 2688352)
  ("idlgrclipboard::getfontnames" . 2690730)
  ("idlgrclipboard::getproperty" . 2692608)
  ("idlgrclipboard::gettextdimensions" . 2693581)
  ("idlgrclipboard::init" . 2695650)
  ("idlgrclipboard::setproperty" . 2699875)
  ("idlgrcolorbar" . 2700270)
  ("idlgrcolorbar::cleanup" . 2701385)
  ("idlgrcolorbar::computedimensions" . 2702100)
  ("idlgrcolorbar::getproperty" . 2703750)
  ("idlgrcolorbar::init" . 2705692)
  ("idlgrcolorbar::setproperty" . 2716865)
  ("idlgrcontour" . 2717247)
  ("idlgrcontour::cleanup" . 2718268)
  ("idlgrcontour::getctm" . 2718981)
  ("idlgrcontour::getproperty" . 2721098)
  ("idlgrcontour::init" . 2724664)
  ("idlgrcontour::setproperty" . 2744918)
  ("idlgrfont" . 2746856)
  ("idlgrfont::cleanup" . 2747346)
  ("idlgrfont::getproperty" . 2748062)
  ("idlgrfont::init" . 2748973)
  ("idlgrfont::setproperty" . 2752561)
  ("idlgrimage" . 2752923)
  ("idlgrimage::cleanup" . 2755202)
  ("idlgrimage::getctm" . 2755909)
  ("idlgrimage::getproperty" . 2758086)
  ("idlgrimage::init" . 2760078)
  ("idlgrimage::setproperty" . 2773698)
  ("idlgrlegend" . 2774058)
  ("idlgrlegend::cleanup" . 2775791)
  ("idlgrlegend::computedimensions" . 2776513)
  ("idlgrlegend::getproperty" . 2778146)
  ("idlgrlegend::init" . 2780039)
  ("idlgrlegend::setproperty" . 2790163)
  ("idlgrlight" . 2790883)
  ("idlgrlight::cleanup" . 2791794)
  ("idlgrlight::getctm" . 2792522)
  ("idlgrlight::getproperty" . 2794659)
  ("idlgrlight::init" . 2795757)
  ("idlgrlight::setproperty" . 2803418)
  ("idlgrmodel" . 2803787)
  ("idlgrmodel::add" . 2804968)
  ("idlgrmodel::cleanup" . 2805969)
  ("idlgrmodel::draw" . 2806697)
  ("idlgrmodel::getbyname" . 2807667)
  ("idlgrmodel::getctm" . 2809180)
  ("idlgrmodel::getproperty" . 2811317)
  ("idlgrmodel::init" . 2812225)
  ("idlgrmodel::reset" . 2815821)
  ("idlgrmodel::rotate" . 2816240)
  ("idlgrmodel::scale" . 2817048)
  ("idlgrmodel::setproperty" . 2817739)
  ("idlgrmodel::translate" . 2818097)
  ("idlgrmpeg" . 2818850)
  ("idlgrmpeg::cleanup" . 2819654)
  ("idlgrmpeg::getproperty" . 2820370)
  ("idlgrmpeg::init" . 2821014)
  ("idlgrmpeg::put" . 2830103)
  ("idlgrmpeg::save" . 2831145)
  ("idlgrmpeg::setproperty" . 2832038)
  ("idlgrpalette" . 2832403)
  ("idlgrpalette::cleanup" . 2833064)
  ("idlgrpalette::getrgb" . 2833777)
  ("idlgrpalette::getproperty" . 2834437)
  ("idlgrpalette::init" . 2835541)
  ("idlgrpalette::loadct" . 2839554)
  ("idlgrpalette::nearestcolor" . 2840158)
  ("idlgrpalette::setrgb" . 2840912)
  ("idlgrpalette::setproperty" . 2841528)
  ("idlgrpattern" . 2841917)
  ("idlgrpattern::cleanup" . 2842554)
  ("idlgrpattern::getproperty" . 2843306)
  ("idlgrpattern::init" . 2844236)
  ("idlgrplot" . 2848505)
  ("idlgrplot::cleanup" . 2849205)
  ("idlgrplot::getctm" . 2849915)
  ("idlgrplot::getproperty" . 2852041)
  ("idlgrplot::init" . 2853842)
  ("idlgrplot::setproperty" . 2866899)
  ("idlgrpolygon" . 2867253)
  ("idlgrpolygon::cleanup" . 2868257)
  ("idlgrpolygon::getctm" . 2868970)
  ("idlgrpolygon::getproperty" . 2871142)
  ("idlgrpolygon::init" . 2873313)
  ("idlgrpolygon::setproperty" . 2893530)
  ("idlgrpolyline" . 2893903)
  ("idlgrpolyline::cleanup" . 2894665)
  ("idlgrpolyline::getctm" . 2895381)
  ("idlgrpolyline::getproperty" . 2897534)
  ("idlgrpolyline::init" . 2899675)
  ("idlgrpolyline::setproperty" . 2911673)
  ("idlgrprinter" . 2912054)
  ("idlgrprinter::cleanup" . 2913073)
  ("idlgrprinter::draw" . 2914014)
  ("idlgrprinter::getcontiguouspixels" . 2915368)
  ("idlgrprinter::getfontnames" . 2916599)
  ("idlgrprinter::getproperty" . 2918477)
  ("idlgrprinter::gettextdimensions" . 2920257)
  ("idlgrprinter::init" . 2922402)
  ("idlgrprinter::newdocument" . 2928014)
  ("idlgrprinter::newpage" . 2928357)
  ("idlgrprinter::setproperty" . 2928591)
  ("idlgrroi" . 2928961)
  ("idlgrroi::cleanup" . 2929780)
  ("idlgrroi::getproperty" . 2930512)
  ("idlgrroi::init" . 2932523)
  ("idlgrroi::pickvertex" . 2940290)
  ("idlgrroi::setproperty" . 2941824)
  ("idlgrroigroup" . 2942320)
  ("idlgrroigroup::add" . 2943161)
  ("idlgrroigroup::cleanup" . 2943740)
  ("idlgrroigroup::getproperty" . 2944507)
  ("idlgrroigroup::init" . 2946544)
  ("idlgrroigroup::pickregion" . 2949413)
  ("idlgrroigroup::setproperty" . 2950988)
  ("idlgrscene" . 2951384)
  ("idlgrscene::add" . 2952181)
  ("idlgrscene::cleanup" . 2952759)
  ("idlgrscene::getbyname" . 2953487)
  ("idlgrscene::getproperty" . 2954964)
  ("idlgrscene::init" . 2955882)
  ("idlgrscene::setproperty" . 2958251)
  ("idlgrsurface" . 2958610)
  ("idlgrsurface::cleanup" . 2959325)
  ("idlgrsurface::getctm" . 2960038)
  ("idlgrsurface::getproperty" . 2962184)
  ("idlgrsurface::init" . 2964448)
  ("idlgrsurface::setproperty" . 2984949)
  ("idlgrsymbol" . 2985322)
  ("idlgrsymbol::cleanup" . 2985879)
  ("idlgrsymbol::getproperty" . 2986601)
  ("idlgrsymbol::init" . 2987525)
  ("idlgrsymbol::setproperty" . 2991450)
  ("idlgrtessellator" . 2991814)
  ("idlgrtessellator::addpolygon" . 2993472)
  ("idlgrtessellator::cleanup" . 2995172)
  ("idlgrtessellator::init" . 2995896)
  ("idlgrtessellator::reset" . 2996627)
  ("idlgrtessellator::tessellate" . 2996968)
  ("idlgrtext" . 2998369)
  ("idlgrtext::cleanup" . 2999649)
  ("idlgrtext::getctm" . 3000365)
  ("idlgrtext::getproperty" . 3002491)
  ("idlgrtext::init" . 3004917)
  ("idlgrtext::setproperty" . 3016133)
  ("idlgrview" . 3016485)
  ("idlgrview::add" . 3017289)
  ("idlgrview::cleanup" . 3017691)
  ("idlgrview::getbyname" . 3018407)
  ("idlgrview::getproperty" . 3019828)
  ("idlgrview::init" . 3020949)
  ("idlgrview::setproperty" . 3029273)
  ("idlgrviewgroup" . 3029627)
  ("idlgrviewgroup::add" . 3030760)
  ("idlgrviewgroup::cleanup" . 3031510)
  ("idlgrviewgroup::getbyname" . 3032234)
  ("idlgrviewgroup::getproperty" . 3033667)
  ("idlgrviewgroup::init" . 3034523)
  ("idlgrviewgroup::setproperty" . 3036176)
  ("idlgrvolume" . 3036567)
  ("idlgrvolume::cleanup" . 3037447)
  ("idlgrvolume::computebounds" . 3038185)
  ("idlgrvolume::getctm" . 3039386)
  ("idlgrvolume::getproperty" . 3041607)
  ("idlgrvolume::init" . 3043958)
  ("idlgrvolume::pickvoxel" . 3060232)
  ("idlgrvolume::setproperty" . 3061794)
  ("idlgrvrml" . 3062158)
  ("idlgrvrml::cleanup" . 3065409)
  ("idlgrvrml::draw" . 3066113)
  ("idlgrvrml::getdeviceinfo" . 3066482)
  ("idlgrvrml::getfontnames" . 3068859)
  ("idlgrvrml::getproperty" . 3070728)
  ("idlgrvrml::gettextdimensions" . 3071766)
  ("idlgrvrml::init" . 3073804)
  ("idlgrvrml::setproperty" . 3078331)
  ("idlgrwindow" . 3078695)
  ("idlgrwindow::cleanup" . 3080115)
  ("idlgrwindow::draw" . 3080837)
  ("idlgrwindow::erase" . 3082220)
  ("idlgrwindow::getcontiguouspixels" . 3082656)
  ("idlgrwindow::getdeviceinfo" . 3083829)
  ("idlgrwindow::getfontnames" . 3086201)
  ("idlgrwindow::getproperty" . 3088078)
  ("idlgrwindow::gettextdimensions" . 3090472)
  ("idlgrwindow::iconify" . 3092494)
  ("idlgrwindow::init" . 3093372)
  ("idlgrwindow::pickdata" . 3102311)
  ("idlgrwindow::read" . 3105149)
  ("idlgrwindow::select" . 3105429)
  ("idlgrwindow::setcurrentcursor" . 3107801)
  ("idlgrwindow::setproperty" . 3109967)
  ("idlgrwindow::show" . 3110331)
  ("trackball" . 3110646)
  ("trackball::init" . 3111406)
  ("trackball::reset" . 3113100)
  ("trackball::update" . 3114572)
  ("supported devices" . 3119039)
  ("keywords accepted by the idl devices" . 3121015)
  ("window systems" . 3205928)
  ("printing graphics output files" . 3213514)
  ("the cgm device" . 3220728)
  ("the hp-gl device" . 3224165)
  ("the lj device" . 3227974)
  ("the macintosh display device" . 3235007)
  ("the metafile display device" . 3235714)
  ("the null display device" . 3237558)
  ("the pcl device" . 3237825)
  ("the printer device" . 3241355)
  ("the postscript device" . 3242565)
  ("the regis terminal device" . 3261928)
  ("the tektronix device" . 3263935)
  ("the microsoft windows device" . 3269040)
  ("the x windows device" . 3269786)
  ("the z-buffer device" . 3288369)
  ("graphics keywords" . 3295987)
  ("system variables" . 3328787)
  ("idl operators" . 3386933)
  ("reserved words" . 3405930)
  ("fonts" . 3407247)
  ("overview" . 3407909)
  ("fonts in idl direct vs. object graphics" . 3409610)
  ("about vector fonts" . 3411328)
  ("about truetype fonts" . 3416492)
  ("about device fonts" . 3427493)
  ("choosing a font type" . 3441764)
  ("embedded formatting commands" . 3446157)
  ("formatting command examples" . 3452624)
  ("truetype font samples" . 3457408)
  ("vector font samples" . 3458339)
  ("formats overview" . 3477526)
  ("supports cdf v2.6r7. note that cdf v2.6 files" . 3481568)
  ("variables and attributes" . 3481958)
  ("cdf file options" . 3486138)
  ("creating cdf files" . 3487723)
  ("alphabetical listing of cdf routines" . 3491495)
  ("cdf_attcreate" . 3491618)
  ("cdf_attdelete" . 3492646)
  ("cdf_attexists" . 3495320)
  ("cdf_attget" . 3497882)
  ("cdf_attinq" . 3500615)
  ("cdf_attnum" . 3501882)
  ("cdf_attput" . 3502372)
  ("cdf_attrename" . 3507081)
  ("cdf_close" . 3507661)
  ("cdf_compression" . 3508359)
  ("cdf_control" . 3514865)
  ("cdf_create" . 3527602)
  ("cdf_delete" . 3536192)
  ("cdf_doc" . 3536717)
  ("cdf_encode_epoch" . 3537916)
  ("cdf_epoch" . 3540465)
  ("cdf_error" . 3543405)
  ("cdf_exists" . 3543670)
  ("cdf_inquire" . 3544238)
  ("cdf_lib_info" . 3547567)
  ("cdf_open" . 3549359)
  ("cdf_parse_epoch" . 3550016)
  ("cdf_varcreate" . 3551494)
  ("cdf_vardelete" . 3557671)
  ("cdf_varget" . 3560524)
  ("cdf_varget1" . 3564943)
  ("cdf_varinq" . 3566456)
  ("cdf_varnum" . 3568615)
  ("cdf_varput" . 3569353)
  ("cdf_varrename" . 3571835)
  ("supports hdf 4.1r3. the following" . 3572778)
  ("hdf interfaces" . 3573089)
  ("creating hdf files" . 3576399)
  ("hdf scientific dataset id numbers" . 3578880)
  ("alphabetical listing of hdf routines" . 3584538)
  ("hdf_an_annlen" . 3585062)
  ("hdf_an_annlist" . 3585477)
  ("hdf_an_atype2tag" . 3586450)
  ("hdf_an_create" . 3587419)
  ("hdf_an_createf" . 3588596)
  ("hdf_an_end" . 3589554)
  ("hdf_an_endaccess" . 3589923)
  ("hdf_an_fileinfo" . 3590343)
  ("hdf_an_get_tagref" . 3591628)
  ("hdf_an_id2tagref" . 3593186)
  ("hdf_an_numann" . 3594037)
  ("hdf_an_readann" . 3595598)
  ("hdf_an_select" . 3596385)
  ("hdf_an_start" . 3597296)
  ("hdf_an_tag2atype" . 3597994)
  ("hdf_an_tagref2id" . 3598942)
  ("hdf_an_writeann" . 3599647)
  ("hdf_browser" . 3600549)
  ("hdf_close" . 3600609)
  ("hdf_deldd" . 3600890)
  ("hdf_df24_addimage" . 3601278)
  ("hdf_df24_getimage" . 3604365)
  ("hdf_df24_getinfo" . 3605259)
  ("hdf_df24_lastref" . 3606720)
  ("hdf_df24_nimages" . 3608098)
  ("hdf_df24_readref" . 3608939)
  ("hdf_df24_restart" . 3609313)
  ("hdf_dfan_addfds" . 3609502)
  ("hdf_dfan_addfid" . 3609852)
  ("hdf_dfan_getdesc" . 3611126)
  ("hdf_dfan_getfds" . 3612907)
  ("hdf_dfan_getfid" . 3614953)
  ("hdf_dfan_getlabel" . 3615589)
  ("hdf_dfan_lablist" . 3616619)
  ("hdf_dfan_lastref" . 3618980)
  ("hdf_dfan_putdesc" . 3619180)
  ("hdf_dfan_putlabel" . 3620326)
  ("hdf_dfp_addpal" . 3621439)
  ("hdf_dfp_getpal" . 3621821)
  ("hdf_dfp_lastref" . 3622148)
  ("hdf_dfp_npals" . 3622345)
  ("hdf_dfp_putpal" . 3622678)
  ("hdf_dfp_readref" . 3624658)
  ("hdf_dfp_restart" . 3625021)
  ("hdf_dfp_writeref" . 3625201)
  ("hdf_dfr8_addimage" . 3625685)
  ("hdf_dfr8_getimage" . 3629933)
  ("hdf_dfr8_getinfo" . 3630503)
  ("hdf_dfr8_lastref" . 3631777)
  ("hdf_dfr8_nimages" . 3633080)
  ("hdf_dfr8_putimage" . 3633767)
  ("hdf_dfr8_readref" . 3637527)
  ("hdf_dfr8_restart" . 3637901)
  ("hdf_dfr8_setpalette" . 3638088)
  ("hdf_dupdd" . 3638646)
  ("hdf_exists" . 3639207)
  ("hdf_gr_attrinfo" . 3639740)
  ("hdf_gr_create" . 3641157)
  ("hdf_gr_end" . 3643058)
  ("hdf_gr_endaccess" . 3643995)
  ("hdf_gr_fileinfo" . 3644547)
  ("hdf_gr_findattr" . 3645728)
  ("hdf_gr_getattr" . 3646434)
  ("hdf_gr_getchunkinfo" . 3647737)
  ("hdf_gr_getiminfo" . 3649046)
  ("hdf_gr_getlutid" . 3650628)
  ("hdf_gr_getlutinfo" . 3651219)
  ("hdf_gr_idtoref" . 3652350)
  ("hdf_gr_luttoref" . 3652916)
  ("hdf_gr_nametoindex" . 3653422)
  ("hdf_gr_readimage" . 3654001)
  ("hdf_gr_readlut" . 3656417)
  ("hdf_gr_reftoindex" . 3657065)
  ("hdf_gr_select" . 3657495)
  ("hdf_gr_setattr" . 3658112)
  ("hdf_gr_setchunk" . 3659980)
  ("hdf_gr_setchunkcache" . 3661325)
  ("hdf_gr_setcompress" . 3662004)
  ("hdf_gr_setexternalfile" . 3662921)
  ("hdf_gr_start" . 3664016)
  ("hdf_gr_writeimage" . 3664899)
  ("hdf_gr_writelut" . 3667885)
  ("hdf_hdf2idltype" . 3669231)
  ("hdf_idl2hdftype" . 3669799)
  ("hdf_ishdf" . 3670400)
  ("hdf_lib_info" . 3671043)
  ("hdf_newref" . 3673860)
  ("hdf_number" . 3674122)
  ("hdf_open" . 3674809)
  ("hdf_packdata" . 3676149)
  ("hdf_read" . 3679367)
  ("hdf_sd_adddata" . 3679423)
  ("hdf_sd_attrfind" . 3683049)
  ("hdf_sd_attrinfo" . 3684582)
  ("hdf_sd_attrset" . 3687502)
  ("hdf_sd_create" . 3692837)
  ("hdf_sd_dimget" . 3697611)
  ("hdf_sd_dimgetid" . 3699365)
  ("hdf_sd_dimset" . 3700048)
  ("hdf_sd_end" . 3703232)
  ("hdf_sd_endaccess" . 3704325)
  ("hdf_sd_fileinfo" . 3705292)
  ("hdf_sd_getdata" . 3706647)
  ("hdf_sd_getinfo" . 3708121)
  ("hdf_sd_idtoref" . 3712225)
  ("hdf_sd_iscoordvar" . 3713773)
  ("hdf_sd_nametoindex" . 3714115)
  ("hdf_sd_reftoindex" . 3715045)
  ("hdf_sd_select" . 3715897)
  ("hdf_sd_setcompress" . 3716932)
  ("hdf_sd_setextfile" . 3719358)
  ("hdf_sd_setinfo" . 3721610)
  ("hdf_sd_start" . 3725821)
  ("hdf_unpackdata" . 3727463)
  ("hdf_vd_attach" . 3730780)
  ("hdf_vd_detach" . 3731603)
  ("hdf_vd_fdefine" . 3732010)
  ("hdf_vd_fexist" . 3733312)
  ("hdf_vd_find" . 3733730)
  ("hdf_vd_get" . 3734180)
  ("hdf_vd_getid" . 3736409)
  ("hdf_vd_getinfo" . 3736972)
  ("hdf_vd_getnext" . 3738394)
  ("hdf_vd_insert" . 3739175)
  ("hdf_vd_isvd" . 3739583)
  ("hdf_vd_isvg" . 3740078)
  ("hdf_vd_lone" . 3740564)
  ("hdf_vd_read" . 3741318)
  ("hdf_vd_seek" . 3743001)
  ("hdf_vd_setinfo" . 3743380)
  ("hdf_vd_write" . 3744276)
  ("hdf_vg_addtr" . 3746906)
  ("hdf_vg_attach" . 3747436)
  ("hdf_vg_detach" . 3748330)
  ("hdf_vg_getid" . 3748876)
  ("hdf_vg_getinfo" . 3749448)
  ("hdf_vg_getnext" . 3750677)
  ("hdf_vg_gettr" . 3751463)
  ("hdf_vg_gettrs" . 3751960)
  ("hdf_vg_inqtr" . 3752601)
  ("hdf_vg_insert" . 3753071)
  ("hdf_vg_isvd" . 3753479)
  ("hdf_vg_isvg" . 3753952)
  ("hdf_vg_lone" . 3754433)
  ("hdf_vg_number" . 3755192)
  ("hdf_vg_setinfo" . 3755464)
  ("eos routines" . 3756597)
  ("feature routines" . 3756941)
  ("hdf-eos programming model" . 3758516)
  ("alphabetic listing of eos routines" . 3759169)
  ("eos_eh_convang" . 3759295)
  ("eos_eh_getversion" . 3760236)
  ("eos_eh_idinfo" . 3761177)
  ("eos_exists" . 3762142)
  ("eos_gd_attach" . 3762476)
  ("eos_gd_attrinfo" . 3763212)
  ("eos_gd_blksomoffset" . 3764044)
  ("eos_gd_close" . 3764687)
  ("eos_gd_compinfo" . 3764995)
  ("eos_gd_create" . 3765861)
  ("eos_gd_defboxregion" . 3770176)
  ("eos_gd_defcomp" . 3771198)
  ("eos_gd_defdim" . 3773752)
  ("eos_gd_deffield" . 3774610)
  ("eos_gd_deforigin" . 3776331)
  ("eos_gd_defpixreg" . 3777072)
  ("eos_gd_defproj" . 3777808)
  ("eos_gd_deftile" . 3779517)
  ("eos_gd_deftimeperiod" . 3781308)
  ("eos_gd_defvrtregion" . 3783132)
  ("eos_gd_detach" . 3786732)
  ("eos_gd_diminfo" . 3787261)
  ("eos_gd_dupregion" . 3787810)
  ("eos_gd_extractregion" . 3788964)
  ("eos_gd_fieldinfo" . 3789942)
  ("eos_gd_getfillvalue" . 3790982)
  ("eos_gd_getpixels" . 3791614)
  ("eos_gd_getpixvalues" . 3793019)
  ("eos_gd_gridinfo" . 3794063)
  ("eos_gd_inqattrs" . 3795203)
  ("eos_gd_inqdims" . 3796090)
  ("eos_gd_inqfields" . 3797014)
  ("eos_gd_inqgrid" . 3798194)
  ("eos_gd_interpolate" . 3799039)
  ("eos_gd_nentries" . 3801024)
  ("eos_gd_open" . 3801855)
  ("eos_gd_origininfo" . 3802720)
  ("eos_gd_pixreginfo" . 3803255)
  ("eos_gd_projinfo" . 3803671)
  ("eos_gd_query" . 3804632)
  ("eos_gd_readattr" . 3807742)
  ("eos_gd_readfield" . 3808372)
  ("eos_gd_readtile" . 3809731)
  ("eos_gd_regioninfo" . 3810858)
  ("eos_gd_setfillvalue" . 3812644)
  ("eos_gd_settilecache" . 3813378)
  ("eos_gd_tileinfo" . 3814227)
  ("eos_gd_writeattr" . 3815185)
  ("eos_gd_writefield" . 3816422)
  ("eos_gd_writefieldmeta" . 3818235)
  ("eos_gd_writetile" . 3818927)
  ("eos_pt_attach" . 3820078)
  ("eos_pt_attrinfo" . 3820928)
  ("eos_pt_bcklinkinfo" . 3821747)
  ("eos_pt_close" . 3822480)
  ("eos_pt_create" . 3822792)
  ("eos_pt_defboxregion" . 3823558)
  ("eos_pt_deflevel" . 3824883)
  ("eos_pt_deflinkage" . 3828271)
  ("eos_pt_deftimeperiod" . 3829068)
  ("eos_pt_defvrtregion" . 3830051)
  ("eos_pt_detach" . 3832611)
  ("eos_pt_extractperiod" . 3833096)
  ("eos_pt_extractregion" . 3834261)
  ("eos_pt_fwdlinkinfo" . 3835356)
  ("eos_pt_getlevelname" . 3836086)
  ("eos_pt_getrecnums" . 3836953)
  ("eos_pt_inqattrs" . 3838521)
  ("eos_pt_inqpoint" . 3839276)
  ("eos_pt_levelindx" . 3840141)
  ("eos_pt_levelinfo" . 3840763)
  ("eos_pt_nfields" . 3841855)
  ("eos_pt_nlevels" . 3842472)
  ("eos_pt_nrecs" . 3842936)
  ("eos_pt_open" . 3843512)
  ("eos_pt_periodinfo" . 3844345)
  ("eos_pt_periodrecs" . 3845202)
  ("eos_pt_query" . 3846180)
  ("eos_pt_readattr" . 3847184)
  ("eos_pt_readlevel" . 3847832)
  ("eos_pt_regioninfo" . 3849227)
  ("eos_pt_regionrecs" . 3850137)
  ("eos_pt_sizeof" . 3851125)
  ("eos_pt_updatelevel" . 3851889)
  ("eos_pt_writeattr" . 3853392)
  ("eos_pt_writelevel" . 3854507)
  ("eos_query" . 3855572)
  ("eos_sw_attach" . 3856711)
  ("eos_sw_attrinfo" . 3857477)
  ("eos_sw_close" . 3858294)
  ("eos_sw_compinfo" . 3858603)
  ("eos_sw_create" . 3859439)
  ("eos_sw_defboxregion" . 3860202)
  ("eos_sw_defcomp" . 3862188)
  ("eos_sw_defdatafield" . 3864699)
  ("eos_sw_defdim" . 3866458)
  ("eos_sw_defdimmap" . 3867714)
  ("eos_sw_defgeofield" . 3869764)
  ("eos_sw_defidxmap" . 3871791)
  ("eos_sw_deftimeperiod" . 3873078)
  ("eos_sw_defvrtregion" . 3874876)
  ("eos_sw_detach" . 3878840)
  ("eos_sw_diminfo" . 3879317)
  ("eos_sw_dupregion" . 3879816)
  ("eos_sw_extractperiod" . 3880950)
  ("eos_sw_extractregion" . 3882142)
  ("eos_sw_fieldinfo" . 3883373)
  ("eos_sw_getfillvalue" . 3884553)
  ("eos_sw_idxmapinfo" . 3885185)
  ("eos_sw_inqattrs" . 3886145)
  ("eos_sw_inqdatafields" . 3886927)
  ("eos_sw_inqdims" . 3888011)
  ("eos_sw_inqgeofields" . 3888867)
  ("eos_sw_inqidxmaps" . 3890025)
  ("eos_sw_inqmaps" . 3891045)
  ("eos_sw_inqswath" . 3892199)
  ("eos_sw_mapinfo" . 3893003)
  ("eos_sw_nentries" . 3893958)
  ("eos_sw_open" . 3895072)
  ("eos_sw_periodinfo" . 3895902)
  ("eos_sw_query" . 3897291)
  ("eos_sw_readattr" . 3899674)
  ("eos_sw_readfield" . 3900310)
  ("eos_sw_regioninfo" . 3901722)
  ("eos_sw_setfillvalue" . 3903047)
  ("eos_sw_writeattr" . 3903793)
  ("eos_sw_writedatameta" . 3905032)
  ("eos_sw_writefield" . 3906034)
  ("eos_sw_writegeometa" . 3907888)
  ("idl supports netcdf 2.4. the following" . 3909058)
  ("netcdf data modes" . 3909545)
  ("creating netcdf files" . 3911625)
  ("type conversion" . 3916130)
  ("specifying attributes and variables" . 3916492)
  ("string data in netcdf files" . 3917024)
  ("alphabetical listing of ncdf routines" . 3919304)
  ("ncdf_attcopy" . 3919429)
  ("ncdf_attdel" . 3920850)
  ("ncdf_attget" . 3921699)
  ("ncdf_attinq" . 3922677)
  ("ncdf_attname" . 3925785)
  ("ncdf_attput" . 3926831)
  ("ncdf_attrename" . 3929461)
  ("ncdf_close" . 3930224)
  ("ncdf_control" . 3930665)
  ("ncdf_create" . 3933696)
  ("ncdf_dimdef" . 3935071)
  ("ncdf_dimid" . 3935879)
  ("ncdf_diminq" . 3936325)
  ("ncdf_dimrename" . 3937310)
  ("ncdf_exists" . 3938047)
  ("ncdf_inquire" . 3938635)
  ("ncdf_open" . 3939518)
  ("ncdf_vardef" . 3940083)
  ("ncdf_varget" . 3943444)
  ("ncdf_varget1" . 3945859)
  ("ncdf_varid" . 3947412)
  ("ncdf_varinq" . 3947809)
  ("ncdf_varput" . 3948891)
  ("ncdf_varrename" . 3953145)
  ("overview" . 3955823)
  ("introduction to idl dataminer and odbc" . 3956535)
  ("odbc conformance levels" . 3960490)
  ("where to find additional information" . 3964040)
  ("about this volume" . 3964667)
  ("conventions" . 3966086)
  ("network access requirements" . 3966862)
  ("installation on unix systems" . 3967510)
  ("initialization" . 3968855)
  ("mappings" . 3969540)
  ("error messages" . 3970814)
  ("to use idl dataminer classes to perform actions" . 3973597)
  ("components" . 3974348)
  ("using the db_exists function" . 3975795)
  ("creating a database object" . 3976335)
  ("connecting to a database" . 3978231)
  ("finding tables" . 3981774)
  ("connecting to a table" . 3982852)
  ("working with table data" . 3984111)
  ("example" . 3985963)
  ("odbc sql syntax notes" . 3989359)
  ("idl dataminer api" . 3993210)
  ("dialog_dbconnect()" . 3999021)
  ("db_exists()" . 4000317)
  ("idldbdatabase" . 4000670)
  ("idldbdatabase::connect" . 4001879)
  ("idldbdatabase::executesql" . 4003035)
  ("idldbdatabase::getdatasources" . 4003786)
  ("idldbdatabase::getproperty" . 4004406)
  ("idldbdatabase::gettables" . 4007378)
  ("idldbdatabase::setproperty" . 4008146)
  ("idldbrecordset" . 4010071)
  ("idldbrecordset::addrecord" . 4012072)
  ("idldbrecordset::currentrecord" . 4013491)
  ("idldbrecordset::deleterecord" . 4014132)
  ("idldbrecordset::getfield" . 4014556)
  ("idldbrecordset::getproperty" . 4015473)
  ("idldbrecordset::getrecord" . 4018737)
  ("idldbrecordset::movecursor" . 4019824)
  ("idldbrecordset::nfields" . 4020806)
  ("idldbrecordset::setfield" . 4021016)
  ("overview" . 4022025)
  ("odbc.ini file format" . 4023981)
  ("odbc.ini file example" . 4029551)
  ("supported drivers" . 4030741)
  ("connect odbc for informix" . 4034280)
  ("connect odbc for oracle" . 4060281)
  ("connect odbc for sybase" . 4088922)
  ("connect odbc for text" . 4120823)
  ("the unix environment" . 4162804)
  ("locking and isolation levels" . 4168078)
  ("which you use in sql statements." . 4176823)
  ("api functions" . 4176989)
  ("scalar functions" . 4180843)
  ("overview" . 4197766)
  ("backwards compatibility" . 4197986)
  ("dde routines" . 4200475)
  ("demo_mode" . 4201683)
  ("gethelp" . 4202020)
  ("handle_create" . 4205285)
  ("handle_free" . 4209987)
  ("handle_info" . 4210627)
  ("handle_move" . 4212602)
  ("handle_value" . 4214278)
  ("hdf_dfsd_adddata" . 4216061)
  ("hdf_dfsd_dimget" . 4217461)
  ("hdf_dfsd_dimset" . 4218270)
  ("hdf_dfsd_endslice" . 4219682)
  ("hdf_dfsd_getdata" . 4220083)
  ("hdf_dfsd_getinfo" . 4220850)
  ("hdf_dfsd_getslice" . 4223543)
  ("hdf_dfsd_putslice" . 4224940)
  ("hdf_dfsd_readref" . 4225886)
  ("hdf_dfsd_setinfo" . 4226355)
  ("hdf_dfsd_startslice" . 4231459)
  ("pickfile" . 4235427)
  ("polyfitw" . 4235689)
  ("riemann" . 4238182)
  ("rstrpos" . 4245653)
  ("size executive command" . 4246933)
  ("slicer" . 4250011)
  ("str_sep" . 4260608)
  ("tiff_dump" . 4262351)
  ("tiff_read" . 4262970)
  ("tiff_write" . 4265631)
  ("wided" . 4269010)
  ("widget_message" . 4269350)
  ("calls" . 4269632)
  ("using idl as an rpc server" . 4271232)
  ("the idl rpc library" . 4274271)
  ("free_idl_variable" . 4274688)
  ("get_idl_variable" . 4275190)
  ("idl_server_interactive" . 4278398)
  ("kill_server" . 4279100)
  ("register_idl_client" . 4279494)
  ("send_idl_command" . 4280530)
  ("set_idl_timeout" . 4281799)
  ("set_idl_variable" . 4282278)
  ("set_rpc_verbosity" . 4284190)
  ("unregister_idl_client" . 4284730)
  ("the varinfo_t structure" . 4285243)
  ("v_make_byte" . 4286194)
  ("v_make_complex" . 4286390)
  ("v_make_dcomplex" . 4286584)
  ("v_make_double" . 4286808)
  ("v_make_float" . 4287002)
  ("v_make_int" . 4287196)
  ("v_make_long" . 4287365)
  ("v_make_string" . 4287531)
  ("v_fill_array" . 4287695)
  ("more variable manipulation macros" . 4288808)
  ("management" . 4290680)
  ("rpc examples" . 4294944)
  ("features in idl 5.5" . 4295304)
  ("visualization enhancements" . 4296200)
  ("analysis enhancements" . 4311825)
  ("language enhancements" . 4324661)
  ("user interface toolkit enhancements" . 4353611)
  ("file access enhancements" . 4360942)
  ("development environment enhancements" . 4368573)
  ("scientific data formats enhancements" . 4369025)
  ("idl activex control enhancements" . 4372171)
  ("idl dataminer enhancements" . 4374057)
  ("documentation enhancements" . 4379981)
  ("enhanced idl utilities" . 4380467)
  ("new and enhanced idl objects" . 4386412)
  ("idlgrbuffer::pickdata" . 4387208)
  ("idlgrcontour::getproperty" . 4388287)
  ("idlgrcontour::init" . 4390742)
  ("idlgrcontour::setproperty" . 4392763)
  ("idlgrpolygon::getproperty" . 4395218)
  ("idlgrpolygon::init" . 4397267)
  ("idlgrpolygon::setproperty" . 4399736)
  ("idlgrsurface::getproperty" . 4401785)
  ("idlgrsurface::init" . 4404322)
  ("idlgrsurface::setproperty" . 4407967)
  ("idlgrwindow::pickdata" . 4410051)
  ("new and enhanced idl routines" . 4411125)
  ("new and updated system variables" . 4496983)
  ("platforms supported in this release" . 4500833)
  ("multi-threading in idl" . 4502530)
  ("accelerate your computations." . 4502649)
  ("the idl thread pool" . 4502841)
  ("controlling the thread pool in idl" . 4507313)
  ("routines supporting the thread pool" . 4521598)
  ("in idl" . 4524173)
  ("introduction to idl com objects" . 4524477)
  ("skills required to use com objects" . 4526577)
  ("idl com naming schemes" . 4527834)
  ("using idl idispatch com objects" . 4530048)
  ("using activex controls in idl" . 4543047)
  ("menu widget" . 4558714)
  ("introduction to the shortcut menu widget" . 4559064)
  ("creating a base widget shortcut menu" . 4562401)
  ("creating a draw widget shortcut menu" . 4565394)
  ("creating a list widget shortcut menu" . 4571696)
  ("creating a text widget shortcut menu" . 4578799)
  ("new objects" . 4584989)
  ("idlcomidispatch" . 4585207)
  ("idlcomidispatch::init" . 4586383)
  ("idlcomidispatch::getproperty" . 4588154)
  ("idlcomidispatch::setproperty" . 4589374)
  ("idlffmrsid" . 4590212)
  ("idlffmrsid::cleanup" . 4590726)
  ("idlffmrsid::getdimsatlevel" . 4591145)
  ("idlffmrsid::getimagedata" . 4593058)
  ("idlffmrsid::getproperty" . 4596841)
  ("idlffmrsid::init" . 4600876)
  ("new idl routines" . 4601742)
  ("cpu" . 4601839)
  ("define_msgblk" . 4606723)
  ("define_msgblk_from_file" . 4610890)
  ("erf" . 4616320)
  ("erfc" . 4617231)
  ("erfcx" . 4618172)
  ("file_info" . 4618927)
  ("file_search" . 4625314)
  ("grid_input" . 4653386)
  ("griddata" . 4659995)
  ("hdf_vd_attrfind" . 4704978)
  ("hdf_vd_attrinfo" . 4705945)
  ("hdf_vd_attrset" . 4707770)
  ("hdf_vd_isattr" . 4716743)
  ("hdf_vd_nattrs" . 4717351)
  ("heap_free" . 4718172)
  ("interval_volume" . 4722176)
  ("path_sep" . 4727191)
  ("qgrid3" . 4728170)
  ("qhull" . 4734980)
  ("query_mrsid" . 4738793)
  ("read_mrsid" . 4742348)
  ("real_part" . 4745899)
  ("region_grow" . 4746955)
  ("simplex" . 4751429)
  ("widget_activex" . 4758008)
  ("widget_displaycontextmenu" . 4770895)
  ("xobjview_rotate" . 4773351)
  ("xobjview_write_image" . 4775014)
  ("xroi" . 4776155)
  ("new examples" . 4805873)
  ("overview of new examples" . 4807045)
  ("mapping an image onto a surface" . 4811182)
  ("centering an image object" . 4814421)
  ("image object" . 4819242)
  ("working with mesh objects and routines" . 4824244)
  ("copying and printing objects" . 4852950)
  ("capturing idl direct graphics displays" . 4868029)
  ("creating and restoring .sav files" . 4874242)
  ("handling table widgets in guis" . 4883124)
  ("finding straight lines in images" . 4892226)
  ("color density contrasting in an image" . 4894955)
  ("removing noise from an image with fft" . 4898764)
  ("using double and triple integration" . 4902163)
  ("obtaining irregular grid intervals" . 4907951)
  ("functions" . 4910703)
  ("determining bessel function accuracy" . 4915271)
  ))

;; Some routines need to be searched under a different name.
;; Here are the required translations.
(setq idlwave-help-name-translations
 '(
  ("openr" . "open")
  ("openu" . "open")
  ("openw" . "open")
  ("print" . "print/printf")
  ("printf" . "print/printf")
  ("read" . "read/readf")
  ("readf" . "read/readf")
  ))

;; The description for a keyword is not always in the same topic.
;; Some keywords apply to many routines (graphics keywords).  Keywords
;; listed by INIT methods are not repeated for GETPROPERTY or SETPROPERTY.
;; Thus, if a keyword description is not found, we sometimes must search
;; the description of additional routines as well.
;; The following list associates routines with other routines where keyword
;; descriptions might be found.  Currently we assume that we do not need to
;; look in different object classes.  This would be necessary for inheritance,
;; but we do not treat this case currently.
(setq idlwave-help-alt-names
 '(
  ("axis" "graphics keywords")
  ("contour" "plot" "graphics keywords")
  ("draw_roi" "graphics keywords")
  ("map_continents" "graphics keywords")
  ("map_grid" "graphics keywords")
  ("map_set" "graphics keywords")
  ("oplot" "graphics keywords")
  ("plot" "graphics keywords")
  ("plot_3dbox" "plot" "graphics keywords")
  ("plots" "graphics keywords")
  ("polyfill" "graphics keywords")
  ("shade_surf" "graphics keywords")
  ("surface" "plot" "graphics keywords")
  ("xyouts" "graphics keywords")
  ("getproperty" "init")
  ("setproperty" "init")
  ("device" "keywords accepted by the idl devices")
  ("system variables" "controlling the thread pool in idl")
  ))

;; Special words in the source files with associated help topics
(defconst idlwave-help-special-topic-words
  '(
    ("pro")
    ("end" . "pro")
    ("function")
    ("return")
    ("if" . "if...then...else")
    ("then" . "if...then...else")
    ("endif" . "if...then...else")
    ("else" . "if...then...else")
    ("endelse" . "if...then...else")
    ("for")
    ("endfor" . "for")
    ("while" . "while...do")
    ("endwhile" . "while...do")
    ("do" . "while...do")
    ("repeat" . "repeat...until")
    ("endrep" . "repeat...until")
    ("until" . "repeat...until")
    ("case")
    ("of" . "case")
    ("endcase" . "case")
    ("switch")
    ("endswitch" . "switch")
    ("break")
    ("continue")
    ("begin" . "begin...end")
    ("common")
    ("goto")
    ("forward_function")
    ("compile_opt")
    ("on_ioerror")
)
  "Association list of help topics for special context words.")

  ;; What would be a good width for a dedicated help frame?
(setq idlwave-help-frame-width 67)

;;; idlw-help.el ends here
