;;; idlw-complete --- IDLWAVE completion  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2024  Free Software Foundation, Inc.

;;; Comentary
;;
;; Completion and displaying routine calling sequences

;;; Code:

(require 'idlw-variables)

;;----------------------------------------------------
;; Internal variables
(defvar idlwave-completion-help-info nil
  "Global variable passing information for invoking help during completions.
Format: (WHAT NAME TYPE CLASS KWD SUPER-CLASSES)"
)
(defvar idlwave-completion-help-links nil)
(defvar idlwave-current-obj_new-class nil)
(defvar idlwave--method-selector)
(defvar idlwave--class-selector)
(defvar idlwave--type-selector)
(defvar idlwave--super-classes)
(defvar idlwave-before-completion-wconf nil
  "The window configuration just before the completion buffer was displayed.")
(define-obsolete-variable-alias 'idlwave-complete-special
  'idlwave-complete-functions "28.1")
(defvar idlwave-complete-functions nil
  ;; FIXME: Use `completion-at-point-functions' instead!
  "List of special completion functions.
These functions are called for each completion.  Each function must
check if its own special completion context is present.  If yes, it
should use `idlwave-complete-in-buffer' to do some completion and
return t.  If such a function returns t, *no further* attempts to
complete other contexts will be done.  If the function returns nil,
other completions will be tried.")
(defvar idlwave--complete-after-success-function #'ignore
  "A function to evaluate after successful completion.")
(defvar idlwave--complete-after-success-form-force-function #'ignore
  "A function to evaluate after completion selection in *Completions* buffer.")
(defconst idlwave-completion-mark (make-marker)
  "A mark pointing to the beginning of the completion string.")
(defvar idlwave-completion-setup-hook nil)

;;----------------------------------------------------
;; General Completion system

(defun idlwave-complete (&optional arg module class)
  "Complete a function, procedure (method) or keyword name at point.
This function is smart and figures out what can be completed at
this point.  Extensions are supported.

- At the beginning of a statement it completes procedure names.
- In the middle of a statement it completes function names.
- After a `(' or `,' in the argument list of a function or procedure,
  it completes a keyword of the relevant function or procedure.
- In the first arg of `OBJ_NEW', it completes a class name.

When several completions are possible, a list will be displayed in
the *Completions* buffer.  If this list is too long to fit into the
window, scrolling can be achieved by repeatedly pressing
\\[idlwave-complete].

The function also knows about object methods.  When it needs a class
name, the action depends upon `idlwave-query-class', which see.  You
can force IDLWAVE to ask you for a class name with a
\\[universal-argument] prefix argument to this command.

See also the customizable variables
`idlwave-keyword-completion-adds-equal' and
`idlwave-function-completion-adds-paren'.

The optional ARG can be used to force the completion type in order
to override IDLWAVE's idea of what should be completed at point.
Possible values are:

0  <=>  query for the completion type
1  <=>  `procedure'
2  <=>  `procedure-keyword'
3  <=>  `function'
4  <=>  `function-keyword'
5  <=>  `procedure-method'
6  <=>  `procedure-method-keyword'
7  <=>  `function-method'
8  <=>  `function-method-keyword'
9  <=>  `class'

As a special case, the universal argument \\[universal-argument] forces completion
of function names in places where the default would be, e.g., a
keyword.

Two prefix argument, \\[universal-argument] \\[universal-argument] prompts for a regexp by which to
limit completion list, limited to the list of completions which
would have been generated.

For Lisp programmers only:
When we force a keyword, optional argument MODULE can contain the module name.
When we force a method or a method keyword, CLASS can specify the class."
  (interactive "P")
  (idlwave-routines)
  (defvar idlwave-force-class-query)
  (let* ((where-list
	  (if (and arg
		   (or (and (integerp arg) (not (equal arg '(16))))
		       (symbolp arg)))
	      ;; Force the idea of "where" we are
	      (idlwave-make-force-complete-where-list arg module class)
	    (idlwave-where)))
	 (what (nth 2 where-list))
	 (idlwave-force-class-query (equal arg '(4)))
	 (completion-regexp-list
	  (if (equal arg '(16))
	      (list (read-string (concat "Completion Regexp: "))))))

    (if (and module (string-match "::" module))
	(setq class (substring module 0 (match-beginning 0))
	      module (substring module (match-end 0))))

    (cond
     ;; Just scroll the completions list on repeat commands
     ((and (null arg)
	   (eq (car-safe last-command) 'idlwave-display-completion-list)
	   (get-buffer-window "*Completions*"))
      (setq this-command last-command)
      (idlwave-scroll-completions))

     ;; Complete a filename in quotes
     ((and (idlwave-in-quote)
	   (not (eq what 'class)))
      (idlwave-complete-filename))

     ;; Check for any special completion functions
     ((with-demoted-errors "%S"
	(run-hook-with-args-until-success 'idlwave-complete-functions)))

     ((null what)
      (error "Nothing to complete here"))

     ;; Complete a class name
     ((eq what 'class)
      (setq idlwave-completion-help-info '(class))
      (idlwave-complete-class))

     ((eq what 'procedure)
      ;; Complete a procedure name
      (let* ((cw-list (nth 3 where-list))
	     (idlwave--class-selector (idlwave-determine-class cw-list 'pro))
	     (idlwave--super-classes
	      (unless (idlwave-explicit-class-listed cw-list)
		(idlwave-all-class-inherits idlwave--class-selector)))
	     (isa (concat "procedure"
	                  (if idlwave--class-selector "-method" "")))
	     (idlwave--type-selector 'pro))
	(setq idlwave-completion-help-info
	      (list 'routine nil
	            idlwave--type-selector idlwave--class-selector
	            nil idlwave--super-classes))
	(idlwave-complete-in-buffer
	 'procedure (if idlwave--class-selector 'method 'routine)
	 (idlwave-routines) 'idlwave-selector
	 (format "Select a %s name%s"
		 isa
		 (if idlwave--class-selector
		     (format " (class is %s)"
			     (if (eq idlwave--class-selector t)
				 "unknown" idlwave--class-selector))
		   ""))
	 isa
	 'idlwave-attach-method-classes 'idlwave-add-file-link-selector)))

     ((eq what 'function)
      ;; Complete a function name
      (let* ((cw-list (nth 3 where-list))
	     (idlwave--class-selector (idlwave-determine-class cw-list 'fun))
	     (idlwave--super-classes
	      (unless (idlwave-explicit-class-listed cw-list)
		(idlwave-all-class-inherits idlwave--class-selector)))
	     (isa (concat "function" (if idlwave--class-selector "-method" "")))
	     (idlwave--type-selector 'fun))
	(setq idlwave-completion-help-info
	      (list 'routine nil
	            idlwave--type-selector idlwave--class-selector
	            nil idlwave--super-classes))
	(idlwave-complete-in-buffer
	 'function (if idlwave--class-selector 'method 'routine)
	 (idlwave-routines) 'idlwave-selector
	 (format "Select a %s name%s"
		 isa
		 (if idlwave--class-selector
		     (format " (class is %s)"
			     (if (eq idlwave--class-selector t)
				 "unknown" idlwave--class-selector))
		   ""))
	 isa
	 'idlwave-attach-method-classes 'idlwave-add-file-link-selector)))

     ((and (memq what '(procedure-keyword function-keyword)) ; Special Case
	   (equal arg '(4)))
      (idlwave-complete 3)) 		;force function completion

     ((eq what 'procedure-keyword)
      ;; Complete a procedure keyword
      (let* ((where (nth 3 where-list))
	     (name  (car where))
	     (idlwave--method-selector name)
	     (idlwave--type-selector 'pro)
	     (class (idlwave-determine-class where 'pro))
	     (idlwave--class-selector class)
	     (idlwave--super-classes (idlwave-all-class-inherits
	                              idlwave--class-selector))
	     (isa (format "procedure%s-keyword" (if class "-method" "")))
	     (entry (idlwave-best-rinfo-assq
		     name 'pro class (idlwave-routines)))
	     (system (if entry (eq (car (nth 3 entry)) 'system)))
	     (list (idlwave-entry-keywords entry 'do-link)))
	(unless (or entry (eq class t))
	  (error "Nothing known about procedure %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'pro class list
					 idlwave--super-classes system))
	(unless list (error "No keywords available for procedure %s"
			    (idlwave-make-full-name class name)))
	(setq idlwave-completion-help-info
	      (list 'keyword name
	            idlwave--type-selector idlwave--class-selector
	            entry idlwave--super-classes))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for procedure %s%s"
		 (idlwave-make-full-name class name)
		 (if (or (member '("_EXTRA") list)
			 (member '("_REF_EXTRA") list))
		     " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))

     ((eq what 'function-keyword)
      ;; Complete a function keyword
      (let* ((where (nth 3 where-list))
	     (name  (car where))
	     (idlwave--method-selector name)
	     (idlwave--type-selector 'fun)
	     (class (idlwave-determine-class where 'fun))
	     (idlwave--class-selector class)
	     (idlwave--super-classes (idlwave-all-class-inherits
	                              idlwave--class-selector))
	     (isa (format "function%s-keyword" (if class "-method" "")))
	     (entry (idlwave-best-rinfo-assq
		     name 'fun class (idlwave-routines)))
	     (system (if entry (eq (car (nth 3 entry)) 'system)))
	     (list (idlwave-entry-keywords entry 'do-link))
	     msg-name)
	(unless (or entry (eq class t))
	  (error "Nothing known about function %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'fun class list
					 idlwave--super-classes system))
	;; OBJ_NEW: Messages mention the proper Init method
	(setq msg-name (if (and (null class)
				(string= (upcase name) "OBJ_NEW"))
			   (concat idlwave-current-obj_new-class
				   "::Init (via OBJ_NEW)")
			 (idlwave-make-full-name class name)))
	(unless list (error "No keywords available for function %s"
			    msg-name))
	(setq idlwave-completion-help-info
	      (list 'keyword name
	            idlwave--type-selector idlwave--class-selector
	            nil idlwave--super-classes))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for function %s%s" msg-name
		 (if (or (member '("_EXTRA") list)
			 (member '("_REF_EXTRA") list))
		     " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))

     (t (error "This should not happen (idlwave-complete)")))))

(defun idlwave-complete-in-buffer (type stype list selector prompt isa
					&optional prepare-display-function
					special-selector)
  "Perform TYPE completion of word before point against LIST.
SELECTOR is the PREDICATE argument for the completion function.  Show
PROMPT in echo area.  TYPE is one of the intern types, e.g. `function',
`procedure', `class-tag', `keyword', `sysvar', etc.  SPECIAL-SELECTOR is
used only once, for `all-completions', and can be used to, e.g.,
accumulate information on matching completions."
  (let* ((completion-ignore-case t)
	 beg (end (point)) slash part spart completion all-completions
	 dpart dcompletion)

    (unless list
      (error (concat prompt ": No completions available")))

    ;; What is already in the buffer?
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_$")
      (setq slash (eq (preceding-char) ?/)
	    beg (point)
	    idlwave--complete-after-success-function
	    (lambda () (idlwave-after-successful-completion
		   type slash beg))
	    idlwave--complete-after-success-form-force-function
	    (lambda () (idlwave-after-successful-completion
		   type slash 'force))))

    ;; Try a completion
    (setq part (buffer-substring beg end)
	  dpart (downcase part)
	  spart (idlwave-sintern stype part)
	  completion (try-completion part list selector)
	  dcompletion (if (stringp completion) (downcase completion))
	  idlwave-completion-help-links nil)
    (cond
     ((null completion)
      ;; nothing available.
      (error (concat prompt ": no completion for \"%s\"") part))
     ((and (not (equal dpart dcompletion))
	   (not (eq t completion)))
      ;; We can add something
      (delete-region beg end)
      (insert (if (and (string= part dpart)
                       (or (not (string= part ""))
                           idlwave-complete-empty-string-as-lower-case)
                       (not idlwave-completion-force-default-case))
                  dcompletion
                completion))
      (if (eq t (try-completion completion list selector))
	  ;; Now this is a unique match
	  (idlwave-after-successful-completion type slash beg))
      t)
     ((or (eq completion t)
	  (and (= 1 (length (setq all-completions
				  (idlwave-uniquify
				   (all-completions part list
						    (or special-selector
							selector))))))
	       (equal dpart dcompletion)))
      ;; This is already complete
      (idlwave-after-successful-completion type slash beg)
      (message "%s is already the complete %s" part isa)
      nil)
     (t
      ;; We cannot add something - offer a list.
      (message "Making completion list...")

      (unless idlwave-completion-help-links ; already set somewhere?
	(mapc (lambda (x)  ; Pass link prop through to highlight-linked
		(let ((link (get-text-property 0 'link (car x))))
		  (if link
		      (push (cons (car x) link)
			    idlwave-completion-help-links))))
	      list))
      (let* ((list all-completions)
	     ;; "complete" means, this is already a valid completion
	     (complete (memq spart all-completions)))

	(setq list (sort list (lambda (a b)
				(string< (downcase a) (downcase b)))))
	(if prepare-display-function
	    (setq list (funcall prepare-display-function list)))
	(if (and (string= part dpart)
		 (or (not (string= part ""))
		     idlwave-complete-empty-string-as-lower-case)
		 (not idlwave-completion-force-default-case))
	    (setq list (mapcar (lambda (x)
				 (if (listp x)
				     (setcar x (downcase (car x)))
				   (setq x (downcase x)))
				 x)
			       list)))
	(idlwave-display-completion-list list prompt beg complete))
      t))))

(defun idlwave-scroll-completions (&optional message)
  "Scroll the completion window on this frame."
  (let ((cwin (get-buffer-window "*Completions*" 'visible))
	(win (selected-window)))
    (unwind-protect
	(progn
	  (select-window cwin)
	  (condition-case nil
	      (scroll-up)
	    (error (if (and (listp last-command)
			    (nth 2 last-command))
		       (progn
			 (select-window win)
			 (funcall idlwave--complete-after-success-function))
		     (set-window-start cwin (point-min)))))
	  (and message (message "%s" message)))
      (select-window win))))

(defun idlwave-display-completion-list (list &optional message beg complete)
  "Display the completions in LIST in the completions buffer and echo MESSAGE."
  (unless (and (get-buffer-window "*Completions*")
	       (idlwave-local-value 'idlwave-completion-p "*Completions*"))
    (move-marker idlwave-completion-mark beg)
    (setq idlwave-before-completion-wconf (current-window-configuration)))

  (idlwave-display-completion-list-1 list)

  ;; Store a special value in `this-command'.  When `idlwave-complete'
  ;; finds this in `last-command', it will scroll the *Completions* buffer.
  (setq this-command (list 'idlwave-display-completion-list message complete))

  ;; Mark the completions buffer as created by cib
  (idlwave-set-local 'idlwave-completion-p t "*Completions*")

  ;; Fontify the classes
  (if (and idlwave-completion-fontify-classes
           (consp (car list)))
      (idlwave-completion-fontify-classes))

  ;; Run the hook
  (run-hooks 'idlwave-completion-setup-hook)

  ;; Display the message
  (message "%s" (or message "Making completion list...done")))

(defun idlwave-add-file-link-selector (a)
  ;; Record a file link, if any, for the tested names during selection.
  (let ((sel (idlwave-selector a)) file)
    (if (and sel (setq file (idlwave-entry-has-help a)))
	(push (cons (car a) file) idlwave-completion-help-links))
    sel))

(defun idlwave-after-successful-completion (type slash &optional verify)
  "Add `=' or `(' after successful completion of keyword and function.
Restore the pre-completion window configuration if possible."
  (cond
   ((eq type 'procedure)
    nil)
   ((eq type 'function)
    (cond
     ((equal idlwave-function-completion-adds-paren nil) nil)
     ((or (equal idlwave-function-completion-adds-paren t)
	  (equal idlwave-function-completion-adds-paren 1))
      (insert "("))
     ((equal idlwave-function-completion-adds-paren 2)
      (insert "()")
      (backward-char 1))
     (t nil)))
   ((eq type 'keyword)
    (if (and idlwave-keyword-completion-adds-equal
	     (not slash))
	(progn (insert "=") t)
      nil)))

  ;; Restore the pre-completion window configuration if this is safe.
  (if (or (eq verify 'force)                                    ; force
	  (and
	   (get-buffer-window "*Completions*")                  ; visible
	   (idlwave-local-value 'idlwave-completion-p
				"*Completions*")                ; cib-buffer
	   (eq (marker-buffer idlwave-completion-mark)
	       (current-buffer))                                ; buffer OK
	   (equal (marker-position idlwave-completion-mark)
		  verify)))                                     ; pos OK
      (idlwave-restore-wconf-after-completion))
  (move-marker idlwave-completion-mark nil)
  (setq idlwave-before-completion-wconf nil))

(defun idlwave-make-force-complete-where-list (what &optional module class)
  ;; Return an artificial WHERE specification to force the completion
  ;; routine to complete a specific item independent of context.
  ;; WHAT is the prefix arg of `idlwave-complete', see there for details.
  ;; MODULE and CLASS can be used to specify the routine name and class.
  ;; The class name will also be found in MODULE if that is like "class::mod".
  (let* ((what-list '(("procedure") ("procedure-keyword")
		      ("function") ("function-keyword")
		      ("procedure-method") ("procedure-method-keyword")
		      ("function-method") ("function-method-keyword")
		      ("class")))
	 (module (idlwave-sintern-routine-or-method module class))
	 (class (idlwave-sintern-class class))
	 (what (cond
		((equal what 0)
		 (setq what
		       (intern (completing-read
				"Complete what? " what-list nil t))))
		((integerp what)
		 (setq what (intern (car (nth (1- what) what-list)))))
		((and what
		      (symbolp what)
		      (assoc (symbol-name what) what-list))
		 what)
		(t (error "Invalid WHAT"))))
	 (nil-list '(nil nil nil nil))
	 (class-list (list nil nil (or class t) nil)))

    (cond

     ((eq what 'procedure)
      (list nil-list nil-list 'procedure nil-list nil))

     ((eq what 'procedure-keyword)
      (let* ((idlwave--class-selector nil)
	     (idlwave--super-classes nil)
	     (idlwave--type-selector 'pro)
	     (pro (or module
		      (idlwave-completing-read
		       "Procedure: " (idlwave-routines) 'idlwave-selector))))
	(setq pro (idlwave-sintern-routine pro))
	(list nil-list nil-list 'procedure-keyword
	      (list pro nil nil nil) nil)))

     ((eq what 'function)
      (list nil-list nil-list 'function nil-list nil))

     ((eq what 'function-keyword)
      (let* ((idlwave--class-selector nil)
	     (idlwave--super-classes nil)
	     (idlwave--type-selector 'fun)
	     (func (or module
		       (idlwave-completing-read
			"Function: " (idlwave-routines) 'idlwave-selector))))
	(setq func (idlwave-sintern-routine func))
	(list nil-list nil-list 'function-keyword
	      (list func nil nil nil) nil)))

     ((eq what 'procedure-method)
      (list nil-list nil-list 'procedure class-list nil))

     ((eq what 'procedure-method-keyword)
      (let* ((class (idlwave-determine-class class-list 'pro))
	     (idlwave--class-selector class)
	     (idlwave--super-classes (idlwave-all-class-inherits
	                              idlwave--class-selector))
	     (idlwave--type-selector 'pro)
	     (pro (or module
		      (idlwave-completing-read
		       (format "Procedure in %s class: "
		               idlwave--class-selector)
		       (idlwave-routines) 'idlwave-selector))))
	(setq pro (idlwave-sintern-method pro))
	(list nil-list nil-list 'procedure-keyword
	      (list pro nil class nil) nil)))

     ((eq what 'function-method)
      (list nil-list nil-list 'function class-list nil))

     ((eq what 'function-method-keyword)
      (let* ((class (idlwave-determine-class class-list 'fun))
	     (idlwave--class-selector class)
	     (idlwave--super-classes (idlwave-all-class-inherits
	                              idlwave--class-selector))
	     (idlwave--type-selector 'fun)
	     (func (or module
		       (idlwave-completing-read
			(format "Function in %s class: "
			        idlwave--class-selector)
			(idlwave-routines) 'idlwave-selector))))
	(setq func (idlwave-sintern-method func))
	(list nil-list nil-list 'function-keyword
	      (list func nil class nil) nil)))

     ((eq what 'class)
      (list nil-list nil-list 'class nil-list nil))

     (t (error "Invalid value for WHAT")))))

(defun idlwave-call-special (functions &rest args)
  (declare (obsolete run-hook-with-args-until-success "28.1"))
  (let ((funcs functions)
	fun ret)
    (catch 'exit
      (while (setq fun (pop funcs))
	(if (setq ret (apply fun args))
	    (throw 'exit ret)))
      nil)))

(defun idlwave-completing-read (&rest args)
  ;; Completing read, case insensitive
  (let ((old-value (default-value 'completion-ignore-case)))
    (unwind-protect
	(progn
	  (setq-default completion-ignore-case t)
	  (apply #'completing-read args))
      (setq-default completion-ignore-case old-value))))

(defun idlwave-choose (function &rest args)
  "Call FUNCTION as a completion chooser and pass ARGS to it."
  (let ((completion-ignore-case t))	    ; install correct value
    (apply function args))
  (if (and (derived-mode-p 'idlwave-shell-mode)
	   (not font-lock-mode))
      ;; For the shell, remove the fontification of the word before point
      (let ((beg (save-excursion
		   (skip-chars-backward "a-zA-Z0-9_")
		   (point))))
	(remove-text-properties beg (point) '(face nil))))
  (funcall idlwave--complete-after-success-form-force-function))

(defun idlwave-choose-completion (&rest args)
  "Choose the completion that point is in or next to."
  (interactive (list last-nonmenu-event))
  (apply #'idlwave-choose #'choose-completion args))

;;----------------------------------------------------
;; Mouse/Interaction/Fontification

(defvar idlwave-completion-map nil
  "Keymap for `completion-list-mode' with `idlwave-complete'.")

(define-obsolete-function-alias 'idlwave-display-completion-list-emacs
  #'idlwave-display-completion-list-1 "28.1")

(defun idlwave-display-completion-list-1 (list)
  "Display completion list and install the choose wrappers."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list list))
  (with-current-buffer "*Completions*"
    (use-local-map
     (or idlwave-completion-map
	 (setq idlwave-completion-map
	       (idlwave-make-modified-completion-map
		(current-local-map)))))))

(define-obsolete-function-alias 'idlwave-make-modified-completion-map-emacs
  #'idlwave-make-modified-completion-map "28.1")

(defun idlwave-make-modified-completion-map (old-map)
  "Replace `choose-completion' in OLD-MAP."
  (let ((new-map (copy-keymap old-map)))
    (substitute-key-definition
     #'choose-completion #'idlwave-choose-completion new-map)
    (define-key new-map [mouse-3] #'idlwave-mouse-completion-help)
    new-map))

(define-obsolete-function-alias 'idlwave-mouse-choose-completion
  #'idlwave-choose-completion "28.1")

(defun idlwave-restore-wconf-after-completion ()
  "Restore the old (before completion) window configuration."
  (and idlwave-completion-restore-window-configuration
       idlwave-before-completion-wconf
       (set-window-configuration idlwave-before-completion-wconf)))

(defun idlwave-completion-fontify-classes ()
  "Goto the *Completions* buffer and fontify the class info."
  (with-current-buffer "*Completions*"
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-read-only nil))
	(while (re-search-forward "\\.*<[^>]+>" nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'font-lock-string-face))))))


;;----------------------------------------------------
;; Filenames

(defvar idlwave-shell-default-directory)
(defun idlwave-complete-filename ()
  "Use the comint stuff to complete a file name."
  (require 'comint)
  (dlet ((comint-file-name-chars "~/A-Za-z0-9+@:_.$#%={}\\-")
	 (comint-completion-addsuffix nil)
	 (default-directory
	   (if (and (boundp 'idlwave-shell-default-directory)
		    (stringp idlwave-shell-default-directory)
		    (file-directory-p idlwave-shell-default-directory))
	       idlwave-shell-default-directory
	     default-directory)))
    (comint-dynamic-complete-filename)))

;;----------------------------------------------------
;; Classes

(defun idlwave-complete-class ()
  "Complete a class at point."
  (interactive)
  ;; Call `idlwave-routines' to make sure the class list will be available
  (idlwave-routines)
  ;; Check for the special case of completing empty string after pro/function
  (if (let ((case-fold-search t))
	(save-excursion
	  (and
	   (re-search-backward "\\<\\(pro\\|function\\)[ \t]+\\="
			       (- (point) 15) t)
	   (goto-char (point-min))
	   (re-search-forward
	    "^[ \t]*\\(pro\\|function\\)[ \t]+\\([a-zA-Z0-9_]+::\\)" nil t))))
      ;; Yank the full class specification
      (insert (match-string 2))
    ;; Do the completion, using list gathered from `idlwave-routines'
    (idlwave-complete-in-buffer
     'class 'class (idlwave-class-alist) nil
     "Select a class" "class"
     (lambda (list) ;; Push it to help-links if system help available
       (mapcar (lambda (x)
                 (let* ((entry (idlwave-class-info x))
                        (link (nth 1 (assq 'link entry))))
                   (if link (push (cons x link)
                                  idlwave-completion-help-links))
                   x))
               list)))))

;; Completion selector/predicate function
(defun idlwave-selector (a)
  (and (eq (nth 1 a) idlwave--type-selector)
       (or (and (nth 2 a) (eq idlwave--class-selector t))
	   (eq (nth 2 a) idlwave--class-selector)
	   (memq (nth 2 a) idlwave--super-classes))))

(defun idlwave-attach-classes (list type show-classes)
  ;; Attach the proper class list to a LIST of completion items.
  ;; TYPE, when 'kwd, shows classes for method keywords, when
  ;; 'class-tag, for class tags, and otherwise for methods.
  ;; SHOW-CLASSES is the value of `idlwave-completion-show-classes'.
  (if (or (null show-classes)           ; don't want to see classes
	  (null idlwave--class-selector)         ; not a method call
	  (and
	   (stringp idlwave--class-selector) ; the class is already known
	   (not idlwave--super-classes)))    ; no possibilities for inheritance
      ;; In these cases, we do not have to do anything
      list
    (let* ((do-prop (>= show-classes 0))
	   (do-buf (not (= show-classes 0)))
	   ;; (do-dots t)
	   (inherit (if (and (not (eq type 'class-tag)) idlwave--super-classes)
			(cons idlwave--class-selector idlwave--super-classes)))
	   (max (abs show-classes))
	   (lmax ;; (if do-dots
	             (apply #'max (mapcar #'length list)))
	  classes nclasses class-info space)
      (mapcar
       (lambda (x)
	 ;; get the classes
	 (if (eq type 'class-tag)
	     ;; Just one class for tags
	     (setq classes
		   (list
		    (idlwave-class-or-superclass-with-tag
		     idlwave--class-selector x)))
	   ;; Multiple classes for method or method-keyword
	   (setq classes
		 (if (eq type 'kwd)
		     (idlwave-all-method-keyword-classes
		      idlwave--method-selector x idlwave--type-selector)
		   (idlwave-all-method-classes x idlwave--type-selector)))
	   (if inherit
	       (setq classes
		     (delq nil
			   (mapcar (lambda (x) (if (memq x inherit) x nil))
				   classes)))))
	 (setq nclasses (length classes))
	 ;; Make the separator between item and class-info
	 ;; (if do-dots
	 (setq space (concat " " (make-string (- lmax (length x)) ?.)))
	 ;; (setq space " "))
	 (if  do-buf
	     ;; We do want info in the buffer
	     (if (<= nclasses max)
		 (setq class-info (concat
				   space
				   "<" (mapconcat #'identity classes ",") ">"))
	       (setq class-info (format "%s<%d classes>" space nclasses)))
	   (setq class-info nil))
	 (when do-prop
	   ;; We do want properties
	   (setq x (copy-sequence x))
	   (put-text-property 0 (length x)
                              'help-echo (mapconcat #'identity classes " ")
                              x))
	 (if class-info
	     (list x class-info)
	   x))
       list))))

(defun idlwave-attach-method-classes (list)
  ;; Call idlwave-attach-classes with method parameters
  (idlwave-attach-classes list 'method idlwave-completion-show-classes))

(defun idlwave-attach-keyword-classes (list)
  ;; Call idlwave-attach-classes with keyword parameters
  (idlwave-attach-classes list 'kwd idlwave-completion-show-classes))

(defun idlwave-attach-class-tag-classes (list)
  ;; Call idlwave-attach-classes with class structure tags
  (idlwave-attach-classes list 'class-tag idlwave-completion-show-classes))

;;----------------------------------------------------
;; Class structure tags

(defmacro idlwave-new-sintern-type (tag)
  "Define a variable and a function to sintern the new type TAG.
This defines the function `idlwave-sintern-TAG' and the variable
`idlwave-sint-TAGs'."
  (let* ((name (symbol-name tag))
	 (names (concat name "s"))
	 (var (intern (concat "idlwave-sint-" names)))
	 (func (intern (concat "idlwave-sintern-" name))))
    `(progn
       (defvar ,var nil)      ; Initial value of the association list.
       (defun ,func (name &optional set)
	 (cond ((not (stringp name)) name)
	       ((cdr (assoc (downcase name) ,var)))
	       (set
		(setq ,var (cons (cons (downcase name) name) ,var))
		name)
	       (name))))))

(defvar idlwave-current-tags-class nil)
(defvar idlwave-current-class-tags nil)
(defvar idlwave-current-native-class-tags nil)

(idlwave-new-sintern-type class-tag)
(add-hook 'idlwave-complete-functions #'idlwave-complete-class-structure-tag)
(add-hook 'idlwave-update-rinfo-hook #'idlwave-class-tag-reset)

(defun idlwave-complete-class-structure-tag ()
  "Complete a structure tag on a `self' argument in an object method."
  (interactive)
  (let ((pos (point))
	(case-fold-search t))
    (if (save-excursion
	  ;; Check if the context is right
	  (skip-chars-backward "a-zA-Z0-9._$")
	  (and (< (point) (- pos 4))
	       (looking-at "self\\.")))
	(let* ((idlwave--class-selector (nth 2 (idlwave-current-routine)))
	       (idlwave--super-classes (idlwave-all-class-inherits
	                                idlwave--class-selector)))
	  ;; Check if we are in a class routine
	  (unless idlwave--class-selector
	    (error "Not in a method procedure or function"))
	  ;; Check if we need to update the "current" class
	  (if (not (equal idlwave--class-selector idlwave-current-tags-class))
	      (idlwave-prepare-class-tag-completion idlwave--class-selector))
	  (setq idlwave-completion-help-info
		(list 'idlwave-complete-class-structure-tag-help
		      (idlwave-sintern-routine
		       (concat idlwave--class-selector "__define"))
		      nil))
	  (let  ((idlwave-current-native-class-tags)) ;FIXME: Really?
	    (idlwave-complete-in-buffer
	     'class-tag 'class-tag
	     idlwave-current-class-tags nil
	     (format "Select a tag of class %s" idlwave--class-selector)
	     "class tag"
	     'idlwave-attach-class-tag-classes))
	  t) ; return t to skip other completions
      nil)))

;; Fake help in the source buffer for class structure tags.
;; Get rid of opaque dynamic variable passing of `idlw-help-link'?
(defvar idlw-help-link) ;Dynbound var from `idlwave-do-mouse-completion-help'.
(defvar idlw-help-name)
(defun idlwave-complete-class-structure-tag-help (mode word)
  (cond
   ((eq mode 'test) ; nothing gets fontified for class tags
    nil)
   ((eq mode 'set)
    (let (class-with found-in)
      (when (setq class-with
		(idlwave-class-or-superclass-with-tag
		 idlwave-current-tags-class
		 word))
	(if (assq (idlwave-sintern-class class-with)
		  idlwave-system-class-info)
	    (error "No help available for system class tags"))
	(setq idlw-help-name
	      (if (setq found-in (idlwave-class-found-in class-with))
	          (cons (concat found-in "__define") class-with)
	        (concat class-with "__define")))))
    (setq idlw-help-link word
	  idlwave-help-do-class-struct-tag t))
   (t (error "This should not happen"))))

(defun idlwave-class-tag-reset ()
  (setq idlwave-current-tags-class nil))

(defun idlwave-prepare-class-tag-completion (class)
  "Find and parse the necessary class definitions for class structure tags."
  (setq idlwave-sint-class-tags nil)
  (setq idlwave-current-tags-class class)
  (setq idlwave-current-class-tags
	(mapcar (lambda (x)
		  (list (idlwave-sintern-class-tag x 'set)))
		(idlwave-all-class-tags class)))
  (setq idlwave-current-native-class-tags
	(mapcar #'downcase (idlwave-class-tags class))))

(defun idlwave-class-add-init-special ()
  ;; Create special entries for Class::Init() methods as Class()
  ;; (syntactic sugar in IDL >=8).
  (idlwave-routines)
  (setcdr (last idlwave-routines)
	  (idlwave-sintern-rinfo-list
	   (mapcar
	    (lambda (entry)
	      (let ((new-entry (copy-sequence entry)))
		(setcar new-entry (nth 2 entry)) ;; Function is class name
		(setcar (cddr new-entry) nil) ;; No class
		new-entry))
	    (idlwave-all-assq (idlwave-sintern-method "Init")
			      idlwave-routines))
	   'set)))

;;----------------------------------------------------
;; System variables/fields

(idlwave-new-sintern-type sysvar)
(idlwave-new-sintern-type sysvartag)
(add-hook 'idlwave-complete-functions #'idlwave-complete-sysvar-or-tag)
(add-hook 'idlwave-update-rinfo-hook #'idlwave-sysvars-reset)
(add-hook 'idlwave-update-rinfo-hook #'idlwave-class-add-init-special)
(add-hook 'idlwave-after-load-rinfo-hook #'idlwave-sintern-sysvar-alist)

(defun idlwave-complete-sysvar-or-tag ()
  "Complete a system variable."
  (interactive)
  (let ((pos (point))
	(case-fold-search t))
    (cond ((save-excursion
	     ;; Check if the context is right for system variable
	     (skip-chars-backward "a-zA-Z0-9_$")
	     (equal (char-before) ?!))
	   (setq idlwave-completion-help-info '(idlwave-complete-sysvar-help))
	   (idlwave-complete-in-buffer 'sysvar 'sysvar
				       idlwave-system-variables-alist nil
				       "Select a system variable"
				       "system variable")
	   t)  ; return t to skip other completions
	  ((save-excursion
	     ;; Check if the context is right for sysvar tag
	     (skip-chars-backward "a-zA-Z0-9_$.")
	     (and (equal (char-before) ?!)
		  (looking-at "\\([a-zA-Z][a-zA-Z0-9_$]*\\)\\.")
		  (<= (match-end 0) pos)))
	   ;; Complete a system variable tag
	   (let* ((var (idlwave-sintern-sysvar (match-string 1)))
		  (entry (assq var idlwave-system-variables-alist))
		  (tags (cdr (assq 'tags entry))))
	     (or entry (error "!%s is not a known system variable" var))
	     (or tags (error "System variable !%s is not a structure" var))
	     (setq idlwave-completion-help-info
		   (list 'idlwave-complete-sysvar-tag-help var))
	     (idlwave-complete-in-buffer 'sysvartag 'sysvartag
					 tags nil
					 "Select a system variable tag"
					 "system variable tag")
	     t)) ; return t to skip other completions
	  (t nil))))

(defun idlwave-complete-sysvar-help (mode word)
  (let ((word (or (nth 1 idlwave-completion-help-info) word))
	(entry (assoc word idlwave-system-variables-alist)))
    (cond
     ((eq mode 'test)
      (and (stringp word) entry (nth 1 (assq 'link entry))))
     ((eq mode 'set)
      ;; Setting dynamic!!!
      (if entry (setq idlw-help-link (nth 1 (assq 'link entry)))))
     (t (error "This should not happen")))))

(defun idlwave-complete-sysvar-tag-help (mode word)
  (let* ((var (nth 1 idlwave-completion-help-info))
	(entry (assoc var idlwave-system-variables-alist))
	(tags (cdr (assq 'tags entry)))
	(main (cdr (assq 'link entry)))
	target) 			;N.B.: 'link' is a dynamic value
    (if (listp main)
	(setq main (car main)))
    (cond
     ((eq mode 'test) ; we can at least link the main
      (and (stringp word) entry main))
     ((eq mode 'set)
      (if entry
	  (setq idlw-help-link ;; setting dynamic!!!
		(if (setq target (cdr (assoc-string word tags 'ignore-case)))
		    (idlwave-substitute-link-target main target)
		  main))))
     (t (error "This should not happen")))))

(defvar idlwave-help-do-class-struct-tag nil)

;;----------------------------------------------------
;; Specialized completion in the shell

(defun idlwave-shell-complete (&optional arg)
  "Do completion in the `idlwave-shell' buffer.
Calls `idlwave-shell-complete-filename' after some executive commands or
in strings.  Otherwise, calls `idlwave-complete' to complete modules and
keywords."
  (interactive "P")
  (let (exec-cmd)
    (cond
     ((and
       (setq exec-cmd (idlwave-shell-executive-command))
       (cdr exec-cmd)
       (member (upcase (cadr exec-cmd))
	       '(".R" ".RU" ".RUN" ".RN" ".RNE" ".RNEW"
		 ".COM" ".COMP" ".COMPI" ".COMPIL" ".COMPILE")))
      ;; We are in a command line with an executive command
      (idlwave-shell-complete-filename (not (idlwave-in-quote))))

     ((car-safe exec-cmd)
      (setq idlwave-completion-help-info
	    '(idlwave-shell-complete-execcomm-help))
      (idlwave-complete-in-buffer 'execcomm 'execcomm
				  idlwave-executive-commands-alist nil
				  "Select an executive command"
				  "system variable"))

     ((idlwave-shell-batch-command)
      (idlwave-shell-complete-filename))

     ((idlwave-shell-shell-command)
      (idlwave-shell-complete-filename))

     ((and (idlwave-shell-filename-string)
	   (save-excursion
	     (beginning-of-line)
	     (let ((case-fold-search t))
	       (not (looking-at ".*obj_new")))))
      (idlwave-shell-complete-filename))

     (t
      ;; Default completion of modules and keywords
      (idlwave-complete arg)))))

(defun idlwave-shell-complete-execcomm-help (mode word)
  (let ((word (or (nth 1 idlwave-completion-help-info) word))
	(entry (assoc-string word idlwave-executive-commands-alist t)))
    (cond
     ((eq mode 'test)
      (and (stringp word) entry (cdr entry)))
     ((eq mode 'set)
      (if entry (setq idlw-help-link (cdr entry)))) ;; setting dynamic variable!!!
     (t (error "This should not happen")))))

(defvar comint-file-name-chars)

(defun idlwave-shell-complete-filename (&optional nospace)
  "Complete a file name at point if after a file name.
We assume that we are after a file name when completing one of the
args of an executive .run, .rnew or .compile."
  ;; CWD might have changed, resync, to set default directory
  (idlwave-shell-resync-dirs)
  (let ((comint-file-name-chars
	 (if (and nospace (string-match "[ ]" idlwave-shell-file-name-chars))
	     (replace-match "" nil t idlwave-shell-file-name-chars)
	   idlwave-shell-file-name-chars)))
    (comint-dynamic-complete-filename)))


(provide 'idlw-complete)
(provide 'idlwave-complete)
