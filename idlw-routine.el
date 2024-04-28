;;; idlw-routine.el --- IDLWAVE routine information code and variables  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2024  Free Software Foundation, Inc.

;;; Commentary

;; Develeport note: the list format for all routine info user catalog,
;; library catalogs, etc. is:
;;
;; ("ROUTINE" type class
;;  (system nil nil nil) | (lib pro_file dir "LIBNAME") | (user pro_file dir "USERLIB") |
;;  (buffer pro_file dir) | (compiled pro_file dir)
;;   "calling_string" ("LINKFILE" (("KWD1" . anchorlink1) ...))
;;                    ("LINKFILE2" (("KWD2" . ancorlink2) ...)) ...)
;;
;; DIR will be supplied dynamically while loading library catalogs,
;; and is sinterned (hashed to an internal symbol) to save space, as
;; is LIBNAME.  PRO_FILE can be a complete filepath, in which case DIR
;; is unnecessary.  HELPFILE can be nil, as can LINKFILE, etc., if no
;; HTML help is available for that routine.  Since keywords can be
;; referenced in multiples files (e.g. Graphics Keywords), there are
;; multiple keyword link lists.

;;; Code:

(require 'idlw-variables)
(require 'idlw-scan)                    ;For `idlwave-routines' var.

;;----------------------------------------------------
;; Convenience Routines for routine info lists

(defun idlwave-routine-routine-name (x)
  (car x))

(defun idlwave-routine-class-name (x)
  (nth 2 x))

(defun idlwave-routine-first-link-file (x)
  (car (nth 5 x)))


;;----------------------------------------------------
;; Routine Info

(defvar idlwave-force-class-query)

(defun idlwave-routine-info (&optional arg _external)
  "Display a routine's calling sequence and list of keywords.
When point is on the name a function or procedure, or in the argument
list of a function or procedure, this command displays a help buffer with
the information.  When called with prefix arg, enforce class query.

When point is on an object operator `->', display the class stored in
this arrow, if any (see `idlwave-store-inquired-class').  With a prefix
arg, the class property is cleared out."
  (interactive "P")
  (idlwave-routines)
  (if (or (string-match "->" (buffer-substring
			      (max (point-min) (1- (point)))
			      (min (+ 2 (point)) (point-max))))
	  (looking-at "\\."))
      ;; Cursor is on an arrow/dot
      (if (get-text-property (point) 'idlwave-class)
	  ;; arrow has class property
	  (if arg
	      ;; Remove property
	      (save-excursion
		(backward-char 1)
		(when (looking-at ".?\\(->\\|\\.\\)")
		  (remove-text-properties (match-beginning 1) (match-end 1)
					  '(idlwave-class nil face nil))
		  (message "Class property removed from arrow")))
	    ;; Echo class property
	    (message "Arrow has text property identifying object to be class %s"
		     (get-text-property (point) 'idlwave-class)))
	;; No property found
	(message "Arrow has no class text property"))

    ;; Not on an arrow...
    (let* ((idlwave-query-class nil)
	   (idlwave-force-class-query (equal arg '(4)))
	   (module (idlwave-what-module)))
      (if (car module)
	  (apply #'idlwave-display-calling-sequence
		 (idlwave-fix-module-if-obj_new module))
	(error "Don't know which calling sequence to show")))))

;;----------------------------------------------------
;; Selecting/matching routines

(defun idlwave-rinfo-assoc (name type class list)
  "Like `idlwave-rinfo-assq', but sintern strings first."
  (idlwave-rinfo-assq
   (idlwave-sintern-routine-or-method name class)
   type (idlwave-sintern-class class) list))

(defun idlwave-rinfo-assq (name type class list)
  ;; Works like assq, but also checks type and class
  (catch 'exit
    (let (match)
      (while (setq match (assq name list))
	(and (or (eq type t)
		 (eq (nth 1 match) type))
	     (eq (nth 2 match) class)
	     (throw 'exit match))
	(setq list (cdr (memq match list)))))))

(defun idlwave-best-rinfo-assq (name type class list &optional with-file
				     keep-system)
  "Like `idlwave-rinfo-assq', but get all twins and sort, then return first.
If WITH-FILE is passed, find the best rinfo entry with a file
included.  If KEEP-SYSTEM is set, don't prune system for compiled
syslib files."
  (let ((twins (idlwave-routine-twins
		(idlwave-rinfo-assq-any-class name type class list)
		list))
	syslibp)
    (when (> (length twins) 1)
      (setq twins (sort twins #'idlwave-routine-entry-compare-twins))
      (if (and (null keep-system)
	       (eq 'system (car (nth 3 (car twins))))
	       (setq syslibp (idlwave-any-syslib (cdr twins)))
	       (not (equal 1 syslibp)))
	  ;; Its a compiled syslib, so we need to remove the system entry
	  (setq twins (cdr twins)))
      (if with-file
	  (setq twins (delq nil
			    (mapcar (lambda (x)
				      (if (nth 1 (nth 3 x)) x))
				    twins)))))
    (car twins)))

(defun idlwave-best-rinfo-assoc (name type class list &optional with-file
				      keep-system)
  "Like `idlwave-best-rinfo-assq', but sintern strings first."
  (idlwave-best-rinfo-assq
   (idlwave-sintern-routine-or-method name class)
   type (idlwave-sintern-class class) list with-file keep-system))

(defun idlwave-rinfo-assq-any-class (name type class list)
  ;; Return the first matching method on the inheritance list
  (let* ((classes (cons class (idlwave-all-class-inherits class)))
	 rtn) ;; class
    (while classes
      (if (setq rtn (idlwave-rinfo-assq name type (pop classes) list))
	  (setq classes nil)))
    rtn))

;;----------------------------------------------------
;; Routine Shadows

;; Routine shadows aka twins: same routine name, different routines on
;; path (or in IDL distributed system)

(defun idlwave-routine-twins (entry &optional list)
  "Return all twin entries of ENTRY in LIST.
LIST defaults to `idlwave-routines'.
Twin entries are those which have the same name, type, and class.
ENTRY will also be returned, as the first item of this list."
  (let* ((name (car entry))
	 (type (nth 1 entry))
	 (class (nth 2 entry))
	 (candidates (idlwave-all-assq name (or list (idlwave-routines))))
	 twins candidate)
    (while (setq candidate (pop candidates))
      (if (and (not (eq candidate entry))
	       (eq type (nth 1 candidate))
	       (eq class (nth 2 candidate)))
	  (push candidate twins)))
    (if (setq candidate (idlwave-rinfo-assq name type class
					    idlwave-unresolved-routines))
	(push candidate twins))
    (cons entry (nreverse twins))))


;; Bound in `idlwave-study-twins', `idlwave-routine-entry-compare-twins'.
(defvar idlwave-twin-class)
(defvar idlwave-twin-name)

(defun idlwave-study-twins (entries)
  "Return dangerous twins of first entry in ENTRIES.
Dangerous twins are routines with same name, but in different files on
the load path.  If a file is in the system library and has an entry in
the `idlwave-system-routines' list, we omit the latter as
non-dangerous because many IDL routines are implemented as library
routines, and may have been scanned."
  (let* ((entry (car entries))
	 (idlwave-twin-name (car entry))    ;
	 (type (nth 1 entry))               ; Must be bound for
	 (idlwave-twin-class (nth 2 entry)) ; `idlwave-routine-twin-compare'.
	 (cnt 0)
	 source type-cons file alist syslibp key)
    (while (setq entry (pop entries))
      (cl-incf cnt)
      (setq source (nth 3 entry)
	    type (car source)
	    type-cons (cons type (nth 3 source))
	    file (idlwave-routine-source-file source))

      ;; Make KEY to index entry properly
      (setq key (cond ((eq type 'system) type)
		      (file (file-truename file))
		      (t 'unresolved)))

      ;; Check for an entry in the system library
      (if (and file
	       (not syslibp)
	       (idlwave-syslib-p file))
	  (setq syslibp t))

      ;; If there's more than one matching entry for the same file, just
      ;; append the type-cons to the type list.
      (if (setq entry (assoc key alist))
	  (push type-cons (nth 2 entry))
	(push (list key file (list type-cons)) alist)))

    (setq alist (nreverse alist))

    (when syslibp
      ;; File is in system *library* - remove any 'system entry
      (setq alist (delq (assq 'system alist) alist)))

    ;; If 'system remains and we've scanned the syslib, it's a builtin
    ;; (rather than a !DIR/lib/.pro file bundled as source).
    (when (and (idlwave-syslib-scanned-p)
	       (setq entry (assoc 'system alist)))
      (setcar entry 'builtin))
    (sort alist #'idlwave-routine-twin-compare)))

(defun idlwave-routine-entry-compare (a b)
  "Compare two routine info entries for sorting.
This is the general case.  It first compares class, names, and type.
If it turns out that A and B are twins (same name, class, and type),
calls another routine which compares twins on the basis of their file
names and path locations."
  (let ((name (car a)) (type (nth 1 a)) (class (nth 2 a)))
    (cond
     ((not (equal (idlwave-downcase-safe class)
		  (idlwave-downcase-safe (nth 2 b))))
      ;; Class decides
      (cond ((null (nth 2 b)) nil)
	    ((null class) t)
	    (t (string< (downcase class) (downcase (nth 2 b))))))
     ((not (equal (downcase name) (downcase (car b))))
      ;; Name decides
      (string< (downcase name) (downcase (car b))))
     ((not (eq type (nth 1 b)))
      ;; Type decides
      (< (if (eq type 'fun) 1 0) (if (eq (nth 1 b) 'fun) 1 0)))
     (t
      ;; A and B are twins - so the decision is more complicated.
      ;; Call twin-compare with the proper arguments.
      (idlwave-routine-entry-compare-twins a b)))))

(defun idlwave-routine-entry-compare-twins (a b)
  "Compare two routine entries, under the assumption that they are twins.
This basically calls `idlwave-routine-twin-compare' with the correct args."
  (let* ((idlwave-twin-name (car a))
	 ;; (type (nth 1 a))
	 (idlwave-twin-class (nth 2 a)) ;Used in `idlwave-routine-twin-compare'.
	 (asrc (nth 3 a))
	 (atype (car asrc))
	 (bsrc (nth 3 b))
	 (btype (car bsrc))
	 (afile (idlwave-routine-source-file asrc))
	 (bfile (idlwave-routine-source-file bsrc)))
    (idlwave-routine-twin-compare
     (list (if (stringp afile) (file-truename afile) atype)
	   afile (list atype))
     (list (if (stringp bfile) (file-truename bfile) btype)
           bfile (list btype)))))

(defun idlwave-routine-twin-compare (a b)
  "Compare two routine twin entries for sorting.
In here, A and B are not normal routine info entries, but special
lists (KEY FILENAME (TYPES...)).
This expects `idlwave-twin-name' and `idlwave-twin-class' to be bound to the right values."
  (let* (;; Dis-assemble entries
	 (akey (car a))	     (bkey (car b))
	 (afile (nth 1 a))   (bfile (nth 1 b))
	 (atypes (nth 2 a))  (btypes (nth 2 b))
	 ;; System routines?
	 (asysp (memq akey '(builtin system)))
	 (bsysp (memq bkey '(builtin system)))
	 ;; Compiled routines?
	 (acompp (memq 'compiled atypes))
	 (bcompp (memq 'compiled btypes))
	 ;; Unresolved?
	 (aunresp (or (eq akey 'unresolved)
		      (and acompp (not afile))))
	 (bunresp (or (eq bkey 'unresolved)
		      (and bcompp (not bfile))))
	 ;; Buffer info available?
	 (abufp (memq 'buffer atypes))
	 (bbufp (memq 'buffer btypes))
	 ;; On search path?
	 (tpath-alist (idlwave-true-path-alist))
	 (apathp (and (stringp akey)
		      (assoc (file-name-directory akey) tpath-alist)))
	 (bpathp (and (stringp bkey)
		      (assoc (file-name-directory bkey) tpath-alist)))
	 ;; How early on search path?  High number means early since we
	 ;; measure the tail of the path list
	 (anpath (length (memq apathp tpath-alist)))
	 (bnpath (length (memq bpathp tpath-alist)))
	 ;; Look at file names
	 (aname (if (stringp afile) (downcase (file-name-nondirectory afile)) ""))
	 (bname (if (stringp bfile) (downcase (file-name-nondirectory bfile)) ""))
	 (fname-re (if idlwave-twin-class
		       (format "\\`%s__\\(%s\\|define\\)\\.pro\\'"
			       (regexp-quote (downcase idlwave-twin-class))
			       (regexp-quote (downcase idlwave-twin-name)))
		     (format "\\`%s\\.pro"
		             (regexp-quote (downcase idlwave-twin-name)))))
	 ;; Is file name derived from the routine name?
	 ;; Method file or class definition file?
	 (anamep (string-match fname-re aname))
	 (adefp (and idlwave-twin-class anamep
		     (string= "define" (match-string 1 aname))))
	 (bnamep (string-match fname-re bname))
	 (bdefp (and idlwave-twin-class bnamep
		     (string= "define" (match-string 1 bname)))))

    ;; Now: follow JD's ideas about sorting.  Looks really simple now,
    ;; doesn't it?  The difficult stuff is hidden above...
    (cond
     ((xor asysp  bsysp)       asysp)	; System entries first
     ((xor aunresp bunresp)    bunresp) ; Unresolved last
     ((and idlwave-sort-prefer-buffer-info
	   (xor abufp bbufp))
      abufp)                            ; Buffers before non-buffers
     ((xor acompp bcompp)      acompp)	; Compiled entries
     ((xor apathp bpathp)      apathp)	; Library before non-library
     ((xor anamep bnamep)      anamep)	; Correct file names first
     ((and idlwave-twin-class anamep bnamep  ; both file names match ->
	   (xor adefp bdefp))
      bdefp)                              ; __define after __method
     ((> anpath bnpath)                t)	; Who is first on path?
     (t                                nil))))	; Default

(defun idlwave-list-buffer-load-path-shadows (&optional _arg)
  "List the load path shadows of all routines defined in current buffer."
  (interactive)
  (idlwave-routines)
  (if (derived-mode-p 'idlwave-mode)
      (idlwave-list-load-path-shadows
       nil (idlwave-update-current-buffer-info 'save-buffer)
       "in current buffer")
    (error "Current buffer is not in idlwave-mode")))

(defun idlwave-list-shell-load-path-shadows (&optional _arg)
  "List the load path shadows of all routines compiled under the shell.
This is very useful for checking an IDL application.  Just compile the
application, do RESOLVE_ALL, and \\`C-c C-i' to compile all referenced
routines and update IDLWAVE internal info.  Then check for shadowing
with this command."
  (interactive)
  (cond
   ((or (not (fboundp 'idlwave-shell-is-running))
	(not (idlwave-shell-is-running)))
    (error "Shell is not running"))
   ((null idlwave-compiled-routines)
    (error "No compiled routines.  Maybe you need to update with `C-c C-i'"))
   (t
    (idlwave-list-load-path-shadows nil idlwave-compiled-routines
				    "in the shell"))))

(defun idlwave-list-all-load-path-shadows (&optional _arg)
  "List the load path shadows of all routines known to IDLWAVE."
  (interactive)
  (idlwave-list-load-path-shadows nil nil "globally"))

(defvar idlwave-sort-prefer-buffer-info t
  "Internal variable used to influence `idlwave-routine-twin-compare'.")

(defun idlwave-list-load-path-shadows (_arg &optional special-routines loc)
  "List the routines which are defined multiple times.
Search the information IDLWAVE has about IDL routines for multiple
definitions.
When SPECIAL-ROUTINES in non-nil, only look for shadows of these routines.

When IDL hits a routine call which is not defined, it will search on
the load path in order to find a definition.  The output of this command
can be used to detect possible name clashes during this process."
  (idlwave-routines)  ; Make sure everything is loaded.
  (unless (or idlwave-user-catalog-routines idlwave-library-catalog-routines)
    (or (y-or-n-p
	 "You don't have any user or library catalogs.  Continue anyway? ")
	(error "Abort")))
  (let* ((routines (append idlwave-system-routines
			   idlwave-compiled-routines
			   idlwave-library-catalog-routines
			   idlwave-user-catalog-routines
			   idlwave-buffer-routines
			   nil))
	 (keymap (make-sparse-keymap))
	 (props (list 'mouse-face 'highlight
		      'local-map keymap
		      'help-echo "Mouse2: Find source"))
	 (nroutines (length (or special-routines routines)))
	 (step (max 1 (/ nroutines 100)))
	 (n 0)
	 (cnt 0)
	 (idlwave-sort-prefer-buffer-info nil)
	 routine twins dtwins twin done props1 lroutines)

    (if special-routines
	;; Just looking for shadows of a few special routines
	(setq lroutines routines
	      routines special-routines))

    (message "Sorting routines...")
    (setq routines (sort routines
			 (lambda (a b)
			   (string< (downcase (idlwave-make-full-name
					       (nth 2 a) (car a)))
				    (downcase (idlwave-make-full-name
					       (nth 2 b) (car b)))))))
    (message "Sorting routines...done")

    (define-key keymap [mouse-2]
      (lambda (ev)
	(interactive "e")
	(mouse-set-point ev)
	(apply #'idlwave-do-find-module
	       (get-text-property (point) 'find-args))))
    (define-key keymap "\r"
      (lambda ()
	(interactive)
	(apply #'idlwave-do-find-module
	       (get-text-property (point) 'find-args))))
    ;; FIXME: Use `make-progress-reporter'.
    (message "Compiling list...( 0%%)")
    (with-current-buffer (get-buffer-create "*Shadows*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (while (setq routine (pop routines))
	(if (= (mod (setq n (1+ n)) step) 0)
	    (message "Compiling list...(%2d%%)" (floor (* n 100.0) nroutines)))

	;; Get a list of all twins
	(setq twins (idlwave-routine-twins routine (or lroutines routines)))
	(if (memq routine done)
	    (setq dtwins nil)
	  (setq dtwins (idlwave-study-twins twins)))
	;; Mark all twins as dealt with
	(setq done (append twins done))
	(when (or (> (length dtwins) 1)
		  (> (idlwave-count-memq 'lib (nth 2 (car dtwins))) 1)
		  (> (idlwave-count-memq 'user (nth 2 (car dtwins))) 1)
		  (> (idlwave-count-memq 'buffer (nth 2 (car dtwins))) 1))
	  (cl-incf cnt)
	  (insert (format "\n%s%s"
			  (idlwave-make-full-name (nth 2 routine)
						  (car routine))
			  (if (eq (nth 1 routine) 'fun) "()" "")))
	  (while (setq twin (pop dtwins))
	    (setq props1 (append (list 'find-args
				       (list (nth 0 routine)
					     (nth 1 routine)
					     (nth 2 routine)))
				 props))
	    (idlwave-insert-source-location "\n   - " twin props1))))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (setq loc (or loc ""))
    (if (> cnt 0)
	(progn
	  (display-buffer (get-buffer "*Shadows*"))
	  (message "%d case%s of shadowing found %s"
		   cnt (if (= cnt 1) "" "s") loc))
      (message "No shadowing conflicts found %s" loc))))

;;----------------------------------------------------
;; Routine data structure tools

(defun idlwave-routine-source-file (source)
  (if (nth 2 source)
      (expand-file-name (nth 1 source) (nth 2 source))
    (nth 1 source)))

(defun idlwave-any-syslib (entries)
  "Does the entry list ENTRIES contain a syslib entry?
If yes, return the index (>=1)."
  (let (file (cnt 0))
    (catch 'exit
      (while entries
	(cl-incf cnt)
	(setq file (idlwave-routine-source-file (nth 3 (car entries))))
	(if (and file (idlwave-syslib-p file))
	    (throw 'exit cnt)
	  (setq entries (cdr entries))))
      nil)))

(defun idlwave-all-method-classes (method &optional type)
  "Return all classes which have a method METHOD.
TYPE is `fun' or `pro'.
When TYPE is not specified, both procedures and functions will be considered."
  (if (null method)
      (mapcar #'car (idlwave-class-alist))
    (let (rtn)
      (mapc (lambda (x)
	      (and (nth 2 x)
		   (or (not type)
		       (eq type (nth 1 x)))
		   (push (nth 2 x) rtn)))
	    (idlwave-all-assq method (idlwave-routines)))
      (idlwave-uniquify rtn))))

(defun idlwave-all-method-keyword-classes (method keyword &optional type)
  "Return all classes which have a method METHOD with keyword KEYWORD.
TYPE is `fun' or `pro'.
When TYPE is not specified, both procedures and functions will be considered."
  (if (or (null method)
	  (null keyword))
      nil
    (let (rtn)
      (mapc (lambda (x)
	      (and (nth 2 x)		; non-nil class
		   (or (not type)	; correct or unspecified type
		       (eq type (nth 1 x)))
		   (assoc keyword (idlwave-entry-keywords x))
		   (push (nth 2 x) rtn)))
	    (idlwave-all-assq method (idlwave-routines)))
      (idlwave-uniquify rtn))))

(defun idlwave-make-full-name (class &optional name)
  (when (and (listp class) (not (null class)))
    ;; a routine info or idlwave-what-module entry
    (setq name (car class)
	  class (nth 2 class)))
  ;; Make a fully qualified module name including the class name
  (concat (if class (format "%s::" class) "") name))

(defun idlwave-determine-class (cw-list type)
  ;; Determine the class of a routine call.  CW-LIST is the `cw-list'
  ;; structure as returned by idlwave-where.  The second element in
  ;; this structure is the class.  When nil, we return nil.  When t,
  ;; try to get the class from text properties at the method call
  ;; arrow.  When the object is "self", we use the class of the
  ;; current (enclosing) routine.  Otherwise, we prompt the user for a
  ;; class name.  Also stores the selected class as a text property at
  ;; the arrow.  TYPE is 'fun or 'pro.
  (let* ((class (nth 2 cw-list))
	 (apos (nth 3 cw-list))
	 (nassoc (assoc (if (stringp (car cw-list))
			    (upcase (car cw-list))
			  (car cw-list))
			idlwave-query-class))
	 (dassoc (assq (if (car cw-list) 'keyword-default 'method-default)
		       idlwave-query-class))
	 (query (cond (nassoc (cdr nassoc))
		      (dassoc (cdr dassoc))
		      (t t)))
	 arrow-len
	 (arrow (and apos
		     (or (and (string= (buffer-substring apos (min (point-max)
								   (+ 2 apos)))
				       "->")
			      (setq arrow-len 2))
			 (and (string= (buffer-substring apos (min (point-max)
								   (+ 1 apos)))
				       ".")
			      (setq arrow-len 1)))))
	 (is-self
	  (and arrow
	       (save-excursion (goto-char apos)
			       (forward-word-strictly -1)
			       (let ((case-fold-search t))
				 (looking-at "self\\>")))))
	 (force-query idlwave-force-class-query)
	 store special-class class-alist)
    (cond
     ((null class) nil)
     ((eq t class)
      ;; There is an object which would like to know its class
      (if (and arrow (get-text-property apos 'idlwave-class)
	       idlwave-store-inquired-class
	       (not force-query))
	  (setq class (get-text-property apos 'idlwave-class)
		class (idlwave-sintern-class class)))
      (if (and (eq t class) is-self)
	  (setq class (or (nth 2 (idlwave-current-routine)) class)))

      ;; Before prompting, try any special class determination routines
      (when (and (eq t class)
		 (not force-query))
	(setq special-class
	      (run-hook-with-args-until-success
	       'idlwave-determine-class-functions apos))
	(if special-class
	    (setq class (idlwave-sintern-class special-class)
		  store idlwave-store-inquired-class)))

      ;; Prompt for a class, if we need to
      (when (and (eq class t)
		 (or force-query query))
	(setq class-alist
	      (mapcar #'list (idlwave-all-method-classes (car cw-list) type)))
	(setq class
	      (idlwave-sintern-class
	       (cond
		((and (= (length class-alist) 0) (not force-query))
		 (error "No classes available with method %s" (car cw-list)))
		((and (= (length class-alist) 1) (not force-query))
		 (car (car class-alist)))
		(t
		 (setq store idlwave-store-inquired-class)
		 (idlwave-completing-read
		  (format "Class%s: " (if (stringp (car cw-list))
					  (format " for %s method %s"
						  type (car cw-list))
					""))
		  class-alist nil nil nil 'idlwave-class-history))))))

      ;; Store it, if requested
      (when (and class (not (eq t class)))
	;; We have a real class here
	(when (and store arrow)
	  (condition-case ()
	      (add-text-properties
	       apos (+ apos arrow-len)
	       `(idlwave-class ,class face ,idlwave-class-arrow-face
			       rear-nonsticky t))
	    (error nil)))
	(setf (nth 2 cw-list) class))
      ;; Return the class
      class)
     ;; Default as fallback
     (t class))))

;;----------------------------------------------------
;; Context (buffer-local)

(define-obsolete-variable-alias 'idlwave-determine-class-special
  'idlwave-determine-class-functions "28.1")
(defvar idlwave-determine-class-functions nil
  "Special hook to determine a class.
The functions should accept one argument, APOS.")

(defun idlwave-where ()
  "Find out where we are.
The return value is a list with the following stuff:
\(PRO-LIST FUNC-LIST COMPLETE-WHAT CW-LIST LAST-CHAR)

PRO-LIST       (PRO POINT CLASS ARROW)
FUNC-LIST      (FUNC POINT CLASS ARROW)
COMPLETE-WHAT  a symbol indicating what kind of completion makes sense here
CW-LIST        (PRO-OR-FUNC POINT CLASS ARROW)  Like PRO-LIST, for what can
               be completed here.
LAST-CHAR      last relevant character before point (non-white non-comment,
               not part of current identifier or leading slash).

In the lists, we have these meanings:
PRO:    Procedure name
FUNC:   Function name
POINT:  Where is this
CLASS:  What class has the routine (nil=no, t=is method, but class unknown)
ARROW:  Location of the arrow for method calls"
  (idlwave-routines)
  (let* (;(bos (save-excursion (idlwave-beginning-of-statement) (point)))
         (bos (save-excursion (idlwave-start-of-substatement 'pre) (point)))
 	 (func-entry (idlwave-what-function bos))
         (func (car func-entry))
         (func-class (nth 1 func-entry))
         (func-arrow (nth 2 func-entry))
	 (func-point (or (nth 3 func-entry) 0))
	 (func-level (or (nth 4 func-entry) 0))
	 (pro-entry (idlwave-what-procedure bos))
	 (pro (car pro-entry))
         (pro-class (nth 1 pro-entry))
         (pro-arrow (nth 2 pro-entry))
	 (pro-point (or (nth 3 pro-entry) 0))
	 (last-char (idlwave-last-valid-char))
         (case-fold-search t)
	 (match-string (buffer-substring bos (point)))
	 cw cw-mod cw-arrow cw-class cw-point)
    (if (< func-point pro-point) (setq func nil))
    (cond
     ;; Class name in routine definition
     ((string-match "\\`[ \t]*\\(pro\\|function\\)[ \t]+[a-zA-Z0-9_]*\\'"
                    match-string)
      (setq cw 'class))

     ;; Don't complete pro/function
     ((string-match "\\`[ \t]*\\(pro\\|function\\)\\>"
		    match-string)
      nil)

     ;; Procedure call
     ((string-match
       "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)?\\'"
       (if (> pro-point 0)
	   (buffer-substring pro-point (point))
	 match-string))
      (setq cw 'procedure
	    cw-class pro-class
	    cw-point pro-point
	    cw-arrow pro-arrow))

     ;; Complete class name inside obj_new statement
     ((string-match "OBJ_NEW([ \t]*['\"][a-zA-Z0-9$_]*\\'"
		    match-string)
      (setq cw 'class))

     ;; Or in an INHERITS statement
     ((string-match "\\<inherits\\s-+[a-zA-Z0-9$_]*\\'"
		    match-string)
      (setq cw 'class))

     ;; Function keyword inside function
     ((and func
	   (> func-point pro-point)
	   (= func-level 1)
	   (memq last-char '(?\( ?,)))
      (setq cw 'function-keyword
	    cw-mod func
	    cw-point func-point
	    cw-class func-class
	    cw-arrow func-arrow))

     ;; Procedure keyword otherwise
     ((and pro (eq last-char ?,))
      (setq cw 'procedure-keyword
	    cw-mod pro
	    cw-point pro-point
	    cw-class pro-class
	    cw-arrow pro-arrow))

;     ((member last-char '(?\' ?\) ?\] ?!))
;      ;; after these chars, a function makes no sense
;      ;; FIXME: I am sure there can be more in this list
;      ;; FIXME: Do we want to do this at all?
;      nil)
     ;; Everywhere else we try a function.
     (t
      (setq cw 'function)
      (save-excursion
	(if (re-search-backward "\\(->\\|\\.\\)[ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\s-*\\)?\\(\\([$a-zA-Z0-9_]+\\)::\\)?[$a-zA-Z0-9_]*\\=" bos t)
	    (setq cw-arrow (copy-marker (match-beginning 0))
		  cw-class (if (match-end 4)
			       (idlwave-sintern-class (match-string 4))
			     t))))))

    (list (list pro pro-point pro-class pro-arrow)
          (list func func-point func-class func-arrow)
          cw
	  (list cw-mod cw-point cw-class cw-arrow)
	  last-char)))

(defun idlwave-what-function (&optional bound)
  ;; Find out if point is within the argument list of a function.
  ;; The return value is ("function-name" class arrow-start (point) level).
  ;; Level is 1 on the top level parentheses, higher further down.

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.

  (catch 'exit
    (let (pos
	  func-point
	  (cnt 0)
	  func arrow-start class)
      (with-syntax-table idlwave-find-symbol-syntax-table
       (save-restriction
	 (save-excursion
	   (narrow-to-region (max 1 (or bound 0)) (point-max))
	   ;; move back out of the current parenthesis
	   (while (condition-case nil
		      (progn (up-list -1) t)
		    (error nil))
	     (setq pos (point))
	     (cl-incf cnt)
	     (when (and (= (following-char) ?\()
			(re-search-backward
			 "\\(::\\|\\<\\|\\.\\)\\([a-zA-Z][a-zA-Z0-9$_]*\\)[ \t]*\\="
			 bound t))
	       (setq func (match-string 2)
		     func-point (goto-char (match-beginning 2))
		     pos func-point)
	       (if (re-search-backward
		    "\\(->\\|\\.\\)[ \t]*\\(\\([a-zA-Z][a-zA-Z0-9$_]*\\)::\\)?\\=" bound t)
		   (setq arrow-start (copy-marker (match-beginning 0))
			 class (or (match-string 2) t)))
	       (throw
		'exit
		(list
		 (idlwave-sintern-routine-or-method func class)
		 (idlwave-sintern-class class)
		 arrow-start func-point cnt)))
	     (goto-char pos))
	   (throw 'exit nil)))))))

(defun idlwave-what-procedure (&optional _bound)
  ;; Find out if point is within the argument list of a procedure.
  ;; The return value is ("procedure-name" class arrow-pos (point)).

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.
  (let ((pos (point)) pro-point
	pro class arrow-start string)
    (save-excursion
      ;;(idlwave-beginning-of-statement)
      (idlwave-start-of-substatement 'pre)
      (setq string (buffer-substring (point) pos))
      (if (string-match
	   "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)[ \t]*\\(,\\|\\'\\)" string)
	  (setq pro (match-string 1 string)
		pro-point (+ (point) (match-beginning 1)))
	(if (and (idlwave-skip-object)
		 (setq string (buffer-substring (point) pos))
		 (string-match
		  "\\`[ \t]*\\(->\\|\\.\\)[ \t]*\\(\\([a-zA-Z][a-zA-Z0-9$_]*\\)::\\)?\\([a-zA-Z][a-zA-Z0-9$_]*\\)?[ \t]*\\(,\\|\\(\\$\\s *\\(;.*\\)?\\)?$\\)"
		  string))
	    (setq pro (if (match-beginning 4)
			  (match-string 4 string))
		  pro-point (if (match-beginning 4)
			        (+ (point) (match-beginning 4))
			pos)
		  arrow-start (copy-marker (+ (point) (match-beginning 1)))
		  class (or (match-string 3 string) t)))))
    (list (idlwave-sintern-routine-or-method pro class)
	  (idlwave-sintern-class class)
	  arrow-start
	  pro-point)))

(defun idlwave-skip-object ()
  ;; If there is an object at point, move over it and return t.
  (let ((pos (point)))
    (if (catch 'exit
	  (save-excursion
	    (skip-chars-forward " 	")  ; white space
	    (skip-chars-forward "*")        ; de-reference
	    (cond
	     ((looking-at idlwave-identifier)
	      (goto-char (match-end 0)))
	     ((eq (following-char) ?\()
	      nil)
	     (t (throw 'exit nil)))
	    ;; (catch 'endwhile ; Can't skip dots anymore, they are used for method invocation!
	    ;;   (while t
	    ;; 	(cond ((eq (following-char) ?.)
	    ;; 	       (forward-char 1)
	    ;; 	       (if (not (looking-at idlwave-identifier))
	    ;; 		   (throw 'exit nil))
	    ;; 	       (goto-char (match-end 0)))
	    ;; 	      ((memq (following-char) '(?\( ?\[))
	    ;; 	       (condition-case nil
	    ;; 		   (forward-list 1)
	    ;; 		 (error (throw 'exit nil))))
	    ;; 	      (t (throw 'endwhile t)))))
	    (if (looking-at "[ \t]*\\(->\\|\\.\\)")
		(throw 'exit (setq pos (match-beginning 0)))
	      (throw 'exit nil))))
	(goto-char pos)
      nil)))

(defun idlwave-last-valid-char ()
  "Return the last character before point which is not white or a comment
and also not part of the current identifier.  Since we do this in
order to identify places where keywords are, we consider the initial
`/' of a keyword as part of the identifier.
This function is not general, can only be used for completion stuff."
  (catch 'exit
    (save-excursion
      ;; skip the current identifier
      (skip-chars-backward "a-zA-Z0-9_$")
      ;; also skip a leading slash which might be belong to the keyword
      (if (eq (preceding-char) ?/)
	  (backward-char 1))
      ;; FIXME: does not check if this is a valid identifier
      (while t
	(skip-chars-backward " \t")
	(cond
	 ((memq (preceding-char) '(?\; ?\$)) (throw 'exit nil))
	 ((eq (preceding-char) ?\n)
	  (beginning-of-line 0)
	  (if (looking-at "\\([^\n]*\\)\\$[ \t]*\\(;[^\n]*\\)?\n")
	      ;; continuation line
	      (goto-char (match-end 1))
	    (throw 'exit nil)))
	 (t (throw 'exit (preceding-char))))))))

(defun idlwave-this-word (&optional class)
  ;; Grab the word around point.  CLASS is for the `skip-chars=...' functions
  (setq class (or class "a-zA-Z0-9$_."))
  (save-excursion
    (buffer-substring
     (progn (skip-chars-backward class) (point))
     (progn (skip-chars-forward  class) (point)))))

(defun idlwave-what-module ()
  "Return a default module for stuff near point.
Used by `idlwave-routine-info' and `idlwave-find-module'.
A module specification has the simple format (NAME TYPE CLASS)"
  (idlwave-routines)
  (if (let ((case-fold-search t))
	(save-excursion
	  (idlwave-beginning-of-statement)
	  (looking-at "[ \t]*\\(pro\\|function\\)[ \t]+\\(\\([a-zA-Z0-9_$]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)\\([, \t\n]\\|$\\)")))
      ;; This is a function or procedure definition statement
      ;; We return the defined routine as module.
      (list
       (idlwave-sintern-routine-or-method (match-string-no-properties 4)
					  (match-string-no-properties 2))
       (if (equal (downcase (match-string 1)) "pro") 'pro 'fun)
       (idlwave-sintern-class (match-string 3)))

    ;; Not a definition statement - analyze precise position.
    (let* ((where (idlwave-where))
	   (cw (nth 2 where))
	   (pro (car (nth 0 where)))
	   (func (car (nth 1 where)))
	   (this-word (idlwave-this-word "a-zA-Z0-9$_"))
	   (next-char (save-excursion (skip-chars-forward "a-zA-Z0-9$_")
				      (following-char)))
	   )
      (cond
       ((and (eq cw 'procedure)
	     (not (equal this-word "")))
	(setq this-word (idlwave-sintern-routine-or-method
			 this-word (nth 2 (nth 3 where))))
	(list this-word 'pro
	      (idlwave-determine-class
	       (cons this-word (cdr (nth 3 where)))
	       'pro)))

       ((and (eq cw 'function)
	     (not (equal this-word ""))
	     (or (eq next-char ?\()	; exclude arrays, vars.
		 (looking-at "[a-zA-Z0-9_]*[ \t]*(")))
	(setq this-word (idlwave-sintern-routine-or-method
			 this-word (nth 2 (nth 3 where))))
	(list this-word 'fun
	      (idlwave-determine-class
	       (cons this-word (cdr (nth 3 where)))
	       'fun)))

       ((and (memq cw '(function-keyword procedure-keyword))
	     (not (equal this-word ""))
	     (eq next-char ?\())	; A function!
	(setq this-word (idlwave-sintern-routine this-word))
	(list this-word 'fun nil))

       (func
	(list func 'fun (idlwave-determine-class (nth 1 where) 'fun)))

       (pro
	(list pro 'pro (idlwave-determine-class (nth 0 where) 'pro)))

       (t nil)))))

(defun idlwave-what-module-find-class ()
  "Call `idlwave-what-module' and find the inherited class if necessary."
  (let* ((module (idlwave-what-module))
	 (class (nth 2 module)))
    (if (and (= (length module) 3)
	     (stringp class))
	(list (car module)
	      (nth 1 module)
	      (apply #'idlwave-find-inherited-class module))
      module)))

(defun idlwave-find-inherited-class (name type class)
  "Find the class which defines TYPE NAME and is CLASS or inherited by CLASS."
  (let ((entry (idlwave-best-rinfo-assoc name type class (idlwave-routines))))
    (if entry
	(nth 2 entry)
      class)))

(defun idlwave-fix-module-if-obj_new (module)
  "Check if MODULE points to obj_new.
If yes, and if the cursor is in the keyword region, change to the
appropriate Init method."
  (let* ((name (car module))
	 (pos (point))
	 (case-fold-search t)
	 string)
    (if (and (stringp name)
	     (equal (downcase name) "obj_new")
	     (save-excursion
	       (idlwave-beginning-of-statement)
	       (setq string (buffer-substring (point) pos))
	       (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)))
	(let (;; (name "Init")
	      (class (match-string 1 string)))
	  (setq module (list (idlwave-sintern-method "Init")
			     'fun
			     (idlwave-sintern-class class)))))
    module))

(defun idlwave-fix-keywords (name type class keywords
				  &optional super-classes system)
  "Update a list of keywords.
Translate OBJ_NEW, adding all super-class keywords, or all keywords
from all classes if CLASS equals t.  If SYSTEM is non-nil, don't
demand _EXTRA in the keyword list."
  (let ((case-fold-search t)
        (idlwave--super-classes super-classes))

    ;; If this is the OBJ_NEW function, try to figure out the class and use
    ;; the keywords from the corresponding INIT method.
    (if (and (equal (upcase name) "OBJ_NEW")
	     (derived-mode-p 'idlwave-mode 'idlwave-shell-mode))
	(let* ((bos (save-excursion (idlwave-beginning-of-statement) (point)))
	       (string (buffer-substring bos (point)))
	       (case-fold-search t)
	       class)
	  (and (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)
	       (setq class (idlwave-sintern-class (match-string 1 string)))
	       (setq idlwave-current-obj_new-class class)
	       (setq keywords
		     (append keywords
			     (idlwave-entry-keywords
			      (idlwave-rinfo-assq
			       (idlwave-sintern-method "INIT")
			       'fun
			       class
			       (idlwave-routines))
			      'do-link))))))

    ;; If the class is `t', combine all keywords of all methods NAME
    (when (eq class t)
      (mapc (lambda (entry)
	      (and
	       (nth 2 entry)             ; non-nil class
	       (eq (nth 1 entry) type)   ; correct type
	       (setq keywords
		     (append keywords
			     (idlwave-entry-keywords entry 'do-link)))))
	    (idlwave-all-assq name (idlwave-routines)))
      (setq keywords (idlwave-uniquify keywords)))

    ;; If we have inheritance, add all keywords from superclasses, if
    ;; the user indicated that method in `idlwave-keyword-class-inheritance'
    (when (and
	   idlwave--super-classes
	   idlwave-keyword-class-inheritance
	   (stringp class)
	   (or
	    system
	    (assq (idlwave-sintern-keyword "_extra") keywords)
	    (assq (idlwave-sintern-keyword "_ref_extra") keywords))
	   ;; Check if one of the keyword-class regexps matches the name
	   (let ((regexps idlwave-keyword-class-inheritance) re)
	     (catch 'exit
	       (while (setq re (pop regexps))
		 (if (string-match re name) (throw 'exit t))))))

      (cl-loop for entry in (idlwave-routines) do
	       (and (nth 2 entry)                   ; non-nil class
		    (memq (nth 2 entry) idlwave--super-classes) ; an inherited class
		    (eq (nth 1 entry) type)            ; correct type
		    (eq (car entry) name)              ; correct name
		    (mapc (lambda (k) (add-to-list 'keywords k))
		          (idlwave-entry-keywords entry 'do-link))))
      (setq keywords (idlwave-uniquify keywords)))

    ;; Return the final list
    keywords))

(defun idlwave-expand-keyword (keyword module)
  "Expand KEYWORD to one of the valid keyword parameters of MODULE.
KEYWORD may be an exact match or an abbreviation of a keyword.
If the match is exact, KEYWORD itself is returned, even if there may be other
keywords of which KEYWORD is an abbreviation.  This is necessary because some
system routines have keywords which are prefixes of other keywords.
If KEYWORD is an abbreviation of several keywords, a list of all possible
completions is returned.
If the abbreviation was unique, the correct keyword is returned.
If it cannot be a keyword, the function return nil.
If we do not know about MODULE, just return KEYWORD literally."
  (let* ((name (car module))
	 (type (nth 1 module))
	 (class (nth 2 module))
	 (kwd (idlwave-sintern-keyword keyword))
	 (entry (idlwave-best-rinfo-assoc name type class (idlwave-routines)))
	 (kwd-alist (idlwave-entry-keywords entry))
	 (extra (or (assq (idlwave-sintern-keyword "_EXTRA") kwd-alist)
		    (assq (idlwave-sintern-keyword "_REF_EXTRA") kwd-alist)))
	 (completion-ignore-case t)
	 candidates)
    (cond ((assq kwd kwd-alist)
	   kwd)
	  ((setq candidates (all-completions kwd kwd-alist))
	   (if (= (length candidates) 1)
	       (car candidates)
	     candidates))
	  ((and entry extra)
	   ;; Inheritance may cause this keyword to be correct
	   keyword)
	  (entry
	   ;; We do know the function, which does not have the keyword.
	   nil)
	  (t
	   ;; We do not know the function, so this just might be a correct
	   ;; keyword - return it as it is.
	   keyword))))

(defun idlwave-class-alist ()
  "Return the class alist - make it if necessary."
  (or idlwave-class-alist
      (let (class)
	(cl-loop for x in idlwave-routines do
		 (when (and (setq class (nth 2 x))
			    (not (assq class idlwave-class-alist)))
		   (push (list class) idlwave-class-alist)))
	idlwave-class-alist)))

(defun idlwave-entry-keywords (entry &optional record-link)
  "Return the flat entry keywords alist from routine-info entry.
If RECORD-LINK is non-nil, the keyword text is copied and a text
property indicating the link is added."
  (let (kwds)
    (mapc
     (lambda (key-list)
       (let ((file (car key-list)))
	 (mapcar (lambda (key-cons)
		   (let ((key (car key-cons))
			 (link (cdr key-cons)))
		     (when (and record-link file)
			 (setq key (copy-sequence key))
			 (put-text-property
			  0 (length key)
			  'link
			  (concat
			   file
			   (if link
			       (concat idlwave-html-link-sep link)))
			  key))
		     (push (list key) kwds)))
		 (cdr key-list))))
     (nthcdr 5 entry))
    (nreverse kwds)))

(defun idlwave-entry-find-keyword (entry keyword)
  "Find keyword KEYWORD in entry ENTRY, and return (with link) if set."
  (catch 'exit
    (mapc
     (lambda (key-list)
       (let ((file (car key-list))
	     (kwd (assoc keyword (cdr key-list))))
	 (when kwd
	   (setq kwd (cons (car kwd)
			   (if (and file (cdr kwd))
			       (concat file
				       idlwave-html-link-sep
				       (cdr kwd))
			     (cdr kwd))))
	   (throw 'exit kwd))))
     (nthcdr 5 entry))))

;;----------------------------------------------------
;; Calling Sequence Display

(defvar idlwave-popup-source nil)
(defvar idlwave-rinfo-marker (make-marker))

(defun idlwave-display-calling-sequence (name type class
					      &optional initial-class)
  ;; Display the calling sequence of module NAME, type TYPE in class CLASS.
  (let* ((initial-class (or initial-class class))
	 (entry (or (idlwave-best-rinfo-assq name type class
					     (idlwave-routines))
		    (idlwave-rinfo-assq name type class
					idlwave-unresolved-routines)))
	 (name (or (car entry) name))
	 (class (or (nth 2 entry) class))
	 (superclasses (idlwave-all-class-inherits initial-class))
	 (twins (idlwave-routine-twins entry))
	 (dtwins (idlwave-study-twins twins))
	 (all dtwins)
	 (system (eq (car (nth 3 entry)) 'system))
	 (calling-seq (nth 4 entry))
	 (keywords (idlwave-entry-keywords entry 'do-link))
	 (html-file (car (nth 5 entry)))
	 (help-echo-kwd
	  "Button2: Insert KEYWORD (SHIFT=`/KEYWORD') | Button3: Online Help ")
	 (help-echo-use
	  "Button2/3: Online Help")
	 (help-echo-src
	  "Button2: Jump to source and back | Button3: Source in Help window.")
	 (help-echo-class
	  "Button2: Display info about same method in superclass")
	 (col 0)
	 (data (list name type class (current-buffer) nil initial-class))
	 (face 'idlwave-help-link)
	 beg props win cnt total)
    ;; Fix keywords, but don't add chained idlwave--super-classes, since these
    ;; are shown separately for that super-class
    (setq keywords (idlwave-fix-keywords name type class keywords))
    (cond
     ((null entry)
      (error "No %s %s known %s" type name
	     (if initial-class (concat "in class " initial-class) "")))
     ((or (null name) (equal name ""))
      (error "No function or procedure call at point"))
     ((null calling-seq)
      (error "Calling sequence of %s %s not available" type name))
     (t
      (move-marker idlwave-rinfo-marker (point))
      (with-current-buffer (get-buffer-create "*Help*")
	(use-local-map idlwave-rinfo-map)
	(setq buffer-read-only nil)
	(erase-buffer)
	(set (make-local-variable 'idlwave-popup-source) nil)
	(set (make-local-variable 'idlwave-current-obj_new-class)
				  idlwave-current-obj_new-class)
	(when superclasses
	  (setq props (list 'mouse-face 'highlight
			    'local-map idlwave-rinfo-mouse-map
			    'help-echo help-echo-class
			    'data (cons 'class data)))
	  (let ((classes (cons initial-class superclasses)) c)
	    (insert "Classes: ")
	    (while (setq c (pop classes))
	      (insert " ")
	      (setq beg (point))
	      (insert c)
	      (if (equal (downcase c) (downcase class))
		  (add-text-properties beg (point) (list 'face 'bold))
		;; If Method exists in a different class link it
		(if (idlwave-rinfo-assq name type c (idlwave-routines))
		    (add-text-properties beg (point) props))))
	    (insert "\n")))
	(setq props (list 'mouse-face 'highlight
			  'local-map idlwave-rinfo-mouse-map
			  'help-echo help-echo-use
			  'data (cons 'usage data)))
	(if html-file (setq props (append (list 'face face 'link html-file)
					  props)))
	(insert "Usage:    ")
	(setq beg (point))
	(insert (if class
		    (format calling-seq class name class name class name)
		  (format calling-seq name name name name name name name))
		"\n")
	(add-text-properties beg (point) props)

	(insert "Keywords:")
	(if (null keywords)
	    (insert " No keywords accepted.")
	  (setq col 9)
	  (mapc
	   (lambda (x)
	     (if (>= (+ col 1 (length (car x)))
		     (window-width))
		 (progn
		   (insert "\n         ")
		   (setq col 9)))
	     (insert " ")
	     (setq beg (point)
		   ;; Relevant keywords already have link property attached
		   props (list 'mouse-face 'highlight
			       'local-map idlwave-rinfo-mouse-map
			       'data (cons 'keyword data)
			       'help-echo help-echo-kwd
			       'keyword (car x)))
	     (if system (setq props (append (list 'face face) props)))
	     (insert (car x))
	     (add-text-properties beg (point) props)
	     (setq col (+ col 1 (length (car x)))))
	   keywords))

	(setq cnt 1 total (length all))
	;; Here entry is (key file (list of type-conses))
	(while (setq entry (pop all))
	  (setq props (list 'mouse-face 'highlight
			    'local-map idlwave-rinfo-mouse-map
			    'help-echo help-echo-src
			    'source (list (car (car (nth 2 entry))) ;type
					  (nth 1 entry)
					  nil
					  (cdr (car (nth 2 entry))))
			    'data (cons 'source data)))
	  (idlwave-insert-source-location
	   (format "\n%-8s  %s"
		   (if (equal cnt 1)
		       (if (> total 1) "Sources:" "Source:")
		     "")
		   (if (> total 1) "- " ""))
	   entry props)
	  (cl-incf cnt)
	  (when (and all (> cnt idlwave-rinfo-max-source-lines))
	    ;; No more source lines, please
	    (insert (format
		     "\n          Source information truncated to %d entries."
		     idlwave-rinfo-max-source-lines))
	    (setq all nil)))
	(goto-char (point-min))
	(setq buffer-read-only t))
      (display-buffer "*Help*")
      (if (and (setq win (get-buffer-window "*Help*"))
	       idlwave-resize-routine-help-window)
	  (progn
	    (let ((ww (selected-window)))
	      (unwind-protect
		  (progn
		    (select-window win)
		    (enlarge-window (- (/ (frame-height) 2)
				       (window-height)))
		    (shrink-window-if-larger-than-buffer))
		(select-window ww)))))))))

(defun idlwave-insert-source-location (prefix entry &optional file-props)
  "Insert a source location into the routine info buffer.
Start line with PREFIX.  If a file name is inserted, add FILE-PROPS
to it."
  (let* ((key (car entry))
	 (file (nth 1 entry))
	 (types (nth 2 entry))
	 (shell-flag (assq 'compiled types))
	 (buffer-flag (assq 'buffer types))
	 (user-flag (assq 'user types))
	 (lib-flag (assq 'lib types))
	 (ndupl (or (and buffer-flag (idlwave-count-memq 'buffer types))
		    (and user-flag (idlwave-count-memq 'user types))
		    (and lib-flag (idlwave-count-memq 'lib types))
		    1))
	 (doflags t)
	 beg special)

    (insert prefix)

    (cond
     ((eq key 'system)
      (setq doflags nil)
      (insert "System    "))

     ((eq key 'builtin)
      (setq doflags nil)
      (insert "Builtin   "))

     ((and (not file) shell-flag)
      (insert "Unresolved"))

     ((null file)
      (insert "ERROR"))

     ((idlwave-syslib-p file)
      (if (string-match "obsolete" (file-name-directory file))
	  (insert "Obsolete  ")
	(insert "SystemLib ")))

     ;; New special syntax: taken directly from routine-info for
     ;; library catalog routines
     ((setq special (or (cdr lib-flag) (cdr user-flag)))
      (insert (format "%-10s" special)))

     ;; Old special syntax: a matching regexp
     ((setq special (idlwave-special-lib-test file))
      (insert (format "%-10s" special)))

     ;; Catch-all with file
     ((idlwave-lib-p file)      (insert "Library   "))

     ;; Sanity catch all
     (t                         (insert "Other     ")))

    (when doflags
      (insert (concat
	       "  ["
	       (if lib-flag "L" "-")
	       (if user-flag "C" "-")
	       (if shell-flag "S" "-")
	       (if buffer-flag "B" "-")
	       "] ")))
    (when (> ndupl 1)
      (setq beg (point))
      (insert (format "(%dx) " ndupl))
      (add-text-properties beg (point) (list 'face 'bold)))
    (when (and file (not (equal file "")))
      (setq beg (point))
      (insert (apply 'abbreviate-file-name (list file)))
      (if file-props
	  (add-text-properties beg (point) file-props)))))

(defun idlwave-special-lib-test (file)
  "Check the path of FILE against the regexps which define special libs.
Return the name of the special lib if there is a match."
  (let ((alist idlwave-special-lib-alist)
	entry rtn)
    (cond
     ((stringp file)
      (while (setq entry (pop alist))
	(if (string-match (car entry) file)
	    (setq rtn (cdr entry)
		  alist nil)))
      rtn)
     (t nil))))


;;----------------------------------------------------
;; Routine Info callbacks

(defun idlwave-mouse-active-rinfo-right (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev 'right))

(defun idlwave-mouse-active-rinfo-shift (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev nil 'shift))

(defun idlwave-active-rinfo-space ()
  (interactive)
  (idlwave-mouse-active-rinfo nil 'right))

(defun idlwave-mouse-active-rinfo (ev &optional right shift)
  "Do the mouse actions in the routine info buffer.
Optional args RIGHT and SHIFT indicate, if mouse-3 was used, and if SHIFT
was pressed."
  (interactive "e")
  (if ev (mouse-set-point ev))
  (let (data id name type class buf bufwin source link keyword
	     word initial-class)
    (setq data (get-text-property (point) 'data)
	  source (get-text-property (point) 'source)
	  keyword (get-text-property (point) 'keyword)
	  link (get-text-property (point) 'link)
	  id (car data)
	  name (nth 1 data) type (nth 2 data) class (nth 3 data)
	  buf (nth 4 data)
	  initial-class (nth 6 data)
	  word (idlwave-this-word)
	  bufwin (get-buffer-window buf t))

    (cond ((eq id 'class) ; Switch class being displayed
	   (if (window-live-p bufwin) (select-window bufwin))
	   (idlwave-display-calling-sequence
	    (idlwave-sintern-method name)
	    type (idlwave-sintern-class word)
	    initial-class))
	  ((eq id 'usage) ; Online help on this routine
	   (idlwave-online-help link name type class))
	  ((eq id 'source) ; Source in help or buffer
	   (if right ; In help
	       (let ((idlwave-extra-help-function 'idlwave-help-with-source)
		     (idlwave-help-source-try-header nil)
		     ;; Fake idlwave-routines so help will find the right entry
		     (idlwave-routines
		      (list (list name type class source ""))))
		 (idlwave-help-get-special-help name type class nil))
	     ;; Otherwise just pop to the source
	     (setq idlwave-popup-source (not idlwave-popup-source))
	     (if idlwave-popup-source
		 (condition-case err
		     (idlwave-do-find-module name type class source)
		   (error
		    (setq idlwave-popup-source nil)
		    (if (window-live-p bufwin) (select-window bufwin))
		    (error (nth 1 err))))
	       (if bufwin
		   (select-window bufwin)
		 (pop-to-buffer buf))
	       (goto-char (marker-position idlwave-rinfo-marker)))))
	  ((eq id 'keyword)
	   (if right
	       (idlwave-online-help link name type class keyword)
	     (idlwave-rinfo-insert-keyword keyword buf shift))))))

(defun idlwave-rinfo-insert-keyword (keyword buffer &optional shift)
  "Insert KEYWORD in BUFFER.  Make sure buffer is displayed in a window."
  (let ((bwin (get-buffer-window buffer)))
    (if idlwave-complete-empty-string-as-lower-case
	(setq keyword (downcase keyword)))
    (if bwin
	(select-window bwin)
      (pop-to-buffer buffer)
      (setq bwin (get-buffer-window buffer)))
    (if (eq (preceding-char) ?/)
	(insert keyword)
      (unless (save-excursion
		(re-search-backward
		 "[(,][ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\)?[ \t]*\\="
		 (min (- (point) 100) (point-min)) t))
	(insert ", "))
      (if shift (insert "/"))
      (insert keyword)
      (if (and (not shift)
	       idlwave-keyword-completion-adds-equal)
	  (insert "=")))))



(defun idlwave-print-source (routine)
  (let* ((source (nth 3 routine))
	 (stype (car source))
	 (sfile (idlwave-routine-source-file source)))
    (if (idlwave-syslib-p sfile) (setq stype 'syslib))
    (if (and (eq stype 'compiled)
	     (or (not (stringp sfile))
		 (not (string-match "\\S-" sfile))))
	(setq stype 'unresolved))
    (princ (format "      %-10s %s\n"
		   stype
		   (if sfile sfile "No source code available")))))

(provide 'idlw-routine)
(provide 'idlwave-routine)
