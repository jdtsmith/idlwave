;;; idlw-scan.el --- Scan routine code information for IDLWAVE -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2024  Free Software Foundation, Inc.

;;; Comentary
;; IDLWAVE: scan routine information provided with IDL, and among the
;; user's library, as well as in open buffers (for scanning the shell,
;; see idlw-shell.el)

;;; Code
(require 'timer)
(eval-when-compile (require 'cl-lib))
(require 'idlw-variables)
;; idlwave-routines format (whether system, library, or userlib)
;; ("ROUTINE" type class
;;  (system) | (lib pro_file dir "LIBNAME") | (user pro_file dir "USERLIB") |
;;  (buffer pro_file dir) | (compiled pro_file dir)
;;   "calling_string" ("LINKFILE" (("KWD1" . anchorlink1) ...))
;;                    ("LINKFILE2" (("KWD2" . ancorlink2) ...)) ...)

;;----------------------------------------------------
;; Storage variables

(defvar idlwave-routines nil
  "Holds the combined procedure/function/method routine-info from all sources.")
(defvar idlwave-system-routines nil
  "Holds the system routine-info obtained from the XML catalog.")
(defvar idlwave-buffer-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-compiled-routines nil
  "Holds the routine-info obtained by asking the shell.")
(defvar idlwave-unresolved-routines nil
  "Holds the unresolved routine-info obtained by asking the shell.")
(defvar idlwave-user-catalog-routines nil
  "Holds the routine-info from the user scan.")
(defvar idlwave-library-catalog-routines nil
  "Holds the routine-info from the .idlwave_catalog library files.")
(defvar idlwave-class-alist nil
  "Holds the class names and brief info known to IDLWAVE.")
(defvar idlwave-class-info nil)

(defvar idlwave-library-catalog-libname nil
  "Name of library catalog loaded from .idlwave_catalog files.")
(defvar idlwave-path-alist nil
  "Alist with !PATH directories and zero or more flags if the dir has
been scanned in a user catalog ('user) or discovered in a library
catalog \('lib).")
(defvar idlwave-true-path-alist nil
  "Like `idlwave-path-alist', but with true filenames.")

(defvar idlwave-class-history nil
  "The history of classes selected with the minibuffer.")
(defvar idlwave-last-system-routine-info-cons-cell nil
  "The last cons cell in the system routine info.")

;; XML information from idl_catalog.xml
(defvar idlwave-xml-routine-info-file nil)
(defvar idlwave-system-class-info nil)
(defvar idlwave-system-variables-alist nil
  "Alist of system variables and the associated structure tags.
Gets set in cached XML rinfo.")
(defvar idlwave-executive-commands-alist nil
  "Alist of system variables and their help files.")
(defvar idlwave-help-special-topic-words nil)

(defvar idlwave-catalog-process nil
  "The background process currently updating the catalog.")

;; These are located in the idlwave-config-directory
(defvar idlwave-user-catalog-file "idlusercat.el")
(defvar idlwave-xml-system-rinfo-converted-file "idl_xml_rinfo.el")
(defvar idlwave-path-file "idlpath.el")

;;----------------------------------------------------
;; Global routine info inquiry/scanning

(defun idlwave-routines ()
  "Provide a list of IDL routines.
This routine loads the builtin routines on the first call.
Later it only returns the value of the variable."
  (if (and idlwave-catalog-process
	   (processp idlwave-catalog-process))
      (progn
	(cond
	 ((equal (process-status idlwave-catalog-process) 'exit)
	  (message "updating........")
	  (setq idlwave-catalog-process nil)
	  (idlwave-update-routine-info '(4)))
	 ((equal (process-status idlwave-catalog-process) 'run)
	  ;; Keep it running...
	  )
	 (t
	  ;; Something is wrong, get rid of the process
	  (message "Problem with catalog process") (beep)
	  (condition-case nil
	      (kill-process idlwave-catalog-process)
	    (error nil))
	  (setq idlwave-catalog-process nil)))))
  (or idlwave-routines
      (progn
	(idlwave-update-routine-info)
	;; return the current value
	idlwave-routines)))

(defun idlwave-load-rinfo-next-step ()
  (let ((inhibit-quit t)
	(arr idlwave-load-rinfo-steps-done))
    (if (catch 'exit
	  (when (not (aref arr 0))
	    (message "Loading system routine info in idle time...")
	    (idlwave-load-system-routine-info)
	    (message "Loading system routine info in idle time...done")
	    (aset arr 0 t)
	    (throw 'exit t))

	  (when (not (aref arr 1))
	    (message "Normalizing idlwave-system-routines in idle time...")
	    (idlwave-reset-sintern t)
	    (put 'idlwave-reset-sintern 'done-by-idle t)
	    (setq idlwave-system-routines
		  (idlwave-sintern-rinfo-list idlwave-system-routines 'sys))
	    (message "Normalizing idlwave-system-routines in idle time...done")
	    (aset arr 1 t)
	    (throw 'exit t))

	  (when (not (aref arr 2))
	    (when (and (stringp idlwave-user-catalog-file)
		       (file-regular-p idlwave-user-catalog-file))
	      (message "Loading user catalog in idle time...")
	      (condition-case nil
		  (load-file idlwave-user-catalog-file)
		(error (throw 'exit nil)))
	      ;; Check for the old style catalog and warn
	      (if (and
		   (boundp 'idlwave-library-routines)
		   idlwave-library-routines)
		  (progn
		    (setq idlwave-library-routines nil)
		    (ding)
		    (message "Outdated user catalog: %s... recreate"
			     idlwave-user-catalog-file))
		(message "Loading user catalog in idle time...done")))
	    (aset arr 2 t)
	    (throw 'exit t))

	  (when (not (aref arr 3))
	    (when idlwave-user-catalog-routines
	      (message "Normalizing user catalog routines in idle time...")
	      (setq idlwave-user-catalog-routines
		    (idlwave-sintern-rinfo-list
		     idlwave-user-catalog-routines 'sys))
	      (message
	       "Normalizing user catalog routines in idle time...done"))
	    (aset arr 3 t)
	    (throw 'exit t))

	  (when (not (aref arr 4))
	    (idlwave-scan-library-catalogs
	     "Loading and normalizing library catalogs in idle time...")
	    (aset arr 4 t)
	    (throw 'exit t))
	  (when (not (aref arr 5))
	    (message "Finishing initialization in idle time...")
	    (idlwave-routines)
	    (message "Finishing initialization in idle time...done")
	    (aset arr 5 t)
	    (throw 'exit nil)))
	;; restart the timer
	(if (sit-for 1)
	    (idlwave-load-rinfo-next-step)
	  (setq idlwave-load-rinfo-idle-timer
		(run-with-idle-timer
		 idlwave-init-rinfo-when-idle-after
		 nil #'idlwave-load-rinfo-next-step))))))

(defun idlwave-load-all-rinfo (&optional force)
  ;; Load and case-treat the system, user catalog, and library routine
  ;; info files.

  ;; System
  (when (or force (not (aref idlwave-load-rinfo-steps-done 0)))
    (idlwave-load-system-routine-info))
  (when (or force (not (aref idlwave-load-rinfo-steps-done 1)))
    (message "Normalizing idlwave-system-routines...")
    (setq idlwave-system-routines
	  (idlwave-sintern-rinfo-list idlwave-system-routines 'sys))
    (message "Normalizing idlwave-system-routines...done"))
  (when idlwave-system-routines
    (setq idlwave-routines (copy-sequence idlwave-system-routines))
    (setq idlwave-last-system-routine-info-cons-cell
	  (nthcdr (1- (length idlwave-routines)) idlwave-routines)))

  ;; User catalog
  (when (and (stringp idlwave-user-catalog-file)
	     (file-regular-p idlwave-user-catalog-file))
    (condition-case nil
	(when (or force (not (aref idlwave-load-rinfo-steps-done 2)))
	  (load-file idlwave-user-catalog-file))
      (error nil))
    (when (and
	   (boundp 'idlwave-library-routines)
	   idlwave-library-routines)
      (setq idlwave-library-routines nil)
      (error "Outdated user catalog: %s... recreate"
	     idlwave-user-catalog-file))
    (setq idlwave-true-path-alist nil)
    (when (or force (not (aref idlwave-load-rinfo-steps-done 3)))
      (message "Normalizing user catalog routines...")
      (setq idlwave-user-catalog-routines
	    (idlwave-sintern-rinfo-list
	     idlwave-user-catalog-routines 'sys))
      (message "Normalizing user catalog routines...done")))

  ;; Library catalog
  (when (or force (not (aref idlwave-load-rinfo-steps-done 4)))
    (idlwave-scan-library-catalogs
     "Loading and normalizing library catalogs..."))
  (run-hooks 'idlwave-after-load-rinfo-hook))

(defvar idlwave-load-rinfo-idle-timer)
(defvar idlwave-shell-path-query)

(defun idlwave-update-routine-info (&optional arg no-concatenate)
  "Update the internal routine-info lists.
These lists are used by `idlwave-routine-info' (\\[idlwave-routine-info])
and by `idlwave-complete' (\\[idlwave-complete]) to provide information
about individual routines.

The information can come from 5 sources:
1. IDL programs in buffers in the current editing session.
2. Compiled modules in an IDL shell running under IDLWAVE.
3. The IDL system routines converted from XML catalog supplied with IDL.
4. Pre-scanned library files with .idlwave_catalog files.
5. A pre-scanned user catalog of local directories.

Scans all IDLWAVE-mode buffers of the current editing session (see
`idlwave-scan-all-buffers-for-routine-info').
When an IDL shell is running, this command also queries the IDL program
for currently compiled routines.

With prefix ARG, also reload the system and library lists.
With two prefix ARG's, also rescans the chosen user catalog tree.
With three prefix args, dispatch asynchronous process to do the update.

If NO-CONCATENATE is non-nil, don't pre-concatenate the routine info
lists, but instead wait for the shell query to complete and
asynchronously finish updating routine info.  This is set
automatically when called interactively.  When you need routine
information updated immediately, leave NO-CONCATENATE nil."
  (interactive "P\np")
  ;; Stop any idle processing
  (if (timerp idlwave-load-rinfo-idle-timer)
      (cancel-timer idlwave-load-rinfo-idle-timer))
  (cond
   ((equal arg '(64))
    ;; Start a background process which updates the catalog.
    (idlwave-rescan-asynchronously))
   ((equal arg '(16))
    ;; Update the user catalog now, and wait for them.
    (idlwave-create-user-catalog-file t))
   (t
    (let* ((load (or arg
		     idlwave-buffer-case-takes-precedence
		     (null idlwave-routines)))
	   ;; The override-idle means, even if the idle timer has done some
	   ;; preparing work, load and renormalize everything anyway.
	   (override-idle (or arg idlwave-buffer-case-takes-precedence)))

      (setq idlwave-buffer-routines nil
	    idlwave-compiled-routines nil
	    idlwave-unresolved-routines nil)
      ;; Reset the appropriate hashes
      (if (get 'idlwave-reset-sintern 'done-by-idle)
	  ;; reset was already done in idle time, so skip this step now once
	  (put 'idlwave-reset-sintern 'done-by-idle nil)
	(idlwave-reset-sintern (cond (load t)
				     ((null idlwave-system-routines) t)
				     (t 'bufsh))))

      (if idlwave-buffer-case-takes-precedence
	  ;; We can safely scan the buffer stuff first
	  (progn
	    (idlwave-update-buffer-routine-info)
	    (and load (idlwave-load-all-rinfo override-idle)))
	;; We first do the system info, and then the buffers
	(and load (idlwave-load-all-rinfo override-idle))
	(idlwave-update-buffer-routine-info))

      ;; Let's see if there is a shell
      (let* ((shell-is-running (and (fboundp 'idlwave-shell-is-running)
				    (idlwave-shell-is-running)))
	     (ask-shell (and shell-is-running
			     idlwave-query-shell-for-routine-info)))

	;; Load the library catalogs again, first re-scanning the path
	(when arg
	  (if shell-is-running
	      (idlwave-shell-send-command idlwave-shell-path-query
					  (lambda ()
					    (idlwave-shell-get-path-info)
					    (idlwave-scan-library-catalogs))
					  'hide)
	    (idlwave-scan-library-catalogs)))

	(if (or (not ask-shell)
		(not no-concatenate))
	    ;; 1. If we are not going to ask the shell, we need to do the
	    ;;    concatenation now.
	    ;; 2. When this function is called non-interactively, it
	    ;;    means that someone needs routine info *now*.  The
	    ;;    shell update causes the concatenation to be
	    ;;    *delayed*, so not in time for the current command.
	    ;;    Therefore, we do a concatenation now, even though
	    ;;    the shell might do it again.
	    (idlwave-concatenate-rinfo-lists nil 'run-hooks))

	(when ask-shell
	  ;; Ask the shell about the routines it knows of.
	  (message "Querying the shell")
	  (idlwave-shell-update-routine-info nil t)))))))

(defun idlwave-concatenate-rinfo-lists (&optional quiet run-hook)
  "Put the different sources for routine information together."
  ;; The sequence here is important because earlier definitions shadow
  ;; later ones.  We assume that if things in the buffers are newer
  ;; then in the shell of the system, they are meant to be different.
  (if (consp idlwave-last-system-routine-info-cons-cell)
      ;; FIXME: Where is this var actually used?
      (setcdr idlwave-last-system-routine-info-cons-cell
	      (append idlwave-buffer-routines
		      idlwave-compiled-routines
		      idlwave-library-catalog-routines
		      idlwave-user-catalog-routines)))
  (setq idlwave-class-alist nil)

  ;; Give a message with information about the number of routines we have.
  (unless quiet
    (message
     "Routines Found: buffer(%d) compiled(%d) library(%d) user(%d) system(%d)"
     (length idlwave-buffer-routines)
     (length idlwave-compiled-routines)
     (length idlwave-library-catalog-routines)
     (length idlwave-user-catalog-routines)
     (length idlwave-system-routines)))
  (if run-hook
      (run-hooks 'idlwave-update-rinfo-hook)))

;;----------------------------------------------------
;; Idle time routine updating

(defvar idlwave-after-load-rinfo-hook nil)
(defvar idlwave-update-rinfo-hook nil
  "List of functions which should run after a global rinfo update.
Does not run after automatic updates of buffer or the shell.")
(defvar idlwave-class-reset nil) ; to reset buffer-local classes

(with-eval-after-load 'idlwave
  (or idlwave-routines (idlwave-start-load-rinfo-timer)))
(add-hook 'idlwave-update-rinfo-hook
	  (lambda () (setq idlwave-class-reset t)))
(add-hook 'idlwave-after-load-rinfo-hook
	  (lambda () (setq idlwave-class-info nil)))

(defvar idlwave-load-rinfo-steps-done (make-vector 6 nil))
(defvar idlwave-load-rinfo-idle-timer nil)
(defun idlwave-start-load-rinfo-timer ()
  (if (timerp idlwave-load-rinfo-idle-timer)
      (cancel-timer idlwave-load-rinfo-idle-timer))
  (setq idlwave-load-rinfo-steps-done (make-vector 6 nil))
  (setq idlwave-load-rinfo-idle-timer nil)
  (if (and idlwave-init-rinfo-when-idle-after
	   (numberp idlwave-init-rinfo-when-idle-after)
	   (not (equal 0 idlwave-init-rinfo-when-idle-after))
	   (not idlwave-routines))
      (condition-case nil
	  (progn
	    (setq idlwave-load-rinfo-idle-timer
		  (run-with-idle-timer
		   idlwave-init-rinfo-when-idle-after
		   nil #'idlwave-load-rinfo-next-step)))
	(error nil))))

(defvar idlwave-library-routines nil "Older library routine info.")

;;----------------------------------------------------
;; XML System Catalog

(defun idlwave-xml-system-routine-info-file()
  "Return the file for the XML catalog file bundled with IDL."
  (let* ((dir (file-name-as-directory
	       (expand-file-name "help/" (idlwave-sys-dir)))))
    (if (and (not (file-exists-p (expand-file-name "idl_catalog.xml" dir)))
	     (file-directory-p (expand-file-name "online_help" dir)))
	(setq dir (expand-file-name "online_help" dir)))
    (expand-file-name "idl_catalog.xml" dir)))

(defun idlwave-convert-xml-system-routine-info ()
  "Convert XML supplied IDL routine info into internal form.
Cache to disk for quick recovery."
  (interactive)
  (let* ((catalog-file (idlwave-xml-system-routine-info-file))
	 (elem-cnt 0)
	 props rinfo msg-cnt elem type nelem class-result alias
	 routines routine-aliases graphics-keywords
	 statement-aliases sysvar-aliases)
    (if (not (file-exists-p catalog-file))
	(error "No such XML routine info file: %s" catalog-file)
      (if (not (file-readable-p catalog-file))
	  (error "Cannot read XML routine info file: %s" catalog-file)))
    (message "Reading XML routine info...")
    (require 'xml)
    (setq rinfo
	  (let ((xml-validating-parser t))
	    (condition-case nil
		(xml-parse-file catalog-file)
	      (error ;; Deal with XML.el bug
	       (setq xml-validating-parser nil)
	       (with-temp-buffer
		 (insert-file-contents catalog-file)
		 (while
		     (re-search-forward
		      "^\\s-*<!\\(ATTLIST\\|ELEMENT\\) * [A-Z]+_[A-Z]+.*>\\s-*[\r\n]"
		      nil t)
		   (replace-match ""))
		 (xml-parse-region (point-min) (point-max)))))))
    (message "Reading XML routine info...done")
    (setq rinfo (assq 'CATALOG rinfo))
    (unless rinfo (error "Failed to parse XML routine info"))
    ;;(setq rinfo (car rinfo)) ; Skip the catalog stuff.

    (setq rinfo (cddr rinfo))

    (setq nelem (length rinfo)
	  msg-cnt (/ nelem 20))

    (setq idlwave-xml-routine-info-file nil)
    ;; FIXME: Use `make-progress-reporter'
    (message "Converting XML routine info...")
    (setq idlwave-system-routines nil
	  idlwave-system-variables-alist nil
	  idlwave-system-class-info nil
	  idlwave-executive-commands-alist nil
	  idlwave-help-special-topic-words nil)

    (while rinfo
      (setq elem (car rinfo)
	    rinfo (cdr rinfo)
	    elem-cnt (1+ elem-cnt))
      (when (listp elem)
	(setq type (car elem)
	      props (car (cdr elem)))
	(if (= (mod elem-cnt msg-cnt) 0)
	    (message "Converting XML routine info...%2d%%"
		     (floor (* elem-cnt 100.0) nelem)))
	(cond
	 ((eq type 'ROUTINE)
	  (if (and (setq alias (cdr (assq 'alias_to props)))
		   (not (string= "" alias)))
	      (push (cons (cdr (assq 'name props)) alias)
		    routine-aliases)
	    (setq routines (idlwave-xml-create-rinfo-list elem))
	    (if (listp (cdr routines))
		(setq idlwave-system-routines
		      (nconc idlwave-system-routines routines))
	      ;; a cons cell is an executive commands
	      (push routines idlwave-executive-commands-alist))))

	 ((eq type 'CLASS)
	  (setq class-result (idlwave-xml-create-class-method-lists elem))
	  (push (car class-result) idlwave-system-class-info)
	  (setq idlwave-system-routines
		(nconc idlwave-system-routines (cdr class-result))))

	 ((eq type 'STATEMENT)
	  (push (cons (cdr (assq 'name props))
		      (cdr (assq 'link props)))
		idlwave-help-special-topic-words)
	  ;; Save the links to those which are statement aliases (not routines)
	  (if (and (setq alias (cdr (assq 'alias_to props)))
		   (not (string= "" alias)))
	      (unless (member alias statement-aliases)
		(push alias statement-aliases))))

	 ((eq type 'SYSVAR)
	  (if (and (setq alias (cdr (assq 'alias_to props)))
		   (not (string= "" alias)))
	      (push (cons (substring (cdr (assq 'name props)) 1)
			  (substring alias 1))
		    sysvar-aliases)
	    (push (idlwave-xml-create-sysvar-alist elem)
		  idlwave-system-variables-alist)))
	 (t))))
    (idlwave-convert-xml-add-link-path-information)
    (idlwave-convert-xml-clean-routine-aliases routine-aliases)
    (idlwave-convert-xml-clean-statement-aliases statement-aliases)
    (idlwave-convert-xml-clean-sysvar-aliases sysvar-aliases)

    (setq idlwave-xml-routine-info-file catalog-file)
    (idlwave-save-xml-routine-info)
    (message "Converting XML routine info...done")))

(defun idlwave-save-xml-routine-info ()
  (if idlwave-xml-routine-info-file
      (with-temp-file idlwave-xml-system-rinfo-converted-file
	(insert
	 (concat ";; *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
;; IDLWAVE Routine Information File (IDLWAVE version " idlwave-mode-version ")
;; Automatically generated from source file:
;;  " idlwave-xml-routine-info-file "
;; on " (current-time-string) "
;; " (format "%d routines, %d classes, %d sysvars, %d exec commands"
	     (length idlwave-system-routines)
	     (length idlwave-system-class-info)
	     (length idlwave-system-variables-alist)
	     (length idlwave-executive-commands-alist)) "
;; Do not edit."))
	(insert (format "\n(setq idlwave-xml-routine-info-file \n    \"%s\")"
			idlwave-xml-routine-info-file))
	(insert "\n(setq idlwave-system-routines\n    '")
	(prin1 idlwave-system-routines (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-system-variables-alist\n    '")
	(prin1 idlwave-system-variables-alist (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-system-class-info\n    '")
	(prin1 idlwave-system-class-info (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-executive-commands-alist\n    '")
	(prin1 idlwave-executive-commands-alist (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-help-special-topic-words\n    '")
	(prin1 idlwave-help-special-topic-words (current-buffer))
	(insert ")"))))

(defun idlwave-rescan-xml-routine-info ()
  "Rescan and reload IDL XML routine info."
  (interactive)
  (idlwave-convert-xml-system-routine-info)
  (idlwave-update-routine-info))

(defun idlwave-load-system-routine-info ()
  ;; Load the system routine info from the cached routine info file,
  ;; which, if necessary, will be re-created from the XML file on
  ;; disk.
  (unless (and (load idlwave-xml-system-rinfo-converted-file
		     'noerror 'nomessage)
	       (idlwave-xml-system-routine-info-up-to-date))
    ;; See if we can create it from XML source
    (idlwave-convert-xml-system-routine-info)))

(defun idlwave-xml-system-routine-info-up-to-date()
  (let* ((catalog-file (idlwave-xml-system-routine-info-file)))
    (file-newer-than-file-p ;converted file is newer than catalog
     idlwave-xml-system-rinfo-converted-file
     catalog-file)))

(defun idlwave-shorten-syntax (syntax name &optional class)
  ;; From a list of syntax statements, shorten with %s and group with "or"
  (let ((case-fold-search t))
    (mapconcat
     (lambda (x)
       (while (string-match name x)
	 (setq x (replace-match "%s" t t x)))
       (if class
	   (while (string-match class x)
	     (setq x (replace-match "%s" t t x))))
       x)
     (nreverse syntax)
     " or ")))

(defun idlwave-xml-create-class-method-lists (xml-entry)
  ;; Create a class list entry from the xml parsed list., returning a
  ;; cons of form (class-entry method-entries).
  (let* ((nameblock (nth 1 xml-entry))
	 (class (cdr (assq 'name nameblock)))
	 (link (cdr (assq 'link nameblock)))
	 (params (cddr xml-entry))
	 (case-fold-search t)
	 class-entry
	 method methods-entry extra-kwds
	 props get-props set-props init-props inherits
	 pelem ptype)
    (while params
      (setq pelem (car params))
      (when (listp pelem)
	(setq ptype (car pelem)
	      props (car (cdr pelem)))
	(cond
	 ((eq ptype 'SUPERCLASS)
	  (let ((pname (cdr (assq 'name props)))
		(plink (cdr (assq 'link props))))
	    (unless (and (string= pname "None")
			 (string= plink "None"))
	      (push pname inherits))))

	 ((eq ptype 'PROPERTY)
	  (let ((pname (cdr (assq 'name props)))
		(plink (cdr (assq 'link props)))
		(get (string= (cdr (assq 'get props)) "Yes"))
		(set (string= (cdr (assq 'set props)) "Yes"))
		(init (string= (cdr (assq 'init props)) "Yes")))
	    (if get (push (list pname plink) get-props))
	    (if set (push (list pname plink) set-props))
	    (if init (push (list pname plink) init-props))))

	 ((eq ptype 'METHOD)
	  (let ((mlink (cdr (assq 'link props)))
		(case-fold-search t))
	    (unless (string-match "\.html?$" mlink) ;must be a keyword, use class link
	      (setf (cdr (assq 'link props))
		    (idlwave-substitute-link-target link mlink))))
	  (setq method (cdr (assq 'name props)))
	  (setq extra-kwds ;;Assume all property keywords are gathered already
		(cond
		 ((string-match (concat class "::Init") method)
		  (put 'init-props 'matched t)
		  init-props)
		 ((string-match (concat class "::GetProperty") method)
		  (put 'get-props 'matched t)
		  get-props)
		 ((string-match (concat class "::SetProperty") method)
		  (put 'set-props 'matched t)
		  set-props)
		 (t nil)))
	  (setq methods-entry
		(nconc (idlwave-xml-create-rinfo-list pelem class extra-kwds)
		       methods-entry)))
	 (t)))
      (setq params (cdr params)))
    ;;(unless (get 'init-props 'matched)
    ;;  (message "Failed to match Init in class %s" class))
    ;;(unless (get 'get-props 'matched)
    ;;  (message "Failed to match GetProperty in class %s" class))
    ;;(unless (get 'set-props 'matched)
    ;;  (message "Failed to match SetProperty in class %s" class))
    (setq class-entry
	  (if inherits
	      (list class (append '(inherits) inherits) (list 'link link))
	    (list class (list 'link link))))
    (cons class-entry methods-entry)))

(defun idlwave-xml-create-rinfo-list (xml-entry &optional class extra-kws)
  ;; Create correctly structured list elements from ROUTINE or METHOD
  ;; XML list structures.  Return a list of list elements, with more
  ;; than one sub-list possible if a routine can serve as both
  ;; procedure and function (e.g. call_method).
  (let* ((nameblock (nth 1 xml-entry))
	 (name (cdr (assq 'name nameblock)))
	 (link (cdr (assq 'link nameblock)))
	 (link-dir (file-name-directory link))
	 (params (cddr xml-entry))
	 (syntax-vec (make-vector 3 nil)) ; procedure, function, exec command
	 (case-fold-search t)
	 graphics-kws
	 syntax kwd klink pref-list kwds pelem ptype props result type)
    (if class ;; strip out class name from class method name string
	(if (string-match (concat class "::") name)
	    (setq name (substring name (match-end 0)))))
    (while params
      (setq pelem (car params))
      (when (listp pelem)
	(setq ptype (car pelem)
	      props (car (cdr pelem)))
	(cond
	 ((eq ptype 'SYNTAX)
	  (setq syntax (cdr (assq 'name props))
		type (cdr (assq 'type props)))
	  (if (and (string-match "Graphics *Keywords" syntax)
		   (string-match "^pro" type))
	      ;; Unlinked Graphics Keywords masquerading as Syntax :(.
	      (let ((pos 0))
		(while
		    (string-match "\\[, /?\\({X *| *Y *| *Z}\\)?\\([A-Z0-9]+\\)[]=]" syntax pos)
		  (if (match-string 1 syntax)
		      (cl-loop for x in '("X" "Y" "Z") do
			       (push (concat x (match-string 2 syntax)) graphics-kws))
		    (push (match-string 2 syntax) graphics-kws))
		  (setq pos (match-end 0)))
		(message "Parsing Graphics Keywords for %s: %d found" name (length graphics-kws)))
	    (if (string-match "-&gt;" syntax)
		(setq syntax (replace-match "->" t nil syntax)))
	    (push syntax
		  (aref syntax-vec (cond
				    ((or
				      (string-match "^exec" type)
				      (string= (substring name 0 1) "."))
				     2)
				    ((string-match "^pro" type) 0)
				    ((string-match "^fun" type) 1)
				    (t 0))))))

	 ((eq ptype 'KEYWORD)
	  (setq kwd (cdr (assq 'name props))
		klink (cdr (assq 'link props)))
	  (if (and link-dir klink
		   (not (string= (file-name-directory klink) link-dir)))
	      (setq klink (concat link-dir klink)))
	  (if (string-match "\\s-*(.*?Only)" kwd)
	      (setq kwd (replace-match "" t t kwd)))
	  (if (string-match "^\\[XY\\(Z?\\)\\]" kwd)
	      (progn
		(setq pref-list
		      (if (match-string 1 kwd) '("X" "Y" "Z") '("X" "Y"))
		      kwd (substring kwd (match-end 0)))
		(cl-loop for x in pref-list do
		         (push (list (concat x kwd) klink) kwds)))
	    (push (list kwd klink) kwds)))

	 (t))); Do nothing for the others
      (setq params (cdr params)))

    ;; Debug
    ;; (if (and (null (aref syntax-vec 0))
    ;;          (null (aref syntax-vec 1))
    ;;          (null (aref syntax-vec 2)))
    ;;   (with-current-buffer (get-buffer-create "IDL_XML_catalog_complaints")
    ;;     (if class
    ;;         (insert (format "Missing SYNTAX entry for %s::%s\n" class name))
    ;;       (insert (message "Missing SYNTAX entry for %s\n" name)))))

    ;; Build the routine info list
    (if (aref syntax-vec 2)     ; executive commands are treated specially
	(cons (substring name 1) link)
      (if extra-kws (setq kwds (nconc kwds extra-kws)))
      (if graphics-kws
	  (setq kwds (nconc kwds (idlwave-graphics-keywords graphics-kws))))
      (setq kwds (idlwave-rinfo-group-keywords kwds link))
      (cl-loop for idx from 0 to 1 do ;add a procedure and function if needed
	       (if (aref syntax-vec idx)
		   (push (append (list name (if (eq idx 0) 'pro 'fun)
				       class '(system)
				       (idlwave-shorten-syntax
				        (aref syntax-vec idx) name class))
				 kwds)
			 result)))
      result)))

(defvar idlwave-graphics-keywords-links-alist nil)
(defun idlwave-graphics-keywords (kwds)
  ;; Given a list of keywords, find links and return a list of (kwd link) lists
  ;; from the special (and poorly linked) Graphics Keywords help file
  (unless idlwave-graphics-keywords-links-alist
    ;; Cache a list of links
    (let ((gkwfile (idlwave-recursive-find-file (idlwave-html-help-location)
					     "graphkeyw.htm")))
      (if (file-exists-p gkwfile)
	  (save-match-data
	    (save-excursion
	      (with-temp-buffer
		(insert-file-contents gkwfile)
		(goto-char 1)
		(while (search-forward-regexp
			"a href=\"\\(#graphkeyw.[^\"]+\\)\"[^>]*>\\([^ <]+\\)<"
			nil t)
		  (let ((anchor (match-string 1))
			(kwd (match-string 2)) kwds)
		    (if (string-match "^\\[XYZ\\]" kwd)
			(progn
			  (setq kwd (substring kwd (match-end 0)))
			  (setq kwds (mapcar (lambda (x) (concat x kwd)) '("X" "Y" "Z"))))
		      (setq kwds (list kwd)))
		    (cl-loop for kwd in kwds do
			     (unless (assoc kwd idlwave-graphics-keywords-links-alist)
			       (push (cons kwd (concat gkwfile anchor))
				     idlwave-graphics-keywords-links-alist)))))))))))
  ;; find the keywords and links
  (mapcar
   (lambda (kwd)
     (let ((rec (assoc kwd idlwave-graphics-keywords-links-alist)))
       (list kwd (if rec (cdr rec)))))
   kwds))

(defun idlwave-rinfo-group-keywords (kwds master-link)
  ;; Group keywords by link file, as a list with elements (linkfile (
  ;; ("KWD1" . link1) ("KWD2" . link2)) master-link specifies the link
  ;; for the parent routine.
  (let (kwd link anchor linkfiles block master-elt)
    (while kwds
      (setq kwd (car kwds)
	    link (idlwave-split-link-target (nth 1 kwd))
	    anchor (cdr link)
	    link (car link)
	    kwd (car kwd))
      (if (setq block (assoc link linkfiles))
	  (push (cons kwd anchor) (cdr block))
	(push (list link (cons kwd anchor)) linkfiles))
      (setq kwds (cdr kwds)))
    ;; Ensure the master link is there
    (if (setq master-elt (assoc master-link linkfiles))
	(if (eq (car linkfiles) master-elt)
	    linkfiles
 	  (cons master-elt (delq master-elt linkfiles)))
      (push (list master-link) linkfiles))))

(defun idlwave-convert-xml-clean-statement-aliases (aliases)
  ;; Clean up the syntax of routines which are actually aliases by
  ;; removing the "OR" from the statements
  (let (syntax entry)
    (cl-loop for x in aliases do
	     (setq entry (assoc x idlwave-system-routines))
	     (when entry
	       (while (string-match " +or +" (setq syntax (nth 4 entry)))
	         (setf (nth 4 entry) (replace-match ", " t t syntax)))))))

(defun idlwave-alias-path (file alias-list content-path &optional class-dir)
  "Search for the HTML help file FILE in the help content.
Uses alias information ALIAS-LIST from Alias.xml to link to the
help content, in top level CONTENT-PATH.  CLASS-DIR, if set, is
the directory of the class of a routine to try if it can't be
found through other means.  As a last resort attempt a brute
force directory search."
  (let* (alias linkfile
	       (parts (idlwave-split-link-target file))
	       (anchor (cdr parts)))
    (if anchor
	(setq file (car parts)))
    (if (and file (> (length file) 0))
	(cond
	 ;; Directly on the alias list
	 ((and
	   (setq alias (assoc-string file alias-list 'ignore-case))
	   (file-exists-p (setq linkfile
				(expand-file-name (cdr alias) content-path)))))

	 ;; Sitting on content path already
	 ((file-exists-p (setq linkfile (expand-file-name file content-path))))

	 ;; Leading ../'s
	 ((and (string-match "^\\(\\.\\./\\)+" file)
	       (file-exists-p
		(setq linkfile
		      (expand-file-name
		       (replace-match "" t t file)
		       content-path)))))
	  ;(message "Found extra ../: %s" file))

	 ;; With the optional class directory passed in
	 ((and class-dir
	       (file-exists-p
		(setq linkfile
		      (expand-file-name (concat class-dir file)
					content-path)))))
	  ;(message "Found via class: %s" file))

	 ;; Under Alphabetized Reference Directory
	 ((file-exists-p
	   (setq linkfile
		 (expand-file-name
		  (concat "Reference Material"
			  "/" (upcase (substring file 0 1)) "/" file)
		  content-path))))
	  ;(message "Found alphabetically under Reference Material: %s" file))

	 ;; Dir from root name alias (e.g. CLASS_METHOD.html -> CLASS.html)
	 ((let ((lfroot
		 (replace-regexp-in-string
		  "_+[^_]*\\.htm\\(l?\\)" ".htm\\1" file)))
	    (and (not (string= file lfroot))
		 (setq alias (assoc-string lfroot alias-list 'ignore-case))
		 (file-exists-p
		  (setq linkfile
			(expand-file-name
			 (concat (file-name-directory (cdr alias)) file)
			 content-path))))))
	  ;(message "Found using root name: %s" file))

	 ;; Didn't find it... try a brute-force directory search
	 (t
	  (message "searching for %s" file)
	  (if (setq linkfile
		    (idlwave-recursive-find-file
		     content-path
		     (replace-regexp-in-string "\\.html\\'" ".htm" file)))
	      (progn
		(setq linkfile (file-relative-name linkfile content-path))
		;; Save it as an alias in case it is requested again
		(nconc alias-list (list (cons file linkfile))))
	    (prog1 nil (message "Could not locate %s" file))))))
    (if anchor
	(idlwave-substitute-link-target linkfile anchor)
      linkfile)))


(defun idlwave-convert-xml-add-link-path-information ()
  ;; Add path information missing from idl_catalog.xml since IDL 8
  (let* ((content-path (idlwave-html-help-location))
	 (help-path  (file-name-directory content-path))
	 (alias-file (expand-file-name "Data/Alias.xml" help-path)))
    
    (message "Linking help file info...")
    (if (file-exists-p alias-file)
	(let ((aliases (cdar (xml-parse-file alias-file))) elem alias-list)
	  ;; Parse the aliases
	  (while aliases
	    (setq elem (car aliases)
		  aliases (cdr aliases))
	    (when (and (listp elem) (eq (car elem) 'Map))
	      (setq elem (cadr elem))
	      (let* ((link (car (idlwave-split-link-target
				 (cdr (assoc 'Link elem)))))
		     (file (file-name-nondirectory link)))
		(cl-pushnew (cons file link) alias-list :test #'equal))))

	  ;; System class info
	  (mapc
	   (lambda (x)
	     (let ((linkfile
		    (idlwave-alias-path
		     (nth 1 (assq 'link x)) alias-list content-path)))
	       (when linkfile
	         (setcar (cdr (assq 'link x)) linkfile))))
	   idlwave-system-class-info)

	  ;; Main routines
	  (mapc
	   (lambda (x)
	     (let ((class (nth 2 x))
		   (kwd_blocks (nthcdr 5 x))
		   link linkfile class-entry)
	       (while kwd_blocks
		 (setq link (car kwd_blocks)
		       kwd_blocks (cdr kwd_blocks))
		 (when (and
			(car link)
			(string-match "\\.htm[^.]*\\'" (car link))
			(setq linkfile
			      (idlwave-alias-path
			       (car link) alias-list content-path
			       (if (and class
					(setq class-entry
					      (assoc class
						     idlwave-system-class-info)))
				   (file-name-directory
				    (nth 1 (assq 'link class-entry)))))))
		   (setcar link linkfile)))))
	   idlwave-system-routines)

	  ;; Executive commands/special topics
	  (mapc
	   (lambda (x)
	     (let ((alias (assoc-string (cdr x) alias-list 'ignore-case)))
	       (if alias
		   (setcdr x (cdr alias)))))
	   (append idlwave-help-special-topic-words
		   idlwave-executive-commands-alist))

	  ;; System variables
	  (mapc
	   (lambda (x)
	     (let* (linkfile
		    linkparts
		    (linkcell (assq 'link x))
		    (link (cadr linkcell)))
	       (if (setq linkparts (idlwave-split-link-target link))
		   (setq link (car linkparts)))
	       (if (setq linkfile
			 (idlwave-alias-path link alias-list content-path))
		   (setcdr linkcell (idlwave-substitute-link-target
				     linkfile (cdr linkparts))))))
	   idlwave-system-variables-alist)))
    (message "Linking help file info...done")))

(defun idlwave-convert-xml-clean-routine-aliases (aliases)
  ;; Duplicate and trim original routine aliases from rinfo list
  ;; This if for, e.g. OPENR/OPENW/OPENU
  (let (alias remove-list new parts all-parts)
    (cl-loop for x in aliases do
	     (when (setq parts (split-string (cdr x) "/"))
	       (setq new (assoc (cdr x) all-parts))
	       (unless new
	         (setq new (cons (cdr x) parts))
	         (push new all-parts))
	       (setcdr new (delete (car x) (cdr new)))))

    ;; Add any missing aliases (separate by slashes)
    (cl-loop for x in all-parts do
	     (if (cdr x)
	         (push (cons (nth 1 x) (car x)) aliases)))

    (cl-loop for x in aliases do
	     (when (setq alias (assoc (cdr x) idlwave-system-routines))
	       (unless (memq alias remove-list) (push alias remove-list))
	       (setq alias (copy-sequence alias))
	       (setcar alias (car x))
	       (push alias idlwave-system-routines)))
    (cl-loop for x in remove-list do
             (setq idlwave-system-routines (delq x idlwave-system-routines)))))

(defun idlwave-convert-xml-clean-sysvar-aliases (aliases)
  ;; Duplicate and trim original routine aliases from rinfo list
  ;; This if for, e.g. !X, !Y, !Z.
  (let (alias remove-list)
    (cl-loop for x in aliases do
	     (when (setq alias (assoc (cdr x) idlwave-system-variables-alist))
	       (unless (memq alias remove-list) (push alias remove-list))
	       (setq alias (copy-sequence alias))
	       (setcar alias (car x))
	       (push alias idlwave-system-variables-alist)))
    (cl-loop for x in remove-list do
	     (setq idlwave-system-variables-alist
	           (delq x idlwave-system-variables-alist)))))

(defun idlwave-xml-create-sysvar-alist (xml-entry)
  ;; Create a sysvar list entry from the xml parsed list.
  (let* ((nameblock (nth 1 xml-entry))
	 (name (cdr (assq 'name nameblock)))
	 (sysvar (substring name (progn (string-match "^ *!" name)
					(match-end 0))))
	 (link (cdr (assq 'link nameblock)))
	 (params (cddr xml-entry))
	 (case-fold-search t)
	 pelem ptype props tags)
    (while params
      (setq pelem (car params))
      (when (listp pelem)
	(setq ptype (car pelem)
	      props (car (cdr pelem)))
	(cond
	 ((eq ptype 'FIELD)
	  (push (cons (cdr (assq 'name props))
		      (cdr
		       (idlwave-split-link-target (cdr (assq 'link props)))))
		tags))))
      (setq params (cdr params)))
    (delq nil
	  (list sysvar (if tags (cons 'tags tags)) (list 'link link)))))


;;----------------------------------------------------
;; Buffer Scanning

(defvar idlwave-scanning-lib-dir)
(defvar idlwave-scanning-lib)
(defun idlwave-get-routine-info-from-buffers (buffers)
  "Call `idlwave-get-buffer-routine-info' on `idlwave-mode' buffers in BUFFERS."
  (let (buf routine-lists res)
    (save-excursion
      (while (setq buf (pop buffers))
	(set-buffer buf)
	(if (and (derived-mode-p 'idlwave-mode)
		 buffer-file-name)
	    ;; yes, this buffer has the right mode.
	    (progn (setq res (condition-case nil
				 (idlwave-get-buffer-routine-info)
			       (error nil)))
		   (push res routine-lists)))))
    ;; Concatenate the individual lists and return the result
    (apply #'nconc routine-lists)))

(defun idlwave-get-buffer-routine-info ()
  "Scan the current buffer for routine info.  Return (PRO-LIST FUNC-LIST)."
  (let* ((case-fold-search t)
	 routine-list string entry)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward
		"^[ \t]*\\(pro\\|function\\)[ \t]" nil t)
	  (setq string (buffer-substring-no-properties
			(match-beginning 0)
			(progn
			  (idlwave-end-of-statement)
			  (point))))
	  (setq entry (idlwave-parse-definition string))
	  (push entry routine-list))))
    routine-list))

(defun idlwave-parse-definition (string)
  "Parse a module definition."
  (let ((case-fold-search t)
	start name args type keywords class)
    ;; Remove comments
    (while (string-match ";.*" string)
      (setq string (replace-match "" t t string)))
    ;; Remove the continuation line stuff
    (while (string-match "\\([^a-zA-Z0-9$_]\\)\\$[ \t]*\n" string)
      (setq string (replace-match "\\1 " t nil string)))
    (while (string-match "\n" string)
      (setq string (replace-match " " t nil string)))
    ;; Match the name and type.
    (when (string-match
	   "\\<\\(pro\\|function\\)\\>\\s-+\\(\\([a-zA-Z0-9$_]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)" string)
      (setq start (match-end 0))
      (setq type (downcase (match-string 1 string)))
      (if (match-beginning 3)
	  (setq class (match-string 3 string)))
      (setq name (match-string 4 string)))
    ;; Match normal args and keyword args
    (while (string-match
	    ",\\s-*\\([a-zA-Z][a-zA-Z0-9$_]*\\|\\(_ref\\)?_extra\\)\\s-*\\(=\\)?"
	    string start)
      (setq start (match-end 0))
      (if (match-beginning 3)
	  (push (match-string 1 string) keywords)
	(push (match-string 1 string) args)))
    ;; Normalize and sort.
    (setq args (nreverse args))
    (setq keywords (sort keywords (lambda (a b)
				    (string< (downcase a) (downcase b)))))
    ;; Make and return the entry
    ;; We don't know which argument are optional, so this information
    ;; will not be contained in the calling sequence.
    (list name
	  (if (equal type "pro") 'pro 'fun)
	  class
	  (cond ((not (boundp 'idlwave-scanning-lib))
		 (list  'buffer (buffer-file-name)))
                ;; ((string= (downcase (file-name-base buffer-file-name))
                ;;           (downcase name))
                ;;  (list 'lib))
                ;; (t (cons 'lib (file-name-nondirectory (buffer-file-name))))
		(t (list 'user (file-name-nondirectory (buffer-file-name))
			 idlwave-scanning-lib-dir "UserLib")))
	  (concat
	   (if (string= type "function") "Result = " "")
	   (if class "Obj ->[%s::]" "")
	   "%s"
	   (if args
	       (concat
		(if (string= type "function") "(" ", ")
		(mapconcat #'identity args ", ")
		(if (string= type "function") ")" ""))))
	  (if keywords
	      (cons nil (mapcar #'list keywords)) ;No help file
	    nil))))

(defun idlwave-update-buffer-routine-info ()
  (let (res)
    (cond
     ((eq idlwave-scan-all-buffers-for-routine-info t)
      ;; Scan all buffers, current buffer last
      (message "Scanning all buffers...")
      (setq res (idlwave-get-routine-info-from-buffers
		 (reverse (buffer-list)))))
     ((null idlwave-scan-all-buffers-for-routine-info)
      ;; Don't scan any buffers
      (setq res nil))
     (t
      ;; Just scan this buffer
      (if (derived-mode-p 'idlwave-mode)
	  (progn
	    (message "Scanning current buffer...")
	    (setq res (idlwave-get-routine-info-from-buffers
		       (list (current-buffer))))))))
    ;; Put the result into the correct variable
    (setq idlwave-buffer-routines
	  (idlwave-sintern-rinfo-list res 'set))))

;; Three functions for the file load/save/kill hooks
(defun idlwave-save-buffer-update ()
  (idlwave-update-current-buffer-info 'save-buffer))
(defun idlwave-kill-buffer-update ()
  (idlwave-update-current-buffer-info 'kill-buffer))
(defun idlwave-new-buffer-update ()
  (idlwave-update-current-buffer-info 'find-file))

(defun idlwave-update-current-buffer-info (why)
  "Update `idlwave-routines' for current buffer.
Can run from `after-save-hook'."
  (when (and (derived-mode-p 'idlwave-mode)
	     (or (eq t idlwave-auto-routine-info-updates)
		 (memq why idlwave-auto-routine-info-updates))
	     idlwave-scan-all-buffers-for-routine-info
	     idlwave-routines)
    (condition-case nil
	(let (routines)
	  (idlwave-replace-buffer-routine-info
	   (buffer-file-name)
	   (if (eq why 'kill-buffer)
	       nil
	     (setq routines
		   (idlwave-sintern-rinfo-list
		    (idlwave-get-routine-info-from-buffers
		     (list (current-buffer)))
		    'set))))
	  (idlwave-concatenate-rinfo-lists 'quiet)
	  routines)
      (error nil))))

(defun idlwave-replace-buffer-routine-info (file new)
  "Cut the part from FILE out of `idlwave-buffer-routines' and add NEW."
  (let ((list idlwave-buffer-routines)
	found)
    (while list
      ;; The following test uses eq to make sure it works correctly
      ;; when two buffers visit the same file.  Then the file names
      ;; will be equal, but not eq.
      (if (eq (idlwave-routine-source-file (nth 3 (car list))) file)
	  (progn
	    (setcar list nil)
	    (setq found t))
	(if found
	    ;; End of that section reached. Jump.
	    (setq list nil)))
      (setq list (cdr list)))
    (setq idlwave-buffer-routines
	  (append new (delq nil idlwave-buffer-routines)))))

;;----------------------------------------------------
;; User Catalog

(defun idlwave-create-user-catalog-file (&optional arg)
  "Scan all files on selected dirs of IDL search path for routine information.

A widget checklist will allow you to choose the directories.  Write
the result as a file `idlwave-user-catalog-file'.  When this file
exists, it will be automatically loaded to give routine information
about library routines.  With ARG, just rescan the same directories
as last time - so no widget will pop up."
  (interactive "P")
  ;; Make sure the file is loaded if it exists.
  (if (and (stringp idlwave-user-catalog-file)
	   (file-regular-p idlwave-user-catalog-file))
      (condition-case nil
	  (load-file idlwave-user-catalog-file)
	(error nil)))
  ;; Make sure the file name makes sense
  (unless (and (stringp idlwave-user-catalog-file)
	       (> (length idlwave-user-catalog-file) 0)
	       (file-accessible-directory-p
		(file-name-directory idlwave-user-catalog-file))
	       (not (string= "" (file-name-nondirectory
				 idlwave-user-catalog-file))))
    (error "`idlwave-user-catalog-file' does not point to a file in an accessible directory"))

  (cond
   ;; Rescan the known directories
   ((and arg idlwave-path-alist
	 (consp (car idlwave-path-alist)))
    (idlwave-scan-user-lib-files idlwave-path-alist))

   ;; Expand the directories from library-path and run the widget
   (idlwave-library-path
    (idlwave-display-user-catalog-widget
     (if idlwave-true-path-alist
	 ;; Propagate any flags on the existing path-alist
	 (mapcar (lambda (x)
		   (let ((path-entry (assoc (file-truename x)
					    idlwave-true-path-alist)))
		     (if path-entry
			 (cons x (cdr path-entry))
		       (list x))))
		 (idlwave-expand-path idlwave-library-path))
       (mapcar #'list (idlwave-expand-path idlwave-library-path)))))

   ;; Ask the shell for the path and then run the widget
   (t
    (message "Asking the shell for IDL path...")
    (require 'idlw-shell)
    (idlwave-shell-send-command idlwave-shell-path-query
				(lambda () (idlwave-user-catalog-command-hook nil))
				'hide))))

(defun idlwave-user-catalog-command-hook (&optional arg)
  ;; Parse shell path information and select among it with a widget.
  ;; Command hook used by `idlwave-create-user-catalog-file'.
  (if arg
      ;; Scan immediately
      (idlwave-scan-user-lib-files idlwave-path-alist)
    ;; Set the path and display the widget
    (idlwave-shell-get-path-info 'no-write) ; set to something path-alist
    ;; mark all directories on path which already have a library catalog
    (idlwave-scan-library-catalogs "Locating library catalogs..." 'no-load)
    (idlwave-display-user-catalog-widget idlwave-path-alist)))

(defun idlwave-rescan-catalog-directories ()
  "Rescan the previously selected directories.  For batch processing."
  (idlwave-update-routine-info '(16)))

(defun idlwave-rescan-asynchronously ()
  "Dispatch another Emacs instance to update the idlwave catalog.
After the process finishes normally, the first access to routine info
will re-read the catalog."
  (interactive)
  (if (processp idlwave-catalog-process)
      (if (eq (process-status idlwave-catalog-process) 'run)
	  (if (yes-or-no-p "A catalog-updating process is running.  Kill it? ")
	      (progn
		(condition-case nil
		    (kill-process idlwave-catalog-process)
		  (error nil))
		(error "Process killed, no new process started"))
	    (error "Quit"))
	(condition-case nil
	    (kill-process idlwave-catalog-process)
	  (error nil))))
  (if (or (not idlwave-user-catalog-file)
	  (not (stringp idlwave-user-catalog-file))
	  (not (file-regular-p idlwave-user-catalog-file)))
      (error "No catalog has been produced yet"))
  (let* ((emacs (concat invocation-directory invocation-name))
	 (args (list "-batch"
		     "-l" (expand-file-name "~/.emacs")
		     "-l" "idlwave"
		     "-f" "idlwave-rescan-catalog-directories"))
	 (process (apply #'start-process "idlcat"
			 nil emacs args)))
    (setq idlwave-catalog-process process)
    (set-process-sentinel
     process
     (lambda (_pro why)
       (when (string-match "finished" why)
	 (setq idlwave-routines nil
	       idlwave-system-routines nil
	       idlwave-catalog-process nil)
	 (or (idlwave-start-load-rinfo-timer)
	     (idlwave-update-routine-info '(4))))))
    (message "Background job started to update catalog file")))

(defconst idlwave-user-catalog-widget-help-string
  "This is the front-end to the creation of the IDLWAVE user catalog.
Please select the directories on IDL's search path from which you
would like to extract routine information, to be stored in the file:

             %s

If this is not the correct file, first set variable
`idlwave-user-catalog-file', and call this command again.

N.B. Many libraries include pre-scanned catalog files
\(\".idlwave_catalog\").  These are marked with \"[LIB]\", and need
not be scanned.  You can scan your own libraries off-line using the
perl script `idlwave_catalog'.

After selecting the directories, choose [Scan & Save] to scan the library
directories and save the routine info.
\n")

(defvar idlwave-widget)
(defvar widget-keymap)
(defun idlwave-display-user-catalog-widget (dirs-list)
  "Create the widget to select IDL search path directories for scanning."
  (interactive)
  (require 'widget)
  (require 'wid-edit)
  (unless dirs-list
    (error "Don't know IDL's search path"))

  (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
  (switch-to-buffer (get-buffer-create "*IDLWAVE Widget*"))
  (kill-all-local-variables)
  (make-local-variable 'idlwave-widget)
  (widget-insert (format idlwave-user-catalog-widget-help-string
			 idlwave-user-catalog-file))

  (widget-create 'push-button
		 :notify #'idlwave-widget-scan-user-lib-files
		 "Scan & Save")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify #'idlwave-delete-user-catalog-file
		 "Delete File")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify
		 (lambda (&rest _ignore)
                   (let ((path-list (widget-get idlwave-widget :path-dirs)))
                     (dolist (x path-list)
                       (unless (memq 'lib (cdr x))
                         (idlwave-path-alist-add-flag x 'user)))
                     (idlwave-display-user-catalog-widget path-list)))
		 "Select All Non-Lib")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify
		 (lambda (&rest _ignore)
                   (let ((path-list (widget-get idlwave-widget :path-dirs)))
                     (dolist (x path-list)
                       (idlwave-path-alist-remove-flag x 'user))
                     (idlwave-display-user-catalog-widget path-list)))
		 "Deselect All")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify (lambda (&rest _ignore)
			   (kill-buffer (current-buffer)))
		 "Quit")
  (widget-insert "\n\n")

  (widget-insert "Select Directories: \n")

  (setq idlwave-widget
	(apply #'widget-create
	       'checklist
	       :value  (delq nil (mapcar (lambda (x)
					   (if (memq 'user (cdr x))
					       (car x)))
					 dirs-list))
	       :greedy t
	       :tag "List of directories"
	       (mapcar (lambda (x)
			 (list 'item
			       (if (memq 'lib (cdr x))
				   (concat "[LIB] " (car x) )
				 (car x))))
		       dirs-list)))
  (widget-put idlwave-widget :path-dirs dirs-list)
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (delete-other-windows))

(defun idlwave-delete-user-catalog-file (&rest _ignore)
  (if (yes-or-no-p
       (format "Delete file %s?" idlwave-user-catalog-file))
      (progn
	(delete-file idlwave-user-catalog-file)
	(message "%s has been deleted" idlwave-user-catalog-file))))

(defun idlwave-widget-scan-user-lib-files (&rest _ignore)
  ;; Call `idlwave-scan-user-lib-files' with data taken from the widget.
  (let* ((widget idlwave-widget)
	 (selected-dirs (widget-value widget))
	 (path-alist (widget-get widget :path-dirs))
	 (this-path-alist path-alist)
	 dir-entry)
    (while (setq dir-entry (pop this-path-alist))
      (if (member
	   (if (memq 'lib (cdr dir-entry))
	       (concat "[LIB] " (car dir-entry))
	     (car dir-entry))
	   selected-dirs)
	  (idlwave-path-alist-add-flag dir-entry 'user)
	(idlwave-path-alist-remove-flag dir-entry 'user)))
    (idlwave-scan-user-lib-files path-alist)))

(defun idlwave-scan-user-lib-files (path-alist)
  ;; Scan the PRO files in PATH-ALIST and store the info in the user catalog
  (let* ((idlwave-scanning-lib t)
	 (idlwave-scanning-lib-dir "")
	 (idlwave-completion-case nil)
	 dirs-alist dir files file)
    (setq idlwave-user-catalog-routines nil
	  idlwave-path-alist path-alist ; for library-path instead
	  idlwave-true-path-alist nil)
    (if idlwave-auto-write-paths (idlwave-write-paths))
    (with-current-buffer (get-buffer-create "*idlwave-scan.pro*")
      (idlwave-mode)
      (setq dirs-alist (reverse path-alist))
      (while (setq dir (pop dirs-alist))
	(when (memq 'user (cdr dir))	; Has it marked for scan?
	  (setq dir (car dir))
	  (setq idlwave-scanning-lib-dir dir)
	  (when (file-directory-p dir)
	    (setq files (directory-files dir 'full "\\.[pP][rR][oO]\\'"))
	    (while (setq file (pop files))
	      (when (file-regular-p file)
		(if (not (file-readable-p file))
		    (message "Skipping %s (no read permission)" file)
		  (message "Scanning %s..." file)
		  (erase-buffer)
		  (insert-file-contents file 'visit)
		  (setq idlwave-user-catalog-routines
			(append (idlwave-get-routine-info-from-buffers
				 (list (current-buffer)))
				idlwave-user-catalog-routines)))))))))
    (message "Creating user catalog file...")
    (kill-buffer "*idlwave-scan.pro*")
    (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
    (with-temp-buffer
      (insert ";; IDLWAVE user catalog file\n")
      (insert (format ";; Created %s\n\n" (current-time-string)))

      ;; Define the routine info list
      (insert "\n(setq idlwave-user-catalog-routines\n    '(")
      (let ((standard-output (current-buffer)))
	(mapc (lambda (x)
	        (insert "\n    ")
	        (prin1 x)
	        (goto-char (point-max)))
	      idlwave-user-catalog-routines))
      (insert (format "))\n\n;;; %s ends here\n"
		      (file-name-nondirectory idlwave-user-catalog-file)))
      (write-region nil nil idlwave-user-catalog-file)))
  (message "Creating user catalog file...done")
  (message "Info for %d routines saved in %s"
	   (length idlwave-user-catalog-routines)
	   idlwave-user-catalog-file)
  (sit-for 2)
  (idlwave-update-routine-info t))

;;----------------------------------------------------
;; Library catalogs

(defun idlwave-scan-library-catalogs (&optional message-base no-load)
  "Scan for library catalog files (.idlwave_catalog) and ingest.

All directories on `idlwave-path-alist' (or `idlwave-library-path'
instead, if present) are searched.  Print MESSAGE-BASE along with the
libraries being loaded, if passed, and skip loading/normalizing if
NO-LOAD is non-nil.  The variable `idlwave-use-library-catalogs' can
be set to nil to disable library catalog scanning."
  (when idlwave-use-library-catalogs
    (let ((dirs
	   (if idlwave-library-path
	       (idlwave-expand-path idlwave-library-path)
	     (mapcar #'car idlwave-path-alist)))
	  (old-libname "")
	  dir-entry dir catalog all-routines)
      (if message-base (message "%s" message-base))
      (while (setq dir (pop dirs))
	(catch 'continue
	  (when (file-readable-p
		 (setq catalog (expand-file-name ".idlwave_catalog" dir)))
	    (unless no-load
	      (setq idlwave-library-catalog-routines nil)
	      ;; Load the catalog file
	      (condition-case nil
		  (load catalog t t t)
		(error (throw 'continue t)))
	      (when (and
		     message-base
		     (not (string= idlwave-library-catalog-libname
				   old-libname)))
		(message "%s%s" message-base idlwave-library-catalog-libname)
		(setq old-libname idlwave-library-catalog-libname))
	      (when idlwave-library-catalog-routines
		(setq all-routines
		      (append
		       (idlwave-sintern-rinfo-list
			idlwave-library-catalog-routines 'sys dir)
		       all-routines))))

	    ;;  Add a 'lib flag if on path-alist
	    (when (and idlwave-path-alist
		       (setq dir-entry (assoc dir idlwave-path-alist)))
	      (idlwave-path-alist-add-flag dir-entry 'lib)))))
      (unless no-load (setq idlwave-library-catalog-routines all-routines))
      (if message-base (message "%sdone" message-base)))))

;;----------------------------------------------------
;; Path Info

(defvar idlwave-path-alist)
(defun idlwave-read-paths ()
  (if (and (stringp idlwave-path-file)
	   (file-regular-p idlwave-path-file))
      (condition-case nil
	  (load idlwave-path-file t t t)
	(error nil))))

(defun idlwave-write-paths ()
  (interactive)
  (when (and idlwave-path-alist idlwave-system-directory)
    (with-temp-buffer
      (insert ";; IDLWAVE paths\n")
      (insert (format ";; Created %s\n\n" (current-time-string)))
      ;; Define the variable which knows the value of "!DIR"
      (insert (format "\n(setq idlwave-system-directory \"%s\")\n"
		      idlwave-system-directory))

      ;; Define the variable which contains a list of all scanned directories
      (insert "\n(setq idlwave-path-alist\n    '(")
      (let ((standard-output (current-buffer)))
	(mapc (lambda (x)
	        (insert "\n      ")
	        (prin1 x)
	        (goto-char (point-max)))
	      idlwave-path-alist))
      (insert "))\n")
      (write-region nil nil idlwave-path-file))))

(defun idlwave-path-alist-add-flag (list-entry flag)
  "Add a flag to the path list entry, if not set."
  (cl-pushnew flag (cdr list-entry) :test #'equal))

(defun idlwave-path-alist-remove-flag (list-entry flag)
  "Remove a flag to the path list entry, if set."
  (let ((flags (delq flag (cdr list-entry))))
    (setcdr list-entry flags)))

(defun idlwave-true-path-alist ()
  "Return `idlwave-path-alist' alist with true-names.
Info is cached, but relies on the functions setting `idlwave-path-alist'
to reset the variable `idlwave-true-path-alist' to nil."
  (or idlwave-true-path-alist
      (setq idlwave-true-path-alist
	    (mapcar (lambda(x) (cons
				(file-name-as-directory
				 (file-truename
				  (directory-file-name
				   (car x))))
				(cdr x)))
		    idlwave-path-alist))))

(defun idlwave-syslib-scanned-p ()
  "Non-nil if the system lib file !DIR/lib has been scanned."
  (let* ((true-syslib (file-name-as-directory
		       (file-truename
			(expand-file-name "lib" (idlwave-sys-dir))))))
    (cdr (assoc true-syslib (idlwave-true-path-alist)))))

;;----------------------------------------------------
;; Shell compiled routine/sysvar info

(defconst idlwave-routine-info.pro
  "
;; START OF IDLWAVE SUPPORT ROUTINES
pro idlwave_print_safe,item,limit
  catch,err
  if err ne 0 then begin
     print,'Could not print item.'
     return
  endif
  if n_elements(item) gt limit then $
     print,item[0:limit-1],'<... truncated at ',strtrim(limit,2),' elements>' $
  else print,item
end

pro idlwave_print_info_entry,name,func=func,separator=sep
  ;; See if it's an object method
  if name eq '' then return
  func    = keyword_set(func)
  methsep = strpos(name,'::')
  meth    = methsep ne -1

  ;; Get routine info
  pars   = routine_info(name,/parameters,functions=func)
  source = routine_info(name,/source,functions=func)
  nargs  = pars.num_args
  nkw    = pars.num_kw_args
  if nargs gt 0 then args = pars.args
  if nkw   gt 0 then kwargs = pars.kw_args

  ;; Trim the class, and make the name
  if meth then begin
      class = strmid(name,0,methsep)
      name  = strmid(name,methsep+2,strlen(name)-1)
      if nargs gt 0 then begin
          ;; remove the self argument
          wh = where(args ne 'SELF',nargs)
          if nargs gt 0 then args = args[wh]
      endif
  endif else begin
      ;; No class, just a normal routine.
      class = \"\"
  endelse

  ;; Calling sequence
  cs = \"\"
  if func then cs = 'Result = '
  if meth then cs = cs + 'Obj -> [' + '%s' + '::]'
  cs = cs + '%s'
  if func then cs = cs + '(' else if nargs gt 0 then cs = cs + ', '
  if nargs gt 0 then begin
      for j=0,nargs-1 do begin
          cs = cs + args[j]
          if j lt nargs-1 then cs = cs + ', '
      endfor
  end
  if func then cs = cs + ')'
  ;; Keyword arguments
  kwstring = ''
  if nkw gt 0 then begin
      for j=0,nkw-1 do begin
          kwstring = kwstring + ' ' + kwargs[j]
      endfor
  endif

  ret=(['IDLWAVE-PRO','IDLWAVE-FUN'])[func]

  print,ret + ': ' + name + sep + class + sep + source[0].path  $
    + sep + cs + sep + kwstring
end

pro idlwave_routine_info,file
  on_error,1
  sep = '<@>'
  print,'>>>BEGIN OF IDLWAVE ROUTINE INFO (\"' + sep + '\" IS THE SEPARATOR)'
  all = routine_info()
  fileQ=n_elements(file) ne 0
  if fileQ then file=strtrim(file,2)
  for i=0L,n_elements(all)-1L do begin
     if fileQ then begin
        if (routine_info(all[i],/SOURCE)).path eq file then $
           idlwave_print_info_entry,all[i],separator=sep
     endif else idlwave_print_info_entry,all[i],separator=sep
  endfor
  all = routine_info(/functions)
  for i=0L,n_elements(all)-1L do begin
     if fileQ then begin
        if (routine_info(all[i],/FUNCTIONS,/SOURCE)).path eq file then $
           idlwave_print_info_entry,all[i],separator=sep,/FUNC
     endif else idlwave_print_info_entry,all[i],separator=sep,/FUNC
  endfor
  print,'>>>END OF IDLWAVE ROUTINE INFO'
end

pro idlwave_get_sysvars
  on_error,1
  catch,error_status
  if error_status ne 0 then begin
      print, 'Cannot get info about system variables'
  endif else begin
      help,/brief,output=s,/system_variables  ; ? unsafe use of OUTPUT=
      s = strtrim(strjoin(s,' ',/single),2)   ; make one line
      v = strsplit(s,' +',/regex,/extract)    ; get variables
      for i=0L,n_elements(v)-1 do begin
          t = ['']                            ; get tag list
          a=execute('if n_tags('+v[i]+') gt 0 then t=tag_names('+v[i]+')')
          print, 'IDLWAVE-SYSVAR: '+v[i]+' '+strjoin(t,' ',/single)
      endfor
  endelse
end

pro idlwave_get_class_tags, class
  res = execute('tags=tag_names({'+class+'})')
  if res then print,'IDLWAVE-CLASS-TAGS: '+class+' '+strjoin(tags,' ',/single)
end
;; END OF IDLWAVE SUPPORT ROUTINES
"
  "The IDL programs to get info from the shell.")

(defvar idlwave-idlwave_routine_info-compiled nil
  "Remember if the routine info procedure is already compiled.")

(defvar idlwave-shell-temp-pro-file)
(defvar idlwave-shell-temp-rinfo-save-file)

(defun idlwave-shell-compile-helper-routines (&optional wait)
  (unless (and idlwave-idlwave_routine_info-compiled
	       (file-readable-p (idlwave-shell-temp-file 'rinfo)))
    (with-current-buffer (idlwave-find-file-noselect
                          (idlwave-shell-temp-file 'pro))
      (erase-buffer)
      (insert idlwave-routine-info.pro)
      (save-buffer 0))
    (idlwave-shell-send-command
     (concat ".run \"" idlwave-shell-temp-pro-file "\"")
     nil 'hide wait)
    (idlwave-shell-send-command
     (format "save,'idlwave_print_safe','idlwave_routine_info','idlwave_print_info_entry','idlwave_get_class_tags','idlwave_get_sysvars',FILE='%s',/ROUTINES"
	     (idlwave-shell-temp-file 'rinfo))
     nil 'hide wait)
    (setq idlwave-idlwave_routine_info-compiled t))

  ;; Restore if necessary.  Must use execute to hide lame routine_info
  ;; errors on undefined routine
  (idlwave-shell-send-command
   (format "if execute(\"_v=routine_info('idlwave_routine_info',/SOURCE)\") eq 0 then restore,'%s' else if _v.path eq '' then restore,'%s'"
	   idlwave-shell-temp-rinfo-save-file
	   idlwave-shell-temp-rinfo-save-file)
   nil 'hide))

(defun idlwave-shell-update-routine-info (&optional quiet run-hooks wait file)
  "Query the shell for routine_info of compiled modules and update the lists."
  ;; Save and compile the procedure.  The compiled procedure is then
  ;; saved into an IDL SAVE file, to allow for fast RESTORE.  We may
  ;; need to test for and possibly RESTORE the procedure each time we
  ;; use it, since the user may have killed or redefined it.  In
  ;; particular, .RESET_SESSION will kill all user procedures.  If
  ;; FILE is set, only update routine info for routines in that file.

  (idlwave-shell-compile-helper-routines wait)
					; execute the routine_info procedure, and analyze the output
  (idlwave-shell-send-command
   (format "idlwave_routine_info%s" (if file (concat ",'" file "'") ""))
   (lambda ()
     (idlwave-shell-routine-info-filter)
     (idlwave-concatenate-rinfo-lists quiet run-hooks))
   'hide wait))

(defun idlwave-sysvars-reset ()
  (if (and (fboundp 'idlwave-shell-is-running)
	   (idlwave-shell-is-running)
	   idlwave-idlwave_routine_info-compiled)
      (idlwave-shell-send-command "idlwave_get_sysvars"
				  #'idlwave-process-sysvars 'hide)))

(defun idlwave-process-sysvars ()
  (idlwave-shell-filter-sysvars)
  (setq idlwave-sint-sysvars nil
	idlwave-sint-sysvartags nil)
  (idlwave-sintern-sysvar-alist))

(defvar idlwave-shell-command-output)
(defun idlwave-shell-filter-sysvars ()
  "Get any new system variables and tags."
  (let ((text idlwave-shell-command-output)
	(start 0)
	(old idlwave-system-variables-alist)
	var tags link old-entry) ;; type name class
    (setq idlwave-system-variables-alist nil)
    (while (string-match "^IDLWAVE-SYSVAR: !\\([a-zA-Z0-9_$]+\\)\\( \\(.*\\)\\)?"
			 text start)
      (setq start (match-end 0)
	    var (match-string 1 text)
	    tags (if (match-end 3)
		     (idlwave-split-string (match-string 3 text))))
      ;; Maintain old links, if present
      (setq old-entry (assq (idlwave-sintern-sysvar var) old))
      (setq link (assq 'link old-entry))
      (setq idlwave-system-variables-alist
	    (cons (list var
			(cons
			 'tags
			 (mapcar (lambda (x)
				   (cons x
					 (cdr (assq
					       (idlwave-sintern-sysvartag x)
					       (cdr (assq 'tags old-entry))))))
				 tags))
			link)
		  idlwave-system-variables-alist)))
    ;; Keep the old value if query was not successful
    (setq idlwave-system-variables-alist
	  (or idlwave-system-variables-alist old))))


;;----------------------------------------------------
;; Links in scanned routine info

(defun idlwave-split-link-target (link)
  "Split a given LINK into link file and anchor."
  (if (and (stringp link) (string-match idlwave-html-link-sep link))
      (cons (substring link 0 (match-beginning 0))
	    (substring link (match-end 0)))
    (list link)))

(defun idlwave-substitute-link-target (link target)
  "Substitute the TARGET anchor for the given LINK."
  (let (main-base)
    (setq main-base (if (string-match idlwave-html-link-sep link)
			(substring link 0 (match-beginning 0))
		      link))
    (if target
	(if (string-match idlwave-html-link-sep target)
	    (concat main-base target)
	  (concat main-base idlwave-html-link-sep target))
      link)))


(provide 'idlw-scan)
(provide 'idlwave-scan)
