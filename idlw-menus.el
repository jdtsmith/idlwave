;;; idlw-menus.el --- IDLWAVE menus -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2024  Free Software Foundation, Inc.

;;; Code:
;; Define the mode-maps before we modify them.
(require 'idlw-variables)
(require 'idlw-bindings)

;; Define - using easymenu.el
(defvar idlwave-mode-menu)
(defvar idlwave-mode-debug-menu)

(define-obsolete-function-alias 'idlwave-function-menu #'imenu "29.1")
(defvar idlwave-mode-menu-def
  `("IDLWAVE"
    ["PRO/FUNC menu" imenu t]
    ("Motion"
     ["Subprogram Start" idlwave-beginning-of-subprogram t]
     ["Subprogram End" idlwave-end-of-subprogram t]
     ["Block Start" idlwave-beginning-of-block t]
     ["Block End" idlwave-end-of-block t]
     ["Up Block" idlwave-backward-up-block t]
     ["Down Block" idlwave-down-block t]
     ["Skip Block Backward" idlwave-backward-block t]
     ["Skip Block Forward" idlwave-forward-block t])
    ("Mark"
     ["Subprogram" idlwave-mark-subprogram t]
     ["Block" idlwave-mark-block t]
     ["Header" idlwave-mark-doclib t])
    ("Format"
     ["Indent Entire Statement" idlwave-indent-statement
      :active t :keys "C-u \\[indent-for-tab-command]" ]
     ["Indent Subprogram" idlwave-indent-subprogram t]
     ["(Un)Comment Region" idlwave-toggle-comment-region t]
     ["Continue/Split line" idlwave-split-line t]
     "--"
     ["Toggle Auto Fill" auto-fill-mode :style toggle
      :selected auto-fill-function])
    ("Templates"
     ["Procedure" idlwave-procedure t]
     ["Function" idlwave-function t]
     ["Doc Header" idlwave-doc-header t]
     ["Log" idlwave-doc-modification t]
     "--"
     ["Case" idlwave-case t]
     ["For" idlwave-for t]
     ["Repeat" idlwave-repeat t]
     ["While" idlwave-while t]
     "--"
     ["Close Block" idlwave-close-block t])
    ("Completion"
     ["Complete" idlwave-complete t]
     ("Complete Specific"
      ["1 Procedure Name" (idlwave-complete 'procedure) t]
      ["2 Procedure Keyword" (idlwave-complete 'procedure-keyword) t]
      "--"
      ["3 Function Name" (idlwave-complete 'function) t]
      ["4 Function Keyword" (idlwave-complete 'function-keyword) t]
      "--"
      ["5 Procedure Method Name" (idlwave-complete 'procedure-method) t]
      ["6 Procedure Method Keyword" (idlwave-complete 'procedure-method-keyword) t]
      "--"
      ["7 Function Method Name" (idlwave-complete 'function-method) t]
      ["8 Function Method Keyword" (idlwave-complete 'function-method-keyword) t]
      "--"
      ["9 Class Name"  idlwave-complete-class t]))
    ("Routine Info"
     ["Show Routine Info" idlwave-routine-info t]
     ["Online Context Help" idlwave-context-help t]
     "--"
     ["Find Routine Source" idlwave-find-module t]
     ["Resolve Routine" idlwave-resolve (featurep 'idlw-shell)]
     "--"
     ["Update Routine Info" idlwave-update-routine-info t]
     ["Rescan XML Help Catalog" idlwave-rescan-xml-routine-info t]
     "--"
     "IDL User Catalog"
     ["Select Catalog Directories" (idlwave-create-user-catalog-file nil) t]
     ["Scan Directories" (idlwave-update-routine-info '(16))
      (and idlwave-path-alist (not idlwave-catalog-process))]
     ["Scan Directories &" (idlwave-update-routine-info '(64))
      (and idlwave-path-alist (not idlwave-catalog-process))]
     "--"
     "Routine Shadows"
     ["Check Current Buffer" idlwave-list-buffer-load-path-shadows t]
     ["Check Compiled Routines" idlwave-list-shell-load-path-shadows t]
     ["Check Everything" idlwave-list-all-load-path-shadows t])
    ("Misc"
     ["Kill auto-created buffers" idlwave-kill-autoloaded-buffers t]
     "--"
     ["Insert TAB character" idlwave-hard-tab t])
    "--"
    ("External"
     ["Start IDL shell" idlwave-shell t]
     ["Edit file in IDLDE" idlwave-edit-in-idlde t])
    "--"
    ("Customize"
     ["Browse IDLWAVE Group" idlwave-customize t]
     "--"
     ["Build Full Customize Menu" idlwave-create-customize-menu t])
    ("Documentation"
     ["Describe Mode" describe-mode t]
     ["Abbreviation List" idlwave-list-abbrevs t]
     "--"
     ["Commentary in idlwave.el" idlwave-show-commentary t]
     ["Commentary in idlw-shell.el" idlwave-shell-show-commentary t]
     "--"
     ["Info" idlwave-info t]
     "--"
     ["Help with Topic" idlwave-help-with-topic t]
     ["Launch IDL Help" idlwave-launch-idlhelp t])))

(defvar idlwave-mode-debug-menu-def
  '("Debug"
    ["Start IDL shell" idlwave-shell t]
    ["Save and .RUN buffer" idlwave-shell-save-and-run
     (and (boundp 'idlwave-shell-automatic-start)
	  idlwave-shell-automatic-start)]))

(easy-menu-define idlwave-mode-menu idlwave-mode-map
  "IDL and WAVE CL editing menu"
  idlwave-mode-menu-def)
(easy-menu-define idlwave-mode-debug-menu idlwave-mode-map
 "IDL and WAVE CL editing menu"
 idlwave-mode-debug-menu-def)


;;----------------------------------------------------
;; IDLWAVE menu and support functions
(defun idlwave-customize ()
  "Call the customize function with `idlwave' as argument."
  (interactive)
  ;; Try to load the code for the shell, so that we can customize it
  ;; as well.
  (or (featurep 'idlw-shell)
      (load "idlw-shell" t))
  (customize-browse 'idlwave))

(defun idlwave-create-customize-menu ()
  "Create a full customization menu for IDLWAVE, insert it into the menu."
  (interactive)
  ;; Try to load the code for the shell, so that we can customize it
  ;; as well.
  (require 'idlw-shell)
  (easy-menu-change
   '("IDLWAVE") "Customize"
   `(["Browse IDLWAVE group" idlwave-customize t]
     "--"
     ,(customize-menu-create 'idlwave)
     ["Set" Custom-set t]
     ["Save" Custom-save t]
     ["Reset to Current" Custom-reset-current t]
     ["Reset to Saved" Custom-reset-saved t]
     ["Reset to Standard Settings" Custom-reset-standard t]))
  (message "\"IDLWAVE\"-menu now contains full customization menu"))

(defun idlwave-show-commentary ()
  "Use the finder to view the file documentation from `idlwave.el'."
  (interactive)
  (finder-commentary "idlwave.el"))

(defun idlwave-shell-show-commentary ()
  "Use the finder to view the file documentation from `idlw-shell.el'."
  (interactive)
  (finder-commentary "idlw-shell.el"))

(defun idlwave-info ()
  "Read documentation for IDLWAVE in the info system."
  (interactive)
  (info "idlwave"))

(defun idlwave-list-abbrevs (arg)
  "Show the code abbreviations define in IDLWAVE mode.
This lists all abbrevs where the replacement text differs from the input text.
These are the ones the users want to learn to speed up their writing.

The function does *not* list abbrevs which replace a word with itself
to call a hook.  These hooks are used to change the case of words or
to blink the matching `begin', and the user does not need to know them.

With arg, list all abbrevs with the corresponding hook.

This function was written since `list-abbrevs' looks terrible for IDLWAVE mode."

  (interactive "P")
  (let ((table idlwave-mode-abbrev-table)
	abbrevs
	str rpl func fmt (len-str 0) (len-rpl 0))
    (mapatoms
     (lambda (sym)
       (if (symbol-value sym)
	   (progn
	     (setq str (symbol-name sym)
		   rpl (symbol-value sym)
		   func (symbol-function sym))
	     (if arg
		 (setq func (prin1-to-string func))
	       (if (and (listp func) (stringp (nth 2 func)))
		   (setq rpl (concat "EVAL: " (nth 2 func))
			 func "")
		 (setq func "")))
	     (if (or arg (not (string= rpl str)))
		 (progn
		   (setq len-str (max len-str (length str)))
		   (setq len-rpl (max len-rpl (length rpl)))
		   (setq abbrevs (cons (list str rpl func) abbrevs)))))))
     table)
    ;; sort the list
    (setq abbrevs (sort abbrevs (lambda (a b) (string< (car a) (car b)))))
    ;; Make the format
    (setq fmt (format "%%-%ds   %%-%ds   %%s\n" len-str len-rpl))
    (with-output-to-temp-buffer "*Help*"
      (if arg
	  (progn
	    (princ "Abbreviations and Actions in IDLWAVE-Mode\n")
	    (princ "=========================================\n\n")
	    (princ (format fmt "KEY" "REPLACE" "HOOK"))
	    (princ (format fmt "---" "-------" "----")))
	(princ "Code Abbreviations and Templates in IDLWAVE-Mode\n")
	(princ "================================================\n\n")
	(princ (format fmt "KEY" "ACTION" ""))
	(princ (format fmt "---" "------" "")))
      (dolist (list abbrevs)
	(setq str (car list)
	      rpl (nth 1 list)
	      func (nth 2 list))
	(princ (format fmt str rpl func)))))
  ;; Make sure each abbreviation uses only one display line
  (with-current-buffer "*Help*"
    (setq truncate-lines t)))

;;----------------------------------------------------
;; IDLWAVE help menus
;; Define the key bindings for the Help application


;; Define the menu for the Help application

(easy-menu-define idlwave-help-menu idlwave-help-mode-map
  "Menu for Help IDLWAVE system."
  '("IDLHelp"
    ["Definition <-> Help Text" idlwave-help-toggle-header-match-and-def t]
    ["Find DocLib Header" idlwave-help-find-header t]
    ["Find First DocLib Header" idlwave-help-find-first-header t]
    ["Fontify help buffer" idlwave-help-fontify t]
    "--"
    ["Quit" idlwave-help-quit t]))


(provide 'idlw-menus)
(provide 'idlwave-menus)
