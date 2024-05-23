;;; idlwave.el --- IDL editing mode for GNU Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 1999-2024  Free Software Foundation, Inc.

;; Authors: J.D. Smith <jdtsmith@gmail.com>
;;          Carsten Dominik
;;          Chris Chase
;; Maintainer: J.D. Smith <jdtsmith@gmail.com>
;; Version: 6.5.1
;; Keywords: languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; IDLWAVE enables feature-rich development and interaction with IDL,
;; the Interactive Data Language.  It provides a compelling,
;; full-featured alternative to the IDLDE development environment
;; bundled with IDL.

;; See the mode description ("C-h m" in idlwave-mode or "C-h f
;; idlwave-mode") for features, key bindings, and info.
;;
;; INSTALLATION
;; ============
;;
;; Follow the instructions in the INSTALL file of the distribution.
;; In short, put this file on your load path and add the following
;; lines to your init file:
;;
;; (autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
;; (autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
;; (setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist))
;;
;;
;; SOURCE
;; ======
;;
;; The newest version of IDLWAVE is available from the maintainer's
;; Webpage:
;;
;;   https://github.com/jdtsmith/idlwave
;;
;; DOCUMENTATION
;; =============
;;
;; Info format documentation is available, also accessible with `M-x
;; idlwave-info'.  IDLWAVE is documented online in info format.  A
;; printable version of the documentation is available from the
;; maintainers webpage (see SOURCE).
;;
;;
;; ACKNOWLEDGMENTS
;; ===============
;;
;; In the remotely distant past, based on pascal.el.
;;
;; Incorporates many ideas, such as abbrevs, action routines, and
;; continuation line indenting, from wave.el, originally written by
;; Lubos Pochman, Precision Visuals, Boulder.
;;
;; Thanks to the following people for their contributions and
;; comments:
;;
;;    Ulrik Dickow <dickow_at_nbi.dk>
;;    Eric E. Dors <edors_at_lanl.gov>
;;    Stein Vidar H. Haugan <s.v.h.haugan_at_astro.uio.no>
;;    David Huenemoerder <dph_at_space.mit.edu>
;;    Kevin Ivory <Kevin.Ivory_at_linmpi.mpg.de>
;;    Dick Jackson <dick_at_d-jackson.com>
;;    Xuyong Liu <liu_at_stsci.edu>
;;    Simon Marshall <Simon.Marshall_at_esrin.esa.it>
;;    Laurent Mugnier <mugnier_at_onera.fr>
;;    Lubos Pochman <lubos_at_rsinc.com>
;;    Bob Portmann <portmann_at_al.noaa.gov>
;;    Patrick M. Ryan <pat_at_jaameri.gsfc.nasa.gov>
;;    Marty Ryba <ryba_at_ll.mit.edu>
;;    Matthew Savoie <savoie_at_nsidc.org>
;;    Paul Sorenson <aardvark62_at_msn.com>
;;    Phil Sterne <sterne_at_dublin.llnl.gov>
;;    Phil Williams <williams_at_irc.chmcc.org>
;;    Nathaniel Cunningham
;;
;; CUSTOMIZATION:
;; =============
;;
;; IDLWAVE has extensive customize support; to learn about the
;; variables which control the mode's behavior, use `M-x
;; idlwave-customize'.
;;
;; You can set your own preferred values with Customize, or with Lisp
;; code in your init file.  For an example of what to put into your
;; init file, check the TexInfo documentation.
;;
;; KNOWN PROBLEMS:
;; ==============
;;
;;   IDLWAVE support for the IDL-derived PV-WAVE CL language of Visual
;;   Numerics, Inc. is growing less and less complete as the two
;;   languages grow increasingly apart.  The mode probably shouldn't
;;   even have "WAVE" in its title, but it's catchy, and was required
;;   to avoid conflict with the CORBA idl.el mode.  Caveat WAVEor.
;;
;;   Moving the point backwards in conjunction with abbrev expansion
;;   does not work as I would like it, but this is a problem with
;;   Emacs abbrev expansion done by the self-insert-command.  It ends
;;   up inserting the character that expanded the abbrev after moving
;;   point backward, e.g., "\cl" expanded with a space becomes
;;   "LONG( )" with point before the close paren.  This is solved by
;;   using a temporary function in `post-command-hook' - not pretty,
;;   but it works.
;;
;;   Tabs and spaces are treated equally as whitespace when filling a
;;   comment paragraph.  To accomplish this, tabs are permanently
;;   replaced by spaces in the text surrounding the paragraph, which
;;   may be an undesirable side-effect.  Replacing tabs with spaces is
;;   limited to comments only and occurs only when a comment
;;   paragraph is filled via `idlwave-fill-paragraph'.
;;
;;   Determining the expression at point for printing and other
;;   examination commands is somewhat rough: currently only fairly
;;   simple entities are found.  You can always drag-select or examine
;;   a pre-selected region.
;;
;;   When forcing completion of method keywords, the initial
;;   query for a method has multiple entries for some methods.  Would
;;   be too difficult to fix this hardly used case.
;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'idlw-help)
(require 'idlw-variables)
(require 'idlw-routine)
(require 'idlw-complete)
(require 'idlw-bindings)
(require 'idlw-menus)
(require 'idlw-scan)


(declare-function idlwave-shell-get-path-info "idlw-shell")
(declare-function idlwave-shell-temp-file "idlw-shell")
(declare-function idlwave-shell-is-running "idlw-shell")
(declare-function widget-value "wid-edit" (widget))
(declare-function comint-dynamic-complete-filename "comint" ())

;;----------------------------------------------------
;; Loading/Initialization

(defvar imenu-create-index-function)
(defvar imenu-extract-index-name-function)
(defvar imenu-prev-index-position-function)

(defconst idlwave-mode-version
  (if (fboundp 'package-get-version)
      (package-get-version)
    "6.5.0"))

;;;###autoload
(define-derived-mode idlwave-mode prog-mode "IDLWAVE"
  "Major mode for editing IDL source files.

The main features of this mode are

1. Indentation and Formatting
   --------------------------
   Like other Emacs programming modes, C-j inserts a newline and indents.
   TAB is used for explicit indentation of the current line.

   To start a continuation line, use \\[idlwave-split-line].  This
   function can also be used in the middle of a line to split the line
   at that point.  When used inside a long constant string, the string
   is split at that point with the `+' concatenation operator.

   Comments are indented as follows:

   `;;;' Indentation remains unchanged.
   `;;'  Indent like the surrounding code
   `;'   Indent to a minimum column.

   The indentation of comments starting in column 0 is never changed.

   Use \\[idlwave-fill-paragraph] to refill a paragraph inside a
   comment.  The indentation of the second line of the paragraph
   relative to the first will be retained.  Use
   \\[auto-fill-mode] to toggle auto-fill mode for these
   comments.  When the variable `idlwave-fill-comment-line-only' is
   nil, code can also be auto-filled and auto-indented.

   To convert pre-existing IDL code to your formatting style, mark the
   entire buffer with \\[mark-whole-buffer] and execute
   \\[idlwave-expand-region-abbrevs].  Then mark the entire buffer
   again followed by \\[indent-region] (`indent-region').

2. Routine Info
   ------------
   IDLWAVE displays information about the calling sequence and the
   accepted keyword parameters of a procedure or function with
   \\[idlwave-routine-info].  \\[idlwave-find-module] jumps to the
   source file of a module.  These commands know about system
   routines, all routines in idlwave-mode buffers and (when the
   idlwave-shell is active) about all modules currently compiled under
   this shell.  It also makes use of pre-compiled or custom-scanned
   user and library catalogs many popular libraries ship with by
   default.  Use \\[idlwave-update-routine-info] to update this
   information, which is also used for completion (see item 4).

3. Online IDL Help
   ---------------

   \\[idlwave-context-help] displays the IDL documentation relevant
   for the system variable, keyword, or routines at point.  A single
   key stroke gets you directly to the right place in the docs.  See
   the manual to configure where and how the HTML help is displayed.

4. Completion
   ----------
   \\[idlwave-complete] completes the names of procedures, functions
   class names, keyword parameters, system variables and tags, class
   tags, structure tags, filenames and much more.  It is context
   sensitive and figures out what is expected at point.  Lower case
   strings are completed in lower case, other strings in mixed or
   upper case.

5. Code Templates and Abbreviations
   --------------------------------
   Many Abbreviations are predefined to expand to code fragments and templates.
   The abbreviations start generally with a `\\'.  Some examples:

   \\pr        PROCEDURE template
   \\fu        FUNCTION template
   \\c         CASE statement template
   \\sw        SWITCH statement template
   \\f         FOR loop template
   \\r         REPEAT Loop template
   \\w         WHILE loop template
   \\i         IF statement template
   \\elif      IF-ELSE statement template
   \\b         BEGIN

   For a full list, use \\[idlwave-list-abbrevs].  Some templates also
   have direct keybindings - see the list of keybindings below.

   \\[idlwave-doc-header] inserts a documentation header at the
   beginning of the current program unit (pro, function or main).
   Change log entries can be added to the current program unit with
   \\[idlwave-doc-modification].

6. Automatic Case Conversion
   -------------------------
   The case of reserved words and some abbrevs is controlled by
   `idlwave-reserved-word-upcase' and `idlwave-abbrev-change-case'.

7. Automatic END completion
   ------------------------
   If the variable `idlwave-expand-generic-end' is non-nil, each END typed
   will be converted to the specific version, like ENDIF, ENDFOR, etc.

8. Hooks
   -----
   Turning on `idlwave-mode' runs `idlwave-mode-hook'.

9. Documentation and Customization
   -------------------------------
   Info documentation for this package is available.  Use
   \\[idlwave-info] to display (complain to your sysadmin if that does
   not work).  For Postscript, PDF, and HTML versions of the
   documentation, check IDLWAVE's website at URL
   `https://github.com/jdtsmith/idlwave'.
   IDLWAVE has customize support - see the group `idlwave'.

10.Keybindings
   -----------
   Here is a list of all keybindings of this mode.
   If some of the key bindings below show with ??, use \\[describe-key]
   followed by the key sequence to see what the key sequence does.

\\{idlwave-mode-map}"
  :abbrev-table idlwave-mode-abbrev-table

  (if idlwave-startup-message
      (message "Emacs IDLWAVE mode version %s." idlwave-mode-version))
  (setq idlwave-startup-message nil)

  (set (make-local-variable 'indent-line-function) #'idlwave-indent-and-action)

  (set (make-local-variable idlwave-comment-indent-function)
       #'idlwave-comment-hook)

  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1) ; ";;" for new and regions
  (set (make-local-variable 'abbrev-all-caps) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'completion-ignore-case) t)

  (setq abbrev-mode t)

  (set (make-local-variable 'normal-auto-fill-function) #'idlwave-auto-fill)
  (setq comment-end "")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'paragraph-separate)
       "[ \t\f]*$\\|[ \t]*;+[ \t]*$\\|;+[+=-_*]+$")
  (set (make-local-variable 'paragraph-start) "[ \t\f]\\|[ \t]*;+[ \t]")
  (set (make-local-variable 'paragraph-ignore-fill-prefix) nil)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; ChangeLog
  (set (make-local-variable 'add-log-current-defun-function)
       #'idlwave-current-routine-fullname)

  ;; Set tag table list to use IDLTAGS as file name.
  (if (boundp 'tag-table-alist)
      (add-to-list 'tag-table-alist '("\\.pro\\'" . "IDLTAGS")))

  ;; Font-lock additions
  (set (make-local-variable 'font-lock-defaults) idlwave-font-lock-defaults)
  (set (make-local-variable 'font-lock-mark-block-function)
       #'idlwave-mark-subprogram)
  (set (make-local-variable 'font-lock-fontify-region-function)
       #'idlwave-font-lock-fontify-region)

  ;; Imenu setup
  ;; (set (make-local-variable 'imenu-create-index-function)
  ;;      ;; FIXME: Why set it explicitly to the value it already has?
  ;;      'imenu-default-create-index-function)
  (set (make-local-variable 'imenu-extract-index-name-function)
       #'idlwave-unit-name)
  (set (make-local-variable 'imenu-prev-index-position-function)
       #'idlwave-prev-index-position)

  ;; defun movement
  (set (make-local-variable 'beginning-of-defun-function)
       #'idlwave-beginning-of-subprogram)
  (set (make-local-variable 'end-of-defun-function)
       #'idlwave-end-of-subprogram)

  ;; HideShow setup
  (add-to-list 'hs-special-modes-alist
	       (list 'idlwave-mode
		     idlwave-begin-block-reg
		     idlwave-end-block-reg
		     ";"
		     'idlwave-forward-block nil))

  ;; Make a local post-command-hook and add our hook to it
  (add-hook 'post-command-hook #'idlwave-command-hook nil 'local)

  ;; Make local hooks for buffer updates
  (add-hook 'kill-buffer-hook #'idlwave-kill-buffer-update nil 'local)
  (add-hook 'after-save-hook #'idlwave-save-buffer-update nil 'local)
  (add-hook 'after-save-hook #'idlwave-revoke-license-to-kill nil 'local)

  ;; Setup directories and file, if necessary
  (idlwave-setup)

  ;; Update the routine info with info about current buffer?
  (idlwave-new-buffer-update))

;; Special module
(if idlwave-complete-structure-tags
    ;; FIXME: This `require' causes a cyclic load during byte-compilation.
    nil ;; (require 'idlw-complete-structtag)
  )

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.pro\\'")  'idlwave-mode))
(declare-function speedbar-add-supported-extension "speedbar" (extension))
(with-eval-after-load "speedbar" (speedbar-add-supported-extension ".pro"))

(defvar idlwave--command-function nil
  "If non-nil, a function called from `post-command-hook'.
It is evaluated in the Lisp function `idlwave-command-hook' which is
placed in `post-command-hook'.")
(defun idlwave-command-hook ()
  "Command run after every command.
Evaluates a non-nil value of the *variable* `idlwave--command-function' and
sets the variable to zero afterwards."
  (and idlwave--command-function
       (with-demoted-errors "idlwave-command-hook: %S"
	 (funcall (prog1 idlwave--command-function
	            (setq idlwave--command-function nil))))))

(defvar idlwave-setup-done nil)
(defun idlwave-setup ()
  "Setup directories, files, and path locations for IDLWAVE."
  (unless idlwave-setup-done
    (if (not (file-directory-p idlwave-config-directory))
	(make-directory idlwave-config-directory))
    (setq
     idlwave-user-catalog-file (expand-file-name
				idlwave-user-catalog-file
				idlwave-config-directory)
     idlwave-xml-system-rinfo-converted-file
     (expand-file-name
      idlwave-xml-system-rinfo-converted-file
      idlwave-config-directory)
     idlwave-path-file (expand-file-name
			idlwave-path-file
			idlwave-config-directory))
    ;; Check and setup help location
    (idlwave-read-paths)  ; we may need these early

    (idlwave-help-check-locations)
    (setq idlwave-setup-done t)))

;;----------------------------------------------------
;; Fontification
;;
(defun idlwave-font-lock-fontify-region (beg end &optional verbose)
  "Fontify continuation lines correctly."
  (let (pos)
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (when (setq pos (idlwave-is-continuation-line))
	(goto-char pos)
	(idlwave-beginning-of-statement)
	(setq beg (point)))))
  (font-lock-default-fontify-region beg end verbose))

;;----------------------------------------------------
;; Begin/end blocks

(defun idlwave-block-master ()
  "Define block begin and end delimiters."
  (let ((case-fold-search t))
    (save-excursion
      (cond
       ((looking-at "pro\\|case\\|switch\\|function\\>")
	(assoc (downcase (match-string 0)) idlwave-block-matches))
       ((looking-at "begin\\>")
	(let ((limit (save-excursion
		       (idlwave-beginning-of-statement)
		       (point))))
	  (cond
	   ((re-search-backward ":[ \t]*\\=" limit t)
	    ;; seems to be a case thing
	    '("begin" . "end"))
	   ((re-search-backward idlwave-block-match-regexp limit t)
	    (assoc (downcase (match-string 1))
		   idlwave-block-matches))
	   (t
	    ;; Just a normal block
	    '("begin" . "end")))))
       (t nil)))))

(defun idlwave-close-block ()
  "Terminate the current block with the correct END statement."
  (interactive)
  ;; Start new line if we are not in a new line
  (unless (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
    (let ((idlwave-show-block nil))
      (newline-and-indent)))
  (let ((last-abbrev-location (point)))  ; for upcasing
    (insert "end")
    (idlwave-show-begin)))

(defun idlwave-backward-up-block (&optional arg)
  "Move to beginning of enclosing block if prefix ARG >= 0.
If prefix ARG < 0 then move forward to enclosing block end."
  (interactive "p")
  (idlwave-block-jump-out (- arg) 'nomark))

(defun idlwave-beginning-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (idlwave-block-jump-out -1 'nomark)
  (forward-word-strictly 1))

(defun idlwave-end-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (idlwave-block-jump-out 1 'nomark)
  (backward-word-strictly 1))

(defun idlwave-forward-block (&optional arg)
  "Move across next nested block."
  (interactive)
  (let ((arg (or arg 1)))
    (if (idlwave-down-block arg)
	(idlwave-block-jump-out arg 'nomark))))

(defun idlwave-backward-block ()
  "Move backward across previous nested block."
  (interactive)
  (if (idlwave-down-block -1)
      (idlwave-block-jump-out -1 'nomark)))

(defun idlwave-down-block (&optional arg)
  "Go down a block.
With ARG: ARG >= 0 go forwards, ARG < 0 go backwards.
Returns non-nil if successful."
  (interactive "p")
  (let (status)
    (if (< arg 0)
        ;; Backward
        (let ((eos (save-excursion
                     (idlwave-block-jump-out -1 'nomark)
                     (point))))
          (if (setq status (idlwave-find-key
			    idlwave-end-block-reg -1 'nomark eos))
              (idlwave-beginning-of-statement)
            (message "No nested block before beginning of containing block.")))
      ;; Forward
      (let ((eos (save-excursion
                   (idlwave-block-jump-out 1 'nomark)
                   (point))))
        (if (setq status (idlwave-find-key
			  idlwave-begin-block-reg 1 'nomark eos))
            (idlwave-end-of-statement)
          (message "No nested block before end of containing block."))))
    status))

(defun idlwave-block-jump-out (&optional dir nomark)
  "When optional argument DIR is non-negative, move forward to end of
current block using the `idlwave-begin-block-reg' and `idlwave-end-block-reg'
regular expressions.  When DIR is negative, move backwards to block beginning.
Recursively calls itself to skip over nested blocks.  DIR defaults to
forward.  Calls `push-mark' unless the optional argument NOMARK is
non-nil.  Movement is limited by the start of program units because of
possibility of unbalanced blocks."
  (interactive "P")
  (or dir (setq dir 0))
  (let* ((here (point))
         (case-fold-search t)
         (limit (if (>= dir 0) (point-max) (point-min)))
         (block-limit (if (>= dir 0)
			  idlwave-begin-block-reg
			idlwave-end-block-reg))
         found
         (block-reg (concat idlwave-begin-block-reg "\\|"
			    idlwave-end-block-reg))
         (unit-limit (or (save-excursion
			   (if (< dir 0)
			       (idlwave-find-key
				idlwave-begin-unit-reg dir t limit)
			     (end-of-line)
			     (idlwave-find-key
			      idlwave-end-unit-reg dir t limit)))
			 limit)))
    (if (>= dir 0) (end-of-line)) ;Make sure we are in current block
    (if (setq found (idlwave-find-key  block-reg dir t unit-limit))
        (while (and found (looking-at block-limit))
          (if (>= dir 0) (forward-word-strictly 1))
          (idlwave-block-jump-out dir t)
          (setq found (idlwave-find-key block-reg dir t unit-limit))))
    (if (not nomark) (push-mark here))
    (if (not found) (goto-char unit-limit)
      (if (>= dir 0) (forward-word-strictly 1)))))

(defun idlwave-show-begin-check ()
  "Ensure that the previous word was a token before `idlwave-show-begin'.
An END token must be preceded by whitespace."
  (if
      (save-excursion
	(backward-word-strictly 1)
	(backward-char 1)
	(looking-at "[ \t\n\f]"))
      (idlwave-show-begin)))

(defun idlwave-show-begin ()
  "Find the start of current block and blinks to it for a second.
Also checks if the correct END statement has been used."
  ;; All end statements are reserved words
  ;; Re-indent end line
  ;;(insert-char ?\  1) ;; So indent, etc. work well
  ;;(backward-char 1)
  (let* ((pos (point-marker))
	 (last-abbrev-marker (copy-marker last-abbrev-location))
         (eol-pos (line-end-position))
	 begin-pos end-pos end end1 )
    (if idlwave-reindent-end  (idlwave-indent-line))
    (setq last-abbrev-location (marker-position last-abbrev-marker))
    (when (and (idlwave-modify-abbrev 0 t)
	       idlwave-show-block)
      (save-excursion
	;; Move inside current block
	(goto-char last-abbrev-marker)
	(idlwave-block-jump-out -1 'nomark)
	(setq begin-pos (point))
	(idlwave-block-jump-out 1 'nomark)
	(setq end-pos (point))
	(if (> end-pos eol-pos)
	    (setq end-pos pos))
	(goto-char end-pos)
	(setq end (buffer-substring
		   (progn
		     (skip-chars-backward "a-zA-Z")
		     (point))
		   end-pos))
	(goto-char begin-pos)
	(when (setq end1 (cdr (idlwave-block-master)))
	  (cond
	   ((null end1)) ; no-operation
	   ((string= (downcase end) (downcase end1))
	    (sit-for 0.75))
	   ((string= (downcase end) "end")
	    ;; A generic end
	    (if idlwave-expand-generic-end
		(save-excursion
		  (goto-char pos)
		  (backward-char 3)
		  (insert (if (string= end "END") (upcase end1) end1))
		  (delete-char 3)))
	    (sit-for 1))
	   (t
	    (beep)
	    (message "Warning: Shouldn't this be \"%s\" instead of \"%s\"?"
		     end1 end)
	    (sit-for 1))))))))

(defun idlwave-mark-block ()
  "Mark containing block."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-backward-up-block -1)
  (idlwave-end-of-statement)
  (let ((end (point)))
    (idlwave-backward-block)
    (idlwave-beginning-of-statement)
    (push-mark end nil t)))

;;----------------------------------------------------
;; Routines/Subprograms

(defun idlwave-beginning-of-subprogram (&optional nomark)
  "Move point to the beginning of the current program unit.
If NOMARK is non-nil, do not push mark."
  (interactive)
  (idlwave-find-key idlwave-begin-unit-reg -1 nomark))

(defun idlwave-end-of-subprogram (&optional nomark)
  "Move point to the start of the next program unit.
If NOMARK is non-nil, do not push mark."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-find-key idlwave-end-unit-reg 1 nomark))

(defun idlwave-mark-subprogram ()
  "Put mark at beginning of program, point at end.
The marks are pushed."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-beginning-of-subprogram)
  (let ((beg (point)))
    (idlwave-forward-block)
    (push-mark beg nil t))
  (exchange-point-and-mark))

(defun idlwave-current-routine-fullname ()
  (idlwave-make-full-name (idlwave-current-routine)))

(defun idlwave-current-routine ()
  "Return (NAME TYPE CLASS) of current routine."
  (idlwave-routines)
  (save-excursion
    (idlwave-beginning-of-subprogram 'nomark)
    (if (looking-at "[ \t]*\\<\\(pro\\|function\\)\\>\\s-+\\(\\([a-zA-Z0-9$_]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)")
	(let* ((type (if (string= (downcase (match-string 1)) "pro")
			 'pro 'function))
	       (class (idlwave-sintern-class (match-string 3)))
	       (name (idlwave-sintern-routine-or-method (match-string 4) class)))
	  (list name type class)))))

(defvar idlwave-force-class-query nil)

(defun idlwave-resolve (&optional arg)
  "Call RESOLVE_ROUTINE on the module name at point.
Like `idlwave-routine-info', this looks for a routine call at point.
After confirmation in the minibuffer, it will use the shell to issue
a RESOLVE call for this routine, to attempt to make it defined and its
routine info available for IDLWAVE.  If the routine is a method call,
both `class__method' and `class__define' will be tried.
With ARG, enforce query for the class of object methods."
  (interactive "P")
  (let* ((idlwave-query-class nil)
	 (idlwave-force-class-query (equal arg '(4)))
	 (module (idlwave-what-module))
	 (name (idlwave-make-full-name module))
	 (type (if (eq (nth 1 module) 'pro) "pro" "function"))
	 (resolve (read-string "Resolve: " (format "%s %s" type name)))
	 (kwd "")
	 class)
    (if (string-match "\\(pro\\|function\\)[ \t]+\\(\\(.*\\)::\\)?\\(.*\\)"
		      resolve)
	(setq type (match-string 1 resolve)
	      class (if (match-beginning 2)
			(match-string 3 resolve)
		      nil)
	      name (match-string 4 resolve)))
    (if (string= (downcase type) "function")
	(setq kwd ",/is_function"))

    (cond
     ((null class)
      (idlwave-shell-send-command
       (format "resolve_routine,'%s'%s" (downcase name) kwd)
       #'idlwave-update-routine-info
       nil t))
     (t
      (idlwave-shell-send-command
       (format "resolve_routine,'%s__define'%s" (downcase class) kwd)
       (lambda ()
	 (idlwave-shell-send-command
	  (format "resolve_routine,'%s__%s'%s"
	          (downcase class) (downcase name) kwd)
	  #'idlwave-update-routine-info
	  nil t)))))))

;;----------------------------------------------------
;; Statements/substatements

(defun idlwave-mark-statement ()
  "Mark current IDL statement."
  (interactive)
  (idlwave-end-of-statement)
  (let ((end (point)))
    (idlwave-beginning-of-statement)
    (push-mark end nil t)))

(defvar idlwave-shell-prompt-pattern)
(defun idlwave-beginning-of-statement ()
  "Move to beginning of the current statement.
Skips back past statement continuations.
Point is placed at the beginning of the line whether or not this is an
actual statement."
  (interactive)
  (cond
   ((derived-mode-p 'idlwave-shell-mode)
    (if (re-search-backward idlwave-shell-prompt-pattern nil t)
	(goto-char (match-end 0))))
   (t
    (if (save-excursion (forward-line -1) (idlwave-is-continuation-line))
	(idlwave-previous-statement)
      (beginning-of-line)))))

(defun idlwave-previous-statement ()
  "Move point to beginning of the previous statement.
Returns t if the current line before moving is the beginning of
the first non-comment statement in the file, and nil otherwise."
  (interactive)
  (let (first-statement)
    (if (not (= (forward-line -1) 0))
        ;; first line in file
        t
      ;; skip blank lines, label lines, include lines and line comments
      (while (and
              ;; The current statement is the first statement until we
              ;; reach another statement.
              (setq first-statement
                    (or
                     (looking-at idlwave-comment-line-start-skip)
                     (looking-at "[ \t]*$")
                     (looking-at (concat "[ \t]*" idlwave-label "[ \t]*$"))
                     (looking-at "^@")))
              (= (forward-line -1) 0)))
      ;; skip continuation lines
      (while (and
              (save-excursion
                (forward-line -1)
                (idlwave-is-continuation-line))
              (= (forward-line -1) 0)))
      first-statement)))

(defun idlwave-end-of-statement ()
  "Move point to the end of the current IDL statement.
If not in a statement just moves to end of line.  Returns position."
  (interactive)
  (while (and (idlwave-is-continuation-line)
              (= (forward-line 1) 0))
    (while (and (idlwave-is-comment-or-empty-line)
		(= (forward-line 1) 0))))
  (end-of-line)
  (point))

(defun idlwave-end-of-statement0 ()
  "Move point to the end of the current IDL statement.
If not in a statement just moves to end of line.  Returns position."
  (interactive)
  (while (and (idlwave-is-continuation-line)
              (= (forward-line 1) 0)))
  (end-of-line)
  (point))

(defun idlwave-next-statement (&optional eos)
  "Move point to beginning of the next IDL statement.
Returns t if that statement is the last non-comment IDL statement
in the file, and nil otherwise."
  (interactive)
  (let (last-statement)
    (if eos (goto-char eos) (idlwave-end-of-statement))
    ;; skip blank lines, label lines, include lines and line comments
    (while (and (= (forward-line 1) 0)
                ;; The current statement is the last statement until
                ;; we reach a new statement.
                (setq last-statement
                      (or
                       (looking-at idlwave-comment-line-start-skip)
                       (looking-at "[ \t]*$")
                       (looking-at (concat "[ \t]*" idlwave-label "[ \t]*$"))
                       (looking-at "^@")))))
    last-statement))

(defun idlwave-skip-label-or-case (&optional eos)
  "Skip label or case statement element.
Returns position after label.
If there is no label point is not moved and nil is returned."
  ;; Case expressions and labels are terminated by a colon.
  ;; So we find the first colon in the line and make sure
  ;; - no `?' is before it (might be a ? b : c)
  ;; - it is not in a comment
  ;; - not in a string constant
  ;; - not in parenthesis (like a[0:3])
  ;; - not followed by another ":" in explicit class, ala a->b::c
  ;; As many in this mode, this function is heuristic and not an exact
  ;; parser.
  (let* ((start (point))
	 (eos (or eos (save-excursion (idlwave-end-of-statement) (point))))
	 (end (idlwave-find-key ":" 1 'nomark eos)))
    (if (and end
             (= (nth 0 (parse-partial-sexp start end)) 0)
	     (not (string-match "\\?" (buffer-substring start end)))
	     (not (string-match "^::" (buffer-substring end eos))))
        (progn
          (forward-char)
          (point))
      (goto-char start)
      nil)))

(defun idlwave-start-of-substatement (&optional pre)
  "Move to start of next IDL substatement after point.
Uses the type of the current IDL statement to determine if the next
statement is on a new line or is a subpart of the current statement.
Returns point at start of substatement modulo whitespace.
If optional argument is non-nil move to beginning of current
substatement."
  (let ((orig (point))
        (eos (save-excursion (idlwave-end-of-statement) (point)))
	(ifnest 0)
        st nst last)
    (idlwave-beginning-of-statement)
    (setq last (point))
    (idlwave-skip-label-or-case eos)
    (if (and pre (> (point) orig)) ;; Previous statement isn't beyond point!
	(goto-char last))
    (if (< (point) orig)
	(idlwave-skip-multi-commands orig))
    (setq last (point))
    ;; Continue looking for substatements until we are past orig
    (while (and (<= (point) orig) (not (eobp)))
      (setq last (point))
      (setq nst (nth 1 (cdr (setq st (car (idlwave-statement-type))))))
      (if (equal (car st) 'if) (setq ifnest (1+ ifnest)))
      (cond ((and nst
                  (idlwave-find-key nst 1 'nomark eos))
             (goto-char (match-end 0)))
            ((and (> ifnest 0) (idlwave-find-key "\\<else\\>" 1 'nomark eos))
             (setq ifnest (1- ifnest))
             (goto-char (match-end 0)))
            (t (setq ifnest 0)
               (idlwave-next-statement eos))))
    (if pre (goto-char last))
    ;; If a continuation line starts here, move to next line
    (when (looking-at "[ \t]*\\$\\([ \t]*\\(;\\|$\\)\\)")
      (beginning-of-line 2))
    (while
	(and (not (eobp))
	     (or (looking-at idlwave-comment-line-start-skip) ;comment only
		 (looking-at "[ \t]*$"))) ; blank
      (beginning-of-line 2))
    (point)))

(defun idlwave-prev-index-position ()
  "Search for the previous procedure or function.
Return nil if not found.  For use with imenu.el."
  (save-match-data
    (cond
     ((idlwave-find-key idlwave-profun-reg -1 'nomark))
     (t nil))))

(defun idlwave-unit-name ()
  "Return the unit name.
Assumes that point is at the beginning of the unit as found by
`idlwave-prev-index-position'."
  (forward-sexp 2)
  (forward-sexp -1)
  (let ((begin (point)))
    (re-search-forward
     "[a-zA-Z_][a-zA-Z0-9$_]+\\(::[a-zA-Z_][a-zA-Z0-9$_]+\\)?")
    (buffer-substring-no-properties begin (point))))

;;----------------------------------------------------
;; Comments/strings

(defun idlwave-comment-hook ()
  "Compute indent for the beginning of the IDL comment delimiter."
  (if (or (looking-at idlwave-no-change-comment)
          (looking-at (or idlwave-begin-line-comment "^;")))
      (current-column)
    (if (looking-at idlwave-code-comment)
        (if (save-excursion (skip-chars-backward " \t") (bolp))
            ;; On line by itself, indent as code
            (let ((tem (idlwave-calculate-indent)))
              (if (listp tem) (car tem) tem))
          ;; after code - do not change
          (current-column))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun idlwave-toggle-comment-region (beg end &optional n)
  "Comment the lines in the region if the first non-blank line is
commented, and conversely, uncomment region.  If optional prefix arg
N is non-nil, then for N positive, add N comment delimiters or for N
negative, remove N comment delimiters.
Uses `comment-region' which does not place comment delimiters on
blank lines."
  (interactive "r\nP")
  (if n
      (comment-region beg end (prefix-numeric-value n))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      ;; skip blank lines
      (skip-chars-forward " \t\n")
      (if (looking-at (concat "[ \t]*\\(" comment-start "+\\)"))
          (uncomment-region beg end)
	(comment-region beg end)))))


(defun idlwave-skip-multi-commands (&optional lim)
  "Skip past multiple commands on a line (or multiple lines) (with `&')."
  (let ((save-point (point)))
    (while (re-search-forward "\\s-\\(&\\)[^&]" lim t)
      (backward-char)
      (if (and (not (idlwave-quoted))
	       (not (eq (char-before (- (point) 1)) ?&)))
	  (setq save-point (point))))
    (goto-char save-point)
    save-point))

(defun idlwave-goto-comment ()
  "Move to start of comment delimiter on current line.
Moves to end of line if there is no comment delimiter.
Ignores comment delimiters in strings.
Returns point if comment found and nil otherwise."
  (let ((eos (line-end-position))
        (data (match-data))
        found)
    ;; Look for first comment delimiter not in a string
    (beginning-of-line)
    (setq found (search-forward comment-start eos 'lim))
    (while (and found (idlwave-in-quote))
      (setq found (search-forward comment-start eos 'lim)))
    (store-match-data data)
    (and found (not (idlwave-in-quote))
         (progn
           (backward-char 1)
           (point)))))

(defun idlwave-commented-paragraph-beg-end ()
  "Find and return the beginning and end position of a commented paragraph.
End is calculated as distance from end of buffer, to accommodate
additions from filling."
  (let (pre fill-prefix-reg bcl start end) ;; diff
    (beginning-of-line)
    (setq bcl (point))
    (re-search-forward (concat "^[ \t]*" comment-start "+")
                       (line-end-position) t)
    ;; Get the comment leader on the line and its length
    (setq pre (current-column))
    ;; the comment leader is the indentation plus exactly the
    ;; number of consecutive ";".
        (setq fill-prefix-reg
              (concat
               (setq fill-prefix
                     (regexp-quote (buffer-substring (line-beginning-position)
                                                     (point))))
               "[^;]"))

        ;; Mark the beginning and end of the paragraph
    (goto-char bcl)
    (while (and (looking-at fill-prefix-reg)
		(not (looking-at paragraph-separate))
		(not (bobp)))
      (forward-line -1))
    ;; Move to first line of paragraph
    (if (and (/= (point) bcl) (not (bobp)))
	(forward-line 1))
    (setq start (point))
    (goto-char bcl)
    (while (and (looking-at fill-prefix-reg)
		(not (looking-at paragraph-separate))
		(not (eobp)))
      (forward-line 1))
    (beginning-of-line)
    (if (or (not (looking-at fill-prefix-reg))
	    (looking-at paragraph-separate))
	(forward-line -1))
    (end-of-line)
    ;; if at end of buffer add a newline (need this because
    ;; fill-region needs END to be at the beginning of line after
    ;; the paragraph or it will add a line).
    (if (eobp)
	(progn (insert ?\n) (backward-char 1)))
    ;; Set END to the beginning of line after the paragraph
    ;; N.B. END is calculated as distance from end of buffer
    (setq end (- (point-max) (point) 1))
    (list start end pre)))

(defun idlwave-show-matching-quote ()
  "Insert quote and show matching quote if this is end of a string."
  (interactive)
  (let ((bq (idlwave-in-quote))
        (inq last-command-event))
    (if (and bq (not (idlwave-in-comment)))
        (let ((delim (char-after bq)))
          (insert inq)
          (if (eq inq delim)
              (save-excursion
                (goto-char bq)
                (sit-for 1))))
      ;; Not the end of a string
      (insert inq))))

;;----------------------------------------------------
;; Structures

(defvar idlwave-struct-skip
  "[ \t]*\\(\\$.*\n\\(^[ \t]*\\(\\$[ \t]*\\)?\\(;.*\\)?\n\\)*\\)?[ \t]*"
  "Regexp for skipping continued blank or comment-only lines in structures.")
(defvar idlwave-struct-tag-regexp
  (concat "[{,]" ;leading comma/brace
	  idlwave-struct-skip ; 4 groups
	  "\\([a-zA-Z][a-zA-Z0-9_]*\\)" ;the tag itself, group 5
	  "[ \t]*:") ; the final colon
  "Regexp for structure tags.")

(defun idlwave-struct-tags ()
  "Return a list of all tags in the structure defined at point.
Point is expected just before the opening `{' of the struct definition."
  (save-excursion
    (let* ((borders (idlwave-struct-borders))
	   (beg (car borders))
	   (end (cdr borders))
	   tags)
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(while (re-search-forward idlwave-struct-tag-regexp end t)
	  ;; Check if we are still on the top level of the structure.
	  (if (and (condition-case nil (progn (up-list -1) t) (error nil))
		   (= (point) beg))
	      (push (match-string-no-properties 5) tags))
	  (goto-char (match-end 0))))
	(nreverse tags))))

(defun idlwave-find-struct-tag (tag)
  "Find a given TAG in the structure defined at point."
  (let* ((borders (idlwave-struct-borders))
	 (end (cdr borders))
	 (case-fold-search t))
    (re-search-forward (concat "\\(^[ \t]*\\|[,{][ \t]*\\)" tag "[ \t]*:")
		       end t)))

(defun idlwave-struct-inherits ()
  "Return a list of all `inherits' names in the struct at point.
Point is expected just before the opening `{' of the struct definition."
  (save-excursion
    (let* ((borders (idlwave-struct-borders))
	   (beg (car borders))
	   (end (cdr borders))
	   (case-fold-search t)
	   names)
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(while (re-search-forward
		(concat "[{,]"  ;leading comma/brace
			idlwave-struct-skip ; 4 groups
			"inherits"    ; The INHERITS tag
			idlwave-struct-skip ; 4 more
			"\\([a-zA-Z][a-zA-Z0-9_]*\\)") ; The super-group, #9
		end t)
	  ;; Check if we are still on the top level of the structure.
	  (if (and (condition-case nil (progn (up-list -1) t) (error nil))
		   (= (point) beg))
	      (push (match-string-no-properties 9) names))
	  (goto-char (match-end 0))))
      (nreverse names))))

(defun idlwave-struct-borders ()
  "Return the borders of the {...} after point as a cons cell."
  (let (beg)
    (save-excursion
      (skip-chars-forward "^{")
      (setq beg (point))
      (condition-case nil (forward-list 1)
	(error (goto-char beg)))
      (cons beg (point)))))

(defun idlwave-find-structure-definition (&optional var name bound)
  "Search forward for a structure definition.
If VAR is non-nil, search for a structure assigned to variable VAR.
If NAME is non-nil, search for a named structure NAME, if a string,
or a generic named structure otherwise.  If BOUND is an integer, limit
the search.  If BOUND is the symbol `all', we search first back and
then forward through the entire file.  If BOUND is the symbol `back'
we search only backward."
  (let* ((ws "[ \t]*\\(\\$.*\n[ \t]*\\)*")
	 (case-fold-search t)
	 (lim (if (integerp bound) bound nil))
	 (re (concat
	      (if var
		  (concat "\\<" (regexp-quote (downcase var)) "\\>" ws)
		"\\(\\)")
	      "=" ws "\\({\\)"
	      (if name
		  (if (stringp name)
		      (concat ws "\\(\\<" (downcase name) "\\)[^a-zA-Z0-9_$]")
		    ;; Just a generic name
		    (concat ws "\\<\\([a-zA-Z_0-9$]+\\)" ws ","))
		""))))
    (if (or (and (or (eq bound 'all) (eq bound 'back))
		 (re-search-backward re nil t))
	    (and (not (eq bound 'back)) (re-search-forward re lim t)))
	(progn
	  (goto-char (match-beginning 3))
	  (if name (match-string-no-properties 5)
	    t)))))

;;----------------------------------------------------
;; Class Definitions

(defun idlwave-find-class-definition (class &optional all-hook alt-class)
  "Find class structure definition(s).
If ALL-HOOK is set, find all named structure definitions in a given
class__define routine, on which ALL-HOOK will be run.  If ALT-CLASS is
set, look for the name__define pro, and inside of it, for the ALT-CLASS
class/struct definition."
  (let ((case-fold-search t) end-lim name)
    (when (re-search-forward
	   (concat "^[ \t]*pro[ \t]+" (downcase class) "__define" "\\>") nil t)
      (if all-hook
	  (progn
	    ;; For everything there
	    (setq end-lim (save-excursion (idlwave-end-of-subprogram) (point)))
	    (while (setq name
			 (idlwave-find-structure-definition nil t end-lim))
	      (funcall all-hook name)))
	(idlwave-find-structure-definition nil (or alt-class class))))))

(defun idlwave-class-info (class)
  (let (list entry)
    (if idlwave-class-info
	(if idlwave-class-reset
	    (setq
	     idlwave-class-reset nil
	     idlwave-class-info ; Remove any visited in a buffer
	     (delq nil (mapcar
			(lambda (x)
			  (let ((filebuf
				 (idlwave-class-file-or-buffer
				  (or (cdr (assq 'found-in x)) (car x)))))
			    (if (cdr filebuf)
				nil
			      x)))
			idlwave-class-info))))
      ;; Info is nil, put in the system stuff to start.
      (setq idlwave-class-info idlwave-system-class-info)
      (setq list idlwave-class-info)
      (while (setq entry (pop list))
	(idlwave-sintern-class-info entry)))
    (setq class (idlwave-sintern-class class))
    (or (assq class idlwave-class-info)
	(progn (idlwave-scan-class-info class)
	       (assq class idlwave-class-info)))))

(defun idlwave-class-file-or-buffer (class)
  "Find buffer visiting CLASS definition."
  (let* ((pro (concat (downcase class) "__define"))
	 (file (idlwave-routine-source-file
		(nth 3 (idlwave-rinfo-assoc pro 'pro nil
					    (idlwave-routines))))))
    (cons file (if file (find-buffer-visiting file)))))


(defun idlwave-scan-class-info (class)
  "Scan all class and named structure info in the class__define pro."
  (let* ((idlwave-auto-routine-info-updates nil)
	 (filebuf (idlwave-class-file-or-buffer class))
	 (file (car filebuf))
	 (buf (cdr filebuf))
	 (class (idlwave-sintern-class class)))
    (if (or
	 (not file)
	 (and ;; neither a regular file nor a visited buffer
	  (not buf)
	  (not (file-regular-p file))))
	nil ; Cannot find the file/buffer to get any info
      (save-excursion
	(if buf (set-buffer buf)
	  ;; Read the file in temporarily
	  (set-buffer (get-buffer-create " *IDLWAVE-tmp*"))
	  (erase-buffer)
	  (unless (derived-mode-p 'idlwave-mode)
	    (idlwave-mode))
	  (insert-file-contents file))
	(save-excursion
	  (goto-char 1)
	  (idlwave-find-class-definition class
	   ;; Scan all of the structures found there
	   (lambda (name)
	     (let* ((this-class (idlwave-sintern-class name))
		    (entry
		     (list this-class
			   (cons 'tags (idlwave-struct-tags))
			   (cons 'inherits (idlwave-struct-inherits)))))
	       (if (not (eq this-class class))
		   (setq entry (nconc entry (list (cons 'found-in class)))))
	       (idlwave-sintern-class-info entry)
	       (push entry idlwave-class-info)))))))))

(defun idlwave-class-found-in (class)
  "Return the FOUND-IN property of the CLASS."
  (cdr (assq 'found-in (idlwave-class-info class))))

(defun idlwave-class-tags (class)
  "Return the native tags in CLASS."
  (cdr (assq 'tags (idlwave-class-info class))))

(defun idlwave-class-inherits (class)
  "Return the direct superclasses of CLASS."
  (cdr (assq 'inherits (idlwave-class-info class))))

(defun idlwave-all-class-tags (class)
  "Return a list of native and inherited tags in CLASS."
  (condition-case err
      (apply #'append (mapcar #'idlwave-class-tags
			      (cons class (idlwave-all-class-inherits class))))
    (error
     (idlwave-class-tag-reset)
     (error "%s" (error-message-string err)))))

(defun idlwave-all-class-inherits (class)
  "Return a list of all superclasses of CLASS (recursively expanded).
The list is cached in `idlwave-class-info' for faster access."
  (cond
   ((not idlwave-support-inheritance) nil)
   ((eq class nil) nil)
   ((eq class t) nil)
   (t
    (let ((info (idlwave-class-info class))
	  entry)
      (if (setq entry (assq 'all-inherits info))
	  (cdr entry)
	;; Save the depth of inheritance scan to check for circular references
	(let ((inherits (mapcar (lambda (x) (cons x 0))
				(idlwave-class-inherits class)))
	      rtn all-inherits cl)
	  (while inherits
	    (setq cl (pop inherits)
		  rtn (cons (car cl) rtn)
		  inherits (append (mapcar (lambda (x)
					     (cons x (1+ (cdr cl))))
					   (idlwave-class-inherits (car cl)))
				   inherits))
	    (if (> (cdr cl) 999)
	      (error
               "Class scan: inheritance depth exceeded.  Circular inheritance?")))
	  (setq all-inherits (nreverse rtn))
	  (nconc info (list (cons 'all-inherits all-inherits)))
	  all-inherits))))))

(defun idlwave-class-or-superclass-with-tag (class tag)
  "Find and return the CLASS or one of its superclass with the
associated TAG, if any."
  (let ((sclasses (cons class (idlwave-all-class-inherits class)))
	cl)
   (catch 'exit
     (while sclasses
       (setq cl (pop sclasses))
       (let ((tags (idlwave-class-tags cl)))
	 (while tags
	   (if (string-equal-ignore-case tag (car tags))
	     (throw 'exit cl))
	   (setq tags (cdr tags))))))))

(defun idlwave-explicit-class-listed (info)
  "Return whether or not the class is listed explicitly, ala a->b::c.
INFO is as returned by `idlwave-what-function' or `-procedure'."
  (let ((apos (nth 3 info)))
    (if apos
	(save-excursion (goto-char apos)
			(looking-at "\\(->|\\.\\)[a-zA-Z][a-zA-Z0-9$_]*::")))))

;;----------------------------------------------------
;; Indent and indent action

(defun idlwave-calculate-indent ()
  "Return appropriate indentation for current line as IDL code."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Check for beginning of unit - main (beginning of buffer), pro, or
     ;; function
     ((idlwave-look-at idlwave-begin-unit-reg)
      0)
     ;; Check for continuation line
     ((save-excursion
        (and (= (forward-line -1) 0)
             (idlwave-is-continuation-line)))
      (idlwave-calculate-cont-indent))
     ;; calculate indent based on previous and current statements
     (t (let* (beg-prev-pos
	       (the-indent
		;; calculate indent based on previous statement
		(save-excursion
		  (cond
		   ;; Beginning of file
		   ((prog1
			(idlwave-previous-statement)
		      (setq beg-prev-pos (point)))
		    0)
		   ;; Main block
		   ((idlwave-look-at idlwave-profun-reg t)
		    (+ (idlwave-current-statement-indent)
		       idlwave-main-block-indent))
		   ;; Begin block
		   ((idlwave-look-at idlwave-begin-block-reg t)
		    (+ (idlwave-min-current-statement-indent)
		       idlwave-block-indent))
		   ;; End Block
		   ((idlwave-look-at idlwave-end-block-reg t)
		    (progn
		      ;; Match to the *beginning* of the block opener
		      (goto-char beg-prev-pos)
		      (idlwave-block-jump-out -1 'nomark) ; go to begin block
		      (idlwave-min-current-statement-indent)))
		   ;;		      idlwave-end-offset
		   ;;		      idlwave-block-indent))

		   ;; Default to current indent
		   ((idlwave-current-statement-indent))))))
          ;; adjust the indentation based on the current statement
          (cond
           ;; End block
           ((idlwave-look-at idlwave-end-block-reg)
	    (+ the-indent idlwave-end-offset))
           (the-indent)))))))

(defun idlwave-indent-and-action (&optional arg)
  "Call `idlwave-indent-line' and do expand actions.
With prefix ARG non-nil, indent the entire sub-statement."
  (interactive "p")
  (save-excursion
    (if	(and idlwave-expand-generic-end
	     (re-search-backward "\\<\\(end\\)\\s-*\\="
				 (max 0 (- (point) 10)) t)
	     (looking-at "\\(end\\)\\([ \n\t]\\|\\'\\)"))
	(progn (goto-char (match-end 1))
	       ;;Expand the END abbreviation, just as RET or Space would have.
	       (if abbrev-mode (expand-abbrev)
		 (idlwave-show-begin)))))
  (when (and (not arg) current-prefix-arg)
    (setq arg current-prefix-arg)
    (setq current-prefix-arg nil))
  (if arg
      (idlwave-indent-statement)
    (idlwave-indent-line t)))

(defun idlwave-indent-line (&optional expand)
  "Indent current IDL line as code or as a comment.
The actions in `idlwave-indent-action-table' are performed.
If the optional argument EXPAND is non-nil then the actions in
`idlwave-indent-expand-table' are performed."
  (interactive)
  ;; Move point out of left margin.
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      (skip-chars-forward " \t"))
  (let ((mloc (point-marker)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at idlwave-comment-line-start-skip)
          ;; Indentation for a line comment
          (progn
            (skip-chars-forward " \t")
            (idlwave-indent-left-margin (idlwave-comment-hook)))
        ;;
        ;; Code Line
        ;;
        ;; Before indenting, run action routines.
        ;;
        (if (and expand idlwave-do-actions)
            (mapc #'idlwave-do-action idlwave-indent-expand-table))
        ;;
        (if idlwave-do-actions
            (mapc #'idlwave-do-action idlwave-indent-action-table))
        ;;
        ;; No longer expand abbrevs on the line.  The user can do this
        ;; manually using expand-region-abbrevs.
        ;;
        ;; Indent for code line
        ;;
        (beginning-of-line)
        (if (or
             ;; a label line
             (looking-at (concat "^" idlwave-label "[ \t]*$"))
             ;; a batch command
             (looking-at "^[ \t]*@"))
            ;; leave flush left
            nil
          ;; indent the line
          (idlwave-indent-left-margin (idlwave-calculate-indent)))
        ;; Adjust parallel comment
	(end-of-line)
	(if (idlwave-in-comment)
            (let ((fill-column (1- (frame-width))))
	      (indent-for-comment)))))
    (goto-char mloc)
    ;; Get rid of marker
    (set-marker mloc nil)))

(defun idlwave-newline ()
  "Insert a newline and indent the current and previous line."
  (interactive)
  ;;
  ;; Handle unterminated single and double quotes
  ;; If not in a comment and in a string then insertion of a newline
  ;; will mean unbalanced quotes.
  ;;
  (if (and (not (idlwave-in-comment)) (idlwave-in-quote))
      (progn (beep)
             (message "Warning: unbalanced quotes?")))
  (newline)
  ;;
  ;; The current line is being split, the cursor should be at the
  ;; beginning of the new line skipping the leading indentation.
  ;;
  ;; The reason we insert the new line before indenting is that the
  ;; indenting could be confused by keywords (e.g. END) on the line
  ;; after the split point.  This prevents us from just using
  ;; `indent-for-tab-command' followed by `newline-and-indent'.
  ;;
  (beginning-of-line 0)
  (idlwave-indent-line)
  (forward-line)
  (idlwave-indent-line))

(defun idlwave-split-line (&optional noindent)
  "Continue line by breaking line at point and indent the lines.
For a code line, insert continuation marker.  Don't indent if
NOINDENT is passed.  If the line is a line comment then the new
line will contain a comment with the same indentation.  Splits
strings with the IDL operator `+' if `idlwave-split-line-string'
is non-nil."
  (interactive)
  ;; Expand abbreviation, just like normal RET would.
  (and abbrev-mode (expand-abbrev))
  (let (beg)
    (if (not (idlwave-in-comment))
        ;; For code line add continuation.
        ;; Check if splitting a string.
        (progn
          (if (setq beg (idlwave-in-quote))
              (if idlwave-split-line-string
                  ;; Split the string.
                  (progn (insert (setq beg (char-after beg)) " + "
                                 idlwave-continuation-char beg)
                         (backward-char 1)
			 (newline-and-indent)
			 (forward-char 1))
                ;; Do not split the string.
                (beep)
                (message "Warning: continuation inside string!!")
                (insert " " idlwave-continuation-char))
            ;; Not splitting a string.
	    (if (not (member (char-before) '(?\  ?\t)))
		(insert " "))
            (insert idlwave-continuation-char)
	    (if (null noindent)
		(newline-and-indent)
	      (newline))))
      (indent-new-comment-line))
    ;; Indent previous line
    (when (null noindent)
      (setq beg (- (point-max) (point)))
      (forward-line -1)
      (idlwave-indent-line)
      (goto-char (- (point-max) beg))
      ;; Reindent new line
      (idlwave-indent-line))))

(defun idlwave-do-action (action)
  "Perform an action repeatedly on a line.
ACTION is a list (REG . FUNC).  REG is a regular expression.  FUNC is
either a function which will be called with one argument `is-action' or
a list to be evaluated with `eval'.
The action performed by FUNC should leave point after the match for REG
- otherwise an infinite loop may be entered.
FUNC is always passed a final argument of `is-action', so it
can discriminate between being run as an action, or a key binding."
  (let ((action-key (car action))
        (action-routine (cdr action)))
    (beginning-of-line)
    (while (idlwave-look-at action-key)
      (if (functionp action-routine)
          (funcall action-routine 'is-action)
        (eval (append action-routine '('is-action)) t)))))

(defun idlwave-indent-to (col &optional min)
  "Indent from point with spaces until column COL.
Inserts space before markers at point."
  (if (not min) (setq min 0))
  (insert-before-markers
   (make-string (max min (- col (current-column))) ?\ )))

(defun idlwave-indent-left-margin (col)
  "Indent the current line to column COL.
Indents such that first non-whitespace character is at column COL
Inserts spaces before markers at point."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (idlwave-indent-to col)))

(defun idlwave-indent-subprogram ()
  "Indent program unit which contains point."
  (interactive)
  (save-excursion
    (idlwave-end-of-statement)
    (idlwave-beginning-of-subprogram)
    (let ((beg (point)))
      (idlwave-forward-block)
      (message "Indenting subprogram...")
      (indent-region beg (point) nil))
    (message "Indenting subprogram...done.")))

(defun idlwave-indent-statement ()
  "Indent current statement, including all continuation lines."
  (interactive)
  (save-excursion
    (idlwave-beginning-of-statement)
    (let ((beg (point)))
      (idlwave-end-of-statement)
      (indent-region beg (point) nil))))

(defun idlwave-calculate-paren-indent (beg-reg end-reg close-exp)
  "Calculate the continuation indent inside a paren group.
Returns a cons-cell with (open . indent), where open is the
location of the open paren."
  (let ((open (nth 1 (parse-partial-sexp beg-reg end-reg))))
    ;; Found an innermost open paren.
    (when open
      (goto-char open)
      ;; Line up with next word unless this is a closing paren.
      (cons open
	    (cond
	     ;; This is a closed paren - line up under open paren.
	     (close-exp
	      (current-column))

	     ;; Plain Kernighan-style nested indent
	     (idlwave-indent-parens-nested
	      (+ idlwave-continuation-indent (current-column)))

	     ;; Empty (or just comment) follows -- revert to basic indent
	     ((progn
		;; Skip paren
		(forward-char 1)
		(looking-at "[ \t$]*\\(;.*\\)?$"))
	      nil)

	     ;; Line up with first word after any blank space
	     ((progn
		(skip-chars-forward " \t")
		(current-column))))))))

(defun idlwave-calculate-cont-indent ()
  "Calculates the IDL continuation indent column from the previous statement.
Note that here previous statement usually means the beginning of the
current statement if this statement is a continuation of the previous
line.  Various special types of continuations, including assignments,
routine definitions, and parenthetical groupings, are treated separately."
  (save-excursion
    (let* ((case-fold-search t)
           (end-reg (progn (beginning-of-line) (point)))
	   (beg-last-statement (save-excursion (idlwave-previous-statement)
					       (point)))
           (beg-reg (progn (idlwave-start-of-substatement 'pre)
			   (if (eq (line-beginning-position) end-reg)
			       (goto-char beg-last-statement)
			     (point))))
	   (basic-indent (+ (idlwave-min-current-statement-indent end-reg)
			    idlwave-continuation-indent))
	   fancy-nonparen-indent fancy-paren-indent)
      (cond
       ;; Align then with its matching if, etc.
       ((let ((matchers '(("\\<if\\>" . "[ \t]*then")
			  ("\\<\\(if\\|end\\(if\\)?\\)\\>" . "[ \t]*else")
			  ("\\<\\(for\\|while\\)\\>" . "[ \t]*do")
			  ("\\<\\(repeat\\|end\\(rep\\)?\\)\\>" .
			   "[ \t]*until")
			  ("\\<case\\>" . "[ \t]*of")))
	      match cont-re)
	  (goto-char end-reg)
	  (and
	   (setq cont-re
		 (catch 'exit
		   (while (setq match (car matchers))
		     (if (looking-at (cdr match))
			 (throw 'exit (car match)))
		     (setq matchers (cdr matchers)))))
	   (idlwave-find-key cont-re -1 'nomark beg-last-statement)))
	(if (looking-at "end") ;; that one's special
	    (- (idlwave-current-indent)
	       (+ idlwave-block-indent idlwave-end-offset))
	  (idlwave-current-indent)))

       ;; Indent in from the previous line for continuing statements
       ((let ((matchers '("\\<then\\>"
			  "\\<do\\>"
			  "\\<repeat\\>"
			  "\\<else\\>"))
	      match)
	  (catch 'exit
	    (goto-char end-reg)
	    (if (/= (forward-line -1) 0)
		(throw 'exit nil))
	    (while (setq match (car matchers))
	      (if (looking-at (concat ".*" match "[ \t]*\\$[ \t]*"
				      "\\(;.*\\)?$"))
		  (throw 'exit t))
	      (setq matchers (cdr matchers)))))
	(+ idlwave-continuation-indent (idlwave-current-indent)))

       ;; Parenthetical indent, either traditional or Kernighan style
       ((setq fancy-paren-indent
	      (let* ((end-reg end-reg)
		     (close-exp (progn
				  (goto-char end-reg)
				  (skip-chars-forward " \t")
				  (looking-at "\\s)")))
		     indent-cons)
		(catch 'loop
		  (while (setq indent-cons (idlwave-calculate-paren-indent
					    beg-reg end-reg close-exp))
		    ;; First permitted containing paren
		    (if (or
			 idlwave-indent-to-open-paren
			 idlwave-indent-parens-nested
                         (null (cdr indent-cons))
			 (< (- (cdr indent-cons) basic-indent)
			    idlwave-max-extra-continuation-indent))
			(throw 'loop (cdr indent-cons)))
		    (setq end-reg (car indent-cons))))))
	fancy-paren-indent)

       ;; A continued assignment, or procedure call/definition
       ((and
	 (> idlwave-max-extra-continuation-indent 0)
	 (setq fancy-nonparen-indent
	       (progn
		 (goto-char beg-reg)
		 (while (idlwave-look-at "&"))  ; skip continued statements
		 (cond
		  ;; A continued Procedure call or definition
		  ((progn
		     (idlwave-look-at "^[ \t]*\\(pro\\|function\\)") ;skip over
		     (looking-at "[ \t]*\\([a-zA-Z0-9.$_]+[ \t]*\\(->|\\.\\)[ \t]*\\)?[a-zA-Z][:a-zA-Z0-9$_]*[ \t]*\\(,\\)[ \t]*"))
		   (goto-char (match-end 0))
		   ;; Comment only, or blank line with "$"?  Basic indent.
		   (if (save-match-data (looking-at "[ \t$]*\\(;.*\\)?$"))
		       nil
		     (current-column)))

		  ;; Continued assignment (with =):
		  ((catch 'assign ;
		     (while (looking-at "[^=\n\r]*\\(=\\)[ \t]*")
		       (goto-char (match-end 0))
		       (if (null (idlwave-what-function beg-reg))
			   (throw 'assign t))))
		   (unless (or
			    (idlwave-in-quote)
			    (looking-at "[ \t$]*\\(;.*\\)?$") ; use basic
			    (save-excursion
			      (goto-char beg-last-statement)
			      (eq (caar (idlwave-statement-type)) 'for)))
		     (current-column))))))
	 (< (- fancy-nonparen-indent basic-indent)
	    idlwave-max-extra-continuation-indent))
	(if fancy-paren-indent ;calculated but disallowed paren indent
	    (+ fancy-nonparen-indent idlwave-continuation-indent)
	  fancy-nonparen-indent))

       ;; Basic indent, by default
       (t basic-indent)))))

(defun idlwave-find-key (key-re &optional dir nomark limit)
  "Move to next match of the regular expression KEY-RE.
Matches inside comments or string constants will be ignored.
If DIR is negative, the search will be backwards.
At a successful match, the mark is pushed unless NOMARK is non-nil.
Searches are limited to LIMIT.
Searches are case-insensitive and use a special syntax table which
treats `$' and `_' as word characters.
Return value is the beginning of the match or (in case of failure) nil."
  (setq dir (or dir 0))
  (let ((case-fold-search t)
	(search-func (if (> dir 0) 're-search-forward 're-search-backward))
	found)
    (with-syntax-table idlwave-find-symbol-syntax-table
     (save-excursion
       (catch 'exit
	 (while (funcall search-func key-re limit t)
	   (if (not (idlwave-quoted))
	       (throw 'exit (setq found (match-beginning 0)))
	     (if (or (and (> dir 0) (eobp))
		     (and (< dir 0) (bobp)))
		 (throw 'exit nil)))))))
    (if found
	(progn
	  (if (not nomark) (push-mark))
	  (goto-char found)
	  found)
      nil)))

(defun idlwave-min-current-statement-indent (&optional end-reg)
  "The minimum indent in the current statement."
  (idlwave-beginning-of-statement)
  (if (not (idlwave-is-continuation-line))
      (idlwave-current-indent)
    (let ((min (idlwave-current-indent)) comm-or-empty)
      (while (and (= (forward-line 1) 0)
		  (or (setq comm-or-empty (idlwave-is-comment-or-empty-line))
		      (idlwave-is-continuation-line))
		  (or (null end-reg) (< (point) end-reg)))
	(unless comm-or-empty (setq min (min min (idlwave-current-indent)))))
      (if (or comm-or-empty (and end-reg (>= (point) end-reg)))
	  min
	(min min (idlwave-current-indent))))))

(defun idlwave-current-statement-indent (&optional last-line)
  "Return indentation of the current statement.
If in a statement, moves to beginning of statement before finding indent."
  (if last-line
      (idlwave-end-of-statement)
    (idlwave-beginning-of-statement))
  (idlwave-current-indent))

(defun idlwave-current-indent ()
  "Return the column of the indentation of the current line.
Skips any whitespace.  Returns 0 if the end-of-line follows the whitespace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;; if we are at the end of blank line return 0
    (cond ((eolp) 0)
          ((current-column)))))

(defun idlwave-fill-paragraph (&optional nohang)
  "Fill paragraphs in comments.
A paragraph is made up of all contiguous lines having the same comment
leader (the leading whitespace before the comment delimiter and the
comment delimiter).  In addition, paragraphs are separated by blank
line comments.  The indentation is given by the hanging indent of the
first line, otherwise by the minimum indentation of the lines after
the first line.  The indentation of the first line does not change.
Does not effect code lines.  Does not fill comments on the same line
with code.  The hanging indent is given by the end of the first match
matching `idlwave-hang-indent-regexp' on the paragraph's first line.
If the optional argument NOHANG is non-nil then the hanging indent is
ignored."
  (interactive "P")
  ;; check if this is a line comment
  (if (save-excursion
        (beginning-of-line)
        (skip-chars-forward " \t")
        (looking-at comment-start))
      (let
          ((indent 999)
           first-indent hang here pre start end)
        ;; Change tabs to spaces in the surrounding paragraph.
        ;; The surrounding paragraph will be the largest containing block of
        ;; contiguous line comments. Thus, we may be changing tabs in
        ;; a much larger area than is needed, but this is the easiest
        ;; brute force way to do it.
        ;;
        ;; This has the undesirable side effect of replacing the tabs
        ;; permanently without the user's request or knowledge.
        (save-excursion
          (backward-paragraph)
          (setq start (point)))
        (save-excursion
          (forward-paragraph)
          (setq end (point)))
        (untabify start end)
        ;;
	(setq here (point)
	      start (idlwave-commented-paragraph-beg-end)
	      end (cadr start)
	      pre (car (cddr start))
	      start (car start))
        ;;
        ;; Calculate the indentation for the paragraph.
        ;;
        ;; In the following while statements, after one iteration
        ;; point will be at the beginning of a line in which case
        ;; the while will not be executed for the
        ;; first paragraph line and thus will not affect the
        ;; indentation.
        ;;
        ;; First check to see if indentation is based on hanging indent.
        (if (and (not nohang) idlwave-hanging-indent
                 (setq hang
                       (save-excursion
                         (goto-char start)
                         (idlwave-calc-hanging-indent))))
            ;; Adjust lines of paragraph by inserting spaces so that
            ;; each line's indent is at least as great as the hanging
            ;; indent. This is needed for fill-paragraph to work with
            ;; a fill-prefix.
            (progn
              (setq indent hang)
              (beginning-of-line)
              (while (> (point) start)
                (re-search-forward comment-start-skip (line-end-position) t)
                (let ((diff (- indent (current-column))))
                  (when (> diff 0)
                    (if (>= here (point))
                        ;; adjust the original location for the
                        ;; inserted text.
                        (setq here (+ here diff)))
                    (insert (make-string diff ?\ ))))
                (forward-line -1))
              )

          ;; No hang. Instead find minimum indentation of paragraph
          ;; after first line.
          ;; For the following while statement, since START is at the
          ;; beginning of line and END is at the end of line
          ;; point is greater than START at least once (which would
          ;; be the case for a single line paragraph).
          (while (> (point) start)
            (beginning-of-line)
            (setq indent
                  (min indent
                       (progn
                         (re-search-forward comment-start-skip (line-end-position) t)
                         (current-column))))
            (forward-line -1)))
        (setq fill-prefix (concat fill-prefix
                                  (make-string (- indent pre)
                                               ?\ )))
        ;; first-line indent
        (setq first-indent
              (max
               (progn
                 (re-search-forward comment-start-skip (line-end-position) t)
                 (current-column))
               indent))

        ;; try to keep point at its original place
        (goto-char here)

        ;; In place of the more modern fill-region-as-paragraph, a hack
        ;; to keep whitespace untouched on the first line within the
        ;; indent length and to preserve any indent on the first line
        ;; (first indent).
        (save-excursion
          (let ((diff
                 (buffer-substring start (+ start first-indent -1))))
            (subst-char-in-region start (+ start first-indent -1) ?\  ?~ nil)
            (fill-region-as-paragraph
             start
             (- (point-max) end)
             (current-justification)
             nil)
            (delete-region start (+ start first-indent -1))
            (goto-char start)
            (insert diff)))
        ;; When we want the point at the beginning of the comment
        ;; body fill-region will put it at the beginning of the line.
        (if (bolp) (skip-chars-forward (concat " \t" comment-start)))
        (setq fill-prefix nil))))

(defun idlwave-calc-hanging-indent ()
  "Calculate the position of the hanging indent for the comment paragraph.
The hanging indent position is given by the first match with the
`idlwave-hang-indent-regexp'.  If `idlwave-use-last-hang-indent' is
non-nil then use last occurrence matching `idlwave-hang-indent-regexp'
on the line.
If not found returns nil."
  (if idlwave-use-last-hang-indent
      (save-excursion
        (end-of-line)
        (if (re-search-backward idlwave-hang-indent-regexp (line-beginning-position) t)
            (+ (current-column) (length idlwave-hang-indent-regexp))))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward idlwave-hang-indent-regexp (line-end-position) t)
          (current-column)))))

(defun idlwave-auto-fill ()
  "Called to break lines in auto fill mode.
Only fills non-comment lines if `idlwave-fill-comment-line-only'
is nil (it is t by default).  Places a continuation character at
the end of the line if not in a comment.  Splits strings with IDL
concatenation operator `+' if `idlwave-auto-fill-split-string' is
non-nil."
  (if (<= (current-column) fill-column)
      nil                             ; do not to fill
    (if (or (not idlwave-fill-comment-line-only)
	    (save-excursion
	      ;; Check for comment line
	      (beginning-of-line)
	      (looking-at idlwave-comment-line-start-skip)))
	(let (beg)
	  (idlwave-indent-line)
	  ;; Prevent actions do-auto-fill which calls indent-line-function.
	  (let (idlwave-do-actions
		(fill-nobreak-predicate
		 (if (and (idlwave-in-quote)
			  idlwave-auto-fill-split-string)
		     (lambda () ;; We'll need 5 spaces for " ' + $"
		       (<= (- fill-column (current-column)) 5)
		       ))))
	    (do-auto-fill))
	  (save-excursion
	    (end-of-line 0)
	    ;; Indent the split line
	    (idlwave-indent-line))
	  (if (save-excursion
		(beginning-of-line)
		(looking-at idlwave-comment-line-start-skip))
	      ;; A continued line comment
	      ;; We treat continued line comments as part of a comment
	      ;; paragraph. So we check for a hanging indent.
	      (if idlwave-hanging-indent
		  (let ((here (- (point-max) (point)))
			(indent
			 (save-excursion
			   (goto-char
			    (car (idlwave-commented-paragraph-beg-end)))
			   (idlwave-calc-hanging-indent))))
		    (when indent
		      ;; Remove whitespace between comment delimiter and
		      ;; text, insert spaces for appropriate indentation.
		      (beginning-of-line)
                      (re-search-forward comment-start-skip (line-end-position) t)
		      (delete-horizontal-space)
		      (idlwave-indent-to indent)
		      (goto-char (- (point-max) here)))))
	    ;; Split code or comment?
	    (if (save-excursion
		  (end-of-line 0)
		  (idlwave-in-comment))
		;; Splitting a non-full-line comment.
		;; Insert the comment delimiter from split line
		(progn
		  (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    ;; Insert blank to keep off beginning of line
		    (insert " "
			    (save-excursion
			      (forward-line -1)
			      (buffer-substring (idlwave-goto-comment)
						(progn
						  (skip-chars-forward "; ")
						  (point))))))
		  (idlwave-indent-line))
	      ;; Split code line - add continuation character
	      (save-excursion
		(end-of-line 0)
		;; Check to see if we split a string
		(if (and (setq beg (idlwave-in-quote))
			 idlwave-auto-fill-split-string)
		    ;; Split the string and concatenate.
		    ;; The first extra space is for the space
		    ;; the line was split. That space was removed.
		    (insert " " (char-after beg) " +"))
		(insert " $"))
	      (if beg
		  (if idlwave-auto-fill-split-string
		      ;; Make the second part of continued string
		      (save-excursion
			(beginning-of-line)
			(skip-chars-forward " \t")
			(insert (char-after beg)))
		    ;; Warning
		    (beep)
		    (message "Warning: continuation inside a string.")))
	      ;; Although do-auto-fill (via indent-new-comment-line) calls
	      ;; idlwave-indent-line for the new line, re-indent again
	      ;; because of the addition of the continuation character.
	      (idlwave-indent-line))
	    )))))

(define-obsolete-function-alias 'idlwave-auto-fill-mode #'auto-fill-mode "28.1")

;;----------------------------------------------------
;; Space padding

(defun idlwave-expand-equal (&optional before after _is-action)
  "Pad `=' with spaces.
Two cases: Assignment statement, and keyword assignment.
Which case is determined using `idlwave-start-of-substatement' and
`idlwave-statement-type'.  The equal sign will be surrounded by BEFORE
and AFTER blanks.  If `idlwave-pad-keyword' is t then keyword assignment
is treated just like assignment statements.  When nil, spaces are
removed for keyword assignment.  Any other value keeps the current space
around the `='.  Limits in for loops are treated as keyword assignment.

Starting with IDL 6.0, a number of op= assignments are available.
Since ambiguities of the form:

r and= b
rand= b

can occur, alphanumeric operator assignment will never be pre-padded,
only post-padded.  You must use a space before these to disambiguate
\(not just for padding, but for proper parsing by IDL too!).  Other
operators, such as ##=, ^=, etc., will be pre-padded.

IS-ACTION is ignored.

See `idlwave-surround'."
  (if idlwave-surround-by-blank
      (let
	  ((non-an-ops "\\(##\\|\\*\\|\\+\\|-\\|/\\|<\\|>\\|\\^\\)\\=")
	   (an-ops
	    "\\s-\\(AND\\|EQ\\|GE\\|GT\\|LE\\|LT\\|MOD\\|NE\\|OR\\|XOR\\)\\=")
	   (len 1))

	(save-excursion
	  (let ((case-fold-search t))
	    (backward-char)
	    (if (or
		 (re-search-backward non-an-ops nil t)
		 ;; Why doesn't ##? work for both?
		 (re-search-backward "\\(#\\)\\=" nil t))
		(setq len (1+ (length (match-string 1))))
	      (when (re-search-backward an-ops nil t)
					;(setq begin nil) ; won't modify begin
		(setq len (1+ (length (match-string 1))))))))

	(if (eq t idlwave-pad-keyword)
	    ;; Everything gets padded equally
	    (idlwave-surround before after len)
	  ;; Treating keywords/for variables specially...
	  (let ((st (save-excursion   ; To catch "for" variables
		      (idlwave-start-of-substatement t)
		      (idlwave-statement-type)))
		(what (save-excursion ; To catch keywords
			(skip-chars-backward "= \t")
			(nth 2 (idlwave-where)))))
	    (cond ((or (memq what '(function-keyword procedure-keyword))
		       (memq (caar st) '(for pdef)))
		   (cond
		    ((null idlwave-pad-keyword)
		     (idlwave-surround 0 0)
		     ) ; remove space
		    (t))) ; leave any spaces alone
		  (t (idlwave-surround before after len))))))))

(defun idlwave-surround (&optional before after length _is-action)
  "Surround the LENGTH characters before point with blanks.
LENGTH defaults to 1.
Optional arguments BEFORE and AFTER affect the behavior before and
after the characters (see also description of `idlwave-make-space'):

nil            do nothing
0              force no spaces
integer > 0    force exactly n spaces
integer < 0    at least |n| spaces

The function does nothing if any of the following conditions is true:
- `idlwave-surround-by-blank' is nil
- the character before point is inside a string or comment"
  (when (and idlwave-surround-by-blank (not (idlwave-quoted)))
    (let ((length (or length 1))) ; establish a default for LENGTH
      (backward-char length)
      (save-restriction
	(let ((here (point)))
	  (skip-chars-backward " \t")
	  (if (bolp)
	      ;; avoid clobbering indent
	      (progn
		(move-to-column (idlwave-calculate-indent))
		(if (<= (point) here)
		    (narrow-to-region (point) here))
		(goto-char here)))
	  (idlwave-make-space before))
	(skip-chars-forward " \t"))
      (forward-char length)
      (idlwave-make-space after)
      ;; Check to see if the line should auto wrap
      (if (and (equal (char-after (1- (point))) ?\ )
	       (> (current-column) fill-column))
	  (funcall auto-fill-function)))))

(defun idlwave-custom-ampersand-surround (&optional is-action)
  "Surround &, leaving room for && (which surround as well)."
  (let* ((prev-char (char-after (- (point) 2)))
	 (next-char (char-after (point)))
	 (amp-left (eq prev-char ?&))
	 (amp-right (eq next-char ?&))
	 (len (if amp-left 2 1)))
    (unless amp-right ;no need to do it twice, amp-left will catch it.
      (idlwave-surround -1 (if (or is-action amp-left) -1) len))))

(defun idlwave-custom-ltgtr-surround (gtr &optional is-action)
  "Surround > and < by blanks, leaving room for >= and <=, and considering ->."
  (let* ((prev-char (char-after (- (point) 2)))
	(next-char (char-after (point)))
	(method-invoke (and gtr (eq prev-char ?-)))
	(len (if method-invoke 2 1)))
    (unless (eq next-char ?=)
      ;; Key binding: pad only on left, to save for possible >=/<=
      (idlwave-surround -1 (if (or is-action method-invoke) -1) len))))

(defun idlwave-make-space (n)
  "Make space at point.
The space affected is all the spaces and tabs around point.
If n is non-nil then point is left abs(n) spaces from the beginning of
the contiguous space.
The amount of space at point is determined by N.
If the value of N is:
nil   - do nothing.
> 0   - exactly N spaces.
< 0   - a minimum of -N spaces, i.e., do not change if there are
        already -N spaces.
0     - no spaces (i.e. remove any existing space)."
  (if (integerp n)
      (let
          ((start-col (progn (skip-chars-backward " \t") (current-column)))
           (left (point))
           (end-col (progn (skip-chars-forward " \t") (current-column))))
        (delete-horizontal-space)
        (cond
         ((> n 0)
          (idlwave-indent-to (+ start-col n))
          (goto-char (+ left n)))
         ((< n 0)
          (idlwave-indent-to end-col (- n))
          (goto-char (- left n)))
         ;; n = 0, done
         ))))

;;----------------------------------------------------
;; In-source Documentation ("Doclib")

(defun idlwave-doc-header (&optional nomark)
  "Insert a documentation header at the beginning of the unit.
Inserts the value of the variable `idlwave-file-header'.  Sets mark
before moving to do insertion unless the optional prefix argument
NOMARK is non-nil."
  (interactive "P")
  (or nomark (push-mark))
  ;; make sure we catch the current line if it begins the unit
  (if idlwave-header-to-beginning-of-file
      (goto-char (point-min))
    (end-of-line)
    (idlwave-beginning-of-subprogram)
    (beginning-of-line)
    ;; skip function or procedure line
    (if (idlwave-look-at "\\<\\(pro\\|function\\)\\>")
	(progn
	  (idlwave-end-of-statement)
	  (if (> (forward-line 1) 0) (insert "\n")))))
  (let ((pos (point)))
    (if idlwave-file-header
	(cond ((car idlwave-file-header)
	       (insert-file-contents (car idlwave-file-header)))
	      ((stringp (car (cdr idlwave-file-header)))
	       (insert (car (cdr idlwave-file-header))))))
    (goto-char pos)))

(defun idlwave-default-insert-timestamp ()
  "Default timestamp insertion function."
  (insert (current-time-string))
  (insert ", " (user-full-name))
  (if (boundp 'user-mail-address)
      (insert " <" user-mail-address ">")
    (insert " <" (user-login-name) "@" (system-name) ">"))
  ;; Remove extra spaces from line
  (idlwave-fill-paragraph)
  ;; Insert a blank line comment to separate from the date entry -
  ;; will keep the entry from flowing onto date line if re-filled.
  (insert "\n;\n;\t\t"))

(defun idlwave-doc-modification ()
  "Insert a brief modification log at the beginning of the current program.
Looks for an occurrence of the value of user variable
`idlwave-doc-modifications-keyword' if non-nil.  Inserts time and user
name and places the point for the user to add a log.  Before moving, saves
location on mark ring so that the user can return to previous point."
  (interactive)
  (push-mark)
  (let* (beg end)
    (if (and (or (re-search-backward idlwave-doclib-start nil t)
		 (progn
		   (goto-char (point-min))
		   (re-search-forward idlwave-doclib-start nil t)))
	     (setq beg (match-beginning 0))
	     (re-search-forward idlwave-doclib-end nil t)
	     (setq end (match-end 0)))
	(progn
	  (goto-char beg)
	  (if (re-search-forward
	       (concat idlwave-doc-modifications-keyword ":")
	       end t)
	      (end-of-line)
	    (goto-char end)
	    (end-of-line -1)
	    (insert "\n" comment-start "\n")
	    (insert comment-start " " idlwave-doc-modifications-keyword ":"))
	  (insert "\n;\n;\t")
	  (run-hooks 'idlwave-timestamp-hook))
      (error "No valid DOCLIB header"))))

(defun idlwave-mark-doclib ()
  "Put point at beginning of doc library header, mark at end.
The marks are pushed."
  (interactive)
  (let (beg
        (here (point)))
    (goto-char (point-max))
    (if (re-search-backward idlwave-doclib-start nil t)
        (progn
	  (setq beg (progn (beginning-of-line) (point)))
	  (if (re-search-forward idlwave-doclib-end nil t)
	      (progn
		(forward-line 1)
		(push-mark beg nil t)
		(message "Could not find end of doc library header.")))
	  (message "Could not find doc library header start.")
	  (goto-char here)))))

;;----------------------------------------------------
;; Information on surrounding context
;; (see also idlw-routine for local routine context)

(defun idlwave-statement-type ()
  "Return the type of the current IDL statement.
Uses `idlwave-statement-match' to return a cons of (type . point) with
point the ending position where the type was determined.  Type is the
association from `idlwave-statement-match', i.e. the cons cell from the
list not just the type symbol.  Returns nil if not an identifiable
statement."
  (save-excursion
    ;; Skip whitespace within a statement which is spaces, tabs, continuations
    ;; and possibly comments
    (while (looking-at "[ \t]*\\$")
      (forward-line 1))
    (skip-chars-forward " \t")
    (let ((st idlwave-statement-match)
          (case-fold-search t))
      (while (and (not (looking-at (nth 0 (cdr (car st)))))
                  (setq st (cdr st))))
      (if st
          (append st (match-end 0))))))

(defun idlwave-is-continuation-line ()
  "Test if current line is continuation line.
Blank or comment-only lines following regular continuation lines (with
`$') count as continuations too."
  (let (p)
    (save-excursion
      (or
       (idlwave-look-at "\\<\\$")
       (catch 'loop
	 (while (and (looking-at "^[ \t]*\\(;.*\\)?$")
		     (eq (forward-line -1) 0))
	   (if (setq p (idlwave-look-at "\\<\\$")) (throw 'loop p))))))))

(defun idlwave-is-comment-line ()
  "Test if the current line is a comment line."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*;")))

(defun idlwave-is-comment-or-empty-line ()
  "Test if the current line is a comment line."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*[;\n]")))

(defun idlwave-quoted ()
  "Return t if point is in a comment or quoted string.
Returns nil otherwise."
  ;; FIXME: Use (nth 8 (synx-ppss))!
  (and (or (idlwave-in-comment) (idlwave-in-quote)) t))

(defun idlwave-in-quote ()
  "Return location of the opening quote
if point is in a IDL string constant, nil otherwise.
Ignores comment delimiters on the current line.
Properly handles nested quotation marks and octal
constants - a double quote followed by an octal digit."
  ;; Treat an octal inside an apostrophe to be a normal string. Treat a
  ;; double quote followed by an octal digit to be an octal constant
  ;; rather than a string. Therefore, there is no terminating double
  ;; quote.
  (save-excursion
    ;; Because single and double quotes can quote each other we must
    ;; search for the string start from the beginning of line.
    (let* ((start (point))
           (eol (line-end-position))
           (bq (progn (beginning-of-line) (point)))
           (endq (point))
           (data (match-data))
           delim
           found)
      (while  (< endq start)
	;; Find string start
	;; Don't find an octal constant beginning with a double quote
	(if (re-search-forward "[\"']" eol 'lim)
	    ;; Find the string end.
	    ;; In IDL, two consecutive delimiters after the start of a
	    ;; string act as an
	    ;; escape for the delimiter in the string.
	    ;; Two consecutive delimiters alone (i.e., not after the
	    ;; start of a string) is the null string.
	    (progn
	      ;; Move to position after quote
	      (goto-char (1+ (match-beginning 0)))
	      (setq bq (1- (point)))
	      ;; Get the string delimiter
	      (setq delim (char-to-string (preceding-char)))
	      ;; Check for null string
	      (if (looking-at delim)
		  (progn (setq endq (point)) (forward-char 1))
		;; Look for next unpaired delimiter
		(setq found (search-forward delim eol 'lim))
		(while (looking-at delim)
		  (forward-char 1)
		  (setq found (search-forward delim eol 'lim)))
		(setq endq (if found (1- (point)) (point)))
		))
	  (progn (setq bq (point)) (setq endq (point)))))
      (store-match-data data)
      ;; return string beginning position or nil
      (if (> start bq) bq))))

(defun idlwave-is-pointer-dereference (&optional limit)
  "Determine if the character after point is a pointer dereference *."
  (and
   (eq (char-after) ?\*)
   (not (idlwave-in-quote))
   (save-excursion
     (forward-char)
     (re-search-backward (concat "\\(" idlwave-idl-keywords
                                 "\\|[-[(*+/=,^><]\\)\\s-*\\*")
                         limit t))))

(defun idlwave-in-comment ()
  "Return t if point is inside a comment, nil otherwise."
  (save-excursion
    (let ((here (point)))
      (and (idlwave-goto-comment) (> here (point))))))

(defun idlwave-in-structure ()
  "Return t if point is inside an IDL structure definition."
  (let ((beg (point)))
    (save-excursion
      (if (not (or (idlwave-in-comment) (idlwave-in-quote)))
	  (if (idlwave-find-structure-definition nil nil 'back)
	      (let ((borders (idlwave-struct-borders)))
		(or (= (car borders) (cdr borders)) ;; struct not yet closed...
		    (and (> beg (car borders)) (< beg (cdr borders))))))))))

;;----------------------------------------------------
;; Utility: file/path

(defun idlwave-sys-dir ()
  "Return the syslib directory, or a dummy that never matches."
  (cond
   ((and idlwave-system-directory
	 (not (string= idlwave-system-directory "")))
    idlwave-system-directory)
   ((getenv "IDL_DIR"))
   (t "@@@@@@@@")))

(defun idlwave-recursive-find-file (dir file)
  (catch 'found
    (let (found)
      (mapc (lambda (name)
	      (if (file-directory-p (concat dir "/" name)) ;directory
		  (when (not (string= (substring name 0 1) "."))
		    (setq found
			  (idlwave-recursive-find-file
			   (concat dir "/" name) file))
		    (if found (throw 'found found)))
		(if (string= name file)
		    (throw 'found (concat dir "/" name)))))
	    (directory-files dir))
    nil)))

(defun idlwave-expand-path (path &optional default-dir)
  ;; Expand parts of path starting with '+' recursively into directory list.
  ;; Relative recursive path elements are expanded relative to DEFAULT-DIR.
  (message "Expanding path...")
  (let (path1 dir recursive)
    (while (setq dir (pop path))
      (if (setq recursive (string= (substring dir 0 1) "+"))
	  (setq dir (substring dir 1)))
      (if (and recursive
	       (not (file-name-absolute-p dir)))
	  (setq dir (expand-file-name dir default-dir)))
      (if recursive
	  ;; Expand recursively
	  (setq path1 (append (idlwave-recursive-directory-list dir) path1))
	;; Keep unchanged
	(push dir path1)))
    (message "Expanding path...done")
    (nreverse path1)))

(defun idlwave-recursive-directory-list (dir)
  ;; Return a list of all directories below DIR, including DIR itself
  (let ((path (list dir)) path1 file files)
    (while (setq dir (pop path))
      (when (file-directory-p dir)
	(setq files (nreverse (directory-files dir t "[^.]")))
	(while (setq file (pop files))
	  (if (file-directory-p file)
	      (push (file-name-as-directory file) path)))
	(push dir path1)))
    path1))

(define-obsolete-function-alias 'idlwave-get-buffer-visiting
  #'find-buffer-visiting "28.1")

;;----------------------------------------------------
;;  Utility: string

(defun idlwave-split-string (string &optional pattern)
  "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

(defun idlwave-replace-string (string replace_string replace_with)
  (let* ((start 0)
	 (last (length string))
	 (ret_string "")
	 end)
    (while (setq end (string-match replace_string string start))
      (setq ret_string
	    (concat ret_string (substring string start end) replace_with))
      (setq start (match-end 0)))
    (setq ret_string (concat ret_string (substring string start last)))))

(defun idlwave-downcase-safe (string)
  "Downcase if string, else return unchanged."
  (if (stringp string)
      (downcase string)
    string))

;;----------------------------------------------------
;; Utility: list/value

(defun idlwave-set-local (var value &optional buffer)
  "Set the buffer-local value of VAR in BUFFER to VALUE."
  (with-current-buffer (or buffer (current-buffer))
    (set (make-local-variable var) value)))

(defun idlwave-local-value (var &optional buffer)
  "Return the value of VAR in BUFFER, but only if VAR is local to BUFFER."
  (when (local-variable-p var buffer)
    (buffer-local-value var (or buffer (current-buffer)))))

(defun idlwave-members-only (list club)
  "Return list of all elements in LIST which are also in CLUB."
  (let (rtn)
    (while list
      (if (member (car list) club)
	  (setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-nonmembers-only (list club)
  "Return list of all elements in LIST which are not in CLUB."
  (let (rtn)
    (while list
      (if (member (car list) club)
	  nil
	(setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(define-obsolete-function-alias 'idlwave-uniquify #'seq-uniq "28.1")

(defun idlwave-count-eq (elt list)
  "How often is ELT in LIST?"
  (declare (obsolete nil "30.1"))
  (seq-count (lambda (x) (eq x elt)) list))

(defun idlwave-count-memq (elt alist)
  "How often is ELT a key in ALIST?"
  (seq-count (lambda (x) (eq (car x) elt)) alist))

(define-obsolete-function-alias 'idlwave-xor #'xor "27.1")

(defun idlwave-all-assq (key list)
  "Return a list of all associations of Key in LIST."
  (let (rtn elt)
    (while (setq elt (assq key list))
      (push elt rtn)
      (setq list (cdr (memq elt list))))
    (nreverse rtn)))

;;----------------------------------------------------
;; Utility: file/library status

(defun idlwave-syslib-p (file)
  "Non-nil if FILE is in the system library."
  (let* ((true-syslib (file-name-as-directory
		       (file-truename
			(expand-file-name "lib" (idlwave-sys-dir)))))
	 (true-file (file-truename file)))
    (string-match (concat "^" (regexp-quote true-syslib)) true-file)))

(defun idlwave-lib-p (file)
  "Non-nil if FILE is in the library."
  (let ((true-dir (file-name-directory (file-truename file))))
    (assoc true-dir (idlwave-true-path-alist))))

(defun idlwave-locate-lib-file (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  (let* ((dirs idlwave-path-alist)
	 dir efile)
    (catch 'exit
      (while (setq dir (car (pop dirs)))
	(if (file-regular-p
	     (setq efile (expand-file-name file dir)))
	    (throw 'exit efile))))))

(defun idlwave-expand-lib-file-name (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  ;; This is for, e.g., finding source with no user catalog
  (cond
   ((null file) nil)
   ((file-name-absolute-p file) file)
   (t (idlwave-locate-lib-file file))))

;;----------------------------------------------------
;; Utility: misc

(define-obsolete-function-alias 'idlwave-region-active-p #'use-region-p "23.1")

(defun idlwave-hard-tab ()
  "Insert TAB in buffer in current position."
  (interactive)
  (insert "\t"))

(defun idlwave-look-at (regexp &optional cont beg)
  "Search current line from current point for REGEXP.
If optional argument CONT is non-nil, searches to the end of
the current statement.
If optional arg BEG is non-nil, search starts from the beginning of the
current statement.
Ignores matches that end in a comment or inside a string expression.
Returns point if successful, nil otherwise.
This function produces unexpected results if REGEXP contains quotes or
a comment delimiter.  The search is case insensitive.
If successful, leaves point after the match, otherwise, does not move point."
  (let ((here (point))
        (case-fold-search t)
        (eos (save-excursion
	       (if cont (idlwave-end-of-statement) (end-of-line))
	       (point)))
        found)
    (with-syntax-table idlwave-find-symbol-syntax-table
     (if beg (idlwave-beginning-of-statement))
     (while (and (setq found (re-search-forward regexp eos t))
		 (idlwave-quoted))))
    (if (not found) (goto-char here))
    found))

;;----------------------------------------------------
;; Module visit

(defun idlwave-find-module-this-file ()
  (interactive)
  (idlwave-find-module '(4)))

(defun idlwave-find-module (&optional arg)
  "Find the source code of an IDL module.
Works for modules for which IDLWAVE has routine info available.
The function offers as default the module name `idlwave-routine-info'
would use.  With ARG limit to this buffer.  With two prefix ARG's
force class query for object methods."
  (interactive "P")
  (let* ((idlwave-query-class nil)
	 (idlwave-force-class-query (equal arg '(16)))
	 (this-buffer (equal arg '(4)))
	 (module (idlwave-fix-module-if-obj_new (idlwave-what-module)))
	 (default (if module
		      (concat (idlwave-make-full-name
			       (nth 2 module) (car module))
			      (if (eq (nth 1 module) 'pro) "<p>" "<f>"))
		    "none"))
	 (list
	  (seq-uniq
	   (delq nil
		 (mapcar (lambda (x)
			   (if (eq 'system (car-safe (nth 3 x)))
			       ;; Take out system routines with no source.
			       nil
			     (list
			      (concat (idlwave-make-full-name
				       (nth 2 x) (car x))
				      (if (eq (nth 1 x) 'pro) "<p>" "<f>")))))
			 (if this-buffer
			     (idlwave-save-buffer-update)
			   (idlwave-routines))))))
	 (name (idlwave-completing-read
		(if (or (not this-buffer)
			(assoc default list))
		    (format-prompt "Module" default)
		  "Module in this file: ")
		list))
	 type class)
    (if (string-match "\\`\\s-*\\'" name)
	;; Nothing, use the default.
	(setq name default))
    (if (string-match "<[fp]>" name)
	(setq type (substring name -2 -1)
	      name (substring name 0 -3)))
    (if (string-match "\\(.*\\)::\\(.*\\)" name)
	(setq class (match-string 1 name)
	      name (match-string 2 name)))
    (setq name (idlwave-sintern-routine-or-method name class)
	  class (idlwave-sintern-class class)
	  type (cond ((equal type "f") 'fun)
		     ((equal type "p") 'pro)
		     (t t)))
    (idlwave-do-find-module name type class nil this-buffer)))

(defun idlwave-do-find-module (name type class
				    &optional force-source this-buffer)
  (let ((name1 (idlwave-make-full-name class name))
	source buf1 entry
	(buf (current-buffer))
	(pos (point))
	file name2)
    (setq entry (idlwave-best-rinfo-assq name type class (idlwave-routines)
					 'WITH-FILE)
	  source (or force-source (nth 3 entry))
	  name2 (if (nth 2 entry)
		    (idlwave-make-full-name (nth 2 entry) name)
		  name1))
    (if source
	(setq file (idlwave-routine-source-file source)))
    (unless file  ; Try to find it on the path.
      (setq file
	    (idlwave-expand-lib-file-name
	     (if class
		 (format "%s__define.pro" (downcase class))
	       (format "%s.pro" (downcase name))))))
    (cond
     ((or (null name) (equal name ""))
      (error "Abort"))
     ((eq (car source) 'system)
      (error "Source code for system routine %s is not available"
	     name2))
     ((or (not file) (not (file-regular-p file)))
      (error "Source code for routine %s is not available"
	     name2))
     (t
      (when (not this-buffer)
	(setq buf1
	      (idlwave-find-file-noselect file 'find))
	(pop-to-buffer buf1 t))
      (goto-char (point-max))
      (let ((case-fold-search t))
	(if (re-search-backward
	     (concat "^[ \t]*\\<"
		     (cond ((eq type 'fun) "function")
			   ((eq type 'pro) "pro")
			   (t "\\(pro\\|function\\)"))
		     "\\>[ \t]+"
		     (regexp-quote (downcase name2))
		     "[^a-zA-Z0-9_$]")
	     nil t)
	    (goto-char (match-beginning 0))
	  (pop-to-buffer buf)
	  (goto-char pos)
	  (error "Could not find routine %s" name2)))))))

(defun idlwave-edit-in-idlde ()
  "Edit the current file in IDL Development environment."
  (interactive)
  (start-process "idlde" nil
		 idlwave-shell-explicit-file-name "-c" "-e"
                 (buffer-file-name)))

;;----------------------------------------------------
;; Help/info from context

(defun idlwave-mouse-context-help (ev &optional arg)
  "Call `idlwave-context-help' on the clicked location."
  (interactive "eP")
  (mouse-set-point ev)
  (idlwave-context-help arg))

(defvar idlwave-last-context-help-pos nil)
(defun idlwave-context-help (&optional arg)
  "Display IDL Online Help on context.
If point is on a keyword, help for that keyword will be shown.
If point is on a routine name or in the argument list of a
routine, help for that routine will be displayed.  Works for
system routines and keywords, it pulls up text help.  For other
routines and keywords, visits the source file, finding help in
the header (if `idlwave-help-source-try-header' is non-nil) or
the routine definition itself."
  (interactive "P")
  (idlwave-do-context-help arg))

(defun idlwave-mouse-completion-help (ev)
  "Display online help about the completion at point."
  (interactive "eP")
  ;; Restore last-command for next command, to make
  ;; scrolling/canceling of completions work.
  (setq this-command last-command)
  (idlwave-do-mouse-completion-help ev))

(defun idlwave-quit-help ()
  (interactive)
  (let ((ri-window (get-buffer-window "*Help*"))
	(olh-window (get-buffer-window "*IDLWAVE Help*")))
    (when (and olh-window
	       (fboundp 'idlwave-help-quit))
      (select-window olh-window)
      (idlwave-help-quit))
    (when (window-live-p ri-window)
      (quit-window nil ri-window))))

;;----------------------------------------------------
;; Widget/Interaction

(defun idlwave-popup-select (ev list title &optional sort)
  "Select an item in LIST with a popup menu.
TITLE is the title to put atop the popup.  If SORT is non-nil,
sort the list before displaying."
  (let ((maxpopup idlwave-max-popup-menu-items)
	rtn menu)
    (cond ((null list))
	  ((= 1 (length list))
	   (setq rtn (car list)))
	  (t
	   (if sort (setq list (sort list (lambda (a b)
					    (string< (upcase a) (upcase b))))))
	   (setq menu (cons title
			    (list
			     (append (list "")
				     (mapcar (lambda(x) (cons x x)) list)))))
	   (setq menu (idlwave-split-menu menu maxpopup))
	   (setq rtn (x-popup-menu ev menu))))
    rtn))

(define-obsolete-function-alias 'idlwave-split-menu-emacs
  #'idlwave-split-menu "28.1")

(defun idlwave-split-menu (menu N)
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

(defun idlwave-one-key-select (sym prompt delay)
  "Make the user select an element from the alist in the variable SYM.
The keys of the alist are expected to be strings.  The function returns the
car of the selected association.
To do this, PROMPT is displayed and the user must hit a letter key to
select an entry.  If the user does not reply within DELAY seconds, a help
window with the options is displayed automatically.
The key which is associated with each option is generated automatically.
First, the strings are checked for preselected keys, like in \"[P]rint\".
If these don't exist, a letter in the string is automatically selected."
  (let* ((alist (symbol-value sym))
         (temp-buffer-show-hook '(fit-window-to-buffer))
         keys-alist char)
    ;; First check the cache
    (if (and (eq (symbol-value sym) (get sym :one-key-alist-last)))
        (setq keys-alist (get sym :one-key-alist-cache))
      ;; Need to make new list
      (setq keys-alist (idlwave-make-one-key-alist alist))
      (put sym :one-key-alist-cache keys-alist)
      (put sym :one-key-alist-last alist))
    ;; Display prompt and wait for quick reply
    (message "%s[%s]" prompt
             (mapconcat (lambda(x) (char-to-string (car x)))
                        keys-alist))
    (if (sit-for delay)
        ;; No quick reply: Show help
        (save-window-excursion
          (with-output-to-temp-buffer "*Completions*"
	    (dolist (x keys-alist)
	      (princ (nth 1 x))
	      (princ "\n")))
          (setq char (read-char)))
      (setq char (read-char)))
    (message nil)
    ;; Return the selected result
    (nth 2 (assoc char keys-alist))))

(defun idlwave-make-one-key-alist (alist)
  "Make an alist for single key selection."
  ;; Used for, e.g., electric debug super-examine.
  (let ((l alist) keys-alist name start char help
        (cnt 0)
        (case-fold-search nil))
    (while l
      (setq name (car (car l))
            l (cdr l))
      (catch 'exit
        ;; First check if the configuration predetermined a key
        (if (string-match "\\[\\(.\\)\\]" name)
            (progn
              (setq char (string-to-char (downcase (match-string 1 name)))
                    help (format "%c:  %s" char name)
                    keys-alist (cons (list char help name) keys-alist))
              (throw 'exit t)))
        ;; Then check for capital letters
        (setq start 0)
        (while (string-match "[A-Z]" name start)
          (setq start (match-end 0)
                char (string-to-char (downcase (match-string 0 name))))
          (if (not (assoc char keys-alist))
              (progn
                (setq help (format "%c:  %s" char
                                   (replace-match
                                    (concat "[" (match-string 0 name) "]")
                                          t t name))
                      keys-alist (cons (list char help name) keys-alist))
                (throw 'exit t))))
        ;; Now check for lowercase letters
        (setq start 0)
        (while (string-match "[a-z]" name start)
          (setq start (match-end 0)
                char (string-to-char (match-string 0 name)))
          (if (not (assoc char keys-alist))
              (progn
                (setq help (format "%c:  %s" char
                                   (replace-match
                                    (concat "[" (match-string 0 name) "]")
                                    t t name))
                      keys-alist (cons (list char help name) keys-alist))
                (throw 'exit t))))
        ;; Bummer, nothing found!  Use a stupid number
        (setq char (string-to-char (int-to-string (setq cnt (1+ cnt))))
              help (format "%c:  %s" char name)
              keys-alist (cons (list char help name) keys-alist))))
    (nreverse keys-alist)))

;;----------------------------------------------------
;; File autoloading kill

(defvar idlwave-outlawed-buffers nil
  "List of buffers pulled up by IDLWAVE for special reasons.
Buffers in this list may be killed by `idlwave-kill-autoloaded-buffers'.")

(defun idlwave-find-file-noselect (file &optional why)
  ;; Return a buffer visiting file.
  (or (find-buffer-visiting file)
      (let ((buf (find-file-noselect file)))
	(if why (add-to-list 'idlwave-outlawed-buffers (cons buf why)))
	buf)))

(defun idlwave-kill-autoloaded-buffers ()
  "Kill buffers created automatically by IDLWAVE.
Function prompts for a letter to identify the buffers to kill.
Possible letters are:

f    Buffers created by the command \\[idlwave-find-module] or mouse
     clicks in the routine info window.
s    Buffers created by the IDLWAVE Shell to display where execution
     stopped or an error was found.
a    Both of the above.

Buffers containing unsaved changes require confirmation before they are killed."
  (interactive)
  (if (null idlwave-outlawed-buffers)
      (error "No IDLWAVE-created buffers available")
    (princ (format "Kill IDLWAVE-created buffers: [f]ind source(%d), [s]hell display(%d), [a]ll ? "
		   (idlwave-count-outlawed-buffers 'find)
		   (idlwave-count-outlawed-buffers 'shell)))
    (let ((c (read-char)))
      (cond
       ((member c '(?f ?\C-f))
	(idlwave-do-kill-autoloaded-buffers 'find))
       ((member c '(?s ?\C-s))
	(idlwave-do-kill-autoloaded-buffers 'shell))
       ((member c '(?a ?\C-a))
	(idlwave-do-kill-autoloaded-buffers t))
       (t (error "Abort"))))))

(defun idlwave-count-outlawed-buffers (tag)
  "How many outlawed buffers have tag TAG?"
  (length (delq nil
		(mapcar
		 (lambda (x) (eq (cdr x) tag))
		 idlwave-outlawed-buffers))))

(defun idlwave-do-kill-autoloaded-buffers (&rest reasons)
  "Kill all buffers pulled up by IDLWAVE matching REASONS."
  (let* ((list (copy-sequence idlwave-outlawed-buffers))
	 (cnt 0)
	 entry)
    (while (setq entry (pop list))
      (if (buffer-live-p (car entry))
	  (and (or (memq t reasons)
		   (memq (cdr entry) reasons))
	       (kill-buffer (car entry))
	       (cl-incf cnt)
	       (setq idlwave-outlawed-buffers
		     (delq entry idlwave-outlawed-buffers)))
	(setq idlwave-outlawed-buffers
	      (delq entry idlwave-outlawed-buffers))))
    (message "%d buffer%s killed" cnt (if (= cnt 1) "" "s"))))

(defun idlwave-revoke-license-to-kill ()
  "Remove BUFFER from the buffers which may be killed.
Killing would be done by `idlwave-do-kill-autoloaded-buffers'.
Intended for `after-save-hook'."
  (let* ((buf (current-buffer))
	 (entry (assq buf idlwave-outlawed-buffers)))
    ;; Revoke license
    (if entry
	(setq idlwave-outlawed-buffers
	      (delq entry idlwave-outlawed-buffers)))
    ;; Remove this function from the hook.
    (remove-hook 'after-save-hook #'idlwave-revoke-license-to-kill 'local)))

;;----------------------------------------------------
;; Statement templates

(defun idlwave-template (s1 s2 &optional prompt noindent)
  "Build a template with optional prompt expression.

Opens a line if point is not followed by a newline modulo intervening
whitespace.  S1 and S2 are strings.  S1 is inserted at point followed
by S2.  Point is inserted between S1 and S2.  The case of S1 and S2 is
adjusted according to `idlwave-abbrev-change-case'.  If optional
argument PROMPT is a string then it is displayed as a message in the
minibuffer.  The PROMPT serves as a reminder to the user of an
expression to enter.

The lines containing S1 and S2 are reindented using `indent-region'
unless the optional second argument NOINDENT is non-nil."
  (if (derived-mode-p 'idlwave-shell-mode)
      ;; This is a gross hack to avoid template abbrev expansion
      ;; in the shell.  FIXME: This is a dirty hack.
      (if (and (eq this-command 'self-insert-command)
	       (equal last-abbrev-location (point)))
	  (insert last-abbrev-text)
	(error "No templates in idlwave-shell"))
    (cond ((eq idlwave-abbrev-change-case 'down)
	   (setq s1 (downcase s1) s2 (downcase s2)))
	  (idlwave-abbrev-change-case
	   (setq s1 (upcase s1) s2 (upcase s2))))
    (let ((beg (line-beginning-position))
	  end)
      (if (not (looking-at "\\s-*\n"))
	  (open-line 1))
      (insert s1)
      (save-excursion
	(insert s2)
	(setq end (point)))
      (if (not noindent)
	  (indent-region beg end nil))
      (if (stringp prompt)
	  (message "%s" prompt)))))

(defun idlwave-rw-case (string)
  "Make STRING have the case required by `idlwave-reserved-word-upcase'."
  (if idlwave-reserved-word-upcase
      (upcase string)
    string))

(defun idlwave-elif ()
  "Build skeleton IDL if-else block."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "if")
   (idlwave-rw-case " then begin\n\nendif else begin\n\nendelse")
   "Condition expression"))

(defun idlwave-case ()
  "Build skeleton IDL case statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "case")
   (idlwave-rw-case " of\n\nendcase")
   "Selector expression"))

(defun idlwave-switch ()
  "Build skeleton IDL switch statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "switch")
   (idlwave-rw-case " of\n\nendswitch")
   "Selector expression"))

(defun idlwave-for ()
  "Build skeleton IDL for loop statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "for")
   (idlwave-rw-case " do begin\n\nendfor")
   "Loop expression"))

(defun idlwave-foreach ()
  "Build skeleton IDL for loop statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "foreach")
   (idlwave-rw-case " do begin\n\nendforeach")
   "Loop expression"))

(defun idlwave-if ()
  "Build skeleton IDL if statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "if")
   (idlwave-rw-case " then begin\n\nendif")
   "Scalar logical expression"))

(defun idlwave-procedure ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "pro")
   (idlwave-rw-case "\n\nreturn\nend")
   "Procedure name"))

(defun idlwave-function ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "function")
   (idlwave-rw-case "\n\nreturn\nend")
   "Function name"))

(defun idlwave-repeat ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "repeat begin\n\nendrep until")
   (idlwave-rw-case "")
   "Exit condition"))

(defun idlwave-while ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "while")
   (idlwave-rw-case " do begin\n\nendwhile")
   "Entry condition"))

;;----------------------------------------------------
;;  String normalization ("interning")

;; For the completion and routine info function, we want to normalize
;; the case of procedure names etc.  We do this by "interning" these
;; strings is a hand-crafted way.  Hashes are used to map the downcase
;; version of the strings to the cased versions.  Most *-sint-*
;; variables consist of *two* hashes, a buffer+shell, followed by a
;; system hash.  The former is re-scanned, and the latter takes case
;; precedence.
;;
;; Since these cased versions are really individual lisp objects, we
;; can use `eq' to search (or assq to find in an association list),
;; which is a large performance boost.  All new strings need to be
;; "sinterned".  We do this as early as possible after getting these
;; strings from completion or buffer substrings.  So most of the code
;; can simply assume to deal with "sinterned" strings.  The only
;; exception is that the functions which scan whole buffers for
;; routine information do not intern the grabbed strings.  This is
;; only done afterwards.  Therefore in these functions it is *not*
;; safe to assume the strings can be compared with `eq' and be fed
;; into the routine assq functions.

;; Note that this does not employ the normal obarray/intern method
;; Emacs uses.

;; Here we define the hashing functions.

;; The variables which hold the hashes.
(defvar idlwave-sint-routines '(nil))
(defvar idlwave-sint-keywords '(nil))
(defvar idlwave-sint-methods  '(nil))
(defvar idlwave-sint-classes  '(nil))
(defvar idlwave-sint-dirs    '(nil))
(defvar idlwave-sint-libnames '(nil))
(defun idlwave-sintern (stype &rest args)
  (apply (intern (concat "idlwave-sintern-" (symbol-name stype))) args))

(defun idlwave-reset-sintern (&optional what)
  "Reset all sintern hashes."
  ;; Make sure the hash functions are accessible.
  (let ((entries '((idlwave-sint-routines 1000 10)
		   (idlwave-sint-keywords 1000 10)
		   (idlwave-sint-methods   100 10)
		   (idlwave-sint-classes    10 10))))

    ;; Make sure these are lists
    (cl-loop for entry in entries
      for var = (car entry)
      do (if (not (consp (symbol-value var))) (set var (list nil))))

    ;; Reset the system & library hash
    (when (or (eq what t) (eq what 'syslib)
	      (null (cdr idlwave-sint-routines)))
      (cl-loop for entry in entries
	for var = (car entry) for size = (nth 1 entry)
	do (setcdr (symbol-value var)
		   (make-hash-table ':size size ':test 'equal)))
      (setq idlwave-sint-dirs nil
	    idlwave-sint-libnames nil))

    ;; Reset the buffer & shell hash
    (when (or (eq what t) (eq what 'bufsh)
	      (null (car idlwave-sint-routines)))
      (cl-loop for entry in entries
	for var = (car entry) for size = (nth 1 entry)
	do (setcar (symbol-value var)
		   (make-hash-table ':size size ':test 'equal))))))
(defun idlwave-sintern-routine-or-method (name &optional class set)
  (if class
      (idlwave-sintern-method name set)
    (idlwave-sintern-routine name set)))

(defun idlwave-sintern-routine (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-routines)))
	((gethash (downcase name) (car idlwave-sint-routines)))
	(set (idlwave-sintern-set name 'routine idlwave-sint-routines set))
	(name)))

(defun idlwave-sintern-keyword (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-keywords)))
	((gethash (downcase name) (car idlwave-sint-keywords)))
	(set (idlwave-sintern-set name 'keyword idlwave-sint-keywords set))
	(name)))

(defun idlwave-sintern-method (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-methods)))
	((gethash (downcase name) (car idlwave-sint-methods)))
	(set (idlwave-sintern-set name 'method idlwave-sint-methods set))
	(name)))

(defun idlwave-sintern-class (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-classes)))
	((gethash (downcase name) (car idlwave-sint-classes)))
	(set (idlwave-sintern-set name 'class idlwave-sint-classes set))
	(name)))

(defun idlwave-sintern-dir (dir &optional _set)
  (car (or (member dir idlwave-sint-dirs)
	   (setq idlwave-sint-dirs (cons dir idlwave-sint-dirs)))))
(defun idlwave-sintern-libname (name &optional _set)
  (car (or (member name idlwave-sint-libnames)
	   (setq idlwave-sint-libnames (cons name idlwave-sint-libnames)))))

(defun idlwave-sintern-set (name type tables set)
  (let* ((func (or (cdr (assq type idlwave-completion-case))
		   'identity))
	 (iname (funcall (if (eq func 'preserve) 'identity func) name))
	 (table (if (eq set 'sys) (cdr tables) (car tables))))
    (puthash (downcase name) iname table)
    iname))

(defun idlwave-sintern-keyword-list (kwd-list &optional set)
  "Sintern a set of keywords (file (key . link) (key2 . link2) ...)."
  (mapc (lambda(x)
	  (setcar x (idlwave-sintern-keyword (car x) set)))
	(cdr kwd-list))
  kwd-list)

(defun idlwave-sintern-rinfo-list (list &optional set default-dir)
  "Sintern all strings in the rinfo LIST.
With optional parameter SET: also set new patterns.  Probably this
will always have to be t.  If DEFAULT-DIR is passed, it is used as
the base of the directory."
  (let (entry name type class kwds res source call new)
    (while list
      (setq entry (car list)
	    list (cdr list)
	    name (car entry)
	    type (nth 1 entry)
	    class (nth 2 entry)
	    source (nth 3 entry)
	    call (nth 4 entry)
	    kwds (nthcdr 5 entry))

      ;; The class and name
      (if class
	  (progn
	    (if (symbolp class) (setq class (symbol-name class)))
	    (setq class (idlwave-sintern-class class set))
	    (setq name (idlwave-sintern-method name set)))
	(setq name (idlwave-sintern-routine name set)))

      ;; The source
      (let ((source-type (car source))
	    (source-file  (nth 1 source))
	    (source-dir  (if default-dir
			     (file-name-as-directory default-dir)
			   (nth 2 source)))
	    (source-lib (nth 3 source)))
	(if (stringp source-dir)
	    (setq source-dir (idlwave-sintern-dir source-dir set)))
	(if (stringp source-lib)
	    (setq source-lib (idlwave-sintern-libname source-lib set)))
	(setq source (list source-type source-file source-dir source-lib)))

      ;; The keywords
      (setq kwds (mapcar (lambda (x)
			   (idlwave-sintern-keyword-list x set))
			 kwds))

      ;; Build a canonicalized list
      (setq new (nconc (list name type class source call) kwds)
	    res (cons new res)))
    (nreverse res)))

(defun idlwave-sintern-class-info (entry)
  "Sintern the class names in a class-info entry."
  (let ((inherits (assq 'inherits entry)))
    (setcar entry (idlwave-sintern-class (car entry) 'set))
    (if inherits
	(setcdr inherits (mapcar (lambda (x) (idlwave-sintern-class x 'set))
				 (cdr inherits))))))

(defun idlwave-sintern-sysvar-alist ()
  (let ((list idlwave-system-variables-alist) entry tags)
    (while (setq entry (pop list))
      (setcar entry (idlwave-sintern-sysvar (car entry) 'set))
      (setq tags (assq 'tags entry))
      (if tags
	  (setcdr tags
		  (mapcar (lambda (x)
			    (cons (idlwave-sintern-sysvartag (car x) 'set)
				  (cdr x)))
			  (cdr tags)))))))

(defun idlwave-reset-sintern-type (tag)
  "Reset the sintern variable associated with TAG."
  (set (intern (concat "idlwave-sint-" (symbol-name tag) "s")) nil))

;; Run the hook
(run-hooks 'idlwave-load-hook)
(provide 'idlwave)

;;; idlwave.el ends here
