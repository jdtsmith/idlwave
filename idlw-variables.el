;;; idlw-variable.el --- Shared variables for IDLWAVE -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2024  Free Software Foundation, Inc.

;;; Commentary
;; IDLWAVE variables, customizations, and constants

;;; Code

(require 'browse-url)

(defgroup idlwave nil
  "Major mode for editing IDL .pro files."
  :tag "IDLWAVE"
  :link '(emacs-commentary-link :tag "Commentary in idlw-shell.el"
				"idlw-shell.el")
  :link '(emacs-commentary-link :tag "Commentary in idlwave.el" "idlwave.el")
  :link '(custom-manual "(idlwave)Top")
  :prefix "idlwave"
  :group 'languages)


;;----------------------------------------------------
;;;; Indentation behavior

(defgroup idlwave-code-formatting nil
  "Indentation and formatting options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-main-block-indent 2
  "Extra indentation for the main block of code.
That is the block between the FUNCTION/PRO statement and the END
statement for that program unit."
  :type 'integer)

(defcustom idlwave-block-indent 3
  "Extra indentation applied to block lines.
If you change this, you probably also want to change `idlwave-end-offset'."
  :type 'integer)

(defcustom idlwave-end-offset -3
  "Extra indentation applied to block END lines.
A value equal to negative `idlwave-block-indent' will make END lines
line up with the block BEGIN lines."
  :type 'integer)

(defcustom idlwave-continuation-indent 3
  "Extra indentation applied to continuation lines.
This extra offset applies to the first of a set of continuation lines.
The following lines receive the same indentation as the first."
  :type 'integer)

(defcustom idlwave-max-extra-continuation-indent 40
  "Maximum additional indentation for special continuation indent.
Several special indentations are tried to help line up continuation
lines in routine calls or definitions, other statements with
parentheses, or assignment statements.  This variable specifies a
maximum amount by which this special indentation can exceed the
standard continuation indentation, otherwise defaulting to a fixed
offset.  Set to 0 to effectively disable all special continuation
indentation, or to a large number (like 100) to enable it in all
cases.  See also `idlwave-indent-to-open-paren', which can override
this variable."
  :type 'integer)

(defcustom idlwave-indent-to-open-paren t
  "Non-nil means, indent continuation lines to innermost open parenthesis.
This indentation occurs even if otherwise disallowed by
`idlwave-max-extra-continuation-indent'.  Matching parens and the
interleaving args are lined up.  Example:

  x = function_a(function_b(function_c( a, b, [1,2,3, $
                                               4,5,6 $
                                              ], $
                                        c, d $
                                      )))

When this variable is nil, paren alignment may still occur, based on
the value of `idlwave-max-extra-continuation-indent', which, if zero,
would yield:

  x = function_a(function_b(function_c( a, b, [1,2,3, $
     4,5,6 $
     ], $
     c, d $
     )))"
  :type 'boolean)

(defcustom idlwave-indent-parens-nested nil
  "Non-nil means, indent continuation lines with parens by nesting
lines at consecutively deeper levels."
  :type 'boolean)

(defcustom idlwave-hanging-indent t
  "If set non-nil then comment paragraphs are indented under the
hanging indent given by `idlwave-hang-indent-regexp' match in the first line
of the paragraph."
  :type 'boolean)

(defcustom idlwave-hang-indent-regexp "- "
  "Regular expression matching the position of the hanging indent
in the first line of a comment paragraph.  The size of the indent
extends to the end of the match for the regular expression."
  :type 'regexp)

(defcustom idlwave-use-last-hang-indent nil
  "If non-nil then use last match on line for `idlwave-hang-indent-regexp'."
  :type 'boolean)

(defcustom idlwave-fill-comment-line-only t
  "If non-nil then auto fill will only operate on comment lines."
  :type 'boolean)

(defcustom idlwave-auto-fill-split-string t
  "If non-nil then auto fill will split strings with the IDL `+' operator.
When the line end falls within a string, string concatenation with the
`+' operator will be used to distribute a long string over lines.
If nil and a string is split then a terminal beep and warning are issued.

This variable is ignored when `idlwave-fill-comment-line-only' is
non-nil, since in this case code is not auto-filled."
  :type 'boolean)

(defcustom idlwave-split-line-string t
  "If non-nil then `idlwave-split-line' will split strings with `+'.
When the splitting point of a line falls inside a string, split the string
using the `+' string concatenation operator.  If nil and a string is
split then a terminal beep and warning are issued."
  :type 'boolean)

(defcustom idlwave-no-change-comment ";;;"
  "The indentation of a comment that starts with this regular
expression will not be changed.  Note that the indentation of a comment
at the beginning of a line is never changed."
  :type 'regexp)

(defcustom idlwave-begin-line-comment nil
  "A comment anchored at the beginning of line.
A comment matching this regular expression will not have its
indentation changed.  If nil the default is \"^;\", i.e., any line
beginning with a \";\".  Expressions for comments at the beginning of
the line should begin with \"^\"."
  :type '(choice (const :tag "Any line beginning with `;'" nil)
		 regexp))

(defcustom idlwave-code-comment ";;[^;]"
  "A comment that starts with this regular expression on a line by
itself is indented as if it is a part of IDL code.  As a result if
the comment is not preceded by whitespace it is unchanged."
  :type 'regexp)

;; Comments not matching any of the above will be indented as a
;; right-margin comment, i.e., to a minimum of `comment-column'.

;;----------------------------------------------------
;;;; Routine Info and Completion

(defgroup idlwave-routine-info nil
  "Routine Info options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-use-library-catalogs t
  "Non-nil means search the IDL path for library catalog files.

These files, named .idlwave_catalog, document routine information for
individual directories and libraries of IDL .pro files.  Many popular
libraries come with catalog files by default, so leaving this on is
usually a good idea."
  :type 'boolean)

(defcustom idlwave-init-rinfo-when-idle-after 10
  "Seconds of idle time before routine info is automatically initialized.
Initializing the routine info can take a long time, in particular if a
large number of library catalogs are involved.  When Emacs is idle for
more than the number of seconds specified by this variable, it starts
the initialization.  The process is split into five steps, in order to
keep work interruption as short as possible.  If one of the steps
finishes, and no user input has arrived in the mean time, initialization
proceeds immediately to the next step.  A good value for this variable
is about 1/3 of the time initialization take in your setup.  So if you
have a fast machine and no problems with a slow network connection,
don't hesitate to set this to 2 seconds.  A value of 0 means, don't
initialize automatically, but instead wait until routine information is
needed, and initialize then."
  :type 'number)

(defcustom idlwave-scan-all-buffers-for-routine-info t
  "Non-nil means, scan buffers for IDL programs when updating info.
The scanning is done by the command `idlwave-update-routine-info'.
The following values are allowed:

nil       Don't scan any buffers.
t         Scan all `idlwave-mode' buffers in the current editing session.
`current' Scan only the current buffer, but no other buffers."
  :type '(choice
	  (const :tag "No buffer" nil)
	  (const :tag "All buffers" t)
	  (const :tag "Current buffer only" current)))

(defcustom idlwave-query-shell-for-routine-info t
  "Non-nil means query the shell for info about compiled routines.
Querying the shell is useful to get information about compiled modules,
and it is turned on by default.  However, when you have a complete library
scan, this is not necessary."
  :type 'boolean)

(defcustom idlwave-auto-routine-info-updates
  '(find-file save-buffer kill-buffer compile-buffer)
  "Controls under what circumstances routine info is updated automatically.
Possible values:
nil       Never
t         All available
\(...)     A list of circumstances.  Allowed members are:
           find-file       Add info for new IDLWAVE buffers.
           save-buffer     Update buffer info when buffer is saved
           kill-buffer     Remove buffer info when buffer gets killed
           compile-buffer  Update shell info after `idlwave-shell-save-and...'"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "As often as possible" t)
	  (set :tag "Checklist" :greedy t
	       (const :tag "When visiting a file" find-file)
	       (const :tag "When saving a buffer" save-buffer)
	       (const :tag "After a buffer was killed" kill-buffer)
	       (const :tag "After a buffer was compiled successfully, update shell info" compile-buffer))))

(defcustom idlwave-rinfo-max-source-lines 5
  "Maximum number of source files displayed in the Routine Info window.
When an integer, it is the maximum number of source files displayed.
A value of t means to show all source files."
  :type 'integer)

(defcustom idlwave-library-path nil
  "Library path for Windows and Mac OS (OS9).  Not needed under UNIX.
When selecting the directories to scan for IDL user catalog routine
info, IDLWAVE can, under UNIX, query the shell for the exact search
path \(the value of !PATH).  However, under MS-Windows, the
IDLWAVE shell does not work.  In this case, this
variable can be set to specify the paths where IDLWAVE can find PRO
files.  The shell will only be asked for a list of paths when this
variable is nil.  The value is a list of directories.  A directory
preceded by a `+' will be searched recursively.  If you set this
variable on a UNIX system, the shell will not be queried.
See also `idlwave-system-directory'."
  :type '(repeat (directory)))

(defcustom idlwave-system-directory ""
  "The IDL system directory for Windows and Mac OS.  Not needed under
UNIX.  Set this to the value of the `!DIR' system variable in IDL.
IDLWAVE uses this to find out which of the library routines belong to
the official system library.  All files inside the `lib' subdirectory
are considered system library files - so don't install private stuff
in this directory.  On UNIX systems, IDLWAVE queries the shell for the
value of `!DIR'.  See also `idlwave-library-path'."
  :type 'directory)

;; Configuration files
(defcustom idlwave-config-directory
  (locate-user-emacs-file "idlwave" ".idlwave")
  "Directory for configuration files and user-library catalog."
  :version "24.4"			; added locate-user-emacs-file
  :type 'file)

(defcustom idlwave-special-lib-alist nil
  "Alist of regular expressions matching special library directories.
When listing routine source locations, IDLWAVE gives a short hint where
the file defining the routine is located.  By default it lists `SystemLib'
for routines in the system library `!DIR/lib' and `Library' for anything
else.  This variable can define additional types.  The car of each entry
is a regular expression matching the file name (they normally will match
on the path).  The cdr is the string to be used as identifier.  Max 10
chars are allowed."
  :type '(repeat
	  (cons regexp string)))

(defcustom idlwave-auto-write-paths t
  "Write out path (!PATH) and system directory (!DIR) info automatically.
Path info is needed to locate library catalog files.  If non-nil,
whenever the path-list changes as a result of shell-query, etc., it is
written to file.  Otherwise, the menu option \"Write Paths\" can be
used to force a write."
  :type 'boolean)

;;;; Completion
(defgroup idlwave-completion nil
  "Completion options for IDLWAVE mode."
  :prefix "idlwave"
  :group 'idlwave)

(eval-and-compile
  (defconst idlwave-tmp
    '(choice :tag "by applying the function"
      (const upcase)
      (const downcase)
      (const capitalize)
      (const preserve)
      (symbol :tag "Other"))))

(defcustom idlwave-completion-case '((routine . upcase)
				     (keyword . upcase)
				     (class   . preserve)
				     (method  . preserve))
  "Association list setting the case of completed words.

This variable determines the case (UPPER/lower/Capitalized...) of
words inserted into the buffer by completion.  The preferred case can
be specified separately for routine names, keywords, classes and
methods.
This alist should therefore have entries for `routine' (normal
functions and procedures, i.e. non-methods), `keyword', `class', and
`method'.  Plausible values are

upcase      upcase whole word, like `BOX_CURSOR'
downcase    downcase whole word, like `read_ppm'
capitalize  capitalize each part, like `Widget_Control'
preserve    preserve case as is, like `IDLgrView'

The value can also be any Emacs Lisp function which transforms the
case of characters in a string.

A value of `preserve' means that the case of the completed word is
identical to the way it was written in the definition statement of the
routine.  This was implemented to allow for mixed-case completion, in
particular of object classes and methods.
If a completable word is defined in multiple locations, the meaning of
`preserve' is not unique since the different definitions might be
cased differently.  Therefore IDLWAVE always takes the case of the
*first* definition it encounters during routine info collection and
uses the case derived from it consistently.

Note that a lowercase-only string in the buffer will always be completed in
lower case (but see the variable `idlwave-completion-force-default-case').

After changing this variable, you need to either restart Emacs or press
`C-u C-c C-i' to update the internal lists."
  :type `(repeat
	  (cons (symbol :tag "Derive completion case for")
		,idlwave-tmp)))

(defcustom idlwave-completion-force-default-case nil
  "Non-nil means, completion will always honor `idlwave-completion-case'.
When nil, only the completion of a mixed case or upper case string
will honor the default settings in `idlwave-completion-case', while
the completion of lower case strings will be completed entirely in
lower case."
  :type 'boolean)

(defcustom idlwave-complete-empty-string-as-lower-case nil
  "Non-nil means, the empty string is considered downcase for completion.
The case of what is already in the buffer determines the case of completions.
When this variable is non-nil, the empty string is considered to be downcase.
Completing on the empty string then offers downcase versions of the possible
completions."
  :type 'boolean)

(defcustom idlwave-buffer-case-takes-precedence nil
  "Non-nil means, the case of tokens in buffers dominates over system stuff.
To make this possible, we need to re-case everything each time we update
the routine info from the buffers.  This is slow.
The default is to consider the case given in the system and library files
first which makes updating much faster."
  :type 'boolean)

(defcustom idlwave-highlight-help-links-in-completion t
  "Non-nil means, highlight completions for which system help is available.
Help can then be accessed with mouse-3.
This option is only effective when the online help system is installed."
  :type 'boolean)

(defcustom idlwave-support-inheritance t
  "Non-nil means, treat inheritance with completion, online help etc.
When nil, IDLWAVE only knows about the native methods and tags of a class,
not about inherited ones."
  :group 'idlwave-routine-info          ;FIXME: Is this the right group?
  :type 'boolean)

(defcustom idlwave-keyword-class-inheritance '("^[gs]etproperty$" "^init$")
  "List of regular expressions for class-driven keyword inheritance.
Keyword inheritance is often tied to class inheritance by \"chaining\"
up the class tree.  While it cannot be assumed that the presence of an
_EXTRA or _REF_EXTRA symbol guarantees such chaining will occur, for
certain methods this assumption is almost always true.  The methods
for which to assume this can be set here."
  :group 'idlwave-routine-info          ;FIXME: Is this the right group?
  :type '(repeat (regexp :tag "Match method:")))

(defcustom idlwave-complete-structure-tags t
  "Whether to complete structure tags in source and shell."
  :group 'idlwave-routine-info          ;FIXME: Is this the right group?
  :type 'boolean)

(defcustom idlwave-completion-show-classes 1
  "Number of classes to show when completing object methods and keywords.
When completing methods or keywords for an object with unknown class,
the *Completions* buffer will show the valid classes for each completion
like this:

MyMethod <Class1,Class2,Class3>

The value of this variable may be nil to inhibit display, or an integer to
indicate the maximum number of classes to display."
  :type '(choice (const :tag "Don't show" nil)
		 (integer :tag "Number of classes shown" 1)))

(defcustom idlwave-completion-fontify-classes t
  "Non-nil means, fontify the classes in completions buffer.
This makes it easier to distinguish the completion items from the extra
class info listed.  See `idlwave-completion-show-classes'."
  :type 'boolean)

(defcustom idlwave-query-class '((method-default . nil)
				 (keyword-default . nil))
  "Association list governing specification of object classes for completion.

When IDLWAVE tries to complete object-oriented methods, it usually
cannot determine the class of a given object from context.  In order
to provide the user with a correct list of methods or keywords, it
needs to determine the appropriate class.  IDLWAVE has two ways of
doing this (well, three ways if you count the shell... see
`idlwave-shell-query-for-class'):

1. Combine the items of all available classes which contain this
   method for the purpose of completion.  So when completing a method,
   all methods of all known classes are available, and when completing
   a keyword, all keywords allowed for this method in any class are
   shown.  This behavior is very much like normal completion and is
   therefore the default.  It works much better than one might think -
   only for the INIT, GETPROPERTY and SETPROPERTY the keyword lists
   become uncomfortably long.  See also
   `idlwave-completion-show-classes'.

2. The second possibility is to ask the user on each occasion.  To
   make this less interruptive, IDLWAVE can store the class as a text
   property on the object operator `->'.  For a given object in the
   source code, class selection will then be needed only once
   - for example to complete the method.  Keywords to the method can
   then be completed directly, because the class is already known.
   You will have to turn on the storage of the selected class
   explicitly with the variable `idlwave-store-inquired-class'.

This variable allows you to configure IDLWAVE's method and
method-keyword completion behavior.  Its value is an alist, which
should contain at least two elements: (method-default . VALUE) and
\(keyword-default . VALUE), where VALUE is either t or nil.  These
specify if the class should be found during method and keyword
completion, respectively.

The alist may have additional entries specifying exceptions from the
keyword completion rule for specific methods, like INIT or
GETPROPERTY.  In order to turn on class specification for the INIT
method, add an entry (\"INIT\" . t).  The method name must be ALL-CAPS."
  :type '(list
	  (cons (const method-default)
		(boolean :tag "Determine class when completing METHODS    "))
	  (cons (const keyword-default)
		(boolean :tag "Determine class when completing KEYWORDS   "))
	  (repeat
	   :tag "Exceptions to defaults"
	   :inline t
	   (cons (string  :tag "MODULE" :value "")
		 (boolean :tag "Determine class for this method")))))

(defcustom idlwave-store-inquired-class t
  "Non-nil means, store class of a method call as text property on `->'.
IDLWAVE sometimes has to ask the user for the class associated with a
particular object method call.  This happens during the commands
`idlwave-routine-info' and `idlwave-complete', depending upon the
value of the variable `idlwave-query-class'.

When you specify a class, this information can be stored as a text
property on the `->' arrow in the source code, so that during the same
editing session, IDLWAVE will not have to ask again.  When this
variable is non-nil, IDLWAVE will store and reuse the class information.
The class stored can be checked and removed with \\[idlwave-routine-info]
on the arrow.

The default of this variable is nil, since the result of commands then
is more predictable.  However, if you know what you are doing, it can
be nice to turn this on.

An arrow which knows the class will be highlighted with
`idlwave-class-arrow-face'.  The command \\[idlwave-routine-info]
displays (with prefix arg: deletes) the class stored on the arrow
at point."
  :type 'boolean)

(defcustom idlwave-class-arrow-face 'bold
  "Face to highlight object operator arrows `->' which carry a class property.
When IDLWAVE stores a class name as text property on an object arrow
\(see variable `idlwave-store-inquired-class', it highlights the arrow
with this font in order to remind the user that this arrow is special."
  :type 'symbol)

(defcustom idlwave-resize-routine-help-window t
  "Non-nil means, resize the Routine-info *Help* window to fit the content."
  :type 'boolean)

(defcustom idlwave-keyword-completion-adds-equal t
  "Non-nil means, completion automatically adds `=' after completed keywords."
  :type 'boolean)

(defcustom idlwave-function-completion-adds-paren t
  "Non-nil means, completion automatically adds `(' after completed function.
nil means, don't add anything.
A value of `2' means, also add the closing parenthesis and position cursor
between the two."
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "(" t)
		 (const :tag "()" 2)))

(defcustom idlwave-completion-restore-window-configuration t
  "Non-nil means, try to restore the window configuration after completion.
When completion is not unique, Emacs displays a list of completions.
This messes up your window configuration.  With this variable set, IDLWAVE
restores the old configuration after successful completion."
  :type 'boolean)

;;----------------------------------------------------
;;;; Abbrev and action
(defgroup idlwave-abbrev-and-indent-action nil
  "IDLWAVE performs actions when expanding abbreviations or indenting lines.
The variables in this group govern this."
  :group 'idlwave)

(defcustom idlwave-do-actions nil
  "Non-nil means performs actions when indenting.
The actions that can be performed are listed in `idlwave-indent-action-table'."
  :type 'boolean)

(defcustom idlwave-abbrev-start-char "\\"
  "A single character string used to start abbreviations in abbrev mode.
Possible characters to choose from: ~\\=`%
or even `?'.  `.' is not a good choice because it can make structure
field names act like abbrevs in certain circumstances.

Changes to this in `idlwave-mode-hook' will have no effect.  Instead a user
must set it directly using `setq' in the init file before idlwave.el
is loaded."
  :type 'string)

(defcustom idlwave-surround-by-blank nil
  "Non-nil means, enable `idlwave-surround'.
If non-nil, `=',`<',`>',`&',`,', `->' are surrounded with spaces by
`idlwave-surround'.
See help for `idlwave-indent-action-table' for symbols using `idlwave-surround'.

Also see the default key bindings for keys using `idlwave-surround'.
Keys are bound and made into actions calling `idlwave-surround' with
`idlwave-action-and-binding'.
See help for `idlwave-action-and-binding' for examples.

Also see help for `idlwave-surround'."
  :type 'boolean)

(defcustom idlwave-pad-keyword t
  "Non-nil means pad `=' in keywords (routine calls or defs) like assignment.
Whenever `idlwave-surround' is non-nil then this affects how `=' is
padded for keywords and for variables.  If t, pad the same as for
assignments.  If nil then spaces are removed.  With any other value,
spaces are left unchanged."
  :type '(choice
	  (const :tag "Pad like assignments" t)
	  (const :tag "Remove space near `='" nil)
	  (other :tag "Keep space near `='" keep)))

(defcustom idlwave-show-block t
  "Non-nil means point blinks to block beginning for `idlwave-show-begin'."
  :type 'boolean)

(defcustom idlwave-expand-generic-end nil
  "Non-nil means expand generic END to ENDIF/ENDELSE/ENDWHILE etc."
  :type 'boolean)

(defcustom idlwave-reindent-end t
  "Non-nil means re-indent line after END was typed."
  :type 'boolean)

(defcustom idlwave-abbrev-move t
  "Non-nil means the abbrev hook can move point.
Set to nil by `idlwave-expand-region-abbrevs'.  To see the abbrev
definitions, use the command `list-abbrevs', for abbrevs that move
point.  Moving point is useful, for example, to place point between
parentheses of expanded functions.

See `idlwave-modify-abbrev'."
  :type 'boolean)

(defcustom idlwave-abbrev-change-case nil
  "Non-nil means all abbrevs will be forced to either upper or lower case.
If the value t, all expanded abbrevs will be upper case.
If the value is `down' then abbrevs will be forced to lower case.
If nil, the case will not change.
If `idlwave-reserved-word-upcase' is non-nil, reserved words will always be
upper case, regardless of this variable."
  :type 'boolean)

(defcustom idlwave-reserved-word-upcase nil
  "Non-nil means, reserved words will be made upper case via abbrev expansion.
If nil case of reserved words is controlled by `idlwave-abbrev-change-case'.
Has effect only if in abbrev-mode."
  :type 'boolean)

;;; Action/Expand Tables.
;;
;; The average user may have difficulty modifying this directly.  It
;; can be modified/set in idlwave-mode-hook, but it is easier to use
;; idlwave-action-and-binding. See help for idlwave-action-and-binding for
;; examples of how to add an action.
;;
;; The action table is used by `idlwave-indent-line' whereas both the
;; action and expand tables are used by `idlwave-indent-and-action'.  In
;; general, the expand table is only used when a line is explicitly
;; indented.  Whereas, in addition to being used when the expand table
;; is used, the action table is used when a line is indirectly
;; indented via line splitting, auto-filling or a new line creation.
;;
;; Example actions:
;;
;;  Capitalize system vars
;;   (idlwave-action-and-binding idlwave-sysvar
;;                               (lambda (_) (capitalize-word 1)) t)
;;
;;  Capitalize procedure name
;;   (idlwave-action-and-binding "\\<\\(pro\\|function\\)\\>[ \t]*\\<"
;;                               (lambda (_) (capitalize-word 1)) t)
;;
;;  Capitalize common block name
;;   (idlwave-action-and-binding "\\<common\\>[ \t]+\\<"
;;                               (lambda (_) (capitalize-word 1)) t)
;;  Capitalize label
;;   (idlwave-action-and-binding (concat "^[ \t]*" idlwave-label)
;;                               (lambda (_) (capitalize-word 1)) t)

(defvar idlwave-indent-action-table nil
  "Associated array containing action lists of search string (car),
and function as a cdr.  This table is used by `idlwave-indent-line'.
See documentation for `idlwave-do-action' for a complete description of
the action lists.

Additions to the table are made with `idlwave-action-and-binding' when a
binding is not requested.
See help on `idlwave-action-and-binding' for examples.")

(defvar idlwave-indent-expand-table nil
  "Associated array containing action lists of search string (car),
and function as a cdr.  The table is used by the
`idlwave-indent-and-action' function.  See documentation for
`idlwave-do-action' for a complete description of the action lists.

Additions to the table are made with `idlwave-action-and-binding' when a
binding is requested.
See help on `idlwave-action-and-binding' for examples.")

;;----------------------------------------------------
;;;; Documentation header and history keyword
(defgroup idlwave-documentation nil
  "Options for documenting IDLWAVE files."
  :group 'idlwave)

(defvar idlwave-file-header
  (list nil
        ";+
; NAME:
;
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
")
  "A list (PATHNAME STRING) specifying the doc-header template to use for
summarizing a file.  If PATHNAME is non-nil then this file will be included.
Otherwise STRING is used.  If nil, the file summary will be omitted.
For example you might set PATHNAME to the path for the
lib_template.pro file included in the IDL distribution.")

(defcustom idlwave-header-to-beginning-of-file t
  "Non-nil means, the documentation header will always be at start of file.
When nil, the header is positioned between the PRO/FUNCTION line of
the current routine and the code, allowing several routine headers in
a file."
  :type 'boolean)

(defcustom idlwave-timestamp-hook 'idlwave-default-insert-timestamp
  "The hook function used to update the timestamp of a function."
  :type 'function)

(defcustom idlwave-doc-modifications-keyword "HISTORY"
  "The modifications keyword to use with the log documentation commands.
A `:' is added to the keyword end.
Inserted by doc-header and used to position logs by doc-modification.
If nil it will not be inserted."
  :type 'string)

(defcustom idlwave-doclib-start "^;+\\+"
  "Regexp matching the start of a document library header."
  :type 'regexp)

(defcustom idlwave-doclib-end "^;+-"
  "Regexp matching the end of a document library header."
  :type 'regexp)

;;----------------------------------------------------
;;;; External Programs
(defgroup idlwave-external-programs nil
  "Path locations of external commands used by IDLWAVE."
  :group 'idlwave)

(defcustom idlwave-shell-explicit-file-name "idl"
  "If non-nil, this is the command to run IDL.
Should be an absolute file path or path relative to the current environment
execution search path.  If you want to specify command line switches
for the IDL program, use `idlwave-shell-command-line-options'.

I know the name of this variable is badly chosen, but I cannot change
it without compromising backwards-compatibility."
  :type 'string)

(defcustom idlwave-shell-command-line-options nil
  "A list of command line options for calling the IDL program.
Since IDL is executed directly without going through a shell like /bin/sh,
this should be a list of strings like (\"-rt=file\" \"-nw\") with a separate
string for each argument.  But you may also give a single string which
contains the options whitespace-separated.  Emacs will be kind enough to
split it for you."
  :type '(choice
	  string
	  (repeat (string :value ""))))

(defcustom idlwave-help-application "idlhelp"
  "The external application providing reference help for programming.
Obsolete, if the IDL Assistant is being used for help."
  :type 'string)

;;----------------------------------------------------
;;;; Help
(defgroup idlwave-online-help nil
  "Online Help options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-html-help-pre-v6 nil
  "Whether pre or post-v6.0 IDL help documents are being used.
OBSOLETE.  The full link anchor is now stored."
  :type 'boolean)

(defcustom idlwave-html-system-help-location  nil
  "The directory, relative to `idlwave-system-directory', where the
IDL HTML help files live, for IDL 7.0 and later.  By default,
this location is discovered automatically from the installation.
This location, if found, is used in preference to the old
`idlwave-html-help-location'.  Note that IDL v6.3-v7.0 used
\"help/online_help\"."
  :type 'directory)

(defcustom idlwave-html-help-location
   (if (memq system-type '(ms-dos windows-nt))
      nil
    "/usr/local/etc/")
  "The directory where the \"idl_html_help/\" dir lives.
OBSOLETE (see `idlwave-html-system-help-location')."
  :type 'directory)

(defcustom idlwave-help-browser-function browse-url-browser-function
  "Function to use to display HTML help.
Defaults to `browse-url-browser-function', which see."
  :type 'function)

(defcustom idlwave-help-browser-generic-program browse-url-generic-program
  "Program to run if using `browse-url-generic-program'."
  :type '(choice (const nil) string))

;; FIXME: AFAICS, never used since it was introduced in 2004.
(defcustom idlwave-help-browser-generic-args
  (if (boundp 'browse-url-generic-args)
      browse-url-generic-args "")
  "Program args to use if using browse-url-generic-program."
  :type 'string)

(defcustom idlwave-help-browser-is-local nil
  "Whether the browser will display locally in an Emacs window.
Several browsers run and/or display inside Emacs windows, but most are
external programs.  If the browser name contains \"-w3\", it is
assumed to be local to Emacs.  For other local browsers, this variable
must be explicitly set non-nil in order for the variable
`idlwave-help-use-dedicated-frame' to function."
  :type 'boolean)

(defcustom idlwave-help-use-dedicated-frame t
  "Non-nil means, use a separate frame for Online Help if possible."
  :type 'boolean)

(defcustom idlwave-help-frame-parameters
  '((height . 32) (unsplittable . t))
  "The frame parameters for the special Online Help frame.
See also `idlwave-help-use-dedicated-frame'.
If you do not set the frame width here, the value specified in
`idlw-help.el' will be used."
  :type '(repeat
	  (cons symbol sexp)))

(defcustom idlwave-max-popup-menu-items 20
  "Maximum number of items per pane in popup menus.
Currently only used for class selection during completion help."
  :type 'integer)

(defcustom idlwave-extra-help-function 'idlwave-help-with-source
  "The function to call for online help if the normal help fails.
Online help works only for system routines which are described in the
IDL manuals.  A function may be specified to access help from other sources.

The function must accept four arguments: NAME, TYPE, CLASS, KEYWORD.
The Help buffer is current when this function is called, and the help
text should be loaded into this buffer.  If help is found, the
function should return the buffer position which should be used as
`window-start' in the help window.  Also, the variable
`idlwave-help-mode-line-indicator' should be set to a useful string,
which will be displayed in the mode line of the help window.  If
should also set the variable `idlwave-help-min-frame-width' to a
positive integer.  IDLWAVE will ensure that the help frame is at least
that many columns wide.  Failure to find help should be indicated by
throwing an error.

When this variable is non-nil, IDLWAVE will allow the mouse-3 help click
for every routine and keyword, even though the item may not be highlighted
in blue (indicating the availability of system documentation).

The default value for this function is `idlwave-help-with-source' which
loads the routine source file into the help buffer.  If you try to write
a different function which accesses a special help file or so, it is
probably a good idea to still call this function as a fallback."
  :type 'symbol)

(defcustom idlwave-help-fontify-source-code t
  "Non-nil means, fontify source code displayed as help like normal code."
  :type 'boolean)

(defcustom idlwave-help-source-try-header t
  "Non-nil means, try to find help in routine header when displaying source.
Routines which are not documented in the system manual use their source as
help text.  When this variable is non-nil, we try to find a description of
the help item in the first routine doclib header above the routine definition.
If the variable is nil, or if we cannot find/parse the header, the routine
definition is displayed instead."
  :type 'boolean)

(defcustom idlwave-help-doclib-name "name"
  "A regexp for the heading word to search for in doclib headers
which specifies the `name' section.  Can be used for localization
support."
  :type 'regexp)

(defcustom idlwave-help-doclib-keyword "KEYWORD"
  "A regexp for the heading word to search for in doclib headers
which specifies the `keywords' section.  Can be used for localization
support."
  :type 'regexp)

;;----------------------------------------------------
;;;; Shell
(defcustom idlwave-shell-debug-modifiers '()
  "List of modifiers to be used for the debugging commands.
Will be used to bind debugging commands in the shell buffer and in all
source buffers.  These are additional convenience bindings, the debugging
commands are always available with the \\`C-c C-d' prefix.
If you set this to (control shift), this means setting a breakpoint will
be on \\`C-S-b', compiling a source file on \\`C-S-c' etc.  Possible modifiers
are `control', `meta', `super', `hyper', `alt', and `shift'."
  :group 'idlwave-shell-general-setup
  :type '(set :tag "Specify modifiers"
	       (const control)
	       (const meta)
	       (const super)
	       (const hyper)
	       (const alt)
	       (const shift)))

(defcustom idlwave-shell-automatic-start nil
  "If non-nil attempt invoke `idlwave-shell' if not already running.
This is checked when an attempt to send a command to an
IDL process is made."
  :group 'idlwave-shell-general-setup
  :type 'boolean)


;;----------------------------------------------------
;;;; Miscellaneous variables
(defgroup idlwave-misc nil
  "Miscellaneous options for IDLWAVE mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'idlwave)

(defcustom idlwave-startup-message t
  "Non-nil displays a startup message when `idlwave-mode' is first called."
  :type 'boolean)

(defcustom idlwave-default-font-lock-items
  '(pros-and-functions batch-files idlwave-idl-keywords label goto
		       common-blocks class-arrows)
  "Items which should be fontified on the default fontification level 2.
IDLWAVE defines 3 levels of fontification.  Level 1 is very little, level 3
is everything and level 2 is specified by this list.
This variable must be set before IDLWAVE gets loaded.
It is a list of symbols; the following symbols are allowed:

`pros-and-functions'   Procedure and Function definitions
`batch-files'          Batch Files
`idlwave-idl-keywords' IDL Keywords
`label'                Statement Labels
`goto'                 Goto Statements
`common-blocks'        Common Blocks
`keyword-parameters'   Keyword Parameters in routine definitions and calls
`system-variables'     System Variables
`class-arrows'         Object Arrows with class property"
  :type '(set
	  :inline t :greedy t
	  (const :tag "Procedure and Function definitions" pros-and-functions)
	  (const :tag "Batch Files"                       batch-files)
	  (const :tag "IDL Keywords (reserved words)"     idlwave-idl-keywords)
	  (const :tag "Statement Labels"                  label)
	  (const :tag "Goto Statements"                   goto)
	  (const :tag "Tags in Structure Definition"      structtag)
	  (const :tag "Structure Name"                    structname)
	  (const :tag "Common Blocks"                     common-blocks)
	  (const :tag "Keyword Parameters"                keyword-parameters)
	  (const :tag "System Variables"                  system-variables)
	  (const :tag "Object Arrows with class property " class-arrows)))

(defcustom idlwave-mode-hook nil
  "Normal hook.  Executed when a buffer is put into `idlwave-mode'."
  :type 'hook)

(defcustom idlwave-load-hook nil
  "Normal hook.  Executed when idlwave.el is loaded."
  :type 'hook)
(make-obsolete-variable 'idlwave-load-hook
                        "use `with-eval-after-load' instead." "24.4")

(defvar idlwave-experimental nil
  "Non-nil means turn on a few experimental features.
This variable is only for the maintainer, to test difficult stuff,
while still distributing stable releases.
As a user, you should not set this to t.")

;;;
;;; End customization variables section
;;;

;;; Non customization variables

;; font-lock mode - Additions by Phil Williams, Ulrik Dickow and
;; Simon Marshall <simon_at_gnu.ai.mit.edu>
;; and Carsten Dominik...

;; The following are the reserved words in IDL.  Maybe we should
;; highlight some more stuff as well?
;; Procedure declarations.  Fontify keyword plus procedure name.
(defvar idlwave-idl-keywords
  ;; To update this regexp, update the list of keywords and
  ;; evaluate the form.
  	;; (insert
  	;;  (prin1-to-string
  	;;   (concat
  	;;    "\\<\\("
  	;;    (regexp-opt
  	;;     '("||" "&&" "and" "or" "xor" "not"
  	;;       "eq" "ge" "gt" "le" "lt" "ne"
  	;;       "for" "do" "endfor" "foreach" "endforeach"
  	;;       "if" "then" "endif" "else" "endelse"
  	;;       "case" "of" "endcase"
  	;;       "switch" "break" "continue" "endswitch"
  	;;       "begin" "end"
  	;;       "repeat" "until" "endrep"
  	;;       "while" "endwhile"
  	;;       "goto" "return"
  	;;       "inherits" "mod"
  	;;       "compile_opt" "forward_function"
  	;;       "on_error" "on_ioerror"))  ; on_error is not officially reserved
  	;;    "\\)\\>")))

  "\\<\\(\\(?:&&\\|and\\|b\\(?:egin\\|reak\\)\\|c\\(?:ase\\|o\\(?:mpile_opt\\|ntinue\\)\\)\\|do\\|e\\(?:lse\\|nd\\(?:case\\|else\\|for\\(?:each\\)?\\|if\\|rep\\|switch\\|while\\)?\\|q\\)\\|for\\(?:each\\|ward_function\\)?\\|g\\(?:oto\\|[et]\\)\\|i\\(?:f\\|nherits\\)\\|l[et]\\|mod\\|n\\(?:e\\|ot\\)\\|o\\(?:n_\\(?:\\(?:io\\)?error\\)\\|[fr]\\)\\|re\\(?:peat\\|turn\\)\\|switch\\|then\\|until\\|while\\|xor\\|||\\)\\)\\>")

(defmacro idlwave--dlet (binders &rest body)
  "Like `dlet' but without warnings about non-prefixed var names."
  (declare (indent 1) (debug let))
  (let ((vars (mapcar (lambda (binder)
                        (if (consp binder) (car binder) binder))
                      binders)))
    `(with-suppressed-warnings ((lexical ,@vars))
       (dlet ,binders ,@body))))

(idlwave--dlet
    (;; Procedure declarations.  Fontify keyword plus procedure name.
     ;; Function  declarations.  Fontify keyword plus function  name.
     (pros-and-functions
      '("\\<\\(function\\|pro\\)\\>[ \t]+\\(\\sw+\\(::\\sw+\\)?\\)"
	(1 'font-lock-keyword-face)
	(2 'font-lock-function-name-face nil t)))

     ;; Common blocks
     (common-blocks
      '("\\<\\(common\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*,?"
	(1 'font-lock-keyword-face)          ; "common"
	(2 'font-lock-constant-face nil t)   ; block name
	("[ \t]*\\(\\sw+\\)[ ,]*"
	 ;; Start with point after block name and comma
	 nil nil (1 'font-lock-variable-name-face)))) ; variable names

     ;; Batch files
     (batch-files
      '("^[ \t]*\\(@[^ \t\n]+\\)" (1 'font-lock-string-face)))

     ;; Labels
     (label
      '("^[ \t]*\\([a-zA-Z]\\sw*:\\)" (1 'font-lock-constant-face)))

     ;; The goto statement and its label
     (goto
      '("\\(goto\\)[ \t]*,[ \t]*\\([a-zA-Z]\\sw*\\)"
	(1 'font-lock-keyword-face)
	(2 'font-lock-constant-face)))

     ;; Tags in structure definitions.  Note that this definition
     ;; actually collides with labels, so we have to use the same
     ;; face.  It also matches named subscript ranges,
     ;; e.g. vec{bottom:top].  No good way around this.
     (structtag
      '("\\<\\([a-zA-Z][a-zA-Z0-9_]*:\\)[^:]" (1 'font-lock-constant-face)))

     ;; Structure names
     (structname
      '("\\({\\|\\<inherits\\s-\\)\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)[},\t \n]"
	(2 'font-lock-function-name-face)))

     ;; Keyword parameters, like /xlog or ,xrange=[]
     ;; This is anchored to the comma preceding the keyword.
     ;; Treats continuation lines, works only during whole buffer
     ;; fontification.  Slow, use it only in fancy fontification.
     (keyword-parameters
      '("\\(,\\|[a-zA-Z0-9_](\\)[ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\([ \t]*\\(;.*\\)?\n\\)*[ \t]*\\)?\\(/[a-zA-Z_]\\sw*\\|[a-zA-Z_]\\sw*[ \t]*=\\)"
	(6 'font-lock-constant-face)))

     ;; System variables start with a bang.
     (system-variables
      '("\\(![a-zA-Z_0-9]+\\(\\.\\sw+\\)?\\)"
	(1 'font-lock-variable-name-face)))

     ;; Special and unusual operators (not used because too noisy)
     ;; (special-operators
     ;;  '("[<>#]" (0 font-lock-keyword-face)))

     ;; All operators (not used because too noisy)
     ;; (all-operators
     ;;  '("[-*^#+<>/]" (0 font-lock-keyword-face)))

     ;; Arrows with text property `idlwave-class'
     (class-arrows
      '(idlwave-match-class-arrows (0 idlwave-class-arrow-face))))

  (defconst idlwave-font-lock-keywords-1
    (list pros-and-functions batch-files)
    "Subdued level highlighting for IDLWAVE mode.")

  (defconst idlwave-font-lock-keywords-2
    (mapcar #'symbol-value idlwave-default-font-lock-items)
    "Medium level highlighting for IDLWAVE mode.")

  (defconst idlwave-font-lock-keywords-3
    (list pros-and-functions
	  batch-files
	  idlwave-idl-keywords
	  label goto
	  structtag
	  structname
	  common-blocks
	  keyword-parameters
	  system-variables
	  class-arrows)
    "Gaudy level highlighting for IDLWAVE mode."))

(defun idlwave-match-class-arrows (limit)
  ;; Match an object arrow with class property
  (and idlwave-store-inquired-class
       (re-search-forward "->" limit 'limit)
       (get-text-property (match-beginning 0) 'idlwave-class)))

(defvar idlwave-font-lock-keywords idlwave-font-lock-keywords-2
  "Default expressions to highlight in IDLWAVE mode.")

(defvar idlwave-font-lock-defaults
  '((idlwave-font-lock-keywords
     idlwave-font-lock-keywords-1
     idlwave-font-lock-keywords-2
     idlwave-font-lock-keywords-3)
    nil t
    ((?$ . "w") (?_ . "w") (?. . "w") (?| . "w") (?& . "w"))
    beginning-of-line))

(defconst idlwave-comment-line-start-skip "^[ \t]*;"
  "Regexp to match the start of a full-line comment.
That is the _beginning_ of a line containing a comment delimiter `;' preceded
only by whitespace.")

(defconst idlwave-begin-block-reg
  "\\<\\(pro\\|function\\|begin\\|case\\|switch\\)\\>"
  "Regular expression to find the beginning of a block.
The case does not matter. The search skips matches in comments.")

(defconst idlwave-profun-reg "^\\s-*\\(pro\\|function\\)\\>")

(defconst idlwave-begin-unit-reg (concat idlwave-profun-reg "\\|\\`")
  "Regular expression to find the beginning of a unit.
The case does not matter.")

(defconst idlwave-end-unit-reg "^\\s-*\\(pro\\|function\\)\\>\\|\\'"
  "Regular expression to find the line that indicates the end of unit.
This line is the end of buffer or the start of another unit. The
case does not matter. The search skips matches in comments.")

(defconst idlwave-continue-line-reg "\\<\\$"
  "Regular expression to match a continued line.")

(defconst idlwave-end-block-reg
  "\\<end\\(\\|case\\|switch\\|else\\|for\\|foreach\\|if\\|rep\\|while\\)\\>"
  "Regular expression to find the end of a block.
The case does not matter. The search skips matches found in
comments.")

(defconst idlwave-block-matches
  '(("pro"      . "end")
    ("function" . "end")
    ("case"     . "endcase")
    ("else"     . "endelse")
    ("for"      . "endfor")
    ("foreach"  . "endforeach")
    ("then"     . "endif")
    ("repeat"   . "endrep")
    ("switch"   . "endswitch")
    ("while"    . "endwhile"))
  "Matches between statements and the corresponding END variant.
The cars are the reserved words starting a block.  If the block really
begins with BEGIN, the cars are the reserved words before the begin
which can be used to identify the block type.
This is used to check for the correct END type, to close blocks and
to expand generic end statements to their detailed form.")

(defconst idlwave-block-match-regexp
  "\\<\\(else\\|for\\|foreach\\|then\\|repeat\\|while\\)\\>"
"Regular expression matching reserved words which can stand before
blocks starting with a BEGIN statement.  The matches must have associations
`idlwave-block-matches'.")

(defconst idlwave-identifier "[a-zA-Z_][a-zA-Z0-9$_]*"
  "Regular expression matching an IDL identifier.")

(defconst idlwave-sysvar (concat "!" idlwave-identifier)
  "Regular expression matching IDL system variables.")

(defconst idlwave-variable (concat idlwave-identifier "\\|" idlwave-sysvar)
  "Regular expression matching IDL variable names.")

(defconst idlwave-label (concat idlwave-identifier ":")
  "Regular expression matching IDL labels.")

(defconst idlwave-method-call (concat idlwave-identifier  "\\s *\\(->\\|\\.\\)"
				      "\\(\\s *" idlwave-identifier "::\\)?"
))

(defconst idlwave-statement-match
  (list
   ;; "endif else" is the only possible "end" that can be
   ;; followed by a statement on the same line.
   '(endelse . ("end\\(\\|if\\)\\s +else" "end\\(\\|if\\)\\s +else"))
   ;; all other "end"s can not be followed by a statement.
   (cons 'end (list idlwave-end-block-reg nil))
   '(if . ("if\\>" "then"))
   '(for . ("for\\>" "do"))
   '(foreach . ("foreach\\>" "do"))
   '(begin . ("begin\\>" nil))
   '(pdef . ("pro\\>\\|function\\>" nil))
   '(while . ("while\\>" "do"))
   '(repeat . ("repeat\\>" "repeat"))
   '(goto . ("goto\\>" nil))
   '(case . ("case\\>" nil))
   '(switch . ("switch\\>" nil))
   (cons 'call (list (concat "\\(" idlwave-variable "\\) *= *"
			     "\\(" idlwave-method-call "\\s *\\)?"
			     idlwave-identifier
			     "\\s *(")
		     nil))
   (cons 'call (list (concat
		      "\\(" idlwave-method-call "\\s *\\)?"
		      idlwave-identifier
		      "\\( *\\($\\|\\$\\)\\|\\s *,\\)")
		     nil))
   (cons 'assign (list (concat
			"\\(" idlwave-variable "\\) *=")
		       nil)))

  "Associated list of statement matching regular expressions.
Each regular expression matches the start of an IDL statement.
The first element of each association is a symbol giving the statement
type.  The associated value is a list.  The first element of this list
is a regular expression matching the start of an IDL statement for
identifying the statement type.  The second element of this list is a
regular expression for finding a substatement for the type.  The
substatement starts after the end of the found match modulo
whitespace.  If it is nil then the statement has no substatement.  The
list order matters since matching an assignment statement exactly is
not possible without parsing.  Thus assignment statement become just
the leftover unidentified statements containing an equal sign.")

(defvar idlwave-comment-indent-function 'comment-indent-function
  "IDL mode comment indent function.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar idlwave-comment-indent-char ?\s
  "Character to be inserted for IDL comment indentation.
Normally a space.")

(defconst idlwave-continuation-char ?$
  "Character which is inserted as a last character on previous line by
\\[idlwave-split-line] to begin a continuation line.  Normally $.")

(provide 'idlw-variables)
(provide 'idlwave-variables)
