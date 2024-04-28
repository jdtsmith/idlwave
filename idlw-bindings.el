;;; idlw-bindings ---  IDLWAVE bindings  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2024  Free Software Foundation, Inc.

;;; Code
(require 'idlw-variables)   ;At least for `idlwave-abbrev-start-char'.

(defalias 'idlwave-debug-map (make-sparse-keymap))

;;----------------------------------------------------
;; Keyboard bindings, in buffer
(defvar idlwave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c "    #'idlwave-hard-tab)
    (define-key map [(control tab)] #'idlwave-hard-tab)
    ;;(define-key map "\C-c\C- " #'idlwave-hard-tab)
    (define-key map "'"        #'idlwave-show-matching-quote)
    (define-key map "\""       #'idlwave-show-matching-quote)
    (define-key map "\C-g"     #'idlwave-keyboard-quit)
    (define-key map "\C-c;"    #'idlwave-toggle-comment-region)
    (define-key map "\C-\M-a"  #'idlwave-beginning-of-subprogram)
    (define-key map "\C-\M-e"  #'idlwave-end-of-subprogram)
    (define-key map "\C-c{"    #'idlwave-beginning-of-block)
    (define-key map "\C-c}"    #'idlwave-end-of-block)
    (define-key map "\C-c]"    #'idlwave-close-block)
    (define-key map [(meta control h)] #'idlwave-mark-subprogram)
    (define-key map "\M-\C-n"  #'idlwave-forward-block)
    (define-key map "\M-\C-p"  #'idlwave-backward-block)
    (define-key map "\M-\C-d"  #'idlwave-down-block)
    (define-key map "\M-\C-u"  #'idlwave-backward-up-block)
    (define-key map "\M-\r"    #'idlwave-split-line)
    (define-key map "\M-\C-q"  #'idlwave-indent-subprogram)
    (define-key map "\C-c\C-p" #'idlwave-previous-statement)
    (define-key map "\C-c\C-n" #'idlwave-next-statement)
    ;; (define-key map "\r"       #'idlwave-newline)
    ;; (define-key map "\t"       #'idlwave-indent-line)
    (define-key map [(shift iso-lefttab)] #'idlwave-indent-statement)
    (define-key map "\C-c\C-a" #'auto-fill-mode)
    (define-key map "\M-q"     #'idlwave-fill-paragraph)
    (define-key map "\M-s"     #'idlwave-edit-in-idlde)
    (define-key map "\C-c\C-h" #'idlwave-doc-header)
    (define-key map "\C-c\C-m" #'idlwave-doc-modification)
    (define-key map "\C-c\C-c" #'idlwave-case)
    (define-key map "\C-c\C-d" #'idlwave-debug-map)
    (when (and (listp idlwave-shell-debug-modifiers)
               (not (equal idlwave-shell-debug-modifiers '())))
      ;; Bind the debug commands also with the special modifiers.
      (let ((shift (memq 'shift idlwave-shell-debug-modifiers))
            (mods-noshift
             (delq 'shift (copy-sequence idlwave-shell-debug-modifiers))))
        (define-key map
          (vector (append mods-noshift (list (if shift ?C ?c))))
          #'idlwave-shell-save-and-run)
        (define-key map
          (vector (append mods-noshift (list (if shift ?B ?b))))
          #'idlwave-shell-break-here)
        (define-key map
          (vector (append mods-noshift (list (if shift ?E ?e))))
          #'idlwave-shell-run-region)))
    (define-key map "\C-c\C-d\C-c" #'idlwave-shell-save-and-run)
    (define-key map "\C-c\C-d\C-b" #'idlwave-shell-break-here)
    (define-key map "\C-c\C-d\C-e" #'idlwave-shell-run-region)
    (define-key map "\C-c\C-f" #'idlwave-for)
    ;;  (define-key map "\C-c\C-f" #'idlwave-function)
    ;;  (define-key map "\C-c\C-p" #'idlwave-procedure)
    (define-key map "\C-c\C-r" #'idlwave-repeat)
    (define-key map "\C-c\C-w" #'idlwave-while)
    (define-key map "\C-c\C-k" #'idlwave-kill-autoloaded-buffers)
    (define-key map "\C-c\C-s" #'idlwave-shell)
    (define-key map "\C-c\C-l" #'idlwave-shell-recenter-shell-window)
    (define-key map "\C-c\C-b" #'idlwave-list-buffer-load-path-shadows)
    (define-key map "\C-c\C-v"   #'idlwave-find-module)
    (define-key map "\C-c\C-t"   #'idlwave-find-module-this-file)
    (define-key map "\C-c?"      #'idlwave-routine-info)
    (define-key map "\M-?"       #'idlwave-context-help)
    (define-key map [(control meta ?\?)]
      #'idlwave-help-with-topic)
    ;; Don't pickup both forms of Esc/Meta binding, since that overrides
    ;; the tab => \t remap and thus other \t bindings of higher priority!
    ;; (define-key map [(meta tab)] #'idlwave-complete)
    ;; FIXME: Use `completion-at-point'!
    (define-key map [?\e?\t] #'idlwave-complete)
    (define-key map "\M-\C-i" #'idlwave-complete)
    (define-key map "\C-c\C-i" #'idlwave-update-routine-info)
    (define-key map "\C-c="    #'idlwave-resolve)
    (define-key map [(shift mouse-3)] #'idlwave-mouse-context-help)
    map)
  "Keymap used in IDL mode.")

;;----------------------------------------------------
;; Keyboard bindings, in routine info window
(defvar idlwave-rinfo-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'idlwave-mouse-active-rinfo)
    (define-key map [(shift mouse-2)] #'idlwave-mouse-active-rinfo-shift)
    (define-key map [mouse-3] #'idlwave-mouse-active-rinfo-right)
    (define-key map " " #'idlwave-active-rinfo-space)
    (define-key map "q" #'idlwave-quit-help)
    map))
(defvar idlwave-rinfo-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'idlwave-quit-help)
    map))

;;----------------------------------------------------
;;  Keyboard bindings, in source-help window
(defvar idlwave-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'idlwave-help-quit)
    (define-key map "w" #'widen)
    (define-key map "\C-m" (lambda (arg)
			     (interactive "p")
			     (scroll-up arg)))
    (define-key map " " #'scroll-up-command)
    (define-key map [?\S-\ ] #'scroll-down-command)
    (define-key map [delete] #'scroll-down-command)
    (define-key map "h" #'idlwave-help-find-header)
    (define-key map "H" #'idlwave-help-find-first-header)
    (define-key map "." #'idlwave-help-toggle-header-match-and-def)
    (define-key map "F" #'idlwave-help-fontify)
    (define-key map "\M-?" #'idlwave-help-return-to-calling-frame)
    (define-key map "x" #'idlwave-help-return-to-calling-frame)
    map)
  "The keymap used in `idlwave-help-mode'.")

;;----------------------------------------------------
;; Keyboard utility callbacks
(defun idlwave-keyboard-quit ()
  (interactive)
  (unwind-protect
      (if (eq (car-safe last-command) 'idlwave-display-completion-list)
	  (idlwave-restore-wconf-after-completion))
    (keyboard-quit)))


;;----------------------------------------------------
;; Syntax
(defvar idlwave-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+   "."  st)
    (modify-syntax-entry ?-   "."  st)
    (modify-syntax-entry ?*   "."  st)
    (modify-syntax-entry ?/   "."  st)
    (modify-syntax-entry ?^   "."  st)
    (modify-syntax-entry ?#   "."  st)
    (modify-syntax-entry ?=   "."  st)
    (modify-syntax-entry ?%   "."  st)
    (modify-syntax-entry ?<   "."  st)
    (modify-syntax-entry ?>   "."  st)
    (modify-syntax-entry ?\'  "\"" st)
    (modify-syntax-entry ?\"  "\"" st)
    (modify-syntax-entry ?\\  "."  st)
    (modify-syntax-entry ?_   "_"  st)
    (modify-syntax-entry ?{   "(}" st)
    (modify-syntax-entry ?}   "){" st)
    (modify-syntax-entry ?$   "_"  st)
    (modify-syntax-entry ?.   "."  st)
    (modify-syntax-entry ?\;  "<"  st)
    (modify-syntax-entry ?\n  ">"  st)
    (modify-syntax-entry ?\f  ">"  st)
    st)
  "Syntax table in use in `idlwave-mode' buffers.")

(defvar idlwave-find-symbol-syntax-table
  (let ((st (copy-syntax-table idlwave-mode-syntax-table)))
    (modify-syntax-entry ?$   "w"  st)
    (modify-syntax-entry ?_   "w"  st)
    (modify-syntax-entry ?!   "w"  st)
    (modify-syntax-entry ?.   "w"  st)
    st)
  "Syntax table that treats symbol characters as word characters.")

(defun idlwave-action-and-binding (key cmd &optional select)
  "KEY and CMD are made into a key binding and an indent action.
KEY is a string - same as for the `define-key' function.  CMD is a
function of one argument.  CMD is bound to
KEY in `idlwave-mode-map' by defining an anonymous function calling
`self-insert-command' followed by CMD.  If KEY contains more than one
character a binding will only be set if SELECT is `both'.

\(KEY . CMD) is also placed in the `idlwave-indent-expand-table',
replacing any previous value for KEY.  If a binding is not set then it
will instead be placed in `idlwave-indent-action-table'.

If the optional argument SELECT is nil then an action and binding are
created.  If SELECT is `noaction', then a binding is always set and no
action is created.  If SELECT is `both' then an action and binding
will both be created even if KEY contains more than one character.
Otherwise, if SELECT is non-nil then only an action is created.

Some examples:
No spaces before and 1 after a comma
   (idlwave-action-and-binding \",\"  (lambda (_) (idlwave-surround 0 1)))
A minimum of 1 space before and after `=' (see `idlwave-expand-equal').
   (idlwave-action-and-binding \"=\"  (lambda (_) (idlwave-expand-equal -1 -1)))
Capitalize system variables - action only
   (idlwave-action-and-binding idlwave-sysvar
                               (lambda (_) (capitalize-word 1) t) t)"
  (if (not (equal select 'noaction))
      ;; Add action
      (let* ((table (if select 'idlwave-indent-action-table
                      'idlwave-indent-expand-table))
	     (table-key (regexp-quote key)))
        (setf (alist-get table-key (symbol-value table) nil nil #'equal) cmd)))
  ;; Make key binding for action
  (if (if (null select) (= (length key) 1)
        (memq select '(noaction both)))
      ;; FIXME: Use `post-self-insert-hook'!
      (define-key idlwave-mode-map key
        (lambda ()
	  (interactive)
	  (self-insert-command 1)
	  (if (functionp cmd) (funcall cmd nil) (eval cmd t))))))

;; Set action and key bindings.
;; See description of the function `idlwave-action-and-binding'.
;; Automatically add spaces for the following characters

;; Actions for & are complicated by &&
(idlwave-action-and-binding "&"  #'idlwave-custom-ampersand-surround)

;; Automatically add spaces to equal sign if not keyword.  This needs
;; to go ahead of > and <, so >= and <= will be treated correctly
(idlwave-action-and-binding "="  (lambda (_) (idlwave-expand-equal -1 -1)))

;; Actions for > and < are complicated by >=, <=, and ->...
(idlwave-action-and-binding "<"  (lambda (a) (idlwave-custom-ltgtr-surround nil a)))
(idlwave-action-and-binding ">"  (lambda (a) (idlwave-custom-ltgtr-surround t a)))

(idlwave-action-and-binding ","  (lambda (a) (idlwave-surround 0 -1 1 a)))


;;----------------------------------------------------
;; Abbreviations
;;
;; When expanding abbrevs and the abbrev hook moves backward, an extra
;; space is inserted (this is the space typed by the user to expanded
;; the abbrev).
;; FIXME: This can be controlled with `no-self-insert' property.
;;
(define-abbrev-table 'idlwave-mode-abbrev-table ()
  "Abbreviation table used for IDLWAVE mode."
  :enable-function (lambda () (not (idlwave-quoted))))

(defun idlwave-modify-abbrev (arg &optional reserved)
  "Tweak the abbrev we just expanded.
Argument ARG is the number of characters to move point
backward if `idlwave-abbrev-move' is non-nil.
If optional argument RESERVED is non-nil then the expansion
consists of reserved words, which will be capitalized if
`idlwave-reserved-word-upcase' is non-nil.
Otherwise, the abbrev will be capitalized if `idlwave-abbrev-change-case'
is non-nil, unless its value is `down' in which case the abbrev will be
made into all lowercase.
Returns non-nil if abbrev is left expanded."
  (require 'idlwave)
  (defvar idlwave--command-function)
  (if (and reserved idlwave-reserved-word-upcase)
      (upcase-region last-abbrev-location (point))
    (cond
     ((equal idlwave-abbrev-change-case 'down)
      (downcase-region last-abbrev-location (point)))
     (idlwave-abbrev-change-case
      (upcase-region last-abbrev-location (point)))))
  (if (and idlwave-abbrev-move (> arg 0))
      (setq idlwave--command-function (lambda () (backward-char (1+ arg)))))
  t)

(defun idlwave-define-abbrev (name expansion hook &optional noprefix table)
  ;; FIXME: `table' is never passed.
  "Define-abbrev with backward compatibility.

If NOPREFIX is non-nil, don't prepend prefix character.  Installs into
`idlwave-mode-abbrev-table' unless TABLE is non-nil."
  (let ((abbrevs-changed nil)  ;; mask the current value to avoid save
	(args (list (or table idlwave-mode-abbrev-table)
		    (if noprefix name (concat idlwave-abbrev-start-char name))
		    expansion
		    hook)))
    (condition-case nil
	(apply #'define-abbrev (append args '(0 t)))
      (error (apply #'define-abbrev args)))))

(defun idlwave-expand-region-abbrevs (start end)
  "Expand each abbrev occurrence in the region.
Calling from a program, arguments are START END."
  (interactive "r")
  (save-excursion
    (goto-char (min start end))
    (let ((idlwave-show-block nil)          ;Do not blink
          (idlwave-abbrev-move nil))        ;Do not move
      (expand-region-abbrevs start end 'noquery))))

(defun idlwave-keyword-abbrev (&rest args)
  "Create a function for abbrev hooks to call `idlwave-check-abbrev' with ARGS."
  (lambda () (apply #'idlwave-modify-abbrev args)))

(condition-case nil
    (modify-syntax-entry (string-to-char idlwave-abbrev-start-char)
			 "w" idlwave-mode-syntax-table)
  (error nil))


;;----------------------------------------------------
;; Templates

(idlwave-define-abbrev "c"    "" #'idlwave-case)
(idlwave-define-abbrev "sw"   "" #'idlwave-switch)
(idlwave-define-abbrev "f"    "" #'idlwave-for)
(idlwave-define-abbrev "fe"   "" #'idlwave-foreach)
(idlwave-define-abbrev "fu"   "" #'idlwave-function)
(idlwave-define-abbrev "pr"   "" #'idlwave-procedure)
(idlwave-define-abbrev "r"    "" #'idlwave-repeat)
(idlwave-define-abbrev "w"    "" #'idlwave-while)
(idlwave-define-abbrev "i"    "" #'idlwave-if)
(idlwave-define-abbrev "elif" "" #'idlwave-elif)
;;
;; Keywords, system functions, conversion routines
;;
(idlwave-define-abbrev "ap" "arg_present()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "b"  "begin"        (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "co" "common"       (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "cb" "byte()"       (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cx" "fix()"        (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cl" "long()"       (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cf" "float()"      (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cs" "string()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cc" "complex()"    (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cd" "double()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "e"  "else"         (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "ec" "endcase"      #'idlwave-show-begin)
(idlwave-define-abbrev "es" "endswitch"    #'idlwave-show-begin)
(idlwave-define-abbrev "ee" "endelse"      #'idlwave-show-begin)
(idlwave-define-abbrev "ef" "endfor"       #'idlwave-show-begin)
(idlwave-define-abbrev "ei" "endif else if" #'idlwave-show-begin)
(idlwave-define-abbrev "el" "endif else"   #'idlwave-show-begin)
(idlwave-define-abbrev "en" "endif"        #'idlwave-show-begin)
(idlwave-define-abbrev "er" "endrep"       #'idlwave-show-begin)
(idlwave-define-abbrev "ew" "endwhile"     #'idlwave-show-begin)
(idlwave-define-abbrev "g"  "goto,"        (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "h"  "help,"        (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "k"  "keyword_set()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "n"  "n_elements()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "on" "on_error,"    (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "oi" "on_ioerror,"  (idlwave-keyword-abbrev 0 1))
(idlwave-define-abbrev "ow" "openw,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "or" "openr,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "ou" "openu,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "p"  "print,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "pt" "plot,"        (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "re" "read,"        (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "rf" "readf,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "ru" "readu,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "rt" "return"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "sc" "strcompress()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sn" "strlen()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sl" "strlowcase()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "su" "strupcase()"  (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sm" "strmid()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sp" "strpos()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "st" "strput()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sr" "strtrim()"    (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "t"  "then"         (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "u"  "until"        (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "wu" "writeu,"      (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "iap" "if arg_present() then"     (idlwave-keyword-abbrev 6))
(idlwave-define-abbrev "ik" "if keyword_set() then" (idlwave-keyword-abbrev 6))
(idlwave-define-abbrev "ine" "if n_elements() eq 0 then" (idlwave-keyword-abbrev 11))
(idlwave-define-abbrev "inn" "if n_elements() ne 0 then" (idlwave-keyword-abbrev 11))
(idlwave-define-abbrev "np" "n_params()"   (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "s"  "size()"       (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "wi" "widget_info()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "wc" "widget_control," (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "pv" "ptr_valid()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "ipv" "if ptr_valid() then" (idlwave-keyword-abbrev 6))

;; This section is reserved words only. (From IDL user manual)
;;
(idlwave-define-abbrev "and"        "and"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "begin"      "begin"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "break"      "break"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "case"       "case"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "common"     "common"    (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "continue"   "continue"  (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "do"         "do"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "else"       "else"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "end"        "end"       #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endcase"    "endcase"   #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endelse"    "endelse"   #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endfor"     "endfor"    #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endforeach" "endforeach" #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endif"      "endif"     #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endrep"     "endrep"    #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endswitch"  "endswitch" #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endwhi"     "endwhi"    #'idlwave-show-begin-check t)
(idlwave-define-abbrev "endwhile"   "endwhile"  #'idlwave-show-begin-check t)
(idlwave-define-abbrev "eq"         "eq"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "for"        "for"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "function"   "function"  (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "ge"         "ge"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "goto"       "goto"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "gt"         "gt"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "if"         "if"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "le"         "le"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "lt"         "lt"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "mod"        "mod"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "ne"         "ne"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "not"        "not"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "of"         "of"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "on_ioerror" "on_ioerror" (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "or"         "or"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "pro"        "pro"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "repeat"     "repeat"    (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "switch"     "switch"    (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "then"       "then"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "until"      "until"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "while"      "while"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "xor"        "xor"       (idlwave-keyword-abbrev 0 t) t)

(provide 'idlw-bindings)
(provide 'idlwave-bindings)
