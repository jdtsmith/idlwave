;; idlw-shell.el --- run IDL as an inferior process of Emacs.
;; Copyright (c) 2002 Free Software Foundation

;; Author: J.D. Smith <jdsmith@as.arizona.edu>
;; Maintainer: J.D. Smith <jdsmith@as.arizona.edu>
;; Version: 1.0
;; Date: $Date: 2003/05/13 18:42:27 $
;; Keywords: processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Advises the appropriate comint function to make the IDL> prompt
;; read-only.  This functionality may be available in future versions
;; of comint directy, in which case this file will be obviated.
;;
;; New versions of IDLWAVE, documentation, and more information
;; available from:
;;                 http://idlwave.org
;;
;; INSTALLATION:
;; =============
;; 
;; Follow the instructions in the INSTALL file of the distribution.
;; In short, put this file on your load path and add the following
;; lines to your .emacs file:
;;
;;   (add-hook 'idlwave-shell-mode-hook
;;             (lambda () (require 'idlw-roprompt)))
;;
;;
;; SOURCE
;; ======
;;
;;   The newest version of this file can be found on the maintainers
;;   web site.
;; 
;;     http://idlwave.org
;; 
;; DOCUMENTATION
;; =============
;;
;; IDLWAVE is documented online in info format.
;; A printable version of the documentation is available from the
;; maintainers webpage (see under SOURCE)
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; 
;; This functionality will probably fail for some future version of
;; comint, given how explicitly
;;
;;--------------------------------------------------------------------------
;;

;;; Code:
(when (fboundp 'comint-snapshot-last-prompt)
  (defvar idlwave-shell-save-comint-last-prompt-overlay nil)
  (defun idlwave-shell-comint-signal-read-only (overlay after start end 
							&optional len)
    (if (and (not after)
	     (or (< (overlay-start overlay) start)
		 (> (overlay-end overlay) end)))
	(error "")))

  ;; Caution: in Emacs <~21.2, a new overlay gets created for each
  ;; prompt... in later versions, text-properties for old prompts
  ;; are used instead, and the original overlay is recycled.  In
  ;; this case, we can advise snapshot-prompt to remove the
  ;; read-only text properties (not the overlay properties), and
  ;; here we test to ensure the prompt isn't in the same position as
  ;; the process-mark before removing the read-only stuff.
  (defadvice idlwave-shell-comint-filter (around swap-read-only activate)
    "Add a read-only equivalency to the last prompt overlay."
;    (when (and idlwave-shell-save-comint-last-prompt-overlay 
;	       (not (equal
;		     (marker-position (process-mark 
;				       (get-buffer-process 
;					(get-buffer (idlwave-shell-buffer)))))
;		     (overlay-end 
;		      idlwave-shell-save-comint-last-prompt-overlay))))
;      (setq save-overlay idlwave-shell-save-comint-last-prompt-overlay)

    

    ;; Remove the read-only status in case comint needs to do
    ;; something with the prompt area.
    (save-current-buffer
      (set-buffer (idlwave-shell-buffer))
      (when (and idlwave-shell-save-comint-last-prompt-overlay
		 (not (eq idlwave-shell-save-comint-last-prompt-overlay 
			  comint-last-prompt-overlay)))
	(overlay-put idlwave-shell-save-comint-last-prompt-overlay 
		     'insert-in-front-hooks nil)
	(overlay-put idlwave-shell-save-comint-last-prompt-overlay
		     'modification-hooks nil))
      (when comint-last-prompt-overlay
	(overlay-put comint-last-prompt-overlay 'insert-in-front-hooks nil)
	(overlay-put comint-last-prompt-overlay 'modification-hooks nil)))

    ad-do-it
    
    (save-current-buffer
      (set-buffer (idlwave-shell-buffer))
      (when comint-last-prompt-overlay
	(setq idlwave-shell-save-comint-last-prompt-overlay 
	      comint-last-prompt-overlay)
	(overlay-put comint-last-prompt-overlay 'modification-hooks 
		     '(idlwave-shell-comint-signal-read-only))
	(overlay-put comint-last-prompt-overlay 'insert-in-front-hooks 
		     '(idlwave-shell-comint-signal-read-only)))))

  (defadvice comint-snapshot-last-prompt (after remove-text-read-only activate)
    "Remove the read-only text properties potentially set by snapshot"
    (save-current-buffer
      (set-buffer (idlwave-shell-buffer))
      (when comint-last-prompt-overlay
	(remove-text-properties 
	 (overlay-start comint-last-prompt-overlay)
	 (overlay-end comint-last-prompt-overlay)
	 '(modification-hooks nil insert-in-front-hooks nil))))))

(provide 'idlw-roprompt)
;;; idlw-roprompt.el ends here