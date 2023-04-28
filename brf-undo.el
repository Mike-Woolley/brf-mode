;;; brf-undo.el --- Brief Cursor Motion Undo -*- lexical-binding: t -*-

;; Copyright (C) 1999-2023 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  Implements Cursor Motion Undo just like in Brief.
;;  This works with both default Emacs Undo and Redo(+).el.
;;  Finally finished this off 20 years after I started it...

;;; Code:

(require 'brf-compat)
(eval-when-compile (require 'cl-lib))

(defcustom brf-undo-enable nil
  "Enable cursor motion undo."
  :type 'boolean
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'post-command-hook #'brf-undo-post-command-hook)
	   (remove-hook 'post-command-hook #'brf-undo-post-command-hook))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :group 'brf)

(defvar brf-undo-point (point-min)
  "The location of point after a command is executed.")
(make-variable-buffer-local 'brf-undo-point)
(defvar brf-undo-list-head nil
  "The first non-boundary item on the undo list after a command is executed.")
(make-variable-buffer-local 'brf-undo-list-head)

(defvar brf-undo-debug-enabled nil
  "Undo debug is enabled.")

(defconst brf-undo-debug-buffer-name "*Brf Debug*"
  "Name of undo debug buffer.")

(defvar brf-real-this-command nil
  "Real name of the current command.")
(add-hook 'pre-command-hook
	  (lambda () (setq brf-real-this-command this-command)))

(defun brf-undo-command-p ()
  "Return non-nil if COMMAND is an undo command."
  (memq brf-real-this-command '(undo undo-only)))

(defun brf-redo-command-p ()
  "Return non-nil if COMMAND is a redo command."
  (memq brf-real-this-command '(redo undo-redo)))

(defun brf-undo-post-command-hook ()
  "Post-command hook to implement cursor-motion undo."
  ;; Put point on the undo list if necessary
  (when (listp buffer-undo-list)
    (let ((point (point))
	  (head (car buffer-undo-list)))
      (unless head
	(setq head (cadr buffer-undo-list)))		; Real head is the second item
      (unless (brf-redo-command-p)			; Ignore if this is a redo
	;; Check if there was cursor motion with no other changes
	(when (and (/= point brf-undo-point) 		; Point has moved
		   (eq head brf-undo-list-head)) 	; and a change has not been made
	  (setq buffer-undo-list (cons brf-undo-point buffer-undo-list)
		head brf-undo-point)
	  ;; If we're undoing then the cursor motion is a redo, so mark it as such...
	  ;; Copied verbatim from `undo' in simple.el (unfortunately it's not in its own function)
	  (when (brf-undo-command-p)
	    (let ((list buffer-undo-list))
	      (puthash list
		       (cond (undo-in-region 'undo-in-region)
			     ((eq list pending-undo-list)
			      (or (gethash list undo-equiv-table)
				  'empty))
			     (t pending-undo-list))
		       undo-equiv-table))
	    ;; Add the terminal undo boundary
	    (undo-boundary))))

      ;; Save point and the undo-list head for next time
      (setq brf-undo-point point)
      (setq brf-undo-list-head head))

    ;; Debug output
    (when brf-undo-debug-enabled
      (brf-undo-debug))))

(defun brf-undo-debug ()
  "Show Undo state in a buffer for debugging."
  (unless (or (active-minibuffer-window)
	      (not (listp buffer-undo-list)))
    (let ((undo-list buffer-undo-list)
	  (pending pending-undo-list)
	  (point (point))
	  (buffer-name (buffer-name)))
      (let* ((buffer (get-buffer brf-undo-debug-buffer-name))
	     (window (get-buffer-window buffer t)))
	(when buffer
	  (save-current-buffer
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (let ((log-point (point))	; Current point in the debug buffer
		  (print-length 20)	; Only show this many undo elements
		  (print-level 2))	; and only 2 deep
	      ;; Show point, the current undo-list and its length
	      (insert buffer-name " (" (number-to-string point) "): " (prin1-to-string undo-list)
		      " (#" (prin1-to-string (length undo-list)) ")" ?\n)
	      ;; Also show what's on the pending list when we're undoing/redoing
	      (when (or (brf-undo-command-p) (brf-redo-command-p))
		(insert (prin1-to-string brf-real-this-command) " pending: " (prin1-to-string pending)
			(if (listp pending) (concat " (#" (prin1-to-string (length pending)) ")") "") ?\n)
		;; Show the matching Equiv table entry
		(while (null (car undo-list))
		  (setq undo-list (cdr undo-list)))
		(insert "Equiv(" (prin1-to-string (car undo-list)) "): "
			(prin1-to-string (gethash undo-list undo-equiv-table)) ?\n))
	      ;; Make sure the latest debug line is visible
	      (when (and (window-live-p window)
			 (not (pos-visible-in-window-p log-point window)))
		(set-window-start window log-point)))))))))

(defun brf-undo-toggle-debug (&optional arg)
  "Turn Undo debugging on or off with ARG."
  (interactive "P")
  (setq brf-undo-debug-enabled
	(if (null arg)
	    (not brf-undo-debug-enabled)
	  (> (prefix-numeric-value arg) 0)))

  (when brf-undo-debug-enabled
    (save-current-buffer
      (let ((buffer (get-buffer-create brf-undo-debug-buffer-name)))
	(set-buffer buffer)
	(buffer-disable-undo buffer)
	(erase-buffer)
	(display-buffer-at-bottom buffer nil)))))

(provide 'brf-undo)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf-undo.el ends here
