;;; b.el --- Brief editor emulator

;; Copyright (C) 2001 Mike Woolley mike@bulsara.com
;; Author: Mike Woolley <mike@bulsara.com>
;; Version: 1.07
;; Keywords: brief editing tools

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  This package provides an implementation of the some features that
;;  I miss from the old DOS editor `Brief'. Principally, these are:
;;
;;  * Line-mode cut and paste.
;;  * Column-mode cut and paste (not implemented yet).
;;  * Decent paging and scrolling.
;;  * Temporary bookmarks.
;;  * Cursor motion undo (not fully working yet).
;;
;;  However, the functions have been implemented in an Emacs-style
;;  way, respond to prefix args and where they override Emacs
;;  functions live on the Emacs key bindings etc.
;;
;;  The code has been tested on Emacs 20, Emacs 21 pretest
;;  and XEmacs 21.1 & 21.2.

;;; Change Log:
;;
;;  Version 1.07 2001-08-12 Mike Woolley <mike@bulsara.com>
;;  * Lots of small changes and bug fixes.
;;
;;  Version 1.06 2001-08-02 Mike Woolley <mike@bulsara.com>
;;  * Renamed to b-mode, due to the large number of `brief.el's out
;;  there and particularly because this mode is not really an emulation
;;  of Brief, more a homage to Brief in Emacs.
;;  * Added new commands to cycle backwards and forwards through the
;;  bookmarks and to list them.
;;  * Added new prefix key \C-c\c-b for infrequently used commands for
;;  this mode.
;;
;;  Version 1.05 2001-05-21 Mike Woolley <mike@bulsara.com>
;;  * Fixed some minor problems in the bookmark code.
;;  * Now displays the bookmark number in the overlay.
;;  * Turned `brief-protect-overlay' into a closure.
;;  * Add command to remove bookmarks.
;;
;;  Version 1.04 2001-03-12 Mike Woolley <mike@bulsara.com>
;;  * Added bookmarks.
;;  * Moved the XEmacs specific code into functions.
;;  * Removed the byte compiler warnings.
;;
;;  Version 1.03 2001-02-22 Mike Woolley <mike@bulsara.com>
;;  * Added tab key handling.
;;  * newline-and-indent setup in global map.
;;  * Tidied up doc strings.
;;
;;  Version 1.02 2001-02-15 Mike Woolley <mike@bulsara.com>
;;  * Changed M-d to delete a line rather than kill a line.
;;
;;  Version 1.01 - Mike Woolley <mike@bulsara.com>
;;  * Added Brief-style Home and End key handling
;;
;;  Version 1.00 - Mike Woolley <mike@bulsara.com>
;;  * Initial version.
;;  * Cursor motion undo not working yet.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup b nil
  "Emulator for Brief."
  :prefix "b-"
  :group 'editing)

;;;###autoload
(defcustom b-mode nil
  "Track status of B mode.
A value of t indicates B mode is enabled.
A value of nil means B mode is not enabled.

Setting this variable directly does not take effect;
use either \\[execute-extended-command] customize or the function `b-mode'."
  :type 'boolean
  :set (lambda (symbol value) (b-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'b
  :version "20.4"
  :group 'b)

(defcustom b-undo-enable nil
  "Enable cursor motion undo."
  :type 'boolean
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'post-command-hook 'b-undo-post-command-hook)
	   (remove-hook 'post-command-hook 'b-undo-post-command-hook))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :group 'b)

(defcustom b-mode-modeline-string " B"
  "String to display in the mode-line when B mode is enabled.
Set this to nil to conserve valuable mode line space."
  :type 'string
  :group 'b)

(defface b-bookmark-face '((t (:background "khaki")))
  "Face used to show bookmark."
  :group 'b)

(defcustom b-mode-hook nil
  "Hook run on entering B mode"
  :type 'hook
  :group 'b)

;;
;; Version
;;
(defconst b-version "1.07"
  "Version number of B mode.")

(defun b-version ()
  "Version number of B mode."
  (interactive)
  (message (concat "B version " b-version)))

;;
;; XEmacs compatibility
;;
(eval-and-compile
  (defconst b-xemacs-flag (featurep 'xemacs)
    "Non-nil means this version of Emacs is XEmacs."))

;; Silence the byte compiler
(eval-when-compile
  (cond (b-xemacs-flag
	 (defvar mark-active nil)
	 (defvar transient-mark-mode nil))
	(t ; GNU Emacs
	 (defvar zmacs-region-active-p nil)
	 (defvar zmacs-regions nil)
	 (defvar zmacs-region-stays nil)
	 (fset 'zmacs-update-region nil)
	 (fset 'zmacs-activate-region nil)
	 (fset 'zmacs-deactivate-region nil))))

;; Load XEmacs overlay compatibility
(when b-xemacs-flag
  (require 'overlay))

;; Function to get the position of the beginning of the line
(cond ((fboundp 'line-beginning-position)
       (defalias 'b-bol-position 'line-beginning-position))
      ((fboundp 'point-at-bol)
       (defalias 'b-bol-position 'point-at-bol))
      (t
       (defun b-bol-position (&optional n)
	 "Return the index of the character at the start of the line."
	 (save-excursion
	   (beginning-of-line n)
	   (point)))))

(defun b-region-active-p ()
  "Emacs/XEmacs compatibility function to test for an active region."
  (if b-xemacs-flag
      zmacs-region-active-p
    mark-active))

(defun b-transient-mark-mode (arg)
  "Emacs/XEmacs compatibility function to set transient mark mode.
Returns the previous setting."
  (if b-xemacs-flag
      (prog1 zmacs-regions
	(setq zmacs-regions arg))
    (prog1 transient-mark-mode
      (setq transient-mark-mode arg))))

(defun b-activate-region ()
  "Ensure region is highlit correctly in XEmacs.
This function does nothing in GNU Emacs."
  (when b-xemacs-flag
    (if zmacs-region-active-p
	(zmacs-update-region)
      (zmacs-activate-region))))

(defun b-deactivate-region (&optional force)
  "Ensure region is deactivated.
This function does nothing in GNU Emacs, as redisplay clears the region,
unless the optional arg FORCE is set."
  (cond (b-xemacs-flag
	 (when zmacs-region-active-p
	   (zmacs-deactivate-region)))
	(t ; GNU Emacs
	 (when force
	   (deactivate-mark)))))

(defun b-keep-region ()
  "Ensure that the current command keeps the region in XEmacs.
This function does nothing in GNU Emacs."
  (when b-xemacs-flag
    (setq zmacs-region-stays t)))

;;
;; Bookmarks
;;
(defstruct b-bookmark
  "Bookmark."
  (number :read-only t)
  (marker :read-only t)
  (overlay :read-only t))

(defconst b-max-bookmarks 10
  "The maximum number of bookmarks.")

(defvar b-bookmarks (make-vector b-max-bookmarks nil)
  "Bookmark vector.")

(defvar b-current-bookmark nil
  "Last bookmark set or jumped to.
This is used as the start point for the next/prev bookmark commands.")

(defun b-valid-bookmark-number-p (number)
  "Return t if NUMBER is inside the range of valid bookmark numbers."
  (and (>= number 0) (< number b-max-bookmarks)))

(defun b-valid-bookmark-p (bookmark)
  "Return non-nil if the given bookmark is set, nil otherwise."
  (and bookmark (marker-buffer (b-bookmark-marker bookmark))))

(defun b-read-bookmark-number (prompt)
  "Read the bookmark number from the minibuffer as a single character digit.
The user is prompted with PROMPT, which can be `nil' for no prompt.
This function is meant to be called from a command's interactive form."
  (let ((number (or current-prefix-arg
		   (- (read-char prompt) ?0))))
    (unless (b-valid-bookmark-number-p number)
      (error (format "Invalid bookmark number %d" number)))
    (list number)))

(defun b-make-set-bookmark (number)
  "Generate a command which sets bookmark NUMBER at point.
If the command is given a prefix argument, then the bookmark is removed."
  `(lambda (&optional arg)
     ,(format "Set bookmark %d at point.
With ARG, remove the bookmark instead." number)
     (interactive "P")
     (if arg
	 (b-kill-bookmark ,number)
       (b-set-bookmark ,number))))

(defun b-set-bookmark (number)
  "Set bookmark NUMBER at point."
  (interactive (b-read-bookmark-number "Set Bookmark: "))

  ;; Don't allow bookmark to be dropped in the minibuffer
  (when (window-minibuffer-p (selected-window))
    (error "Bookmark not allowed in minibuffer"))

  ;; Lookup the bookmark and calculate the position of the overlay
  (let ((bookmark (aref b-bookmarks number))
	(start (b-bol-position 1))
 	(end (b-bol-position 2)))

    (cond ((null bookmark)
	   ;; Create a new bookmark
	   (lexical-let ((marker (point-marker))
			 (overlay (make-overlay start end (current-buffer) t nil)))
	     (set-marker-insertion-type marker t) ; Insert before the marker
	     (overlay-put overlay 'face 'b-bookmark-face)
 	     (overlay-put overlay 'before-string (format "%d>" number))
	     (overlay-put overlay 'help-echo (format "Bookmark %d" number))

	     ;; Ensure the bookmark overlay is on the line containing the bookmark
	     (unless b-xemacs-flag ;; XEmacs overlay compatibility doesn't support modification hook :-(
	       (let ((protect-overlay
		      #'(lambda (overlay after begin end &optional len)
			  "Ensure the bookmark overlay is on the line containing the bookmark."
			  (when after
			    (save-excursion
			      (goto-char marker)
			      (move-overlay overlay (b-bol-position 1) (b-bol-position 2)))))))
		 (overlay-put overlay 'modification-hooks (list protect-overlay))
		 (overlay-put overlay 'insert-in-front-hooks (list protect-overlay))))

	     ;; Add the new bookmark to the vector
	     (setf (aref b-bookmarks number)
		   (make-b-bookmark :number number :marker marker :overlay overlay))))

	  ;; Move existing bookmark to new location
	  (t
	   (let ((buffer (current-buffer)))
	     (move-marker (b-bookmark-marker bookmark) (point) buffer)
	     (move-overlay (b-bookmark-overlay bookmark) start end buffer)))))

  (setq b-current-bookmark number)
  (message "Bookmark %d dropped" number))

(defun b-kill-bookmark (number)
  "Kill bookmark NUMBER."
  (interactive (b-read-bookmark-number "Kill Bookmark: "))
  (let ((bookmark (aref b-bookmarks number)))
    (unless (b-valid-bookmark-p bookmark)
      (error (format "Bookmark %d is not set" number)))
    (move-marker (b-bookmark-marker bookmark) nil)
    (delete-overlay (b-bookmark-overlay bookmark))))

(defun b-kill-all-bookmarks ()
  "Kill all bookmarks."
  (interactive)
  (dotimes (number b-max-bookmarks)
    (let ((bookmark (aref b-bookmarks number)))
      (when (b-valid-bookmark-p bookmark)
	(b-kill-bookmark number)))))

(defun b-jump-to-bookmark (number)
  "Jump to bookmark NUMBER."
  ;; Read the bookmark number
  (interactive (b-read-bookmark-number "Jump to Bookmark: "))

  ;; Lookup the bookmark
  (let ((bookmark (aref b-bookmarks number)))
    (unless (b-valid-bookmark-p bookmark)
      (error (format "Bookmark %d is not set" number)))
    (let ((marker (b-bookmark-marker bookmark)))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)))
  (setq b-current-bookmark number))

(defun b-next-bookmark (&optional arg)
  "Jump to the next bookmark.
  With ARG jump to the previous one."
  (interactive "P")
  (when (null b-current-bookmark)
    (error "No bookmarks have been set"))

  ;; Work out if we're going forwards or backwards through the bookmarks
  (let ((dir-fn (if arg #'- #'+)))
    ;; Find the next bookmark in that direction
    (dotimes (i b-max-bookmarks (error "No bookmarks have been set"))
      (let* ((number (mod (funcall dir-fn b-current-bookmark i 1) b-max-bookmarks))
	     (bookmark (aref b-bookmarks number)))
	(when (b-valid-bookmark-p bookmark)
	  (b-jump-to-bookmark number)
	  (return))))))

(defun b-prev-bookmark (&optional arg)
  "Jump to the previous bookmark.
With ARG jump to the next one."
  (interactive "P")
  (b-next-bookmark (null arg)))

(defun b-list-bookmarks ()
  (interactive)
  (when (null b-current-bookmark)
    (error "No bookmarks have been set"))

  ;; List selection buffer is provided by `generic-menu'
  (eval-and-compile
    (require 'generic-menu))

  (gm-popup :buffer-name "*Bookmarks*"
	    :header-line "Bookmarks: [SELECT] to Jump to bookmark, [q] to Quit."
	    :max-entries b-max-bookmarks
	    :truncate-lines t
	    :regexp-start-position (format "^[* ][ \t]+%d" b-current-bookmark)

	    :elements (loop for idx from 0 to (1- b-max-bookmarks)
			    collect idx)

	    :select-callback
	    #'(lambda (idx)
		(let ((bookmark (aref b-bookmarks idx)))
		  (cond ((b-valid-bookmark-p bookmark)
			 (gm-quit)
			 (b-jump-to-bookmark idx))
			(t ; Bookmark not set
			 (message "Bookmark %d has not been set" idx)
			 (ding)))))

 	    :display-string-function
	    #'(lambda (idx)
		(let ((bookmark (aref b-bookmarks idx)))
		  (cond ((b-valid-bookmark-p bookmark)
			 (let* ((marker (b-bookmark-marker bookmark))
				(buffer (marker-buffer marker)))
			   (format "%s %d\t%d%%\t%s"
				   (if (= b-current-bookmark idx) "*" " ")
				   idx
				   (/ (* (marker-position marker) 100) (buffer-size buffer))
				   buffer)))
			(t ; Bookmark not set
			 (format "  %d <NOT SET>" idx)))))))

;;
;; Keymap
;;
(defvar b-mode-map nil
  "Local keymap for B mode.")
(unless b-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Put our definitions on the same keys as Brief
    (define-key map [(control return)] 'b-insert-line)
    (define-key map "\M-d" 'b-delete-line)
    (define-key map "\M-l" 'b-mark-line)
    (define-key map [(kp-add)] 'b-copy-region)
    (define-key map [(kp-subtract)] 'b-kill-region)
    (define-key map [(kp-multiply)] 'undo)
    (define-key map [(delete)] 'b-delete)
    (define-key map [(meta up)] 'b-row-up)
    (define-key map [(meta down)] 'b-row-down)
    (define-key map [(home)] 'b-home)
    (define-key map [(end)] 'b-end)
    (define-key map "\t" 'b-tab)
    (define-key map "\M-=" 'b-next-bookmark)
    (define-key map [(meta kp-add)] 'b-next-bookmark)
    (define-key map "\M--" 'b-prev-bookmark)
    (define-key map [(meta kp-subtract)] 'b-prev-bookmark)

    ;; Also put them on the Emacs keys
    (substitute-key-definition 'kill-ring-save 'b-copy-region map (current-global-map))
    (substitute-key-definition 'kill-region 'b-kill-region map (current-global-map))
    (substitute-key-definition 'yank 'b-yank map (current-global-map))
    (substitute-key-definition 'yank-pop 'b-yank-pop map (current-global-map))
    (substitute-key-definition 'beginning-of-line 'b-home map (current-global-map))
    (substitute-key-definition 'end-of-line 'b-end map (current-global-map))

    ;; Create new key bindings for my new functions that weren't part of Brief
    (define-key map "\C-c\C-b\C-n" 'b-next-bookmark)
    (define-key map "\C-c\C-b\C-p" 'b-prev-bookmark)
    (define-key map "\C-c\C-b\C-k" 'b-kill-all-bookmarks)
    (define-key map "\C-c\C-b\C-l" 'b-list-bookmarks)

    ;; Try and find the existing commands for scrolling up/down,
    ;; as these are different in Emacs & XEmacs
    (let ((scroll-up-cmd (global-key-binding [(next)]))
	  (scroll-down-cmd (global-key-binding [(prior)])))
      (when scroll-up-cmd
	(substitute-key-definition scroll-up-cmd 'b-page-down map (current-global-map)))
      (when scroll-down-cmd
	(substitute-key-definition scroll-down-cmd 'b-page-up map (current-global-map))))

    ;; Setup the bookmarks on the M-digit keys, like in Brief
    ;; Prefix args will have to be entered with the C-digit or C-U number methods...
    (dotimes (digit b-max-bookmarks)
      (define-key map (vector (list 'meta (+ digit ?0))) (b-make-set-bookmark digit)))
    (define-key map "\M-j" 'b-jump-to-bookmark)

    (setq b-mode-map map)))

;;
;; Brief insert-line command
;;
(defun b-insert-line (&optional arg)
  "Open a new line underneath the current one and indent point.
Do not break current line.  Emulates the Brief insert-line function.
With ARG, do not indent."
  (interactive "*P")
  (end-of-line)
  (if arg
      (newline)
    (newline-and-indent)))

;;
;; Brief delete-line command
;;
(defun b-delete-line (&optional arg)
  "Delete the current line from anywhere on the line.
Emulates the Brief delete-line function.
With ARG, do it that many times."
  (interactive "*P")
  (let ((count (prefix-numeric-value arg))
	(column (current-column))
	start end)
    (beginning-of-line)
    (setq start (point))
    (forward-line count)
    (setq end (point))
    (delete-region start end)
    (move-to-column column)))

;;
;; Brief kill-line command
;;
;; Note that this command is not currently mapped to a key, as its
;; functionality is subsumed by b-kill-region.
;;
(defun b-kill-line (&optional arg)
  "Kill the current line from anywhere on the line.
With ARG, do it that many times."
  (interactive "*P")
  (let ((count (prefix-numeric-value arg))
	(column (current-column)))
    (beginning-of-line)
    (kill-line count)
    (b-set-line-kill (car kill-ring))
    (move-to-column column)))

;;
;; Brief line-mode commands
;;
(defvar b-line-mark-min nil
  "The minimum position of the mark in line marking mode.
The mark is positioned here if point is below this line.")
(defvar b-line-mark-max nil
  "The maximum position of the mark in line marking mode.
The mark is positioned here if point is above or on this line.")
(defvar b-line-mark-col nil
  "The original column where line marking was initiated.
This is restored after saving/killing the region.")

(defun b-start-line-marking ()
  "Start line-marking mode."
  (setq b-line-mark-col (current-column))
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'b-mark-line-hook nil t))

(defun b-stop-line-marking ()
  "Stops line-marking mode."
  (remove-hook 'post-command-hook  'b-mark-line-hook t)
  (move-to-column b-line-mark-col))

(defun b-line-marking-p ()
  "Return true if the buffer is in line marking mode."
  (memq 'b-mark-line-hook post-command-hook))

(defun b-mark-line-hook ()
  "Ensure that the point and mark are correctly positioned for
line-marking after cursor motion commands."
  (cond ((b-region-active-p)
	 ;; Marking - emulate Brief "line mode"
	 (let ((point (point))
	       (mark (mark)))
	   ;; Ensure we're at the beginning of the line
	   (unless (bolp)
	     (beginning-of-line)
	     (when (> point mark)
	       (forward-line)))

	   ;; Ensure mark and point are straddling the original line
	   (cond ((< point mark)
		  (when (/= mark b-line-mark-max)
		    (set-mark b-line-mark-max)))
		 ((> point mark)
		  (when (/= mark b-line-mark-min)
		    (set-mark b-line-mark-min)))
		 ;; point = mark
		 ((= point b-line-mark-max) ; point = mark-max
		  (forward-line 1)
		  (set-mark b-line-mark-min))
		 (t ; point = mark-min
		  (forward-line -1)
		  (set-mark b-line-mark-max)))

	   (b-activate-region)))
	(t ; Not marking
	 (b-stop-line-marking))))

(defun b-mark-line (&optional arg)
  "Mark the current line from anywhere on the line.
Emulates the Brief mark-line function.
With ARG, do it that many times."
  (interactive "P")
  (let ((lines (prefix-numeric-value arg)))
    (when (/= lines 0)
      (b-start-line-marking)
      (beginning-of-line)
      (setq b-line-mark-min (point))
      (forward-line)
      (setq b-line-mark-max (point))
      (cond ((bolp) ; Normal case
	     (forward-line (1- lines))
	     (push-mark b-line-mark-min nil t))
	    (t ; Case where last line is incomplete
	     (goto-char b-line-mark-min)
	     (push-mark b-line-mark-max nil t))))))

(defun b-mark-default ()
  "Mark the default unit in the buffer.
Normally this is the current line, but in lisp modes it is the containing sexp."
      (cond ((b-lisp-mode-p)
	     (condition-case nil
		 (progn
		   (unless (= (following-char) ?\()
		     (backward-up-list))
		   (mark-sexp))
	       (error (b-mark-line))))
	    (t ; Non-lisp mode
	     (b-mark-line))))

(defun b-lisp-mode-p ()
  "Return non-nil if the current major mode is a lisp mode.
This is determined heuristically by looking for `lisp' in the mode name."
  (string-match "lisp" mode-name))

(defun b-emphasise-region (beg end)
  "Emphasise the region like `kill-ring-save' does."
  ;; This code is based on code in `kill-ring-save' from simple.el in GNU Emacs
  (let ((other-end (if (= (point) beg) end beg))
	(opoint (point))
	(inhibit-quit t))		; Inhibit quitting
    (when (pos-visible-in-window-p other-end (selected-window))
      ;; Swap point and mark.
      (set-marker (mark-marker) (point) (current-buffer))
      (goto-char other-end)
      (sit-for 1)
      ;; Swap back.
      (set-marker (mark-marker) other-end (current-buffer))
      (goto-char opoint)))
  (b-deactivate-region t))

;;
;; Brief copy-region command
;;
(defun b-copy-region ()
  "Copy the current active region to the kill ring.
If there is no active region then the current line is copied.
Emulates the Brief copy function."
  (interactive)
  (unless (b-region-active-p)
    (b-mark-default))
  ;; Use call-interactively here so that the line is highlighted
  (call-interactively 'kill-ring-save)
  (when (b-line-marking-p)
    (b-set-line-kill (car kill-ring))
    (b-stop-line-marking)
    (when (> (point) (mark))
      (forward-line -1)
      (move-to-column b-line-mark-col)))
  (b-deactivate-region))

(defun b-copy-to-register (register)
  "Copy the current active region to a register.
If there is no active region then the current line is copied."
  (interactive "cCopy-to-register:")
  (unless (b-region-active-p)
    (b-mark-default))
  (let ((beg (region-beginning))
	(end (region-end)))
    (copy-to-register register beg end)
    (when (b-line-marking-p)
      (b-set-line-kill (get-register register))
      (b-stop-line-marking)
      (when (> (point) (mark))
	(forward-line -1)
	(move-to-column b-line-mark-col)))
    ;; Emphasise the region like `kill-ring-save' does
    (b-emphasise-region beg end)))

;;
;; Brief kill-region command
;;
(defun b-kill-region ()
  "Kill the current active region.
If there is no active region then the current line is killed.
Emulates the Brief cut function."
  (interactive "*")
  (unless (b-region-active-p)
    (b-mark-default))
  (kill-region (region-beginning) (region-end))
  (when (b-line-marking-p)
    (b-set-line-kill (car kill-ring))
    (b-stop-line-marking)))

(defun b-kill-to-register (register)
  "Kill the current active region to a register.
If there is no active region then the current line is killed."
  (interactive "*cCopy-to-register:")
  (unless (b-region-active-p)
    (b-mark-default))
  (copy-to-register register (region-beginning) (region-end) t)
  (when (b-line-marking-p)
    (b-set-line-kill (get-register register))
    (b-stop-line-marking)))

;;
;; Brief delete command
;;
(defun b-delete (&optional arg)
  "Delete the current active region.
If there is no active region then ARG characters following point are deleted.
Emulates the Brief delete function."
  (interactive "*P")
  (cond ((b-region-active-p)
	 (delete-region (region-beginning) (region-end))
	 (when (b-line-marking-p)
	   (b-stop-line-marking)))
	(t ; No active region
	 (delete-char (prefix-numeric-value arg)))))

;;
;; Line kill helper functions
;;
(defmacro b-set-line-kill (place)
  "Make the string at PLACE a line-mode kill.
This is done by adding a text property and ensuring that the (last)
line is terminated with a newline."
  ;; Ensure the line is terminated with a newline
  `(let ((line (b-terminate-line ,place)))
     ;; Indicate that this is a line-mode kill with a text property
     (put-text-property 0 1 'b-line-kill t line)
     (setf ,place line)))

(defun b-terminate-line (line)
  "Ensure LINE is terminated with a newline."
  (let ((len (length line)))
    (if (and (> len 0) (= (aref line (1- len)) ?\n))
	line
      (concat line "\n"))))

(defun b-clear-line-kill (pos)
  "Remove the line-mode kill property from text at position POS in the buffer."
  (remove-text-properties pos (1+ pos) '(b-line-kill t)))

(defun b-line-kill-p (string)
  "Test if STRING is a line-mode kill."
  (get-text-property 0 'b-line-kill string))

;;
;; Brief Yank & Yank-pop commands
;;
(defvar b-yank-col 0
  "The original column where `b-yank' was initiated.
This is restored after the yank.")

(defvar b-last-yank-was-line nil
  "True if the last yank was from a line-mode kill.")

(defun b-yank (&optional arg)
  "Identical to the normal `yank' command, but correctly insert text that
was killed in line-mode and also indent it."
  (interactive "*P")
  (setq this-command 'yank)
  (setq b-yank-col (current-column))
  (cond ((b-line-kill-p (current-kill (cond ((listp arg) 0)
					    ((eq arg '-) -1)
					    (t (1- arg))) t))
	 (beginning-of-line)
	 (yank arg)
	 (let ((point (point))
	       (mark (mark t)))
	   (b-clear-line-kill (min mark point))
	   (indent-region (min mark point) (max mark point) nil))
	 (setq b-last-yank-was-line t)
	 (move-to-column b-yank-col))

	(t ; Not line kill
	 (yank arg)
	 (setq b-last-yank-was-line nil))))

(defun b-yank-pop (arg)
  "Identical to the normal `yank-pop' command, but correctly insert text that
was killed in line-mode and also indent it."
  (interactive "*p")
  (unless (eq last-command 'yank)
    (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (cond ((b-line-kill-p (current-kill arg t))
	 (cond (b-last-yank-was-line
		(beginning-of-line))
	       (t
		(delete-region (point) (mark t))
		(beginning-of-line)
		(set-mark (point))
		(setq b-last-yank-was-line t)))
	 (yank-pop arg)
	 (let ((point (point))
	       (mark (mark t)))
	   (b-clear-line-kill (min mark point))
	   (indent-region (min mark point) (max mark point) nil))
	 (move-to-column b-yank-col))

	(t ; Not line kill
	 (when b-last-yank-was-line
	   (beginning-of-line)
	   (delete-region (point) (mark t))
	   (move-to-column b-yank-col)
	   (set-mark (point))
	   (setq b-last-yank-was-line nil))
	 (yank-pop arg))))

(defun b-insert-register (register)
  (interactive "*cInsert Register:")
  )

;;
;; Brief paging and scrolling commands
;; These commands aim to provide fully reversible paging, so that point
;; returns to the same position after paging up and down.
;;
(defvar b-temporary-goal-column 0
  "Original column of the start of a sequence B scrolling commands.")

(defun b-page-down (&optional arg)
  "Scroll the current window up by one page, respecting `next-screen-context-lines'.
Paging up afterwards should return point to the same position.
The optional argument specifies the number of pages to scroll."
  (interactive "P")
  (b-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(b-page-up (- pages))
      (while (and (> pages 0) (not (pos-visible-in-window-p (point-max))))
	(b-scroll-screen (- (1- (window-height))
				next-screen-context-lines))
	(decf pages)))))
(put 'b-page-down 'b-scroll-command t)

(defun b-page-up (&optional arg)
  "Scroll the current window down by one page, respecting `next-screen-context-lines'.
Paging down afterwards should return point to the same position.
The optional argument specifies the number of pages to scroll."
  (interactive "P")
  (b-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(b-page-down (- pages))
      (while (and (> pages 0) (not (pos-visible-in-window-p (point-min))))
	(b-scroll-screen (- next-screen-context-lines
				(1- (window-height))))
	(decf pages)))))
(put 'b-page-up 'b-scroll-command t)

(defun b-scroll-screen (lines)
  "Scroll current window by LINES, but keep the cursor's relative
position in the window.
This is a helper function used by `b-page-up' and `b-page-down'.
It should still work in the presence of hidden lines."
  (unless (b-scroll-command-p last-command)
    (setq b-temporary-goal-column (current-column)))
  (let ((point (point)))
    (goto-char (window-start))
    (next-line lines)
    (set-window-start (selected-window) (point))
    (goto-char point))
  (next-line lines)
  (move-to-column b-temporary-goal-column))

(defun b-row-up (&optional arg)
  "Scroll the current window down by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (b-keep-region)
  (b-scroll-line (prefix-numeric-value arg)))
(put 'b-row-up 'b-scroll-command t)

(defun b-row-down (&optional arg)
  "Scroll the current window up by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (b-keep-region)
  (b-scroll-line (- (prefix-numeric-value arg))))
(put 'b-row-down 'b-scroll-command t)

(defun b-scroll-line (lines)
  "Scroll the current window down by LINES.
This is a helper function used by b-row-up/down."
  (unless (b-scroll-command-p last-command)
    (setq b-temporary-goal-column (current-column)))
  (scroll-down lines)
  (move-to-column b-temporary-goal-column))

(defun b-scroll-command-p (cmd)
  "Non-nil if CMD is a B scrolling command."
  (and (symbolp cmd) (get cmd 'b-scroll-command)))

;;
;; Brief Home/End key functions
;;
(defvar b-last-last-command nil
  "The previous value of `last-command'.
Used in the implementation of b-home/end.")

(defun b-home ()
  "\"Home\" the point, the way Brief does it.
The first use moves point to beginning of the line.  Second
consecutive use moves point to beginning of the screen.  Third
consecutive use moves point to the beginning of the buffer."
  (interactive)
  (if (eq last-command 'b-home)
      (if (eq b-last-last-command 'b-home)
	  (goto-char (point-min))
	(move-to-window-line 0))
    (beginning-of-line))
  (setq b-last-last-command last-command))

(defun b-end ()
  "\"End\" the point, the way Brief does it.
The first use moves point to end of the line.  Second
consecutive use moves point to the end of the screen.  Third
consecutive use moves point to the end of the buffer."
  (interactive)
  (if (eq last-command 'b-end)
      (if (eq b-last-last-command 'b-end)
	  (goto-char (point-max))
	(move-to-window-line -1))
    (end-of-line))
  (setq b-last-last-command last-command))

;;
;; Tab key handling
;;
(defun b-tab (&optional arg)
  "Indent the region if the region is active, otherwise indent the
current line using the function which would ordinarily be bound to the tab key."
  (interactive "P")
  (let ((tab-fn (or (local-key-binding "\t")
		    (global-key-binding "\t"))))
    (cond ((or (null tab-fn) (b-indent-cmd-p tab-fn))
	   ;; If the region is active, indent it
	   (if (b-region-active-p)
	       (indent-region (region-beginning) (region-end) arg)
	     ;; Otherwise, call the usual binding for the tab key
	     (if tab-fn
		 (if arg
		     (funcall tab-fn arg)
		   (funcall tab-fn))
	       ;; No binding found, so call sensible default
	       (indent-for-tab-command arg))))
	  (t
	   ;; If the normal binding is not an indent command, just invoke it
	   (if arg
	       (funcall tab-fn arg)
	     (funcall tab-fn))))))

(defun b-indent-cmd-p (cmd)
  "Non-nil if CMD is an indent command, nil otherwise.
This is determined heuristically by seeing if the command name contains
the word \"indent\"."
  (and (symbolp cmd) (string-match "indent" (symbol-name cmd))))

;;
;; Brief Cursor Motion Undo
;;
(defvar b-undo-point 0
  "The location of point after a command is executed.")
(make-variable-buffer-local 'b-undo-point)
(defvar b-undo-list-head nil
  "The head of the undo list after a command is executed.")
(make-variable-buffer-local 'b-undo-list-head)
;; (defvar b-undo-list-second nil
;;   "The second element of the undo list after a command is executed.")
;; (make-variable-buffer-local 'b-undo-list-second)

(defvar b-undo-debug-enabled nil)

(defun b-undo-post-command-hook ()
  (when (listp buffer-undo-list)
    (let ((point (point))
	  (head (car buffer-undo-list)))
      ;; Put point on the undo list if necessary
      (unless (or (eq this-command 'undo) (eq this-command 'redo))
	(when (and (/= b-undo-point 0)
		   (/= point b-undo-point)
		   (equal head b-undo-list-head))
	  (when head
	    (undo-boundary))
	  (setq buffer-undo-list (cons b-undo-point buffer-undo-list))))

      ;; Save point and the undo-list head for next time
      (setq b-undo-point point)
      (setq head (car buffer-undo-list))
      (setq b-undo-list-head (if (and (consp head) (integerp (car head)))
				 (cons (car head) (cdr head))
				   head)))
    ;; Debug output
    (when b-undo-debug-enabled
      (b-undo-debug))))

(defun b-undo-debug ()
  (unless (active-minibuffer-window)
    (let ((undo-list buffer-undo-list)
	  (pending pending-undo-list)
	  (point (point)))
      (save-current-buffer
	(set-buffer (get-buffer-create "*B Debug*"))
	(insert "(" (number-to-string point) ") List: " (prin1-to-string undo-list) ?\n)
	(when (or (eq this-command 'undo) (eq this-command 'redo))
	  (insert "Pending: " (prin1-to-string (car pending)) ?\n))
	(goto-char 1)))))

(defun b-undo-toggle-debug (&optional arg)
  (interactive "P")
  (setq b-undo-debug-enabled
	(if (null arg)
	    (not b-undo-debug-enabled)
	  (> (prefix-numeric-value arg) 0)))

  (when b-undo-debug-enabled
    (save-current-buffer
      (let ((buffer (get-buffer-create "*B Debug*")))
	(set-buffer buffer)
	(buffer-disable-undo buffer)
	(erase-buffer)
	(display-buffer buffer t)))))

;;
;; B minor mode
;;
(defvar b-prev-mark-mode nil
  "Previous value of transient mark mode.")
(defvar b-prev-c-m nil
  "Previous global binding of C-m.")
(defvar b-prev-c-j nil
  "Previous global binding of C-j.")

;;;###autoload
(defun b-mode (&optional arg)
  "Toggle B minor mode.
With ARG, turn B mode on if ARG is positive, off otherwise.

Key bindings:
\\{b-mode-map}"
  (interactive "P")

  ;; Turn the mode on or off
  (setq b-mode
	(if (null arg)
	    (not b-mode)
	  (> (prefix-numeric-value arg) 0)))

  ;; Processing that needs to be done when the mode is started or stopped
  (cond (b-mode
	 ;; Force transient-mark-mode, remember old setting
	 (setq b-prev-mark-mode (b-transient-mark-mode t))
	 ;; Setup return to always indent
	 (setq b-prev-c-m (global-key-binding "\C-m"))
	 (setq b-prev-c-j (global-key-binding "\C-j"))
	 (global-set-key "\C-m" 'newline-and-indent)
	 (global-set-key "\C-j" 'newline)
	 ;; Run mode hook
	 (run-hooks 'b-mode-hook))
	(t
	 ;; Restore old settings
	 (global-set-key "\C-m" b-prev-c-m)
	 (global-set-key "\C-j" b-prev-c-j)
	 (b-transient-mark-mode b-prev-mark-mode))))

;; Add B mode as a minor mode
(add-to-list 'minor-mode-alist '(b-mode b-mode-modeline-string))
(add-to-list 'minor-mode-map-alist (cons 'b-mode b-mode-map))

;; Load successful
(provide 'b)

;;; b.el ends here
