;;; brief.el --- Brief editor emulator

;; Copyright (C) 2000 Mike Woolley mike@bulsara.com
;; Author: Mike Woolley <mike@bulsara.com>
;; Version: 1.04
;; Keywords: brief emulator

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
;;  This package provides an emulation of the Brief editor under Emacs.
;;  However, only those functions which don't exist in Emacs are emulated
;;  and functions common to both retain their Emacs keybindings.

;;; Change Log:
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

(defgroup brief nil
  "Emulator for Brief."
  :prefix "brief-"
  :group 'emulations)

;;;###autoload
(defcustom brief-mode nil
  "Track status of Brief mode.
A value of nil means Brief mode is not enabled.  A value of t
indicates Brief mode is enabled.

Setting this variable directly does not take effect;
use either \\[execute-extended-command] customize or the function `brief-mode'."
  :type 'boolean
  :set (lambda (symbol value) (brief-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'brief
  :version "20.4"
  :group 'brief)

(defcustom brief-undo-enable nil
  "Enable cursor motion undo."
  :type 'boolean
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'post-command-hook 'brief-undo-post-command-hook)
	   (remove-hook 'post-command-hook 'brief-undo-post-command-hook))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :group 'brief)

(defcustom brief-mode-modeline-string " Brief"
  "String to display in the modeline when Brief mode is enabled.
Set this to nil to conserve valuable mode line space."
  :type 'string
  :group 'brief)

(defface brief-bookmark-face '((t (:background "khaki")))
  "Face used to show bookmark."
  :group 'brief)

(defcustom brief-load-hook nil
  "Hooks to run after loading the Brief emulator package."
  :type 'hook
  :group 'brief)

(defcustom brief-mode-hook nil
  "Hook run by the function `brief-mode'."
  :type 'hook
  :group 'brief)

;;
;; Version
;;
(defconst brief-version "1.04"
  "Version number of Brief mode.")

(defun brief-version ()
  "Version number of Brief mode."
  (interactive)
  (message (concat "Brief version " brief-version)))

;;
;; XEmacs compatibility
;;
(eval-and-compile
  (defconst brief-xemacs-flag (featurep 'xemacs)
    "Non-nil means this version of Emacs is XEmacs."))

;; Silence the byte compiler
(eval-when-compile
  (cond (brief-xemacs-flag
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
(when brief-xemacs-flag
  (require 'overlay))

;; Function to get the position of the beginning of the line
(cond ((fboundp 'line-beginning-position)
       (defalias 'brief-bol-position 'line-beginning-position))
      ((fboundp 'point-at-bol)
       (defalias 'brief-bol-position 'point-at-bol))
      (t
       (defun brief-bol-position (&optional n)
	 "Return the index of the character at the start of the line."
	 (save-excursion
	   (beginning-of-line n)
	   (point)))))

(defun brief-region-active-p ()
  "Emacs/XEmacs compatibility function to test for an active region."
  (if brief-xemacs-flag
      zmacs-region-active-p
    mark-active))

(defun brief-transient-mark-mode (arg)
  "Emacs/XEmacs compatibility function to set transient mark mode.
Returns the previous setting."
  (if brief-xemacs-flag
      (prog1 zmacs-regions
	(setq zmacs-regions arg))
    (prog1 transient-mark-mode
      (setq transient-mark-mode arg))))

(defun brief-activate-region ()
  "Ensure region is highlited correctly in XEmacs.
This function does nothing in GNU Emacs."
  (when brief-xemacs-flag
    (if zmacs-region-active-p
	(zmacs-update-region)
      (zmacs-activate-region))))

(defun brief-deactivate-region ()
  "Ensure region is deactivated in XEmacs.
This function does nothing in GNU Emacs."
  (when (and brief-xemacs-flag
	     zmacs-region-active-p)
    (zmacs-deactivate-region)))

(defun brief-keep-region ()
  "Ensure that the current command keeps the region in XEmacs.
This function does nothing in GNU Emacs."
  (when brief-xemacs-flag
    (setq zmacs-region-stays t)))

;;
;; Bookmarks
;;
(defvar brief-bookmarks nil
  "List of Brief bookmarks.")

(defun brief-make-set-bookmark (number)
  "Generate a command which sets bookmark NUMBER at point.
If the command is given a prefix argument, then the bookmark is removed."
  `(lambda (&optional arg)
     ,(format "Set Brief bookmark %d at point.
With ARG, remove the bookmark instead." number)
     (interactive "P")
     (if arg
	 (brief-kill-bookmark ,number)
       (brief-set-bookmark ,number))))

(defun brief-set-bookmark (number)
  "Set bookmark NUMBER at point."
  (interactive "NSet bookmark: ")
  ;; Lookup the bookmark
  (let ((bookmark (assq number brief-bookmarks))
	(start (brief-bol-position 1))
 	(end (brief-bol-position 2)))
  (cond ((null bookmark)
	   ;; Create a new bookmark
	   (let ((marker (point-marker))
		 (overlay (make-overlay start end)))
	     (overlay-put overlay 'face 'brief-bookmark-face)
 	     (overlay-put overlay 'before-string (format "%d>" number))
	     (overlay-put overlay 'help-echo (format "Bookmark %d" number))
	     (unless brief-xemacs-flag
	       ;; XEmacs overlay compatibility doesn't support modification hook
	       ;; Need another way to code this
	       (overlay-put overlay 'modification-hooks '(brief-protect-overlay))
	       (overlay-put overlay 'brief-bookmark-marker marker))
	     ;; Add the new bookmark to the list
	     (push (list number marker overlay) brief-bookmarks)))
	  (t ;; Move bookmark to new location
	   (let ((marker (second bookmark))
		 (overlay (third bookmark))
		 (buffer (current-buffer)))
	     (move-marker marker (point) buffer)
	     (move-overlay overlay start end buffer)))))
  (message "Bookmark %d dropped" number))

(defun brief-protect-overlay (overlay after begin end &optional len)
  "Prevent bookmark overlay from being split over multiple lines."
  (when after
      (save-excursion
	(goto-char (overlay-get overlay 'brief-bookmark-marker))
	(move-overlay overlay (brief-bol-position 1) (brief-bol-position 2)))))

(defun brief-kill-bookmark (number)
  "Kill bookmark NUMBER."
  (interactive "NKill Bookmark: ")
  (let ((bookmark (assq number brief-bookmarks)))
    (unless bookmark
      (error "That bookmark has not been set"))
    (let ((marker (second bookmark))
	  (overlay (third bookmark)))
      (move-marker marker nil)
      (delete-overlay overlay))))

(defun brief-jump-to-bookmark (number)
  "Jump to bookmark NUMBER."
  (interactive "NJump to Bookmark: ")
  ;; Lookup the bookmark
  (let ((bookmark (assq number brief-bookmarks)))
    (unless bookmark
      (error "That bookmark has not been set"))
    (let* ((marker (second bookmark))
	   (buffer (marker-buffer marker)))
      (unless buffer
	(error "That bookmark's buffer no longer exists"))
      (switch-to-buffer buffer)
      (goto-char marker))))

;;
;; Keymap
;;
(defvar brief-mode-map nil
  "Local keymap for Brief mode.")
(unless brief-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Put our definitions on the same keys as Brief
    (define-key map [(control return)] 'brief-insert-line)
    (define-key map "\M-d" 'brief-delete-line)
    (define-key map "\M-l" 'brief-mark-line)
    (define-key map [(kp-add)] 'brief-copy-region)
    (define-key map [(kp-subtract)] 'brief-kill-region)
    (define-key map [(kp-multiply)] 'undo)
    (define-key map [(delete)] 'brief-delete)
    (define-key map [(kp-delete)] 'brief-delete)
    (define-key map [(meta up)] 'brief-row-up)
    (define-key map [(meta down)] 'brief-row-down)
    (define-key map [(home)] 'brief-home)
    (define-key map [(end)] 'brief-end)
    (define-key map "\t" 'brief-tab)

    ;; Also put them on the Emacs keys
    (substitute-key-definition 'kill-ring-save 'brief-copy-region map (current-global-map))
    (substitute-key-definition 'kill-region 'brief-kill-region map (current-global-map))
    (substitute-key-definition 'yank 'brief-yank map (current-global-map))
    (substitute-key-definition 'yank-pop 'brief-yank-pop map (current-global-map))
    (substitute-key-definition 'beginning-of-line 'brief-home map (current-global-map))
    (substitute-key-definition 'end-of-line 'brief-end map (current-global-map))

    ;; Try and find the existing commands for scrolling up/down,
    ;; as these are different in Emacs & XEmacs
    (let ((scroll-up-cmd (global-key-binding [(next)]))
	  (scroll-down-cmd (global-key-binding [(prior)])))
      (if scroll-up-cmd
	  (substitute-key-definition scroll-up-cmd 'brief-page-down map (current-global-map)))
      (if scroll-down-cmd
	  (substitute-key-definition scroll-down-cmd 'brief-page-up map (current-global-map))))

    ;; Setup the bookmarks on the M-digit keys, like in Brief
    ;; Prefix args will have to be entered with the C-digit or C-U number methods...
    (dotimes (digit 10)
      (define-key map (vector (list 'meta (string-to-char (number-to-string digit))))
	(brief-make-set-bookmark digit)))
    (define-key map "\M-j" 'brief-jump-to-bookmark)

    (setq brief-mode-map map)))

;;
;; Brief insert-line command
;;
(defun brief-insert-line (&optional arg)
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
(defun brief-delete-line (&optional arg)
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
;; functionality is subsumed by brief-kill-region.
;;
(defun brief-kill-line (&optional arg)
  "Kill the current line from anywhere on the line.
With ARG, do it that many times."
  (interactive "*P")
  (let ((count (prefix-numeric-value arg))
	(column (current-column)))
    (beginning-of-line)
    (kill-line count)
    (brief-set-line-kill)
    (move-to-column column)))

;;
;; Brief mark-line command
;;
(defvar brief-line-mark-min nil
  "The minimum position of the mark in line marking mode.
The mark is positioned here if point is below this line.")
(defvar brief-line-mark-max nil
  "The maximum position of the mark in line marking mode.
The mark is positioned here if point is above or on this line.")
(defvar brief-line-mark-col nil
  "The original column where line marking was initiated.
This is restored after saving/killing the region.")

(defun brief-start-line-marking ()
  "Start line-marking mode."
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'brief-mark-line-hook nil t))

(defun brief-stop-line-marking ()
  "Stops line-marking mode."
  (remove-hook 'post-command-hook  'brief-mark-line-hook t))

(defun brief-line-marking-p ()
  "Return true if the buffer is in line marking mode."
  (memq 'brief-mark-line-hook post-command-hook))

(defun brief-mark-line-hook ()
  "Ensure that the point and mark are correctly positioned for
line-marking after cursor motion commands."
  (if (not (brief-region-active-p))
      (brief-stop-line-marking)

    ;; Marking - emulate Brief "line mode"
    (let ((point (point))
	  (mark (mark)))
      ;; Ensure we're at the beginning of the line
      (unless (bolp)
	(beginning-of-line)
	(if (> point mark)
	    (forward-line)))

      ;; Ensure mark and point are straddling the original line
      (cond ((< point mark)
	     (if (/= mark brief-line-mark-max)
		 (set-mark brief-line-mark-max)))
	    ((> point mark)
	     (if (/= mark brief-line-mark-min)
		 (set-mark brief-line-mark-min)))
	    (t				; point = mark
	     (cond ((= mark brief-line-mark-max)
		    (forward-line 1)
		    (set-mark brief-line-mark-min))
		   (t			; point = mark-min
		    (forward-line -1)
		    (set-mark brief-line-mark-max)))))

      (brief-activate-region))))

(defun brief-mark-line (&optional arg)
  "Mark the current line from anywhere on the line.
Emulates the Brief mark-line function.
With ARG, do it that many times."
  (interactive "P")
  (let ((lines (prefix-numeric-value arg)))
    (when (/= lines 0)
      (setq brief-line-mark-col (current-column))
      (brief-start-line-marking)
      (beginning-of-line)
      (setq brief-line-mark-min (point))
      (forward-line)
      (setq brief-line-mark-max (point))
      (cond ((bolp)			; Normal case
	     (forward-line (1- lines))
	     (push-mark brief-line-mark-min nil t))
	    (t				; Case where last line is incomplete
	     (goto-char brief-line-mark-min)
	     (push-mark brief-line-mark-max nil t))))))

;;
;; Brief copy-region command
;;
(defun brief-copy-region ()
  "Copy the current active region to the kill ring.
If there is no active region then the current line is copied.
Emulates the Brief copy function."
  (interactive)
  (unless (brief-region-active-p)
    (brief-mark-line))
  (call-interactively 'kill-ring-save)
  (when (brief-line-marking-p)
    (brief-set-line-kill)
    (brief-stop-line-marking)
    (when (> (point) (mark))
      (forward-line -1))
    (move-to-column brief-line-mark-col))
  (brief-deactivate-region))

;;
;; Brief kill-region command
;;
(defun brief-kill-region ()
  "Kill the current active region.
If there is no active region then the current line is killed.
Emulates the Brief cut function."
  (interactive "*")
  (unless (brief-region-active-p)
    (brief-mark-line))
  (call-interactively 'kill-region)
  (when (brief-line-marking-p)
    (brief-set-line-kill)
    (brief-stop-line-marking)
    (move-to-column brief-line-mark-col)))

;;
;; Brief delete command
;;
(defun brief-delete (&optional arg)
  "Delete the current active region.
If there is no active region then ARG characters following point are deleted.
Emulates the Brief delete function."
  (interactive "*P")
  (if (not (brief-region-active-p))
      (delete-char (prefix-numeric-value arg))
    (call-interactively 'delete-region)
    (when (brief-line-marking-p)
      (brief-stop-line-marking)
      (move-to-column brief-line-mark-col))))

;;
;; Brief Yank & Yank-pop commands
;;
(defun brief-set-line-kill ()
  "Make the front of the `kill-ring' a line-mode kill.
This is done by adding a text property."
  ;; Ensure the line is terminated with a newline
  (let* ((kill (car kill-ring))
	 (len (length kill)))
    (unless (and (> len 0) (= (aref kill (1- len)) ?\n))
      (setcar kill-ring (concat kill (string ?\n)))))
  ;; Indicate that this is a line-mode kill with a text property
  (put-text-property 0 1 'brief-line-kill t (car kill-ring)))

(defun brief-clear-line-kill (pos)
  "Remove the line-mode kill property from text at position POS in the buffer."
  (remove-text-properties pos (1+ pos) '(brief-line-kill t)))

(defun brief-line-kill-p (string)
  "Test if STRING is a line-mode kill."
  (get-text-property 0 'brief-line-kill string))

(defvar brief-yank-col 0
  "The original column where `brief-yank' was initiated.
This is restored after the yank.")

(defvar brief-last-yank-was-line nil
  "True if the last yank was from a line-mode kill.")

(defun brief-yank (&optional arg)
  "Identical to the normal `yank' command, but correctly insert text that
was killed in line-mode and also indent it."
  (interactive "*P")
  (setq this-command 'yank)
  (setq brief-yank-col (current-column))
  (cond ((brief-line-kill-p (current-kill (cond ((listp arg) 0)
						((eq arg '-) -1)
						(t (1- arg))) t))
	 (beginning-of-line)
	 (yank arg)
	 (let ((point (point))
	       (mark (mark t)))
	   (brief-clear-line-kill (min mark point))
	   (indent-region (min mark point) (max mark point) nil))
	 (setq brief-last-yank-was-line t)
	 (move-to-column brief-yank-col))

	(t				; Not line kill
	 (yank arg)
	 (setq brief-last-yank-was-line nil))))

(defun brief-yank-pop (arg)
  "Identical to the normal `yank-pop' command, but correctly insert text that
was killed in line-mode and also indent it."
  (interactive "*p")
  (unless (eq last-command 'yank)
    (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (cond ((brief-line-kill-p (current-kill arg t))
	 (if brief-last-yank-was-line
	     (beginning-of-line)
	   (delete-region (point) (mark t))
	   (beginning-of-line)
	   (set-mark (point))
	   (setq brief-last-yank-was-line t))
	 (yank-pop arg)
	 (let ((point (point))
	       (mark (mark t)))
	   (brief-clear-line-kill (min mark point))
	   (indent-region (min mark point) (max mark point) nil))
	 (move-to-column brief-yank-col))

	(t				; Not line kill
	 (when brief-last-yank-was-line
	   (beginning-of-line)
	   (delete-region (point) (mark t))
	   (move-to-column brief-yank-col)
	   (set-mark (point))
	   (setq brief-last-yank-was-line nil))
	 (yank-pop arg))))

;;
;; Brief paging and scrolling commands
;; These commands aim to provide fully reversible paging, so that point
;; returns to the same position after paging up and down.
;;
(defvar brief-temporary-goal-column 0
  "Original column of the start of a sequence Brief scrolling commands.")

(defun brief-page-down (&optional arg)
  "Scroll the current window up by one page, respecting `next-screen-context-lines'.
Paging up afterwards should return point to the same position.
The optional argument specifies the number of pages to scroll."
  (interactive "P")
  (brief-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(brief-page-up (- pages))
      (while (and (> pages 0) (not (pos-visible-in-window-p (point-max))))
	(brief-scroll-screen (- (1- (window-height))
				next-screen-context-lines))
	(decf pages)))))
(put 'brief-page-down 'brief-scroll-command t)

(defun brief-page-up (&optional arg)
  "Scroll the current window down by one page, respecting `next-screen-context-lines'.
Paging down afterwards should return point to the same position.
The optional argument specifies the number of pages to scroll."
  (interactive "P")
  (brief-keep-region)
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(brief-page-down (- pages))
      (while (and (> pages 0) (not (pos-visible-in-window-p (point-min))))
	(brief-scroll-screen (- next-screen-context-lines
				(1- (window-height))))
	(decf pages)))))
(put 'brief-page-up 'brief-scroll-command t)

(defun brief-scroll-screen (lines)
  "Scroll current window by LINES, but keep the cursor's relative
position in the window.
This is a helper function used by `brief-page-up' and `brief-page-down'.
It should still work in the presence of hidden lines."
  (unless (brief-scroll-command-p last-command)
    (setq brief-temporary-goal-column (current-column)))
  (let ((point (point)))
    (goto-char (window-start))
    (next-line lines)
    (set-window-start (selected-window) (point))
    (goto-char point))
  (next-line lines)
  (move-to-column brief-temporary-goal-column))

(defun brief-row-up (&optional arg)
  "Scroll the current window down by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (brief-keep-region)
  (brief-scroll-line (prefix-numeric-value arg)))
(put 'brief-row-up 'brief-scroll-command t)

(defun brief-row-down (&optional arg)
  "Scroll the current window up by one line.
ARG specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (brief-keep-region)
  (brief-scroll-line (- (prefix-numeric-value arg))))
(put 'brief-row-down 'brief-scroll-command t)

(defun brief-scroll-line (lines)
  "Scroll the current window down by LINES.
This is a helper function used by brief-row-up/down."
  (unless (brief-scroll-command-p last-command)
    (setq brief-temporary-goal-column (current-column)))
  (scroll-down lines)
  (move-to-column brief-temporary-goal-column))

(defun brief-scroll-command-p (cmd)
  "Non-nil if CMD is a Brief scrolling command."
  (and (symbolp cmd) (get cmd 'brief-scroll-command)))

;;
;; Brief Home/End key functions
;;
(defvar brief-last-last-command nil
  "The previous value of `last-command'.
Used in the implementation of brief-home/end.")

(defun brief-home ()
  "\"Home\" the point, the way Brief does it.
The first use moves point to beginning of the line.  Second
consecutive use moves point to beginning of the screen.  Third
consecutive use moves point to the beginning of the buffer."
  (interactive)
  (if (eq last-command 'brief-home)
      (if (eq brief-last-last-command 'brief-home)
	  (goto-char (point-min))
	(move-to-window-line 0))
    (beginning-of-line))
  (setq brief-last-last-command last-command))

(defun brief-end ()
  "\"End\" the point, the way Brief does it.
The first use moves point to end of the line.  Second
consecutive use moves point to the end of the screen.  Third
consecutive use moves point to the end of the buffer."
  (interactive)
  (if (eq last-command 'brief-end)
      (if (eq brief-last-last-command 'brief-end)
	  (goto-char (point-max))
	(move-to-window-line -1))
    (end-of-line))
  (setq brief-last-last-command last-command))

;;
;; Tab key handling
;;
(defun brief-tab (&optional arg)
  "Indent the region if the region is active, otherwise indent the
current line using the function which would ordinarily be bound to the tab key."
  (interactive "P")
  (let ((tab-fn (or (local-key-binding "\t")
		    (global-key-binding "\t"))))
    (cond ((or (null tab-fn) (brief-indent-cmd-p tab-fn))
	   ;; If the region is active, indent it
	   (if (brief-region-active-p)
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

(defun brief-indent-cmd-p (cmd)
  "Non-nil if CMD is an indent command, nil otherwise.
This is determined heuristically by seeing if the command name contains
the word \"indent\"."
  (and (symbolp cmd) (string-match "indent" (symbol-name cmd))))

;;
;; Brief Cursor Motion Undo
;;
(defvar brief-undo-point 0
  "The location of point after a command is executed.")
(make-variable-buffer-local 'brief-undo-point)
(defvar brief-undo-list-head nil
  "The head of the undo list after a command is executed.")
(make-variable-buffer-local 'brief-undo-list-head)
;; (defvar brief-undo-list-second nil
;;   "The second element of the undo list after a command is executed.")
;; (make-variable-buffer-local 'brief-undo-list-second)

(defun brief-undo-post-command-hook ()
  (when (listp buffer-undo-list)
    (let ((point (point))
	  (head (car buffer-undo-list)))
      ;; Put point on the undo list if necessary
      (unless (or (eq this-command 'undo) (eq this-command 'redo))
	(when (and (/= brief-undo-point 0)
		   (/= point brief-undo-point)
		   (equal head brief-undo-list-head))
	  (if head
	      (undo-boundary))
	  (setq buffer-undo-list (cons brief-undo-point buffer-undo-list))))

      ;; Save point and the undo-list head for next time
      (setq brief-undo-point point)
      (setq head (car buffer-undo-list))
      (setq brief-undo-list-head (if (and (consp head) (integerp (car head)))
				     (cons (car head) (cdr head))
				   head)))
    ;; Debug output
    (if brief-undo-debug-enabled
	(brief-undo-debug))))

(defun brief-undo-debug ()
  (unless (active-minibuffer-window)
    (let ((undo-list buffer-undo-list)
	  (pending pending-undo-list)
	  (point (point)))
      (save-current-buffer
	(set-buffer (get-buffer-create "*Brief Debug*"))
	(insert "(" (number-to-string point) ") List: " (prin1-to-string undo-list) ?\n)
	(if (or (eq this-command 'undo) (eq this-command 'redo))
	    (insert "Pending: " (prin1-to-string (car pending)) ?\n))
	(goto-char 1)))))

(defvar brief-undo-debug-enabled nil)

(defun brief-undo-toggle-debug (&optional arg)
  (interactive "P")
  (setq brief-undo-debug-enabled
	(if (null arg)
	    (not brief-undo-debug-enabled)
	  (> (prefix-numeric-value arg) 0)))

  (when brief-undo-debug-enabled
    (save-current-buffer
      (let ((buffer (get-buffer-create "*Brief Debug*")))
	(set-buffer buffer)
	(buffer-disable-undo buffer)
	(erase-buffer)
	(display-buffer buffer t)))))

;;
;; Brief minor mode
;;
(defvar brief-prev-mark-mode nil
  "Previous value of transient mark mode.")
(defvar brief-prev-c-m nil
  "Previous global binding of C-m.")
(defvar brief-prev-c-j nil
  "Previous global binding of C-j.")

;;;###autoload
(defun brief-mode (&optional arg)
  "Toggle Brief minor mode.
With ARG, turn Brief mode on if ARG is positive, off otherwise.

Key bindings:
\\{brief-mode-map}"
  (interactive "P")
  ;; Turn the mode on or off
  (setq brief-mode
	(if (null arg)
	    (not brief-mode)
	  (> (prefix-numeric-value arg) 0)))

  (cond (brief-mode
	 ;; Force transient-mark-mode, remember old setting
	 (setq brief-prev-mark-mode (brief-transient-mark-mode t))
	 ;; Setup return to always indent
	 (setq brief-prev-c-m (global-key-binding "\C-m"))
	 (setq brief-prev-c-j (global-key-binding "\C-j"))
	 (global-set-key "\C-m" 'newline-and-indent)
	 (global-set-key "\C-j" 'newline)
	 ;; Run mode hook
	 (run-hooks 'brief-mode-hook))
	(t
	 ;; Restore old settings
	 (global-set-key "\C-m" brief-prev-c-m)
	 (global-set-key "\C-j" brief-prev-c-j)
	 (brief-transient-mark-mode brief-prev-mark-mode))))

;; Add Brief as a minor mode
(add-to-list 'minor-mode-alist '(brief-mode brief-mode-modeline-string))
(add-to-list 'minor-mode-map-alist (cons 'brief-mode brief-mode-map))

;; Load finished
(run-hooks 'brief-load-hook)
(provide 'brief)

;;; brief.el ends here
