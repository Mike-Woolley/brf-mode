;;; brief.el - Brief editor emulator

;; Copyright (C) 2000 Mike Woolley mike@bulsara.com

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

;;; Description:
;;
;;  This package provides an emulation of the Brief editor under Emacs.
;;  However, only those functions which don't exist in Emacs are emulated
;;  and functions common to both retain their Emacs keybindings.

(defgroup brief nil
  "Emulator for Brief."
  :prefix "brief-"
  :group 'emulations)

(defvar brief-mode-map nil
  "Local keymap for Brief emulation mode.")
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

    (setq brief-mode-map map)))

;;;###autoload
(defcustom brief-mode nil
  "Track status of Brief emulation mode.
A value of nil means Brief mode is not enabled.  A value of t
indicates Brief mode is enabled.

Setting this variable directly does not take effect;
use either M-x customize or the function `brief-mode'."
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
  "String to display in the modeline when Brief emulation mode is enabled.
Set this to nil to conserve valuable mode line space."
  :type 'string
  :group 'brief)

(defcustom brief-load-hook nil
  "Hooks to run after loading the Brief emulator package."
  :type 'hook
  :group 'brief)

(defcustom brief-mode-hook nil
  "Hook run by the function `brief-mode'."
  :type 'hook
  :group 'brief)

(defconst brief-version "1.02"
  "The version of the Brief emulator.")

(defun brief-version ()
  "Version number of the Brief emulator package."
  (interactive)
  (message (concat "Brief version " brief-version)))

(defconst brief-xemacs-flag (featurep 'xemacs)
  "Indicate if this version of Emacs is XEmacs.")

(defun brief-region-active-p()
  "Emacs/XEmacs compatibility function to test for an active region."
  (if brief-xemacs-flag
      zmacs-region-active-p
    mark-active))

;;
;; Brief insert-line command
;;
(defun brief-insert-line (&optional arg)
  "Create a new line underneath the current one and place point on the new line indented.
Emulates the Brief insert-line function. With argument, do not indent."
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
Emulates the Brief delete-line function. With argument, do it that many times."
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
With argument, do it that many times."
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
  "Starts line-marking mode."
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'brief-mark-line-hook nil t))

(defun brief-stop-line-marking ()
  "Stops line-marking mode."
  (remove-hook 'post-command-hook  'brief-mark-line-hook t))

(defun brief-line-marking-p ()
  "Return true if the buffer is in line marking mode."
  (memq 'brief-mark-line-hook post-command-hook))

(defun brief-mark-line-hook ()
  "Ensures that the point and mark are correctly positioned for
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

      ;; Ensure resulting region is highlited correctly in XEmacs
      (if brief-xemacs-flag
	  (if zmacs-region-active-p
	      (zmacs-update-region)
	    (zmacs-activate-region))))))

(defun brief-mark-line (&optional arg)
  "Mark the current line from anywhere on the line.
Emulates the Brief mark-line function. With argument, do it that many times."
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
  (when (and brief-xemacs-flag zmacs-region-active-p)
    (zmacs-deactivate-region)))

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
If there is no active region then arg characters following point are deleted.
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
  "Make the front of the kill-ring a line-mode kill.
This is done by adding a text property."
  ;; Ensure the line is terminated with a newline
  (let* ((kill (car kill-ring))
	 (len (length kill)))
    (unless (and (> len 0) (= (aref kill (1- len)) ?\n))
      (setcar kill-ring (concat kill (string ?\n)))))
  ;; Indicate that this is a line-mode kill with a text property
  (put-text-property 0 1 'brief-line-kill t (car kill-ring)))

(defun brief-clear-line-kill (pos)
  "Remove the line-mode kill property from text in the buffer."
  (remove-text-properties pos (1+ pos) '(brief-line-kill t)))

(defun brief-line-kill-p (string)
  "Test if a string is a line-mode kill."
  (get-text-property 0 'brief-line-kill string))

(defvar brief-yank-col 0
  "The original column where brief-yank was initiated.
This is restored after the yank.")

(defvar brief-last-yank-was-line nil
  "True if the last yank was from a line-mode kill.")

(defun brief-yank (&optional arg)
  "Identical to the normal yank command, but correctly inserts text that
was killed in line-mode and also indents it."
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
  "Identical to the normal yank-pop command, but correctly inserts text that
was killed in line-mode and also indents it."
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
  "The original column from the start of a sequence Brief
paging and scrolling commands.")

(defun brief-page-down (&optional arg)
  "Scroll the current window up by one page, respecting next-screen-context-lines.
Paging up afterwards should return point to the same position.
The optional argument specifies the number of pages to scroll."
  (interactive "P")
  (if brief-xemacs-flag
      (setq zmacs-region-stays t))
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(brief-page-up (- pages))
      (while (and (> pages 0) (not (pos-visible-in-window-p (point-max))))
	(brief-scroll-screen (- (1- (window-height))
				next-screen-context-lines))
	(decf pages)))))
(put 'brief-page-down 'brief-scroll-command t)
    
(defun brief-page-up (&optional arg)
  "Scroll the current window down by one page, respecting next-screen-context-lines.
Paging down afterwards should return point to the same position.
The optional argument specifies the number of pages to scroll."
  (interactive "P")
  (if brief-xemacs-flag
      (setq zmacs-region-stays t))
  (let ((pages (prefix-numeric-value arg)))
    (if (< pages 0)
	(brief-page-down (- pages))
      (while (and (> pages 0) (not (pos-visible-in-window-p (point-min))))
	(brief-scroll-screen (- next-screen-context-lines 
				(1- (window-height))))
	(decf pages)))))
(put 'brief-page-up 'brief-scroll-command t)

(defun brief-scroll-screen (lines)
  "Scroll current window by given number of lines, but keep the cursor's relative
position in the window.
This is a helper function used by brief-page-up/down.
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
The optional argument specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (if brief-xemacs-flag
      (setq zmacs-region-stays t))
  (brief-scroll-line (prefix-numeric-value arg)))
(put 'brief-row-up 'brief-scroll-command t)

(defun brief-row-down (&optional arg)
  "Scroll the current window up by one line.
The optional argument specifies the number of lines to scroll, defaulting to 1."
  (interactive "P")
  (if brief-xemacs-flag
      (setq zmacs-region-stays t))
  (brief-scroll-line (- (prefix-numeric-value arg))))
(put 'brief-row-down 'brief-scroll-command t)

(defun brief-scroll-line (lines)
  "Scroll the current window down by the given number of lines.
This is a helper function used by brief-row-up/down."
  (unless (brief-scroll-command-p last-command)
    (setq brief-temporary-goal-column (current-column)))
  (scroll-down lines)
  (move-to-column brief-temporary-goal-column))

(defun brief-scroll-command-p (cmd)
  "Determine if the given command is a Brief scrolling command."
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
;;;###autoload
(defun brief-mode (&optional arg)
  "Toggle Brief emulation minor mode.
With ARG, turn Brief mode on if ARG is positive, off otherwise.

Key bindings:
\\{brief-mode-map}"
  (interactive "P")
  ;; Turn the mode on or off
  (setq brief-mode
	(if (null arg)
	    (not brief-mode)
	  (> (prefix-numeric-value arg) 0)))

  (when brief-mode
    ;; Force transient-mark-mode.
    (if brief-xemacs-flag
	(setq zmacs-regions t)
      (transient-mark-mode t))
    ;; Run mode hook
    (run-hooks 'brief-mode-hook)))

;; Add Brief as a minor mode
(if (fboundp 'add-minor-mode)
    (add-minor-mode 'brief-mode 'brief-mode-modeline-string
		    brief-mode-map nil 'brief-mode)
  (add-to-list 'minor-mode-alist '(brief-mode brief-mode-modeline-string))
  (add-to-list 'minor-mode-map-alist (cons 'brief-mode brief-mode-map)))

;; Load finished
(run-hooks 'brief-load-hook)
(provide 'brief)

;;; brief.el ends here
