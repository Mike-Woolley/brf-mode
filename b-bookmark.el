;;; b-bookmark.el --- Bookmark feature of b-mode

;; Copyright (C) 2002 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>
;; Version: $Id$

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
;;  See b.el

;;; Code:

(require 'b-compat)

(defface b-bookmark-face '((t (:background "khaki")))
  "Face used to show bookmark."
  :group 'b)

(defface b-bookmark-number-face '((t (:foreground "red" :weight bold :box t :height 0.7)))
  "Face used for bookmark number."
  :group 'b)

(when b-margin-support-flag
  (defcustom b-bookmark-margin 1
    "Width of margin (in characters) used to display bookmark number."
    :type 'integer
    :group 'b))

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
  (cl-labels ((digitp (char)
		      (and (>= char ?0) (<= char ?9)))
	      (read-digit (prompt)
			  (do* ((cursor-in-echo-area t)
				(char (ignore-errors (read-char prompt))
				      (ignore-errors (read-char
						      (format "%s(single digit) " prompt)))))
			      ((and char (digitp char)) char)
			    (unless (characterp char)
			      ;; Swallow the offending non-character event which is still pending
			      (read-event))
			    ;; Tell user they've made a mistake
			    (beep))))

    (let ((number (or current-prefix-arg
		      (- (read-digit prompt) ?0))))
      (unless (b-valid-bookmark-number-p number)
	(error (format "%d is an invalid bookmark number" number)))
      (list number))))

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

  ;; Lookup the bookmark and move it to the new location, create a new one if it doesn't exist yet
  (let ((bookmark (aref b-bookmarks number)))
    (if bookmark
	(b-move-bookmark bookmark)
      (b-create-bookmark number)))

  (setq b-current-bookmark number)
  (message "Bookmark %d dropped" number))

(defun b-create-bookmark (number)
  "Create new bookmark NUMBER at point."
  (let ((buffer (current-buffer))
	(start-line (b-bol-position 1))
 	(end-line (b-bol-position 2)))
    (lexical-let ((marker (point-marker))
		  (overlay (make-overlay start-line end-line buffer t nil)))
      (set-marker-insertion-type marker t) ; Insert before the marker
      (overlay-put overlay 'face 'b-bookmark-face)
      (overlay-put overlay 'help-echo (format "Bookmark %d" number))
      (let ((overlay-string (format "%d>" number))
	    (margin-string (format "%d" number)))
	(put-text-property 0 (length overlay-string)
			   'display (list '(margin left-margin) margin-string) overlay-string)
	(put-text-property 0 (length margin-string)
			   'face 'b-bookmark-number-face margin-string)
	(overlay-put overlay 'before-string overlay-string)
	(b-set-bookmark-margin buffer))

      ;; Ensure the bookmark overlay is on the line containing the bookmark
      ;; XEmacs overlay compatibility doesn't support modification hook and barfs
      ;; if this property is set, so don't do this if XEmacs
      (unless b-xemacs-flag
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
	    (make-b-bookmark :number number :marker marker :overlay overlay)))))

(defun b-allocate-next-available-bookmark ()
  "Allocate the next available bookmark."
  (interactive)
  (let ((number 0))
    (while (and (b-valid-bookmark-number-p number) (b-valid-bookmark-p (aref b-bookmarks number)))
      (incf number))
    (if (b-valid-bookmark-number-p number)
	(b-set-bookmark number)
      (error "All bookmarks allocated"))))

(defun b-move-bookmark (bookmark)
  "Move BOOKMARK to point."
  (let ((buffer (current-buffer))
	(overlay (b-bookmark-overlay bookmark))
	(start-line (b-bol-position 1))
 	(end-line (b-bol-position 2)))
    (move-marker (b-bookmark-marker bookmark) (point) buffer)
    (move-overlay overlay start-line end-line buffer)
    (b-set-bookmark-margin buffer)))

(defun b-set-bookmark-margin (buffer)
  "Add bookmark margin to BUFFER."
  (when b-margin-support-flag
    ;; Put the number string into the margin
    ;; TODO: Figure out how to do this in XEmacs
    (let ((margin b-bookmark-margin))
      (unless (and left-margin-width (>= left-margin-width margin))
	(setq left-margin-width margin)
	(dolist (win (get-buffer-window-list buffer nil t))
	  (set-window-margins win margin))))))

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

  (let ((select-bookmark #'(lambda (idx)
			     (let ((bookmark (aref b-bookmarks idx)))
			       (cond ((b-valid-bookmark-p bookmark)
				      (gm-quit)
				      (b-jump-to-bookmark idx))
				     (t	; Bookmark not set
				      (message "Bookmark %d has not been set" idx)
				      (ding))))))
	(display-bookmark #'(lambda (idx)
			      (let ((bookmark (aref b-bookmarks idx)))
				(cond ((b-valid-bookmark-p bookmark)
				       (let ((marker (b-bookmark-marker bookmark)))
					 (with-current-buffer (marker-buffer marker)
					   (save-excursion
					     (goto-char marker)
					     (format "%s %d\tL%d\tC%d\t%d%%\t%s"
						     (if (= b-current-bookmark idx) "*" " ")
						     idx
						     (b-current-line) (b-current-column)
						     (/ (* (point) 100) (buffer-size))
						     (buffer-name))))))
				      (t ; Bookmark not set
				       (format "  %d <NOT SET>" idx)))))))
    (gm-popup :buffer-name "*Bookmarks*"
	      :header-line "Bookmarks: [SELECT] to Jump to bookmark, [q] to Quit."
	      :max-entries b-max-bookmarks
	      :truncate-lines t
	      :regexp-start-position (format "^[* ][ \t]+%d" b-current-bookmark)

	      :elements (loop for idx from 0 to (1- b-max-bookmarks)
			      collect idx)

	      :select-callback select-bookmark
	      :display-string-function display-bookmark)))

(defun b-current-line ()
  "Return current line number of point (starting at 1)."
  (+ (count-lines 1 (point))
     (if (= (current-column) 0) 1 0)))

(defun b-current-column ()
  "Return current line number of point (starting at 1)."
  (1+ (current-column)))

(provide 'b-bookmark)

;;; b-bookmark.el ends here
