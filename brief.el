;; -*- Mode: Emacs-Lisp -*-

;;
;; Emulate the Brief insert-line function
;;
(defun my-insert-line ()
  "Create a new line underneath the current one and place point on the new line (indented).
Emulates the Brief insert-line function."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key '[(control return)] 'my-insert-line)

;;
;; Emulate Brief delete-line function
;;
(defun my-kill-line ()
  "Kill the current line from anywhere on the line.
Emulates the Brief delete-line function."
  (interactive)
  (let ((column (current-column)))
	(beginning-of-line)
	(kill-line 1)
	(move-to-column column)))
(global-set-key "\M-d" 'my-kill-line)

;;
;; Mark the current line (and move point to the begining)
;;
(defun my-mark-line (arg)
  "Mark the current line.
Moves point to the begining of the line. With argument, do this that many times."
  (interactive "p")
  (beginning-of-line)
  (mark-something 'my-mark-line 'forward-line arg))
(global-set-key "\M-`" 'my-mark-line)

;;
;; Copy the current line to the kill ring
;;
(defun my-copy-line (arg)
  "Copies the current line to the kill ring. With argument, do this that many times."
  (interactive "p")
  (let ((column (current-column)))
	(my-mark-line arg)
	(call-interactively 'kill-ring-save)
	(move-to-column column)
	(zmacs-deactivate-region)))
(global-set-key "\M-+" 'my-copy-line)
(global-set-key '[(meta kp-add)] 'my-copy-line)

;;
;; Setup the fn keys
;;
;(defmacro my-fkey-setup(num)
;  (list 'global-set-key (concat "[(f" num ")]")
;		(list 'lambda() (list 'interactive) (list 'jump-to-register (concat "?" num)))))
(global-set-key '[(meta f1)] '(lambda () (interactive) (point-to-register ?1) (message "Bookmark 1 dropped")))
(global-set-key '[(f1)]		 '(lambda () (interactive) (jump-to-register ?1)))
(global-set-key '[(meta f2)] '(lambda () (interactive) (point-to-register ?2) (message "Bookmark 2 dropped")))
(global-set-key '[(f2)]		 '(lambda () (interactive) (jump-to-register ?2)))
(global-set-key '[(meta f3)] '(lambda () (interactive) (point-to-register ?3) (message "Bookmark 3 dropped")))
(global-set-key '[(f3)]		 '(lambda () (interactive) (jump-to-register ?3)))
(global-set-key '[(meta f4)] '(lambda () (interactive) (point-to-register ?4) (message "Bookmark 4 dropped")))
(global-set-key '[(f4)]		 '(lambda () (interactive) (jump-to-register ?4)))

;;
;; Setup the fn keys
;;
(defmacro my-fkey-setup(num)
  "Setup the set/jump bookmark functions on the given fn key number."
  (let* ((num-str (number-to-string (eval num)))
	 (reg (string-to-char num-str))
	 (fkey (intern (concat "f" num-str))))
    `(progn
       (global-set-key [(,fkey)] (lambda() (interactive) (jump-to-register ,reg)))
       (global-set-key [(meta ,fkey)] (lambda () (interactive) (point-to-register ,reg) (message ,(concat "Bookmark " num-str " dropped")))))))
(dotimes (key 4) (my-fkey-setup (+ key 1)))

(defun my-row-up()
  "Scroll up one line."
  (interactive)
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (if (not (pos-visible-in-window-p (point-min)))
      (scroll-down 1))
  (move-to-column pager-temporary-goal-column))
(defun my-row-down()
  "Scroll down one line."
  (interactive)
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (if (not (pos-visible-in-window-p (point-max)))
	  (scroll-up 1))
  (move-to-column pager-temporary-goal-column))
(setq pager-keep-column-commands (append pager-keep-column-commands '(my-row-up my-row-down)))
(global-set-key '[(meta up)]		'my-row-up)
(global-set-key '[(meta kp-up)]		'my-row-up)
(global-set-key '[(meta down)]		'my-row-down)
(global-set-key '[(meta kp-down)]	'my-row-down)
