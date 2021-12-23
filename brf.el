;;; brf.el --- Brf-mode provides features from the legendary editor Brief -*- lexical-binding: t -*-

;; Copyright (C) 1999-2021 Mike Woolley
;; Author: Mike Woolley <mike@bulsara.com>
;; Package-Version: 1.21
;; Package-Requires: ((fringe-helper "0.1.1") (emacs "24.3"))
;; Keywords: brief crisp emulations
;; URL: https://bitbucket.org/MikeWoolley/brf-mode

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

;; Brf-mode adds functionality from the legendary programmer's editor `Brief' to Emacs.
;;
;; It is not an emulation of `Brief' (there are plenty of those already), but rather
;; provides an accurate implementation in Emacs of specific features that I miss
;; from `Brief'.  Principally these are:
;;
;; * Line-mode cut and paste.
;; * Column-mode cut and paste.
;; * Fully reversible paging and scrolling.
;; * Temporary bookmarks.
;; * Cursor motion undo.
;; * Easy window management.
;;
;; See README.org or Info manual for further details.

;;; Change Log:
;;
;; See the ChangeLog file.

;;; Code:

(require 'easymenu)

(defgroup brf nil
  "Add functionality from the editor Brief."
  :prefix "brf-"
  :group 'editing)

(defcustom brf-mode-modeline-string " Brf"
  "String to display in the mode-line when Brf mode is enabled.
Set this to nil to conserve valuable mode line space."
  :type 'string
  :group 'brf)

;;;
;;; Version number
;;;
(defconst brf-version "1.21"
  "Version number of Brf mode.")

(defun brf-version ()
  "Display version number of Brf mode."
  (interactive)
  (let ((version (if (fboundp 'pkg-info-version-info)
		     (pkg-info-version-info 'brf)
		   brf-version)))
    (message "Brf version %s" version)))

;;;
;;; Load the different features
;;;
(require 'brf-compat)
(require 'brf-bookmark)
(require 'brf-editing)
(require 'brf-marking)
(require 'brf-movement)
(require 'brf-undo)
(require 'brf-window)

;;;
;;; Keymap
;;;
(defvar brf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Put the Brf mode overrides on the same keys as Brief
    (define-key map [(control return)] 'brf-insert-line)
    (define-key map "\M-a" 'set-mark-command) ; Non-inclusive Mark (ie normal Emacs mark)
    (define-key map "\M-d" 'brf-delete-line)
    (define-key map "\M-m" 'set-mark-command) ; Inclusive Mark (not implemented - use normal mark)
    (define-key map "\M-l" 'brf-mark-line)
    (when (fboundp 'rectangle-mark-mode)
      (define-key map "\M-c" 'rectangle-mark-mode))
    (define-key map [(kp-add)] 'brf-copy-region)
    (define-key map [(kp-subtract)] 'brf-kill-region)
    (define-key map [(kp-multiply)] 'undo)
    (define-key map "\M-u" 'undo)
    (define-key map [(delete)] 'brf-delete)
    (define-key map [(kp-delete)] 'brf-delete)
    (define-key map [(meta up)] 'brf-row-up)
    (define-key map [(meta down)] 'brf-row-down)
    (define-key map [(home)] 'brf-home)
    (define-key map [(end)] 'brf-end)
    (define-key map "\t" 'brf-tab)
    (define-key map "\M-=" 'brf-next-bookmark)
    (define-key map [(meta kp-add)] 'brf-next-bookmark)
    (define-key map "\M--" 'brf-prev-bookmark)
    (define-key map [(meta kp-subtract)] 'brf-prev-bookmark)
    (define-key map [(shift up)] 'brf-change-window-up)
    (define-key map [(shift down)] 'brf-change-window-down)
    (define-key map [(shift left)] 'brf-change-window-left)
    (define-key map [(shift right)] 'brf-change-window-right)

    ;; Also put them on the original Emacs key-mappings
    (substitute-key-definition 'kill-ring-save 'brf-copy-region map (current-global-map))
    (define-key map [menu-bar edit copy] '(menu-item "Copy" brf-copy-region)) ; Override the OS-specific "Copy" menu item.
    (substitute-key-definition 'kill-region 'brf-kill-region map (current-global-map))
    (substitute-key-definition 'yank 'brf-yank map (current-global-map))
    (substitute-key-definition 'yank-pop 'brf-yank-pop map (current-global-map))
    (substitute-key-definition 'beginning-of-line 'brf-home map (current-global-map))
    (substitute-key-definition 'end-of-line 'brf-end map (current-global-map))

    ;; Also override the Cut & Paste commands on the toolbar
    (define-key map [tool-bar cut] 'brf-kill-region)
    (define-key map [tool-bar copy] 'brf-copy-region)
    (define-key map [tool-bar paste] 'brf-yank)

    ;; Function keys
    ;; F1 - Change window
    (let ((f1-prefix (make-sparse-keymap "Point to destination (use cursor keys)")))
      (define-key f1-prefix [(up)] 'brf-change-window-up)
      (define-key f1-prefix [(down)] 'brf-change-window-down)
      (define-key f1-prefix [(left)] 'brf-change-window-left)
      (define-key f1-prefix [(right)] 'brf-change-window-right)
      (define-key map [(f1)] f1-prefix))
    ;; F2 - Resize window
    (define-key map [(f2)] 'brf-resize-window-start)
    (define-key map [(meta f2)] 'brf-zoom-window)
    ;; F3 - Create window
    (let ((f3-prefix (make-sparse-keymap "Select side for new window (use cursor keys)")))
      (define-key f3-prefix [(up)] 'brf-create-window-up)
      (define-key f3-prefix [(down)] 'brf-create-window-down)
      (define-key f3-prefix [(left)] 'brf-create-window-left)
      (define-key f3-prefix [(right)] 'brf-create-window-right)
      (define-key map [(f3)] f3-prefix))
    ;; F4 - Delete window
    (let ((f4-prefix (make-sparse-keymap "Select window to delete (use cursor keys)")))
      (define-key f4-prefix [(up)] 'brf-delete-window-up)
      (define-key f4-prefix [(down)] 'brf-delete-window-down)
      (define-key f4-prefix [(left)] 'brf-delete-window-left)
      (define-key f4-prefix [(right)] 'brf-delete-window-right)
      (define-key map [(f4)] f4-prefix))
    ;; These were not original Brief key-sequences
    (define-key map [(control f4)] 'brf-delete-current-window)
    (define-key map [(shift f4)] 'delete-other-windows)

    ;; Create new key bindings for my new functions that weren't part of Brief
    (define-key map "\C-c\C-b\C-n" 'brf-next-bookmark)
    (define-key map "\C-c\C-b\C-p" 'brf-prev-bookmark)
    (define-key map "\C-c\C-b\C-k" 'brf-kill-all-bookmarks)
    (define-key map "\C-c\C-b\C-l" 'brf-list-bookmarks)
    (define-key map "\C-c\C-b="    'brf-allocate-next-available-bookmark)
    (define-key map "\C-c\C-b\C-w" 'brf-copy-to-register)
    (define-key map "\C-c\C-b\C-y" 'brf-insert-register)

    ;; Try and find the existing commands for scrolling up/down,
    ;; as these are different in Emacs & XEmacs
    (let ((scroll-up-cmd (global-key-binding [(next)]))
	  (scroll-down-cmd (global-key-binding [(prior)])))
      (when scroll-up-cmd
	(substitute-key-definition scroll-up-cmd 'brf-page-down map (current-global-map)))
      (when scroll-down-cmd
	(substitute-key-definition scroll-down-cmd 'brf-page-up map (current-global-map))))

    ;; Setup the bookmarks on the M-digit keys, like in Brief
    ;; Prefix args will have to be entered with the C-digit or C-U number methods...
    (dotimes (digit brf-max-bookmarks)
      (define-key map (vector (list 'meta (+ digit ?0))) (brf-make-set-bookmark digit)))
    (define-key map "\M-j" 'brf-jump-to-bookmark)

    map)
  "Local keymap for Brf mode.")

;;;
;;; Menu
;;;
(easy-menu-define brf-mode-menu nil
  "Menu for Brf mode."
  '("Brf"
    ["Enable Brf-mode" brf-mode
     :style toggle :selected brf-mode
     :Help "Enable Brf-mode"]
    "---"
    ["Copy to Register" brf-copy-to-register
     :enable brf-mode
     :help "Copy marked text to register"]
    ["Insert Register" brf-insert-register
     :enable (and brf-mode (not buffer-read-only))
     :help "Insert text from register"]
    "---"
    ["Allocate Next Free Bookmark" brf-allocate-next-available-bookmark
     :enable brf-mode
     :help "Allocate Next Free Bookmark at Point"]
    ["List Bookmarks" brf-list-bookmarks
     :enable brf-mode
     :help "List and manage bookmarks"]
    ["Delete All Bookmarks" brf-kill-all-bookmarks
     :enable brf-mode
     :help "Remove all bookmarks"]
    "---"
    ["Preferences" (lambda () (interactive) (customize-group "brf"))
     :enable brf-mode
     :Help "Show Brf Mode preferences"]
    "---"
    ["Help" (lambda () (interactive) (describe-function 'brf-mode))
     :help "Show mode help"]
    ["Manual" (lambda () (interactive) (info "Brf-mode"))
     :help "Show Info manual"]
    ["Website" (lambda () (interactive) (browse-url "https://bitbucket.org/MikeWoolley/brf-mode"))
     :help "Show the Brf project website in a browser"]
    ["Version" brf-version
     :help "Display version information"]))
(easy-menu-add brf-mode-menu)
(easy-menu-add-item nil '("Edit") "---")
(easy-menu-add-item nil '("Edit") brf-mode-menu)
(easy-menu-add-item nil '("Edit") "---")

;; Make the menu available from the mode line "lighter"
(define-key brf-mode-map [menu-bar edit brf] (cons "Brf" brf-mode-menu))

;;;
;;; Brf minor mode
;;;
(defvar brf-prev-mark-mode nil
  "Previous value of transient mark mode.")
(defvar brf-prev-truncate-lines nil
  "Previous value of `truncate-lines'.")
(defvar brf-prev-scroll-step nil
  "Previous value of `scroll-step'.")
(defvar brf-prev-hscroll-margin nil
  "Previous value of `hscroll-margin'.")
(defvar brf-prev-hscroll-step nil
  "Previous value of `hscroll-step'.")
(defvar brf-prev-c-m nil
  "Previous global binding of CR.")
(defvar brf-prev-c-j nil
  "Previous global binding of LF.")

;;;###autoload
(define-minor-mode brf-mode
  nil ; Use default doc string
  :lighter brf-mode-modeline-string
  :keymap brf-mode-map
  :global t

  ;; Processing that needs to be done when the mode is started or stopped
  (cond (brf-mode
	 ;; Force transient-mark-mode and conservative scrolling
	 ;; Remember old settings
	 (setq brf-prev-mark-mode (brf-transient-mark-mode t))
	 (setq brf-prev-truncate-lines (default-value truncate-lines))
	 (setq brf-prev-scroll-step scroll-step)
	 (setq brf-prev-hscroll-margin hscroll-margin)
	 (setq brf-prev-hscroll-step hscroll-step)
	 (setq-default truncate-lines t)
	 (setq scroll-step 1)		  ; Scroll line at a time
	 (setq hscroll-margin 1)	  ; Scroll when we get to the end of the line
	 (setq hscroll-step 1)		  ; Scroll one column at a time

	 ;; Setup return (in the global map) to always indent
	 (setq brf-prev-c-m (global-key-binding "\C-m"))
	 (setq brf-prev-c-j (global-key-binding "\C-j"))
	 (global-set-key "\C-m" 'newline-and-indent)
	 (global-set-key "\C-j" 'newline)

	 ;; Override "Select and Paste" menu item
	 (ad-enable-advice 'menu-bar-select-yank 'around 'brf-menu-bar-select-yank)
	 (ad-activate 'menu-bar-select-yank))

	(t ; brf-mode off
	 ;; Disable override of "Select and Paste"
	 (ad-disable-advice 'menu-bar-select-yank 'around 'brf-menu-bar-select-yank)
	 (ad-activate 'menu-bar-select-yank)

	 ;; Restore old settings
	 (global-set-key "\C-j" brf-prev-c-j)
	 (global-set-key "\C-m" brf-prev-c-m)
	 (setq hscroll-step brf-prev-hscroll-step)
	 (setq hscroll-margin brf-prev-hscroll-margin)
	 (setq scroll-step brf-prev-scroll-step)
	 (setq-default truncate-lines brf-prev-truncate-lines)
	 (brf-transient-mark-mode brf-prev-mark-mode))))

(provide 'brf)

;; Local Variables:
;; tab-width: 8
;; indent-tabs-mode: t
;; End:

;;; brf.el ends here
