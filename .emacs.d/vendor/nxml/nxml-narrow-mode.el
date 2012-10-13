;; nxml-script.el -- add support to nxml-mode for blocks of php, css,
;;                   javascript and visual basic embedded in XHTML
;;
;; Copyright (C) 2004 by P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/emacs/download/nxml-script.el
;; Version: 1.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;; Commentary:
;;
;; When using nxml-mode to edit XHTML, this file enables you to
;; narrow the visible buffer to show just a php/javascript/visual
;; basic script block, or a css style block, and then to switch from
;; nxml-mode to the appropriate mode for that block.  When finished
;; editing that block, it enable you easily to show the whole buffer
;; and switch back to nxml-mode again.
;;
;; The idea and much of the code was taken from html-helper-mode.el
;;
;; To install, put this file in your load-path, and in your .emacs you
;; might need to set some variables to indicate what major-modes you
;; wish to use, and load the file when nxml-mode is loaded, like so:
;;
;;  (setq nxml-script-uses-php t)
;;  (setq nxml-script-uses-css t)
;;  (setq nxml-script-uses-generic-javascript t)
;;  (eval-after-load "nxml-mode" (load "nxml-script"))
;;
;; Supported major modes for script/style blocks include php-mode,
;; css-mode, javascript-generic-mode, jde-mode, visual-basic-mode.
;;
;; You might also need to tell emacs to open .php, .jsp and such files
;; in nxml-mode, like so:
;;
;;  (setq auto-mode-alist (cons '("\\.php[34]?$" . nxml-mode) auto-mode-alist))
;;
;; The user interface consists of a single keystroke (by default, F12
;; -- to change it, change the value of 'nxml-script-key).  In
;; nxml-mode, this key looks to see if the cursor is inside a
;; script/style block, and if it is, it narrows the buffer to that
;; block and switches to the appropriate mode.  Hit the same key again
;; to go back to editing the whole file in nxml-mode.
;;
;; As a convenience, if you are in nxml-mode and the cursor is not in
;; a script/style block, this keystroke executes another command
;; instead (by default, nxml-complete, which is usually bound to
;; <C-return> -- to change it, change the value of
;; 'nxml-script-function).  If, however, you are using
;; transient-mark-mode and the mark is active while the cursor is
;; outside a script/style block, then a different function is called
;; (by default, nxml-script-tag-region -- to change it, customize
;; 'nxml-script-region-function).  The default function provided for
;; this purpose puts an < at the start of the current region, prompts
;; you for a tag to use there, then puts the matching closing tag at
;; the end of the current region.
;;
;; BUGS:
;;
;; I regularly use only CSS and PHP, very occasionally Javascript, and
;; Visual Basic not at all, and the code for these last two has not
;; been tested.

(defvar nxml-script-key [f12])
(defvar nxml-script-function 'nxml-complete)
(defvar nxml-script-region-function 'nxml-script-tag-region)

(defvar nxml-script-uses-php t)
(defvar nxml-script-uses-css t)
(defvar nxml-script-uses-javascript t)
(defvar nxml-script-uses-jde nil)
(defvar nxml-script-uses-java-mode nil)
(defvar nxml-script-uses-visual-basic t)

(when nxml-script-uses-php
  (autoload 'php-mode "php-mode" "PHP mode" t))
(when nxml-script-uses-css
  (autoload 'css-mode "css-mode" "CSS mode" t))
(when nxml-script-uses-javascript
  (autoload 'javascript-mode "javascript-mode" "JavaScript-mode" t))
(when nxml-script-uses-jde
  (autoload 'jde-mode "jde" "JDE mode." t))
(when nxml-script-uses-visual-basic
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t))

(defun nxml-script-narrow-to-php ()
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (when (search-backward "<?" nil t)
	(beginning-of-line)
	(let ((beg (point)))
	  (when (and
		 (search-forward "?>" nil t)
		 (>= (point) orig))
	    (narrow-to-region beg (point))
	    (if nxml-script-uses-php
		(php-mode)
	      (c-mode))
	    (nxml-script-install-widen-key)))))))

(defun nxml-script-narrow-to-css ()
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (when (search-backward-regexp "<style[ \t]+type=\"text/css\"" nil t)
	(beginning-of-line)
	(let ((beg (point)))
	  (when (and
		 (search-forward "</style>" nil t)
		 (>= (point) orig))
	    (narrow-to-region beg (point))
	    (if nxml-script-uses-php
		(css-mode)
	      (c-mode))
	    (nxml-script-install-widen-key)))))))

(defun nxml-script-narrow-to-javascript ()
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (when (search-backward-regexp
	     "<script[ \t]+\\(language=\"JavaScript\"\\|type=\"text/javascript\"\\)[ \t]*>"
	     nil t)
	(beginning-of-line)
	(let ((beg (point)))
	  (when (and
		 (search-forward "</script>" nil t)
		 (>= (point) orig))
	    (narrow-to-region beg (point))
	    (if nxml-script-uses-jde
		(jde-mode)
	      (if nxml-script-uses-java-mode
		  (java-mode)
		(if nxml-script-uses-javascript
		    (javascript-generic-mode)
		  (c-mode))))
	    (nxml-script-install-widen-key)))))))


(defun nxml-script-narrow-to-asp ()
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (when (search-backward "<%" nil t)
	(beginning-of-line)
	(let ((beg (point)))
	  (when (and
		 (search-forward "%>" nil t)
		 (>= (point) orig))
	    (narrow-to-region beg (point))
	    (visual-basic-mode)
	    (nxml-script-install-widen-key)))))))


(defun nxml-script-narrow-to-vbscript ()
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (when (search-backward-regexp
	     "<script[ \t]+\\(language=\"VBScript\"\\|type=\"text/vbscript\"\\)[ \t]*>"
	     nil t)
	(beginning-of-line)
	(let ((beg (point)))
	  (when (and
		 (search-forward "</script>" nil t)
		 (>= (point) orig))
	    (narrow-to-region beg (point))
	    (visual-basic-mode)
	    (nxml-script-install-widen-key)))))))

;; Credits from html-helper-mode.el:
;; Still from Stan Lanning here it comes the code for a "smart switch" to 
;; the appropriate scripting mode.

(defvar nxml-script-narrow-alist
  `((,(regexp-quote "<%") . nxml-script-narrow-to-asp)
    (,(regexp-quote "<?") . nxml-script-narrow-to-php)
    ("<style[ \t]+type=\"text/css\"" . nxml-script-narrow-to-css)
    ("<style[ \t]+type=\"css\"" . nxml-script-narrow-to-css)
    ("<script[ \t]+language=\"VBScript\"[ \t]*>" . nxml-script-narrow-to-vbscript)
    ("<script[ \t]+type=\"text/vbscript\"[ \t]*>" . nxml-script-narrow-to-vbscript)
    ("<script[ \t]+language=\"JavaScript\"[ \t]*>" . nxml-script-narrow-to-javascript)
    ("<script[ \t]+type=\"text/javascript\"[ \t]*>" . nxml-script-narrow-to-javascript)))

(defvar nxml-script-start-regexp
  (concat "\\(" (mapconcat (lambda (x) (car x)) nxml-script-narrow-alist "\\|") "\\)"))

(defun nxml-script-narrow ()
  (interactive)
  (let ((handler-fn (save-excursion
		      (if (re-search-backward nxml-script-start-regexp nil t)
			  (catch 'handler-found
			    (mapcar (lambda (p)
				      (if (looking-at (car p))
					  (throw 'handler-found (cdr p))))
				    nxml-script-narrow-alist)
			    nil)))))
    (if handler-fn
	(apply handler-fn nil)
      nil)))

(defun nxml-script-widen ()
  (interactive)
  (widen)
  (nxml-mode))

(defun nxml-script-install-nxml-key ()
  (local-set-key nxml-script-key 'nxml-script-dwim))
(add-hook 'nxml-mode-hook 'nxml-script-install-nxml-key)

(defun nxml-script-install-widen-key ()
  (local-set-key nxml-script-key 'nxml-script-widen))

(defun nxml-script-dwim ()
  (interactive)
  (unless (nxml-script-narrow)
    (if (and transient-mark-mode mark-active)
	(funcall nxml-script-region-function)
      (funcall nxml-script-function))))

(defun nxml-script-tag-region ()
  (when mark-active
    (let ((beg (region-beginning))
	  (end (set-marker (make-marker) (region-end))))
      (deactivate-mark)
      (goto-char beg)
      (insert "<")
      (nxml-complete)
      (insert ">")
      (goto-char (marker-position end))
      (nxml-finish-element)
      (indent-region beg end)
      (nxml-indent-line))))
  
(provide 'nxml-script)

