;; -*-emacs-lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration File
;;
;; Paolo de Dios <paolodedios@gmail.com>
;; Copyright 2010. All Rights Reserved.
;; M-x load-file RET .emacs RET 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Standard Keyboard shortcuts 
;; Notation:
;; C-x means press the control key with for example the x key
;; M-x means press the alt key with for example the x key, 
;; or press the escape key and then x. 
;;
;; Buffers
;; C-x b  changes to a different buffer
;; C-x k  kills the current buffer or type in the name of a buffer
;;        that you want killed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Moving About 
;; To move about the current buffer in emacs use the cursor keys. 
;; A selection of other keys: 
;;
;; C-p    previous line 
;; C-n    next line 
;; C-b    back one character 
;; M-b    back one word 
;; C-f    forward one character 
;; M-f    forward one word 
;; C-home beginning of line 
;; C-end  end of line
;; C-v    page down 
;; M-v    page up 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C-s    locate (search within file)
;; C-r    search backwards
;; C-u    undo
;; C-y    yank from kill buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to a specific line number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-M 
;; type goto-line and press return
;; type in the line number and press return. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selecting and Copying Text 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To select a block of text: 
;;     Go to the start of the block 
;;     Press C-spacebar 
;;     Go to the end of the block
;; To cut (wipe) out the block:
;;     C-w 
;; To copy without cutting out the block:
;;     M-w
;; To paste (yank) the block again:
;;     C-y (you can do this as many times as you want) 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert or realign comment on current line; alternatively, comment or uncomment
;; the region (comment-dwim). 
;;     M-
;; Kill comment on current line (comment-kill). 
;;     C-u M-
;;
;; Set comment column (comment-set-column).  
;;     C-x 
;;
;; Like <RET> followed by inserting and aligning a comment (comment-indent-new-line).
;; See Multi-Line Comments. 
;;     C-M-j
;;     M-j
;;
;; Add or remove comment delimiters on all the lines in the region.
;;     M-x comment-region
;;     C-c C-c (in C-like modes)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To save the current buffer:
;;     C-x and C-s 
;; To save the current buffer, but under a new name: 
;;     C-x C-w 
;;     Type in the name of the file and press return. 
;; To open or create a new buffer: 
;;     Press C-x and C-f; 
;; To insert another file into the current buffer: 
;;     Go to where you wish the file to be inserted 
;;     Press C-x and i 
;;     Type in the name of the file to insert and press return 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Searching and Replacing 
;; Go to the section of the document where you want the search and replace to start 
;; Press the escape key and then press 
;; Type in the text you wish to search for and press return 
;; Type in the text that should replace the match and press return 
;; EMACS now goes through the document looking for matches 
;; Press Y to replace each time a match is made, 
;; N to ingore that match and, 
;; ! goes through the rest of the document, changing all matches without asking you. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell Checking 
;; Press M-x 
;; Type ispell-buffer and press return 
;; EMACS then spells checks the current buffer from start to finish 
;; Possible replacements are given at the top of the document 
;; (press the number or character in square brackets to use that suggestion) 
;; The commands you can use are given at the bottom (press ? for more options) 
;; e.g. 
;; r to replace the word 
;; i to insert the word into your personal dictionary 
;; x to exit the spell check program. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Debugging Configuration Commands and Variables
;; ----------------------------------------------
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html#Help
;; Emacs provides extensive help features, all accessible through the help character
;;   C-h
;; Examples:
;;  C-h k key        Gives more information about a key/command: it displays the documentation
;;                   string of the command as well as its name
;;  C-h K key        Finds the documentation of a key sequence key
;;  C-h w command    Lists the keys that are bound to command.
;;  C-h f function   Describes a function. Displays the documentation of Lisp function
;;  C-h v variable   Similar to "C-h f" but describes Lisp variables instead of Lisp functions
;;
;; elisp stacktrace mode
;; ---------------------
;; M-x set-variable RET debug-on-error  RET t RET
;; M-x set-variable RET debug-on-signal RET t RET
;;
;; This should give a backtrace the next time the error happens, which may show 
;; where the real problem lies.
;;
;; elisp byte compiler
;; -------------------
;; The emacs byte compiler can be used to speed up processing various elisp packages
;;
;; Example:
;;   M-x byte-compile-file RET js2.el RET
;;
;; To byte compile everything
;;   C-u 0 M-x byte-recompile-directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap .emacs reload function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-dot-emacs ()
  "Reload the .emacs file."
  (interactive)
  (load "~/.emacs"))

(defun edit-dot-emacs ()
  "Edit the .emacs file."
  (interactive)
  (find-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set to "t" instead of "nil" to enter the elisp debugger when an error occurs
;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Error-Debugging.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error nil)
(setq debug-on-quit  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the load path for emacs lisp customization packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path  (cons (expand-file-name "~/.emacs.d") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs doesn't include subdirectories of a directory in the
;; load-path. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let* ((my-elisp-dir "~/.emacs.d")
              (default-directory my-elisp-dir))
           (setq load-path (cons my-elisp-dir load-path))
           (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alternatively, load only some of the subdirectories in the load-path
;;
;; (normal-top-level-add-to-load-path
;;   '("emms" "erc" "planner" "w3"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prevent Emacs from cluttering source directories with backup files (filenames
;; ending in ~) and autosave files (filenames starting with #). Move backup 
;; elsewhere by setting backup-directory-alist and auto-save-file-name-transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable backup files.
;; (setq make-backup-files  t)
;; (setq backup-by-copying  t)

;; Enable versioning with default values (keep five last versions, I think!)
;; (setq version-control    t)

;; Save all backup files in this directory.
;; (setq backup-directory-alist (quote ((".*" . "~/.emacs.backups.d/"))))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file))))

 
;; Save all autosave-* backup files to this directory.
;; Example:
;; (defvar user-temporary-file-directory
;;   (concat temporary-file-directory user-login-name "/"))
;;
(defvar user-temporary-file-directory "~/.emacs.backups.d/")

(make-directory user-temporary-file-directory t)
(setq backup-directory-alist
      `(("." . , user-temporary-file-directory)
        (, tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; delete old file versions
(setq delete-old-versions  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save cursor place for all files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'saveplace)

(setq save-place-file     "~/.emacs.saveplace")
(setq-default save-place  t                   ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Window Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-height (selected-frame)    90)
(set-frame-width  (selected-frame)   150)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs Window Frame Transparency
;; see http://www.emacswiki.org/emacs/CustomizeAquamacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Are we in Aquamacs?
(defvar aquamacs-p (boundp 'aquamacs-version))
(when aquamacs-p

  (defun transparency-set-initial-value ()
    "Set initial value of alpha parameter for the current frame"
    (interactive)
    (if (equal (frame-parameter nil 'alpha) nil)
        (set-frame-parameter nil 'alpha 100)))
 
  (defun transparency-set-value (numb)
    "Set level of transparency for the current frame"
    (interactive "Enter transparency level in range 0-100: ")
    (if (> numb 100)
        (message "Error. The maximum value for transparency is 100.")
      (if (< numb 0)
          (message "Error. The minimum value for transparency is 0.")
        (set-frame-parameter nil 'alpha numb))))
 
  (defun transparency-increase ()
    "Increase level of transparency for the current frame"
    (interactive)
    (transparency-set-initial-value)
    (if (> (frame-parameter nil 'alpha) 0)
        (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) -1))
      (message "This is the minimum value for transparency")))
 
  (defun transparency-decrease ()
    "Decrease level of transparency for the current frame"
    (interactive)
    (transparency-set-initial-value)
    (if (< (frame-parameter nil 'alpha) 100)
        (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) +1))
      (message "This is the minimum value for transparency")))
 
  ;; keybinding for transparency manipulation
  (define-key global-map (kbd "C-?") 'transparency-set-value)

  ;; bind transparency control
  (define-key global-map (kbd "C->") 'transparency-increase)
  (define-key global-map (kbd "C-<") 'transparency-decrease)
 
  ;; Set initial frame transparency
  (setq transparency-level 100)
  (transparency-set-value transparency-level)
  (add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))

  ;; DISABLED because of osx-key-mode
  ;;
  ;; Set the mac keyboard up for sane emacs usage.
  ;; [Ctrl][Option][Command/Alt]
  ;;
  ;; Within emacs only:
  ;;   "Alt/A-"   is mapped to the "Option" button
  ;;   "Meta/M-"  is mapped to the "Alt" button
  ;;   "Ctrl/C-"  is mapped to the "Ctrl" button (stays the same)
  ;;
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier  'alt)
  ;; (setq mac-control-modifier 'ctrl)

  ;; make C-v, M-v maintain the mark
  (define-key global-map "\C-v" 'scroll-up)
  (define-key global-map "\M-v" 'scroll-down)

  ;; copy .emacs to Preferences.el
  (defun copy-dot-emacs-to-prefs ()
    "Copy the .emacs file to Preferences.el."
    (interactive)
    (copy-file "~/.emacs" "~/Library/Preferences/Aquamacs Emacs/Preferences.el" t)
    (message "Copied .emacs to Aquamacs Preferences.el file " ))
  
  (defun edit-customizations ()
    "Edit the .emacs file."
    (interactive)
    (find-file "~/Library/Preferences/Aquamacs Emacs/customizations.el"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the title displayed in the title-bar "filename (path)"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq frame-title-format "%b (%f)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the buffer file coding system to the appropriate character set.
;; undecided-unix lets emacs use a platform neutral encoding.
;;
;; (set-buffer-file-coding-system 'iso-latin-1-unix)
;; (set-buffer-file-coding-system 'iso-latin-1-dos)
;; (set-buffer-file-coding-system 'undecided-unix)
;;
;; Default to 8-bit clean utf-8 encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-coding-systems    'utf-8-unix) 
(set-buffer-file-coding-system 'utf-8-unix) 

(prefer-coding-system          'utf-8) 
(setq locale-coding-system     'utf-8) 
(set-terminal-coding-system    'utf-8) 
(set-keyboard-coding-system    'utf-8) 
(set-selection-coding-system   'utf-8) 
(set-language-environment      "UTF-8") 

;; backwards compatibility as default-buffer-file-coding-system is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type    '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set file associations for automatic charset conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modify-coding-system-alist 'file "\\.m\\'"        'utf-8-unix)
(modify-coding-system-alist 'file "\\.c\\'"        'utf-8-unix)
(modify-coding-system-alist 'file "\\.h\\'"        'utf-8-unix)
(modify-coding-system-alist 'file "\\.hpp\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.H\\'"        'utf-8-unix)
(modify-coding-system-alist 'file "\\.cpp\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.C\\'"        'utf-8-unix)
(modify-coding-system-alist 'file "\\.idl\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.java\\'"     'utf-8-unix)
(modify-coding-system-alist 'file "\\.js\\'"       'utf-8-unix)
(modify-coding-system-alist 'file "\\.coffee\\'"   'utf-8-unix)
(modify-coding-system-alist 'file "\\.sh\\'"       'utf-8-unix)
(modify-coding-system-alist 'file "\\.conf\\'"     'utf-8-unix)
(modify-coding-system-alist 'file "\\.xml\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.xul\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.rdf\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.html\\'"     'utf-8-unix)
(modify-coding-system-alist 'file "\\.css\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.vm\\'"       'utf-8-unix)
(modify-coding-system-alist 'file "\\.php\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.xsd\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.dtd\\'"      'utf-8-unix)

;; Emacs recognizes which kind of end-of-line conversion to use based on the contents
;; of the file: if it sees only carriage-returns, or only carriage-return linefeed 
;; sequences, then it chooses the end-of-line conversion accordingly. You can inhibit
;; the automatic use of end-of-line conversion by setting the variable inhibit-eol-conversion
;; to non-nil. If you do that, DOS-style files will be displayed with the `^M' characters
;; visible in the buffer;
(setq inhibit-eol-conversion   nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for toggling end of line encoding
;;
;; M-x set-buffer-file-coding-system utf-8-unix (for LF)
;; M-x set-buffer-file-coding-system utf-8-mac  (for CR)
;; M-x set-buffer-file-coding-system utf-8-dos  (for CR+LF)
;;
;; With Emacs 21 and later, you can preserve the current text coding system and
;; change end-of-line conversion by running the command set-buffer-file-coding-system
;; (C-x RET f) and specifying "unix", "dos", or "mac"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unix-file ()
  "Change the current buffer to UTF-8 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

(defun dos-file ()
  "Change the current buffer to UTF-8 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

(defun mac-file ()
  "Change the current buffer to UTF-8 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))

;; kill blank line
(defun remove-double-newline-unix ()
  "Delete doubled newlines from the current buffer."
  (interactive)
  (save-excursion
    (replace-regexp "\n\n" "\n")))

(defun remove-double-newline-dos ()
  "Delete doubled newlines from the current buffer."
  (interactive)
  (save-excursion
    (replace-regexp "\r\n\r\n" "\r\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatically convert line endings to unix. This tries to preserve the coding
;; system rather than falling back to undecided-unix, instead substituting the -unix
;; variant of the -dos coding system in use:
;;
;; (defun convert-eol-to-unix ()
;;   (let ((coding-str (symbol-name buffer-file-coding-system)))
;;     (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
;;       (setq coding-str
;;	         (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
;;       (message "CODING: %s" coding-str)
;;       (set-buffer-file-coding-system (intern coding-str)) )))
;;
;; (add-hook 'find-file-hooks 'convert-eol-to-unix)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the default Mac OS X fixed width font.
;;
;; (set-default-font "-apple-monaco-medium-r-normal--12-*-*-*-*-*-fontset-monaco12")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the default fixed width font. Best used under Win32
;;
;; (set-default-font "-medium-fixedsys-medium-r-normal-*-12-90-96-96-c-80-iso8859-1")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This sets it to courier. Best used under Linux/Unix
;;
;; (set-default-font "-adobe-courier-medium-r-normal-*-12-120-75-75-m-70-iso8859-1")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This sets it to the default fixed width font. Best used under Linux/Unix
;;
;; (set-default-font "-misc-fixed-medium-r-normal-*-12-120-75-75-m-70-iso8859-1")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the elisp profile "dope"
;; M-x dope-quick-start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require cl.el for multi-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the multi-mode library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'multi-mode "multi-mode" "Allowing multiple major modes in a buffer." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous editor settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load textmade shortcut minor mode
(require 'textmate)
(textmate-mode t)


;; load the color-theme package
(require 'color-theme)

;; set custom variables
;; or use on of the many default themes available
;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-java.html
(custom-set-variables
 '(cursor-type (quote box))
 '(tool-bar-mode nil nil (tool-bar))
 '(default-frame-alist (quote (
                               (tool-bar-lines         .        0)
                               (menu-bar-lines         .        1)
                               (foreground-color       .  "wheat")
                               (background-color       .  "black")
                               (cursor-type            .      box)
                               (cursor-color           . "purple")
                               (vertical-scroll-bars   .    right)
                               (internal-border-width  .        0)
                               )
                              )
    )
 )


;; suppress startup messages
(setq inhibit-startup-echo-area-message   t)
(setq inhibit-startup-screen              t)

(setq minibuffer-max-depth          nil)
(setq mouse-yank-at-point           t  )
(setq query-replace-highlight       t  )
(setq search-highlight              t  )

;; set some limits on history lists
(setq kill-ring-max                 500)
(setq extended-command-history-max   50)
(setq query-replace-history-max      50)
(setq replace-string-history-max     50)
(setq file-name-history-max          50)
(setq replace-regex-history-max      50)
(setq minibuffer-history-max       1000)
(setq shell-command-history-max    1000)
(setq find-file-history-max        1000)

;; use the block cursor instead of the line cursor
(setq bar-cursor                    nil)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; Load uniquify to distinguish between two identically named files instead of
;; appending a <n> extension
(require 'uniquify)

(setq uniquify-buffer-name-style      'reverse)
(setq uniquify-separator              "|"     )
(setq uniquify-after-kill-buffer-p t          )
(setq uniquify-ignore-buffers-re      "^\\*"  )

;; don't allow text to exceed 80 columns before line wrapping
(setq fill-column  80)

;; disable line wrap by default. enable only through specific
;; mode hooks
(setq default-truncate-lines  nil)


(defun toggle-line-wrapping ()
  "Toggles between line wrapping in the current buffer."
  (interactive)
  (if (eq truncate-lines nil)
      (progn
        (setq truncate-lines t)
        (redraw-display)
        (message "Setting truncate-lines to t"))
    (setq truncate-lines nil)
    (redraw-display)
    (message "Setting truncate-lines to nil"))
  )

;; enable line highlighting
(require 'highlight-current-line)
(highlight-current-line-on  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode. Esp for identation settings, require CC-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the Scala mode library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'scala-mode-auto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'clojure-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp editing utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'paredit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'lisp-mode-hook 
          (lambda ()
            (paredit-mode +1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require Python mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP indentation, require PHP-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'php-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the updated mmm-library locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nXhtml mode loader (Breaks MMM mode customizations and file associations)
;;
;; (load "~/.emacs.d/nxhtml/autostart.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript is not recognized by default.  Require
;; the javacsript-mode package (for mmm-mode) and js2-mode for 
;; .js file editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'javascript-mode)
(require 'js2-mode)
(require 'json)

(autoload 'espresso-mode "espresso")

;; Minor mode on top of js2-mode which highlights all occurrences of the variable
;; under the cursor within its defining scope.  
;; When variables are highlighted, you can use the following key bindings:
;;
;;  M-n or C-<down> - move to the next occurrence
;;  M-p or C-<up> - move to the previous occurrence
;;  M-r - rename the variable; this will ask you for confirmation for each
;;  occurrence, but you can press "!" to rename all.

(require 'js2-highlight-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coffeescript mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'coffee-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Statistics Mode (ESS)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS indentation, autoload css-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'css-mode "css-mode")
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

(require 'css-mode)

;; CSS color values colored by themselves
;; http://xahlee.org/emacs/emacs_html.html
(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background 
                     (match-string-no-properties 0)))))))
 
(defun hexcolor-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))

;; add CSS colorization to relevant major/minor modes
(add-hook 'css-mode-hook    'hexcolor-add-to-font-lock)
(add-hook 'html-mode-hook   'hexcolor-add-to-font-lock)
(add-hook 'psgml-mode-hook  'hexcolor-add-to-font-lock)
(add-hook 'xml-mode-hook    'hexcolor-add-to-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load sql-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'sql-mode "sql" "Major mode for SQL"                    )
(eval-after-load "sql" '(load-library "~/.emacs.d/sql/sql-indent"))

(add-hook 'sql-mode-hook
          (function (lambda ()
                      (local-set-key "\C-cu" 'sql-to-update)))
          'turn-on-font-lock
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load matlab-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'matlab-mode "matlab" "Enter Matlab Mode." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab Mode." t)

(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches "-nodesktop -nojvm")

(add-hook 'matlab-mode-hook
          (lambda ()
            (setq matlab-indent-level 4)
            (setq fill-column 80)
            (define-key matlab-mode-map "\M-;" 'comment-dwim)))

;; indent function bodies
(setq matlab-indent-function-body    t) 
;; turn off auto-verify on save 
(setq matlab-verify-on-save-flag   nil) 

(defun my-matlab-shell-mode-hook () '())
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load snippets utility
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet-bundle)

(setq yas/root-directory "~/.emacs.d/snippets")


;; enable yasnippet for generic cc-mode
(add-hook 'cc-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for objc-mode
(add-hook 'objc-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for scala-mode
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for html-mode
(add-hook 'html-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for css-mode
(add-hook 'css-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Color Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 '(custom-mode-default ((t (:inherit autoface-default :background "black"))) t)
 '(highlight-current-line-face ((t (:background "gray10"))))
 '(linum ((t (:inherit (shadow default) :background "gray10" :foreground "dimgray"))))
 '(font-lock-builtin-face       ((((class color) (min-colors 88) (background light)) (:foreground "lightsteelblue"   ))))
 '(font-lock-comment-face       ((((class color) (min-colors 88) (background light)) (:foreground "orangered"        ))))
 '(font-lock-doc-face           ((((class color) (min-colors 88) (background light)) (:foreground "orangered"        ))))
 '(font-lock-constant-face      ((((class color) (min-colors 88) (background light)) (:foreground "aquamarine"       ))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "lightblue"        ))))
 '(font-lock-keyword-face       ((((class color) (min-colors 88) (background light)) (:foreground "turquoise"        ))))
 '(font-lock-string-face        ((((class color) (min-colors 88) (background light)) (:foreground "tan"              ))))
 '(font-lock-type-face          ((((class color) (min-colors 88) (background light)) (:foreground "palegreen"        ))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "khaki"            ))))
 '(font-lock-warning-face       ((((class color) (min-colors 88) (background light)) (:foreground "pink" :weight bold))))
 '(text-mode-default ((t 
                       (:inherit autoface-default
                        :background    "black"
                        :foreground    "wheat"
                        :strike-through nil
                        :underline      nil
                        :slant          normal
                        :weight         normal
                        :height         120
                        :width          normal
                        :family        "monaco"))) t)
 )

(defun my-common-font-lock()
  "font-lock settings"
  (setq-default
   font-lock-auto-fontify           t
   font-lock-use-fonts              nil
   font-lock-use-maximal-decoration t
   font-lock-use-colors             t
   font-lock-mode-enable-list       nil
   font-lock-mode-disable-list      nil
   )
  ;; set-font-lock color scheme
  (set-face-foreground 'font-lock-builtin-face         "lightsteelblue")
  (set-face-foreground 'font-lock-comment-face         "orangered"     )
  (set-face-foreground 'font-lock-doc-face             "orangered"     )
  (set-face-foreground 'font-lock-constant-face        "aquamarine"    )
  (set-face-foreground 'font-lock-function-name-face   "lightblue"     )
  (set-face-foreground 'font-lock-keyword-face         "turquoise"     )
  (set-face-foreground 'font-lock-string-face          "tan"           )
  (set-face-foreground 'font-lock-type-face            "palegreen"     )
  (set-face-foreground 'font-lock-variable-name-face   "khaki"         )
  (set-face-foreground 'font-lock-warning-face         "pink"          )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global font lock and color declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode      t)
       (my-common-font-lock         )
       ;; default color scheme
       (set-background-color             "black"    )
       (set-foreground-color             "wheat"    )
       (set-cursor-color                 "purple"   )
       (set-face-foreground 'modeline    "firebrick")
       (set-face-background 'modeline    "wheat"    )
       
       ;; Maximum colors
       (setq font-lock-maximum-decoration
             '((java-mode       . t)
               (c++-mode        . t)
               (c-mode          . t)
               (objc-mode       . t)
               (javascript-mode . t)
               (js2-mode        . t)
               (coffee-mode     . t)
               (css-mode        . t)
               (scala-mode      . t)
               (clojure-mode    . t)
               (python-mode     . t)
               (sql-mode        . t)
               (R-mode          . t)
               (emacs-lisp-mode . t)
               (t               . 1))
             ))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the max buffer size (in bytes) before fontification is suppressed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default font-lock-maximum-size 5000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default mode to text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; make selected region visible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-mark-mode          1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; turns on word-wrapping in the current buffer, and rebinds C-a, C-e,
;; and C-k to commands that operate by visual lines instead of logical
;; lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
(global-visual-line-mode      1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; underline highlighted region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-underline-p 'region t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move (1) line when at bottom of screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-step             1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace highlighted text with keystroke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode        t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatic newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-auto-newline          t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-indentation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-toggle-auto-state     1)   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hungry delete key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-toggle-hungry-state   1)   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq comment-column         60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent in blocks of 4 spaces for all c-mode and c-mode derivative submodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default c-basic-offset     4)
(setq-default tab-width          4)
(setq espresso-indent-level      4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove tabs from the idents/use spaces instead of tab characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show approximate buffer size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(size-indication-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show time on the bar 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on line/column number modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fringe-mode          0)

(require 'linum)

(line-number-mode     1)
(column-number-mode   t)

;; create toggle functions
(defun turn-linum-on ()
  "Turn line-number column ON. " 
  (interactive) 
  (unless (minibufferp)
    (linum-mode 1)))

(defun turn-linum-off ()
  "Turn line-number column OFF. " 
  (interactive) 
  (unless (minibufferp)
    (linum-mode 1)))

;; enable global linum-mode if not already toggled on
(if (equal linum-mode nil)
    (global-linum-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure whitespace-mode
;; http://www.emacswiki.org/emacs/WhiteSpace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)

(define-key global-map "\C-c_w" 'whitespace-mode)
(define-key global-map "\C-c_t" 'whitespace-toggle-options)
(define-key global-map "\C-c=w" 'global-whitespace-mode)
(define-key global-map "\C-c=t" 'global-whitespace-toggle-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to matching parenthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert formatted date/timestamp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%d.%m.%Y %H:%M")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Programming constants
;;
;; Note - default code identation styles
;; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linux, python, java
;;
;; auto identation constant definition
;; explicit declaration of all syntactic attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-java-programming-style
  ;; hanging brace setup
  '((c-hanging-braces-alist .             
                            ((brace-list-open                  after)
                             (brace-entry-open                 after)
                             (substatement-open               before)
                             (block-close          . c-snug-do-while)
                             (extern-lang-open                 after)
                             (inexpr-class-open                after)
                             (inexpr-class-close              before)
                             ))
    ;; cleanup shortcuts
    (c-cleanup-list                .      
                                   ((brace-else-brace               )
                                    (brace-elseif-brace             )
                                    (brace-catch-brace              )
                                    (list-close-comma               )
                                    ))
    ;; indentation offsets 
    (c-offsets-alist               .      
                                   ((access-label                . 0)
                                    (inline-open                 . 0)
                                    (substatement-open           . 0)
                                    (statement-block-intro       . +)
                                    (block-close                 . 0)
                                    (do-while-closure            . 0)
                                    (case-label                  . *)
                                    (statement-case-intro        . +)
                                    ))
    (c-lineup-math                   1)
    (c-lineup-inexpr-block           1)

    ) "My Java Programming Style")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; C/C++ mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-c-cpp-programming-style
  ;; hanging brace setup
  '((c-hanging-braces-alist .             
                            ((brace-list-open                  after)
                             (brace-entry-open                 after)
                             (substatement-open               before)
                             (block-close          . c-snug-do-while)
                             (extern-lang-open                 after)
                             (inexpr-class-open                after)
                             (inexpr-class-close              before)
                             ))
    ;; cleanup shortcuts
    (c-cleanup-list                .      
                                   ((brace-else-brace               )
                                    (brace-elseif-brace             )
                                    (brace-catch-brace              )
                                    (list-close-comma               )
                                    ))
    ;; indentation offsets 
    (c-offsets-alist               .      
                                   ((access-label                . -2)
                                    (inline-open                 .  0)
                                    (substatement-open           .  0)
                                    (statement-block-intro       .  +)
                                    (block-close                 .  0)
                                    (do-while-closure            .  0)
                                    (case-label                  .  *)
                                    (statement-case-intro        .  +)
                                    ))
    (c-lineup-math                   1)
    (c-lineup-inexpr-block           1)
    
    ) "My C/C++ Programming Style")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global font lock settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs 21.X and earlier use lazy-lock package for fontification
;;
;; (require 'lazy-lock)
;; (setq font-lock-support-mode       'lazy-lock-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs 22.X and later use the new jit-lock package for fontification
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'jit-lock)
(setq font-lock-support-mode       'jit-lock-mode)

(setq lazy-lock-defer-on-scrolling  nil           )
(setq lazy-lock-defer-time          1             )
(setq lazy-lock-stealth-time        20            )
(setq lazy-lock-stealth-lines       25            )
(setq lazy-lock-stealth-verbose     nil           )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set JS2 mode indentation modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With regular JS2-mode, pressing TAB at the beginning of a line won't move the 
;; caret to the first non-space character if the line is already properly. This
;; indentation function fixes that. 

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 0 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

;; With c-mode there is a key binding M-C-q which indents the block starting with 
;; the paren under the cursor.  The following function implements this generically
;; and can be used in js2-mode:

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

;; Unlike c-indent-exp, my-indent-sexp above does not require the cursor to be over
;; a paren.  It looks up the innermost block using (syntax-ppss) which is a neat function
;; provided by Emacs, and reindents it.  It also highlights the block for half a second.

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq indent-tabs-mode nil)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)] 
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "Custom JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set javascript-mode MMM submode indentation function override
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; javascript-mode indentation breaks under Emacs 22 and MMM
;; This is a custom indentation function for exclusive use with
;; javascript-mode in MMM

(defun my-javascript-mmm-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))
        )

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))


(defun my-javascript-mmm-mode-hook ()
  (require 'espresso)
  (setq indent-tabs-mode nil)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-javascript-mmm-indent-function)
  (message "Custom Javascript MMM-mode hook"))

(add-hook 'javascript-mode-hook 'my-javascript-mmm-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mozilla/NSPR specific extra data types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-font-lock-extra-types
      (append
       '("nsresult"
         "PRStatus"  
         "PRBool" 
         "PRPackedBool" 
         "PRFloat64" 
         "PRIntn"
         "PRUWord"
         "PRInt8" 
         "PRInt16" 
         "PRInt32" 
         "PRInt64"
         "PRUint"
         "PRUint8" 
         "PRUint16" 
         "PRUnichar" 
         "PRUint32" 
         "PRUint64"
         "PLOptStatus"
         "PLOptState"
         "PRThread" 
         "PRThreadType" 
         "PRThreadScope" 
         "PRThreadState" 
         "PRThreadPriority" 
         "PRLock" 
         "PRCondVar" 
         "PRMonitor"
         "PRRWLock"
         "PRDir" 
         "PRPollDesc"
         "PRFileDesc" 
         "PRIOMethods" 
         "PRFilePrivate" 
         "PRDescIdentity" 
         "PRFileInfo" 
         "PRFileInfo64" 
         "PRFileType" 
         "PRNetAddr" 
         "PRIPv6Addr" 
         "PRSocketOptionData" 
         "PRSockOption" 
         "PRLinger" 
         "PRMcastRequest"
         "PRFileMap" 
         "PRHostEnt" 
         "PRProtoEnt" 
         "PRIntervalTime" 
         "PRTime" 
         "PRTimeParameters" 
         "PRExplodedTime" 
         "prbitmap_t" 
         "PRCList" 
         "PRLibrary" 
         "PRStaticLinkTable" 
         "PRProcess" 
         "PRProcessAttr" 
         "PRSysInfo" 
         "PRCounterHandle" 
         "PRJobIoDesc" 
         "PRJobFn" 
         "PRThreadPool" 
         "PRJob" 
         "PLHashEntry" 
         "PLHashTable" 
         "PLHashNumber" 
         "PLHashFunction" 
         "PLHashComparator" 
         "PLHashEnumerator" 
         "PLHashAllocOps" 
         "PRCallOnceType"
         )       
       c-font-lock-extra-types))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Win32 specific extra data types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-font-lock-extra-types
      (append
       '("LPC?\\(W\\|T\\|OLE\\)?\\STR" "HRESULT"
         "BOOL" 
         "BYTE" 
         "DWORD" 
         "SOCKET" 
         "idl_char"
         "idl_boolean" 
         "idl_byte" 
         "idl_\\(short\\|long\\)_float"
         "idl_u?\\(small\\|short\\|long\\)_int"
         "boolean32" 
         "unsigned\\(32\\|16\\)"
         "SAFEARRAY" 
         "boolean" 
         "UINT" 
         "ULONG" 
         "VARIANT"
         )  
       c-font-lock-extra-types))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set C++ mode font lock extra types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c++-font-lock-extra-types
      (append
       c-font-lock-extra-types
       c++-font-lock-extra-types
       )
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set C++ mode font lock extra types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'lisp-mode-hook       'turn-on-font-lock)
(add-hook 'cperl-mode-hook      'turn-on-font-lock)
(add-hook 'c-mode-hook          'turn-on-font-lock)
(add-hook 'objc-mode-hook       'turn-on-font-lock)
(add-hook 'c++-mode-hook        'turn-on-font-lock)
(add-hook 'java-mode-hook       'turn-on-font-lock)
(add-hook 'javascript-mode-hook 'turn-on-font-lock)
(add-hook 'php-mode-user-hook   'turn-on-font-lock)
(add-hook 'scala-mode-user-hook 'turn-on-font-lock)
(add-hook 'clojure-mode-hook    'turn-on-font-lock)
(add-hook 'js2-mode-user-hook   'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up a major multi mode group for fancy markup editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mmm-add-group
 'html-php
 '(
   (html-php-tagged
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")
   )
 )


(mmm-add-group
 'xul-js
 '(
   (js-methodbody-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<body[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</body>"
    :insert ((?j js-tag nil @ "<method type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</method>" @))
    )
   (js-methodbody
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<body[^>]*>[ \t]*\n?"
    :back "[ \t]*</body>"
    :insert ((?j js-tag nil @ "<method type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</method>" @))
    )
   (js-script-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-script
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-getter-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<getter[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</getter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-setter-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<setter[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</setter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-getter
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<getter[^>]*>[ \t]*\n?"
    :back "[ \t]*</getter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-setter
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<setter[^>]*>[ \t]*\n?"
    :back "[ \t]*</setter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-cdata-only
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "on\w+=\""
    :back "\"")
   )
 )



(mmm-add-group 
 'html-jsp
 '(
   (html-jsp
    :submode java-mode
    :match-face (("<%!" . mmm-declaration-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[!=]?"
    :back "%>"
    :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
             (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
             (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (jsp-directive
    :submode text-mode
    :face mmm-special-submode-face
    :front "<%@"
    :back "%>"
    :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
    )
   )
 )


(mmm-add-group
 'html-js
 '(
   (js-script
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-script-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-cdata-only
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "on\w+=\""
    :back "\""
    )
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @))
    )
   )
 )


(mmm-add-group
 'html-css
 '(
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @))
    )
   )
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What features should be turned on in this multi-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mmm-add-mode-ext-class 'xml-mode  "\\.xul\\'"       'xul-js   )
(mmm-add-mode-ext-class 'xml-mode  "\\.xml\\'"       'xul-js   )
(mmm-add-mode-ext-class 'xml-mode  "\\.xbl\\'"       'xul-js   )
(mmm-add-mode-ext-class 'sgml-mode "\\.php[34]?\\'"  'html-php )
(mmm-add-mode-ext-class 'sgml-mode "\\.jsp\\'"       'html-jsp )
(mmm-add-mode-ext-class 'sgml-mode "\\.html\\'"      'html-js  )
(mmm-add-mode-ext-class 'sgml-mode "\\.vm\\'"        'html-js  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What features should be turned on in this multi-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'mmm-mode-ext-classes-alist '(xml-mode   nil   xul-js   ) )
(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode  nil   html-php ) )
(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode  nil   html-jsp ) )
(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode  nil   html-js  ) )
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode  nil   html-js  ) )
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode  nil   html-css ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use psgml for sgml and xml mode editing
;;
;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode  "psgml" "Major mode to edit XML files."  t)

(setq sgml-validate-command                "nsgmls -s %s %s"
      sgml-catalog-files                   (list "catalog" "CATALOG" "~/.emacs.d/dtd/catalog")
      sgml-trace-entity-lookup             t
      sgml-set-face                        t
      sgml-auto-insert-required-elements   t
      sgml-auto-activate-dtd               t
      sgml-indent-data                     t
      sgml-indent-step                     2
      sgml-live-element-indicator          nil
      sgml-list-attributes                 nil
      sgml-insert-missing-element-comment  nil
      sgml-warn-about-undefined-entities   nil
      sgml-warn-about-undefined-elements   nil
      sgml-trace-entity-lookup             nil
      )


(autoload 'xxml-mode-routine "xxml")
(add-hook 'sgml-mode-hook 'xxml-mode-routine)
(add-hook 'xml-mode-hook  'xxml-mode-routine)


;; Disable auto fill mode for all markup major/minor modes
(add-hook 'html-mode-hook 
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'psgml-mode-hook 
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'sgml-mode-hook 
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'xml-mode-hook 
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start DTD mode for editing SGML-DTDs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'dtd-mode  "tdtd" "Major mode for SGML and XML DTDs."             )
(autoload 'dtd-etags "tdtd" "Execute etags on FILESPEC"                    t)
(autoload 'dtd-grep  "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

(add-hook 'dtd-mode-hooks 'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close XML tag function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-close-tag ()
  "Close the previously defined XML tag"
  (interactive)
  (let ((tag nil)
        (quote nil))
    (save-excursion
      (do ((skip 1))
          ((= 0 skip))
        (re-search-backward "</?[a-zA-Z0-9_-]+")
        (cond ((looking-at "</")
               (setq skip (+ skip 1)))
              ((not (looking-at "<[a-zA-Z0-9_-]+[^>]*?/>"))
               (setq skip (- skip 1)))))
      (when (looking-at "<\\([a-zA-Z0-9_-]+\\)")
        (setq tag (match-string 1)))
      (if (eq (get-text-property (point) 'face)
              'font-lock-string-face)
          (setq quote t)))
    (when tag
      (setq quote (and quote
                       (not (eq (get-text-property (- (point) 1) 'face)
                                'font-lock-string-face))))
      (if quote
          (insert "\""))
      (insert "</" tag ">")
      (if quote
          (insert "\""))
      )
    )
  )

;; To close a tag, press "C-c /".  It will look for the closest opened tag and
;; it will close it.  The above function is not bullet-proof. It just counts
;; closing tags and discards as many open tags, before finding the one that you
;; need to close.  This means, if you have: "<foo><bar></foo>|" and press "C-c /",
;; it will still enter "</foo>" although your XML is obviously invalid.

(define-key global-map [(control c) (/)] 'xml-close-tag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language Specific Hooks
;; new commands/bindings
;; C-C C-C compile in C++ and C modes
;; C-f12 compile in C++ and C modes
;; C-f11 gdb in C++ and C modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist 
      (append '(("\\.c$"        . c-mode            )
                ("\\.m$"        . objc-mode         )
                ("\\.matlab$"   . matlab-mode       )
                ("\\.h$"        . c++-mode          )
                ("\\.hpp$"      . c++-mode          )
                ("\\.H$"        . c++-mode          )
                ("\\.cpp$"      . c++-mode          )
                ("\\.C$"        . c++-mode          )
                ("\\.cxx$"      . c++-mode          )
                ("\\.idl$"      . c++-mode          )
                ("\\.css\\'"    . css-mode          )
                ("\\.js$"       . js2-mode          )
                ("\\.json$"     . js2-mode          )
                ("\\.coffee$"   . coffee-mode       )
                ("Cakefile"     . coffee-mode       )
                ("\\.java$"     . java-mode         )
                ("\\.scala$"    . scala-mode        )
                ("\\.clj$"      . clojure-mode      )
                ("\\.sql$"      . sql-mode          )
                ("\\.epl$"      . sql-mode          )
                ("\\.tcl$"      . tcl-mode          )
                ("\\.csh$"      . csh-mode          )
                ("\\.sh$"       . sh-mode           )
                ("\\.bash$"     . sh-mode           )
                ("\\.bashrc$"   . sh-mode           )
                ("\\.profile$"  . sh-mode           )
                ("\\.py$"       . python-mode       )
                ("\\.pyx$"      . python-mode       )
                ("\\.R$"        . R-mode            )
                ("\\.r$"        . R-mode            )
                ("\\.html$"     . sgml-mode         )
                ("\\.htm$"      . sgml-mode         )
                ("\\.vm$"       . sgml-mode         )
                ("\\.st$"       . sgml-mode         )
                ("\\.jsp$"      . sgml-mode         )
                ("\\.php$"      . php-mode          )
                ("\\.phtml$"    . sgml-mode         )
                ("\\.incl$"     . sgml-mode         )
                ("\\.phps$"     . sgml-mode         )
                ("\\.rng$"      . xml-mode          )
                ("\\.xml$"      . xml-mode          )
                ("\\.xul$"      . xml-mode          )
                ("\\.xbl$"      . xml-mode          )
                ("\\.xhtml$"    . xml-mode          )
                ("\\.rdf$"      . xml-mode          )
                ("\\.xsd$"      . xml-mode          )
                ("\\.xsl$"      . xml-mode          )
                ("\\.dcl$"      . dtd-mode          )
                ("\\.dec$"      . dtd-mode          )
                ("\\.dtd$"      . dtd-mode          )
                ("\\.ele$"      . dtd-mode          )
                ("\\.ent$"      . dtd-mode          )
                ("\\.mod$"      . dtd-mode          )
                ("\\.conf$"     . conf-mode         )
                ("\\.emacs$"    . lisp-mode         )
                ("\\.el$"       . lisp-mode         )
                ("\\.lisp$"     . lisp-mode         )
                ("Makefile$"    . makefile-mode     )
                ("\\.bib$"      . bibtex-mode       )
                ("\\.tex$"      . TeX-mode          )
                ("\\.txi$"      . Texinfo-mode      )
                ("\\.markdown$" . markdown-mode     )
                ("\\.mdown$"    . markdown-mode     )
                ("\\.mdwn$"     . markdown-mode     )
                ("\\.md$"       . markdown-mode     )
                ("\\.txt$"      . text-mode         )
                ("\\.README$"   . text-mode         )
                (".*READ\\.ME$" . text-mode         )
                ("\\.doc$"      . text-mode         ) )
              auto-mode-alist))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ mode hook section, called on entry of C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c++-mode-hook
          '(lambda ()
             (local-set-key (quote [C-f12]) (quote compile))
             (local-set-key "" (quote compile))
             (local-set-key (quote [C-f11]) (quote gdb))
             (c-add-style "my-c-cpp-programming-style" my-c-cpp-programming-style t)
             (c-set-style "my-c-cpp-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C hook section, called on entry of C mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook
          '(lambda ()
             (auto-fill-mode                         1)    
             (setq fill-column                      80)
             (local-set-key (quote [C-f12]) (quote compile))
             (local-set-key "" (quote compile))
             (local-set-key (quote [C-f11]) (quote gdb))
             (c-add-style "my-c-cpp-programming-style" my-c-cpp-programming-style t)
             (c-set-style "my-c-cpp-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objective-C hook section, called on entry of C mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'objc-mode-hook
          '(lambda ()
             (auto-fill-mode                         1)    
             (setq fill-column                      80)
             (local-set-key (quote [C-f12]) (quote compile))
             (local-set-key "" (quote compile))
             (local-set-key (quote [C-f11]) (quote gdb))
             (c-add-style "my-c-cpp-programming-style" my-c-cpp-programming-style t)
             (c-set-style "my-c-cpp-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java hook section, called on entry of Java mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Java mode hack to get a little better Java 5+ support
(let* ((java-keywords
  (eval-when-compile
    (regexp-opt
     '("catch" "do" "else" "super" "this" "finally" "for" "if"
       "implements" "extends" "throws" "instanceof" "new"
       "interface" "return" "switch" "throw" "try" "while"))))
       ;; Classes immediately followed by an object name.
       (java-type-names
        `(mapconcat 'identity
                    (cons
                     ,(eval-when-compile
                        (regexp-opt '("boolean" "char" "byte" "short" "int" "long"
                                      "float" "double" "void")))
                     java-font-lock-extra-types)
                    "\\|"))
       (java-type-names-depth `(regexp-opt-depth ,java-type-names))
       ;; These are eventually followed by an object name.
       (java-type-specs
        (eval-when-compile
          (regexp-opt
           '("abstract" "const" "final" "synchronized" "transient" "static"
             "volatile" "public" "private" "protected" "native"
             ;; Carl Manning <caroma@xxxxxxxxxx> says this is new.
             "strictfp"))))
       )

  (setq java-font-lock-keywords-3
        (append
         
         (list
          ;; support static import statements
          '("\\<\\(import\\)\\>\\s-+\\(static\\)\\s-+\\(\\sw+\\)"
            (1 font-lock-keyword-face)
            (2 font-lock-keyword-face)
            (3 (if (equal (char-after (match-end 0)) ?\.)
                   'jde-java-font-lock-package-face
                 'font-lock-type-face))
            ("\\=\\.\\(\\sw+\\)" nil nil
             (1 (if (and (equal (char-after (match-end 0)) ?\.)
                         (not (equal (char-after (+ (match-end 0) 1)) ?\*)))
                    'jde-java-font-lock-package-face
                  'font-lock-type-face))))
          )
         
         java-font-lock-keywords-2
         
         ;; More complicated regexps for more complete highlighting for types.
         ;; We still have to fontify type specifiers individually, as Java is hairy.
         (list
          ;; Fontify class names with ellipses
          `(eval .
                 (cons (concat "\\<\\(" ,java-type-names "\\)\\>\\.\\.\\.[^.]")
                       '(1 font-lock-type-face)))
          ;; Fontify random types immediately followed by an item or items.
          `(eval .
                 (list (concat "\\<\\(\\(?:" ,java-type-names "\\)"
                               "\\(?:\\(?:<.*>\\)\\|\\>\\)\\(?:\\.\\.\\.\\)?\\)"
                               "\\([ \t]*\\[[ \t]*\\]\\)*"
                               "\\([ \t]*\\sw\\)")
                       ;; Fontify each declaration item.
                       (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                             ;; Start and finish with point after the type specifier.
                             (list 'goto-char (list 'match-beginning
                                                    (+ ,java-type-names-depth 3)))
                             (list 'goto-char (list 'match-beginning
                                                    (+ ,java-type-names-depth 3)))
                             ;; Fontify as a variable or function name.
                             '(1 (if (match-beginning 2)
                                     font-lock-function-name-face
                                   font-lock-variable-name-face)))))
          ;; Fontify those that are eventually followed by an item or items.
          (list (concat "\\<\\(" java-type-specs "\\)\\>"
                        "\\([ \t]+\\sw+\\>"
                        "\\([ \t]*\\[[ \t]*\\]\\)*"
                        "\\)*")
                ;; Fontify each declaration item.
                '(font-lock-match-c-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  (goto-char (or (match-beginning 5) (match-end 1)))
                  ;; Finish with point after first type specifier.
                  (goto-char (match-end 1))
                  ;; Fontify as a variable or function name.
                  (1 (if (match-beginning 2)
                         font-lock-function-name-face
                       font-lock-variable-name-face))))
          
          )))
  )


(add-hook 'java-mode-hook
          '(lambda ()
             (auto-fill-mode                         1)    
             (setq fill-column                      80)
             (c-add-style "my-java-programming-style" my-java-programming-style t)
             (c-set-style "my-java-programming-style")
             )
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala hook section, called on entry of Scala mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'scala-mode-hook
          '(lambda ()
             (auto-fill-mode                         1)    
             (setq fill-column                      80)
             (c-add-style "my-scala-programming-style" my-java-programming-style t)
             (c-set-style "my-scala-programming-style")
             )
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript hook section, called on entry of Javascript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'javascript-mode-hook
          '(lambda ()
             (c-add-style "my-javascript-programming-style" my-java-programming-style t)
             (c-set-style "my-javascript-programming-style")
             )
          )


(add-hook 'js2-mode-hook
          '(lambda ()
             (c-add-style "my-javascript-programming-style" my-java-programming-style t)
             (c-set-style "my-javascript-programming-style")
             )
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoffeeScript hook section, called on entry of CoffeeScript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(defun coffee-custom ()
  "coffee-mode-hook"

  ;; CoffeeScript uses two spaces.
  (set (make-local-variable 'tab-width) 2)

  ;; Use JS2 mode for viewing compiled coffeescript
  (setq coffee-js-mode 'js2-mode)

  ;; If you don't want your compiled files to be wrapped
  (setq coffee-args-compile '("-c" "--bare"))

  ;; *Messages* spam
  (setq coffee-debug-mode t)

  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

  ;; Riding edge.
  (setq coffee-command "~/dev/coffee"))

  ;; Compile '.coffee' files on every save
  (add-hook 'after-save-hook
      '(lambda ()
         (when (string-match "\.coffee$" (buffer-name))
          (coffee-compile-file))))


(add-hook 'coffee-mode-hook
  '(lambda() 
     (coffee-custom)
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode hook section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook 
          '(lambda ()
             (auto-fill-mode                         1)    
             (setq fill-column                      80)
             (setq tab-width                         4)
             (local-set-key "^C-^t" 'ispell-complete-word)
             )
          )


(add-hook 'sh-mode-hook
          '(lambda ()
             (auto-fill-mode                         1)    
             (setq fill-column                      80)
             (local-set-key [return] 'newline-and-indent)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERL mode hook section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl"     . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5"    . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
          
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Usage: M-x dos2unix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS eolformat to UNIX eol format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: M-x cut-ctrlM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cut-ctrlM () 
  "Eliminate all visible ^M characters. " 
  (interactive) 
  (save-excursion
    (goto-char (point-min)) 
    (while (search-forward "\r" nil t)
      (replace-match "" nil t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean out buffers except shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restart ()
  "Clean out all editing buffers except for shell."
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
             (name (buffer-name buffer)))
        (and (not (string-equal name "*shell*"))
             (kill-buffer buffer)))
      (setq list (cdr list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trim trailing whitespace from the buffer before going to EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trim-end-of-line () 
  "Trim trailing whitespace before going to EOL." 
  (interactive)
  (if buffer-read-only
      (end-of-line)                 ; can't modify it, do it normally 
    (progn
      (trim-trailing-spaces)
      (end-of-line))                    ; end of trimmed line
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts to project directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goto-homedir ()
  "Switch to project directory."
  (interactive)
  (cd "~/"))

(defun goto-projects ()
  "Switch to project directory."
  (interactive)
  (cd "~/Projects"))

(defun goto-aftertext ()
  "Switch to Squarespace project directory."
  (interactive)
  (cd "~/Projects/AfterText"))


;; You can also use CTRL-x CTRL-v to do a “Find alternative file” and choose the same
;; file that you are currently editing.

(defun refresh-file ()
  "Reload the file you're currently editing."
  (interactive)
  (revert-buffer t t t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [f2]               'reload-dot-emacs            )
(define-key global-map [f3]               'edit-dot-emacs              )
(define-key global-map [f4]               'cut-ctrlM                   )
(define-key global-map [f5]               'save-buffer                 )

(define-key global-map [f6]               'replace-string              )
(define-key global-map [f7]               'query-replace               )
(define-key global-map [f8]               'query-replace-regexp        )
(define-key global-map [f9]               'refresh-file                )
(define-key global-map [f12]              'goto-line                   ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electric Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "\C-x\C-b"         'electric-buffer-list )
(define-key global-map "\C-xb"            'bs-show              )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment out a region.  To uncomment, just undo "\C-u"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "\C-q"             'comment-region       )
(define-key global-map "\C-u"             'undo                 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is some Emacs Lisp that will make the % key show the matching
;; parenthesis, like in vi.  In addition, if the cursor isn't over a
;; parenthesis, it simply inserts a % like normal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "%"                'match-paren         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Home End and Delete keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(control home)]   'beginning-of-line   )
(define-key global-map [(control end)]    'end-of-line         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings to automatically paste copyright notice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;x

(define-key global-map [(alt f1)]         'paste-atsl-copyright            )
(define-key global-map [(alt f2)]         'paste-atsl-mpl-copyright-html   )
(define-key global-map [(alt f3)]         'paste-atsl-mpl-copyright        )
(define-key global-map [(alt f4)]         'paste-mpl-copyright             )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proprietary copyright information to be used in 
;; non open-sourced code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-atsl-copyright ()
  "Insert AfterText Proprietary Copyright Text. "
  (interactive)
  (insert 
"/*
 * Version 1.0 ATSL
 *
 * THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AFTERTEXT, INC.
 * The Computer Code and Software contained herein is the sole property
 * of AfterText, Inc  (\"AfterText\").  The copyright notice above does
 * not evidence any actual or intended publication of such source code.
 *
 * Use and distribution of this software and its source code is governed
 * by the terms and conditions of the AfterText Software License
 * (\"ATSL\")
 *
 * The Initial Developer of the Original Code is AfterText, Inc. Portions
 * created by AfterText are Copyright (C) 2010 AfterText, Inc.  All Rights 
 * Reserved.
 */
")
  (cut-ctrlM))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATSL copyright information to be used in open-sourced 
;; code under the terms of the MPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-atsl-mpl-copyright-html ()
  "Insert AfterText Copyright Text. "
  (interactive)
  (insert 
"<!-- Version 1.1 MPL
   -   
   - The contents of this file are subject to the Mozilla Public 
   - License Version 1.1 (the \"License\").  You may not use this 
   - file except in compliance with the License. You may obtain a 
   - copy of the License at http://www.mozilla.org/MPL/ 
   -
   - Software distributed under the License is distributed on an 
   - \"AS IS\" basis, WITHOUT WARRANTY OF ANY KIND, either express  
   - or implied. See the License for the specific language governing 
   - rights and limitations under the License.
   -
   - The Original Code is AfterText.
   - 
   - The Initial Developer of the Original Code is AfterText, Inc. 
   - Portions created by AfterText, Inc are Copyright (C) 2010 
   - AfterText, Inc.  All Rights Reserved.
   -  
   - Redistribution of the Original Code or portions of the Original
   - Code or software under this agreement must retain the above 
   - copyright notice, this redistribution statement and the following 
   - disclaimer.  Neither the name of AfterText, Inc nor the 
   - names of its contributors may be used to endorse or promote any
   - products derived from this software without specific prior 
   - written permission from AfterText, Inc.
   - 
   -->
")
  (cut-ctrlM))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright information to be used in open-sourced 
;; code under the terms of the MPL 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-atsl-mpl-copyright ()
  "Insert AfterText MPL Copyright Text. "
  (interactive)
  (insert 
"/*
 * Version 1.1 MPL
 *
 * The contents of this file are subject to the Mozilla Public 
 * License Version 1.1 (the \"License\").  You may not use this 
 * file except in compliance with the License. You may obtain a 
 * copy of the License at http://www.mozilla.org/MPL/ 
 *
 * Software distributed under the License is distributed on an 
 * \"AS IS\" basis, WITHOUT WARRANTY OF ANY KIND, either express  
 * or implied. See the License for the specific language governing 
 * rights and limitations under the License.
 *
 * The Initial Developer of the Original Code is Aftertext, Inc. 
 * Portions created by AfterText, Inc are Copyright (C) 2010 
 * AfterText, Inc.  All Rights Reserved.  
 *
 * Redistribution of the Original Code or portions of the Original
 * Code or Software under this agreement must retain the above 
 * copyright notice, this redistribution statement and the following 
 * disclaimer.  Neither the name of AfterText, Inc nor the names
 * of its contributors may be used to endorse or promote any
 * products derived from this software without specific prior 
 * written permission from AfterText, Inc.
 *
 * Contributor(s): 
 *   Paolo de Dios <paolodedios@aftertext.com> (original author)
 *
 */
")
  (cut-ctrlM))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright information to be used in open-sourced 
;; code under the terms of the MPL 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun paste-mpl-copyright ()
  "Insert MPL Copyright Text. "
  (interactive)
  (insert 
"/*
 * Version 1.1 MPL
 *
 * The contents of this file are subject to the Mozilla Public 
 * License Version 1.1 (the \"License\").  You may not use this 
 * file except in compliance with the License. You may obtain a 
 * copy of the License at http://www.mozilla.org/MPL/ 
 *
 * Software distributed under the License is distributed on an 
 * \"AS IS\" basis, WITHOUT WARRANTY OF ANY KIND, either express  
 * or implied. See the License for the specific language governing 
 * rights and limitations under the License.
 *
 * The Initial Developer of the Original Code is Paolo de Dios. 
 * Portions Copyright (C) 2010 Paolo de Dios.  All Rights Reserved.  
 *
 * Redistribution of the Original Code or portions of the Original
 * Code or Software under this agreement must retain the above 
 * copyright notice, this redistribution statement and the following 
 * disclaimer.
 *
 * Contributor(s): 
 *   Paolo de Dios <paolodedios@gmail.com> (original author)
 *
 */
")
  (cut-ctrlM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "A-P")
    '(lambda () (interactive) (message "noop"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable email bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")