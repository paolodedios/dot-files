;; -*- mode: emacs-lisp -*-
;;
;; Global key bindings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Keyboard Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notation
;;     C-x             means press the control key with for example the x key
;;     M-x             means press the alt/option key with for example the x key,
;;                     or press the escape key and then x.
;;
;; Buffers
;;     C-x b           changes to a different buffer
;;     C-x k           kills the current buffer or type in the name of a buffer
;;                     that you want killed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A window is a division of the current window
;;
;; To create a new window from the current window
;;     C-x 2           horizontally splits the window
;;     C-x 3           vertically splits the window
;;
;; To destroy the window where the current point is
;;     C-x 0
;;
;; To destroy all windows EXCEPT where the current point is
;;     C-x 1
;;
;; To switch between windows
;;     C-x o (Think of the command as (o)ther window)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frames are generally what most operating systems consider as windows. In
;; general frame operation commands start with a "5"
;;
;; To switch to a buffer by opening it in a new frame
;;     C-x 5 b
;;
;; To close the current frame
;;     C-x 5 0
;;
;; To open a file in a new frame
;;     C-x 5 f
;;
;; To switch between frames
;;     C-x 5 o
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving About
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To move about the current buffer in emacs use the cursor keys.  In emacs
;; the cursor is called the "point"
;;
;;     C-p             previous line
;;     C-n             next line
;;     C-b             back one character
;;     M-b             back one word
;;     C-f             forward one character
;;     M-f             forward one word
;;     C-home          beginning of line
;;     C-end           end of line
;;     C-a             beginning of line
;;     C-e             end of line
;;     M-<             beginning of buffer
;;     M->             end of buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     C-s             locate (search within file)
;;     C-r             search backwards
;;     C-_             undo
;;     C-d             delete the character after point (delete-char).
;;     M-d             kill forward to the end of the next word (kill-word).
;;     M-BACKSPACE     kill back to the beginning of the previous word (backward-kill-word).
;;     C-k             kill line from point and place on kill buffer (kill-line)
;;     C-S-BACKSPACE   kill line from beginning to end (kill-whole-line)
;;     C-y             yank from kill buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to a specific line number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     M-g M-g         Type in the line number and press return.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selecting and Copying Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Selecting text involves using the emacs "mark".  The "mark" is an invisible
;; location that you set in the text.  In conjunction with the "point" it defines
;; a region or "selection" of text.  It can be used to store a position in the
;; text that you can quickly jump back to.
;;
;; The mark is set by
;;     C-spacebar
;;
;; To jump back to where the mark was set from wherever the point may be
;;     C-x C-x         (Works best when transient-mark-mode is nil)
;;
;; To select a block of text:
;;     Go to the start of the block
;;     Press C-spacebar
;;     Go to the end of the block
;;
;; To copy without cutting out the block:
;;     M-w
;;
;; To cut (wipe) out the block:
;;     C-w
;;
;; To paste (yank) the block again:
;;     C-y (you can do this as many times as you want)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;;
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
;; Searching and Replacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Go to the section of the document where you want the search and replace to start
;;     Press M-%
;; You will then be prompted for your search and replace strings. Emacs will find
;; and display each occurrence of the search string and ask for further instructions.
;; You can respond with any of the following options:
;;
;;     SPACEBAR        replace text and find the next occurrence
;;     DEL             leave text as is and find the next occurrence
;;     . (period)      replace text, then stop looking for occurrences
;;     ! (exclamation) replace all occurrences without asking
;;     ^ (caret)       return the cursor to previously replaced text
;;
;; Emacs now goes through the document looking for matches
;;     Press Y to replace each time a match is made,
;;     Press N to ingore that match and,
;;     Press ! to go through the rest of the document, changing all matches
;;     without asking.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To enable the spell checker
;;     Press M-x
;;     Type ispell-buffer and press return
;;
;; EMACS then spells checks the current buffer from start to finish
;;
;; Possible replacements are given at the top of the document
;;     Press the number or character in square brackets to use that suggestion
;;
;; The commands you can use are given at the bottom (press ? for more options)
;;     Press r to replace the word
;;     Press i to insert the word into your personal dictionary
;;     Press x to exit the spell check program.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings
;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [f2]               'electric-buffer-list            )
(define-key global-map [f3]               'ido-find-file                   )
(define-key global-map [f4]               'save-buffer                     )
(define-key global-map [f5]               'rename-this-buffer-and-file     )
(define-key global-map [f6]               'refresh-file                    )

(define-key global-map [f7]               'replace-string                  )
(define-key global-map [f8]               'query-replace                   )
(define-key global-map [f9]               'query-replace-regexp            )
(define-key global-map [f10]              'toggle-comment-current-region   )
(define-key global-map [f11]              'duplicate-current-region        )
(define-key global-map [f12]              'goto-line                       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Home End and Delete keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(control home)]   'beginning-of-line               )
(define-key global-map [(control end)]    'end-of-line                     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer cleanup, indentation and vertical alignment functions
;;
;; Ctrl-[f1 to f5]  are not mapped since those are mostly mapped by the OS
;; Ctrl-[f6 to f12] are mapped to functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(control f5)]     'toggle-margin-to-fill-column    )
(define-key global-map [(control f6)]     'indent-clean-region             )
(define-key global-map [(control f7)]     'align-stacked-c-list            )
(define-key global-map [(control f8)]     'align-stacked-c-funcall         )
(define-key global-map [(control f9)]     'align-stacked-c-params          )
(define-key global-map [(control f10)]    'align-stacked-c-assignments     )
(define-key global-map [(control f12)]    'align-regexp                    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paste c-file-style overrides and copyright notice text
;;
;; Alt-[f1 to f5]  are not mapped since those may be mapped by the OS
;; Alt-[f6 to f12] are mapped to functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(alt f6)]         'paste-default-c-file-style      )
(define-key global-map [(alt f7)]         'paste-knr-c-file-style          )
(define-key global-map [(alt f12)]        'yas-reload-all                  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nameses desktop session shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(meta f3)]        'nameses-load                    )
(define-key global-map [(meta f4)]        'nameses-save                    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-run the 'configure-emacs' customization routine to load new settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(meta f6)]        'configure-emacs                 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electric Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-x C-b")    'electric-buffer-list            )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is some Emacs Lisp that will make the % key show the matching
;; parenthesis, like in vi.  In addition, if the cursor isn't over a
;; parenthesis, it simply inserts a % like normal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "%"                'match-paren                     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "A-P")
    '(lambda () (interactive) (message "noop"))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable email bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key  (kbd "C-x m")       )
(global-unset-key  (kbd "C-z")         )
