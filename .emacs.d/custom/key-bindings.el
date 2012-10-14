;; -*-emacs-lisp-*-
;;
;; Global key bindings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Keyboard shortcuts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;; Moving About 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;;
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
;; Press the escape key and then press 
;; Type in the text you wish to search for and press return 
;; Type in the text that should replace the match and press return 
;; EMACS now goes through the document looking for matches 
;; Press Y to replace each time a match is made, 
;; N to ingore that match and, 
;; ! goes through the rest of the document, changing all matches without asking you. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell Checking 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings
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