;; -*- mode: emacs-lisp -*-
;;
;; Global editor settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial window frame settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-height (selected-frame)           90)
(set-frame-width  (selected-frame)          154)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the title displayed in the title-bar "filename (path)"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq frame-title-format               "[ %f ]")

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

(set-default-coding-systems         'utf-8-unix)
(set-buffer-file-coding-system      'utf-8-unix)

(prefer-coding-system               'utf-8     )
(setq locale-coding-system          'utf-8     )
(set-terminal-coding-system         'utf-8     )
(set-keyboard-coding-system         'utf-8     )
(set-selection-coding-system        'utf-8     )
(set-language-environment           "UTF-8"    )

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
(modify-coding-system-alist 'file "\\.scala\\'"    'utf-8-unix)
(modify-coding-system-alist 'file "\\.clojure\\'"  'utf-8-unix)
(modify-coding-system-alist 'file "\\.go\\'"       'utf-8-unix)
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
(modify-coding-system-alist 'file "\\.groovy\\'"   'utf-8-unix)
(modify-coding-system-alist 'file "\\.gradle\\'"   'utf-8-unix)
(modify-coding-system-alist 'file "\\.python\\'"   'utf-8-unix)
(modify-coding-system-alist 'file "\\.ml\\'"       'utf-8-unix)
(modify-coding-system-alist 'file "\\.ocaml\\'"    'utf-8-unix)
(modify-coding-system-alist 'file "\\.sql\\'"      'utf-8-unix)
(modify-coding-system-alist 'file "\\.epl\\'"      'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs recognizes which kind of end-of-line conversion to use based on the
;; contents of the file: if it sees only carriage-returns, or only carriage
;; return linefeed sequences, then it chooses the end-of-line conversion
;; accordingly. You can inhibit the automatic use of end-of-line conversion by
;; setting the variable inhibit-eol-conversion to non-nil. If you do that, DOS
;; style files will be displayed with the `^M' characters visible in the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-eol-conversion                nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backwards compatibility; default-buffer-file-coding-system is deprecated in 23.2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-request-type    '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(cursor-type (quote box))
 '(tool-bar-mode nil nil (tool-bar))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show approximate buffer size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(size-indication-mode                         t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show time on the bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppress startup messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-echo-area-message       t)
(setq inhibit-startup-screen                  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set some limits on history lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq kill-ring-max                         500)
(setq extended-command-history-max           50)
(setq query-replace-history-max              50)
(setq replace-string-history-max             50)
(setq file-name-history-max                  50)
(setq replace-regex-history-max              50)
(setq minibuffer-history-max               1000)
(setq shell-command-history-max            1000)
(setq find-file-history-max                1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq minibuffer-max-depth                  nil)
(setq mouse-yank-at-point                   t  )
(setq query-replace-highlight               t  )
(setq search-highlight                      t  )

;; use the block cursor instead of the line cursor
(setq bar-cursor                            nil)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p                'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load uniquify to distinguish between two identically named files instead of
;; appending a <n> extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)

(setq uniquify-buffer-name-style       'reverse)
(setq uniquify-separator               "|"     )
(setq uniquify-after-kill-buffer-p t           )
(setq uniquify-ignore-buffers-re       "^\\*"  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line wrapping behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't allow text to exceed 80 columns before line wrapping
(setq fill-column                            80)

;; disable line wrap by default. enable only through specific mode hooks
(setq-default truncate-lines                nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make selected region visible via highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-mark-mode                          1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebinds C-a, C-e, and C-k to commands that operate by visual lines
;; instead of logical lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-visual-line-mode                      1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Underline highlighted region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-underline-p                 'region t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move (1) line when at bottom of screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scroll-step                             1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace highlighted text with keystroke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode                        t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default mode to text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-major-mode             'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove tabs from the idents/use spaces instead of tab characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode              nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable line highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-current-line)

(highlight-current-line-on                    t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable indentation highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable git diff indicators in the gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'git-gutter+)
(require 'git-gutter-fringe+)

(global-git-gutter+-mode)

(setq-default right-fringe-width   20          )
(setq git-gutter-fr+-side         'right-fringe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save cursor point placement for all files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'saveplace)

(setq save-place-file      "~/.emacs.sessions/saveplace")
(setq-default save-place   t                            )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable abbrev mode for modes that use it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq abbrev-file-name     "~/.snippets/abbrev-mode/abbrevs")
(setq-default abbrev-mode  t                                )
(setq save-abbrevs         t                                )

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save editing sessions by name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'desktop)
(require 'nameses)
