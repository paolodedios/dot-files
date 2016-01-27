;; -*- mode: emacs-lisp -*-
;;
;; Encoding system configuration
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default to 8-bit clean utf-8 encoding for all files, comint processes
;; and buffers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system               'utf-8)
(set-default-coding-systems         'utf-8)
(set-terminal-coding-system         'utf-8)
(set-keyboard-coding-system         'utf-8)
(set-language-environment           'utf-8)
(setq locale-coding-system          'utf-8)

;; Disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode        nil   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-request-type    '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set per-file associations for automatic charset conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modify-coding-system-alist 'file "\\.m\\'"        'utf-8)
(modify-coding-system-alist 'file "\\.c\\'"        'utf-8)
(modify-coding-system-alist 'file "\\.h\\'"        'utf-8)
(modify-coding-system-alist 'file "\\.hpp\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.H\\'"        'utf-8)
(modify-coding-system-alist 'file "\\.cpp\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.C\\'"        'utf-8)
(modify-coding-system-alist 'file "\\.idl\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.java\\'"     'utf-8)
(modify-coding-system-alist 'file "\\.scala\\'"    'utf-8)
(modify-coding-system-alist 'file "\\.clojure\\'"  'utf-8)
(modify-coding-system-alist 'file "\\.clj\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.go\\'"       'utf-8)
(modify-coding-system-alist 'file "\\.js\\'"       'utf-8)
(modify-coding-system-alist 'file "\\.jsx\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.coffee\\'"   'utf-8)
(modify-coding-system-alist 'file "\\.sh\\'"       'utf-8)
(modify-coding-system-alist 'file "\\.conf\\'"     'utf-8)
(modify-coding-system-alist 'file "\\.xml\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.xul\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.rdf\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.html\\'"     'utf-8)
(modify-coding-system-alist 'file "\\.css\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.scss\\'"     'utf-8)
(modify-coding-system-alist 'file "\\.vm\\'"       'utf-8)
(modify-coding-system-alist 'file "\\.php\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.xsd\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.dtd\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.groovy\\'"   'utf-8)
(modify-coding-system-alist 'file "\\.gradle\\'"   'utf-8)
(modify-coding-system-alist 'file "\\.python\\'"   'utf-8)
(modify-coding-system-alist 'file "\\.ipyb\\'"     'utf-8)
(modify-coding-system-alist 'file "\\.ml\\'"       'utf-8)
(modify-coding-system-alist 'file "\\.ocaml\\'"    'utf-8)
(modify-coding-system-alist 'file "\\.sql\\'"      'utf-8)
(modify-coding-system-alist 'file "\\.epl\\'"      'utf-8)

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
;; Backwards compatibility; default-buffer-file-coding-system is deprecated
;; starting in Emacs 23.2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs will default to UTF-8 for everything. Not everything is in UTF-8, though.
;; How do you deal with cases where it isn’t?
;; The command M-x universal-coding-system-argument, bound to the handy C-x RET c,
;; takes as an argument the coding system you want to use, and a command to execute
;; it with. That makes it possible to open files, shells or run Emacs commands as
;; though you were using a different coding system. This command is a must-have if
;; you have to deal with stuff encoded in strange coding systems.
;;
;; One problem with the universal coding system argument is that it only cares
;; about Emacs’s settings, not those of your shell or system. That’s a problem,
;; because tools like Python use the environment variable PYTHONIOENCODING to set
;; the coding system for the Python interpreter.
;;
;; The following function advises the universal-coding-system-argument function
;; so it also, temporarily for just that command, sets a user-supplied list of
;; environment variables to the coding system.
;;
;; @see https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List of environment variables `universal-coding-system-argument' should set
(defvar universal-coding-system-env-list '("PYTHONIOENCODING"))

;; Augments `universal-coding-system-argument' so it also sets environment
;; variables
;;
;; Naively sets all environment variables specified in
;; `universal-coding-system-env-list' to the literal string representation of
;; the argument `coding-system'.
;;
;; No guarantees are made that the environment variables set by this advice support
;; the same coding systems as Emacs.
(defadvice universal-coding-system-argument (around provide-env-handler activate)
  (let ((process-environment (copy-alist process-environment)))
    (dolist (extra-env universal-coding-system-env-list)
      (setenv extra-env (symbol-name (ad-get-arg 0))))
    ad-do-it))
