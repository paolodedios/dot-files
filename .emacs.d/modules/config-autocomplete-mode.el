;; -*- mode: emacs-lisp -*-
;;
;; Autocomplete Mode
;; @see https://github.com/auto-complete/auto-complete
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete mode
;;
;; auto-complete-mode should be loaded after yasnippet so that they
;; can work together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the default auto-complete configuration
(ac-config-default)

;; Enable yasnippet completion in all modes
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))

;; Enable specifc modes for auto-complete
;;
;; 3/24/2018: Disabled cc-mode derived configurations due to keyboard
;; response time slow downs and higher than normal cpu utilization.
;; Verified on MacOS 10.12.
;;
(setq ac-modes '(emacs-lisp-mode
                 lisp-mode
                 lisp-interaction-mode
                 ;; c-mode
                 ;; cc-mode
                 ;; c++-mode
                 ;; objc-mode
                 ;; java-mode
                 go-mode
                 scala-mode
                 clojure-mode
                 clojurescript-mode
                 css-mode
                 scss-mode
                 less-css-mode
                 ecmascript-mode
                 js2-mode
                 makefile-mode
                 ocaml-mode
                 tuareg-mode
                 perl-mode
                 cperl-mode
                 php-mode
                 python-mode
                 ruby-mode
                 lua-mode
                 tcl-mode
                 sql-mode
                 sh-mode
                 xml-mode
                 sgml-mode
                 web-mode
                 text-mode
                 apples-mode
                 )
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust key bindings and triggers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the trigger key to TAB by default
;;
;; Completion can be stopped by pressing C-g at any time. Hitting C-g and RET
;; will enter a newline with the autocomplete bindings of RET intact. Also
;; C-j can be used instead of RET when entering a newline. C-j is not bound to
;; to ac-complete by default.
(ac-set-trigger-key                   "TAB"       )

;; Automatically show completion menu
(setq ac-use-menu-map                 t           )

;; Case sensitivity is important when finding matches
(setq ac-ignore-case                  nil         )

;; Map C-n and C-p to navigate the completion menu
(define-key ac-menu-map  (kbd "C-n")  'ac-next    )
(define-key ac-menu-map  (kbd "C-p")  'ac-previous)
