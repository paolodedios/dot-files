;; -*- mode: emacs-lisp -*-
;;
;; Autocomplete Mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete mode
;;
;; auto-complete-mode should be loaded after yasnippet so that they can work together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure location of autocomplete dictionaries for various modes
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/autocomplete/dict")

;; Load the default auto-complete configuration
(ac-config-default)

;; Enable specifc modes for auto-complete
(setq ac-modes '(emacs-lisp-mode
                 lisp-interaction-mode
                 cc-mode
                 c-mode
                 c++-mode
                 objc-mode
                 java-mode
                 scala-mode
                 clojure-mode
                 css-mode
                 ecmascript-mode
                 javascript-mode
                 js-mode
                 js2-mode
                 makefile-mode
                 ocaml-mode
                 tuareg-mode
                 perl-mode
                 php-mode
                 python-mode
                 ruby-mode
                 sql-mode
                 sh-mode
                 xml-mode
                 sgml-mode
                 )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust key bindings and triggers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the trigger key to TAB by default
;;
;; Completion can be stopped by pressing C-g at any time. Hitting C-g and RET
;; will enter a newline with the autocomplete bindings of RET intact. Also
;; C-j can be used instead of  RET when entering a newline. C-j is not bound to
;; to ac-complete by default.
(ac-set-trigger-key               "TAB"       )

;; Automatically show completion menu
(setq ac-use-menu-map             t           )

;; Map C-n and C-p to navigate the completion menu
(define-key ac-menu-map  "\C-n"   'ac-next    )
(define-key ac-menu-map  "\C-p"   'ac-previous)