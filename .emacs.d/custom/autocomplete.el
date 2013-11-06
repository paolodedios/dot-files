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

;; Set the trigger key so that it can work with yasnippet via the TAB key,
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
(ac-set-trigger-key    "TAB"   )
(ac-set-trigger-key    "<tab>" )
