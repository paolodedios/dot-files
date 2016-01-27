;; -*- mode: emacs-lisp -*-
;;
;; Major and minor mode settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load file management minor modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "custom/config-dired-mode")
(load "custom/config-ido-mode")
(load "custom/config-git-mode")
(load "custom/config-abbrev-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load major modes in order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "custom/config-text-mode")
(load "custom/config-markdown-mode")

(load "custom/config-lisp-mode")
(load "custom/config-sh-mode")
(load "custom/config-cc-mode")
(load "custom/config-java-mode")
(load "custom/config-scala-mode")
(load "custom/config-clojure-mode")
(load "custom/config-groovy-mode")
(load "custom/config-jflex-mode")

(load "custom/config-python-mode")
(load "custom/config-js-mode")
(load "custom/config-coffeescript-mode")

(load "custom/config-php-mode")
(load "custom/config-sql-mode")
(load "custom/config-perl-mode")
(load "custom/config-matlab-mode")

(load "custom/config-css-mode")
(load "custom/config-dtd-mode")
(load "custom/config-xml-mode")

(load "custom/config-go-mode")
(load "custom/config-ocaml-mode")

(load "custom/config-mmm")
(load "custom/config-web-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attach YASnippet mode to all other major modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "custom/config-yas-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load auto-complete package after all other major modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "custom/config-autocomplete-mode")
