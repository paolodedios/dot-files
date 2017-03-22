;; -*- mode: emacs-lisp -*-
;;
;; Major and minor mode settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable font-lock for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

(cond ((fboundp 'global-font-lock-mode)
       (global-font-lock-mode                            t)
       (setq-default font-lock-auto-fontify              t)
       (setq-default font-lock-use-maximal-decoration    t)
       (setq-default font-lock-use-colors                t)
       (setq-default font-lock-use-fonts               nil)
       (setq-default font-lock-mode-enable-list        nil)
       (setq-default font-lock-mode-disable-list       nil)
       )
      )

(setq font-lock-maximum-decoration     t)
(setq font-lock-maximum-size         nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load file management minor modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "modules/config-dired-mode")
(load "modules/config-ido-mode")
(load "modules/config-buffer-menu")
(load "modules/config-git-mode")
(load "modules/config-abbrev-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load major modes in order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "modules/config-text-mode")
(load "modules/config-markdown-mode")

(load "modules/config-lisp-mode")
(load "modules/config-sh-mode")
(load "modules/config-cc-mode")
(load "modules/config-java-mode")
(load "modules/config-scala-mode")
(load "modules/config-clojure-mode")
(load "modules/config-groovy-mode")
(load "modules/config-jflex-mode")

(load "modules/config-python-mode")
(load "modules/config-js-mode")
(load "modules/config-coffeescript-mode")
(load "modules/config-typescript-mode")

(load "modules/config-php-mode")
(load "modules/config-sql-mode")
(load "modules/config-perl-mode")
(load "modules/config-matlab-mode")

(load "modules/config-css-mode")
(load "modules/config-dtd-mode")
(load "modules/config-xml-mode")
(load "modules/config-yaml-mode")

(load "modules/config-go-mode")
(load "modules/config-ocaml-mode")

(load "modules/config-mmm")
(load "modules/config-web-mode")
(load "modules/config-ng2-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attach YASnippet mode to all other major modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "modules/config-yas-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load auto-complete package after all other major modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "modules/config-autocomplete-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default major mode to text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-major-mode  'text-mode)
