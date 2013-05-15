;; -*- mode: emacs-lisp -*-
;;
;; IDO mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable IDO mode for everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)

(ido-mode                                     t)
(setq ido-everywhere                          t)
(setq ido-enable-flex-matching                t)
(setq ido-case-fold                           t)
(setq ido-use-filename-at-point          'guess)
(setq ido-enable-prefix                     nil)
(setq ido-max-prospects                      10)

;; ido file listing order
(setq ido-file-extensions-order
      '(".java"
        ".scala"
        ".clj"
        ".groovy"
        ".js"
        ".coffee"
        ".css"
        ".less"
        ".sass"
        ".html"
        ".xml"
        ".c"
        ".cpp"
        ".h"
        ".el"
        ".lisp"
        )
      )
