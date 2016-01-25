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

(ido-mode                                      t)
(setq ido-everywhere                           t)
(setq ido-enable-flex-matching                 t)
(setq ido-case-fold                            t)
(setq ido-create-new-buffer              'always)
(setq ido-use-filename-at-point           'guess)
(setq ido-enable-prefix                      nil)
(setq ido-max-prospects                       10)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-use-virtual-buffers                  t)
(setq ido-handle-duplicate-virtual-buffers     3)

;; ido file listing order
(setq ido-file-extensions-order
      '(".java"
        ".scala"
        ".clj"
        ".groovy"
        ".js"
        ".jsx"
        ".coffee"
        ".css"
        ".less"
        ".sass"
        ".html"
        ".xml"
        ".c"
        ".objc"
        ".cpp"
        ".h"
        ".el"
        ".lisp"
        )
      )
