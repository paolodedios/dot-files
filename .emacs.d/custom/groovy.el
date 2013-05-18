;; -*- mode: emacs-lisp -*-
;;
;; Groovy mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'groovy-electric)

(autoload 'groovy-mode     "groovy-mode" "Groovy Editing Mode"   t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groovy hook section, called on entry of Groovy mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'groovy-mode-hook
          '(lambda ()
             (groovy-electric-mode                   1)
             ;; Jeremy Rayner's groovy-mode is not CC-mode based
             ;; Russel Winder's groovy-mode is CC-mode based but font-locking is broken
             ;; so disable set style
             ;; (c-add-style "my-groovy-programming-style" my-java-mode-programming-style t)
             ;; (c-set-style "my-groovy-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'groovy-mode-hook       'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.groovy$"   . groovy-mode       )) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq interpreter-mode-alist (append '(("groovy" . groovy-mode )) interpreter-mode-alist))

;; can set groovy-home here, if not in environment
(setq inferior-groovy-mode-hook
      '(lambda()
         (setq groovy-home "/Users/paolodedios/Applications/groovy-2.0.6/")
         )
      )
