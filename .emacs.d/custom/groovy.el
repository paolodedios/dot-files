;; -*-emacs-lisp-*-
;;
;; Groovy mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'groovy-mode     "groovy-mode" "Groovy Editing Mode"                              t)
(autoload 'run-groovy      "inf-groovy"  "Run an inferior Groovy process"                    )
(autoload 'inf-groovy-keys "inf-groovy"  "Set local key defs for inf-groovy in groovy-mode"  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groovy hook section, called on entry of Groovy mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'groovy-mode-hook
          '(lambda ()
             ;; toggle major mode editor options
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (subword-mode                           1)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)
             (inf-groovy-keys                         )
             ;; set programming style
             (c-add-style "my-groovy-programming-style" my-java-mode-programming-style t)
             (c-set-style "my-groovy-programming-style")
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