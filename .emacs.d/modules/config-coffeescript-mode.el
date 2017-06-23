;; -*- mode: emacs-lisp -*-
;;
;; CoffeeScript mode
;; @see https://github.com/defunkt/coffee-mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoffeeScript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'coffee-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoffeeScript hook section, called on entry of coffee-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compile .coffee files on every save
(add-hook 'after-save-hook
          '(lambda ()
             (when (string-match "\.coffee$" (buffer-name))
               (coffee-compile-file)
               )
             )
          )


(add-hook 'coffee-mode-hook
  '(lambda()

     ;; *Messages* spam
     (setq coffee-debug-mode                           t)

     ;; CoffeeScript uses two spaces.
     (set (make-local-variable 'tab-width)             2)

     ;; Use JS2 mode for viewing compiled coffeescript
     (setq coffee-js-mode                      'js2-mode                 )
     (setq coffee-cleanup-whitespace           nil                       )

     ;; If you don't want your compiled files to be wrapped
     (setq coffee-args-compile                 '("-c" "--bare")          )

     (define-key coffee-mode-map (kbd "C-j")   'coffee-newline-and-indent)
     (define-key coffee-mode-map [(meta r)]    'coffee-compile-buffer    )
     (setq coffee-command "coffee")
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'coffee-mode-hook       'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.coffee$"   . coffee-mode       )) auto-mode-alist))
(setq auto-mode-alist (append '(("Cakefile"     . coffee-mode       )) auto-mode-alist))
