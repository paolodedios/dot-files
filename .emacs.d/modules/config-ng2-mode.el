;; -*- mode: emacs-lisp -*-
;;
;; Angular 2 (NG-2) mode
;; @see https://github.com/AdamNiederer/ng2-mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anguler 2 (NG-2) mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ng2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable auto fill mode for all markup major/minor modes
(add-hook 'ng2-mode-hook
          '(lambda ()
             (turn-on-font-lock   )
             (setq tab-width     4)
             )
          )

(add-hook 'ng2-html-mode-hook
          '(lambda ()
             (turn-on-font-lock   )
             (auto-fill-mode    -1)
             (setq tab-width     2)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Override typescript-mode with ng2-mode
(setq auto-mode-alist (append '(("\\.ts$"             . ng2-mode      )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.component.html$" . ng2-html-mode )) auto-mode-alist))
