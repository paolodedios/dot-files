;; -*- mode: emacs-lisp -*-
;;
;; OCaml mode
;; @see https://github.com/ocaml/tuareg
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ocaml mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'tuareg-mode              "tuareg"        "Major mode for editing Caml code"  t)
(autoload 'camldebug                "camldebug"     "Run the Caml debugger"             t)
(autoload 'tuareg-imenu-set-imenu   "tuareg-imenu"  "Configuration of imenu for tuareg" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'tuareg-mode-hook       'turn-on-font-lock)
(add-hook 'tuareg-mode-hook       'tuareg-imenu-set-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.ml[ily]?$" . tuareg-mode       )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.topml$"    . tuareg-mode       )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ocaml$"    . tuareg-mode       )) auto-mode-alist))
