;; -*- mode: emacs-lisp -*-
;;
;; C/C++ mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C hook section, called on entry of C mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook            'turn-on-font-lock)
(add-hook 'objc-mode-hook         'turn-on-font-lock)
(add-hook 'c++-mode-hook          'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.c$"        . c-mode            )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.m$"        . objc-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.h$"        . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.hpp$"      . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.H$"        . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cpp$"      . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cxx$"      . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.C$"        . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.idl$"      . c++-mode          )) auto-mode-alist))
