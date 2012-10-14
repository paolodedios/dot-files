;; -*-emacs-lisp-*-
;;
;; Shell mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell hook section, called on entry of Shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sh-mode-hook
          '(lambda ()
             (auto-fill-mode                              1)    
             (setq fill-column                           80)
             (setq tab-width                              4)
             (local-set-key [return]    'newline-and-indent)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'csh-mode-hook          'turn-on-font-lock)
(add-hook 'sh-mode-hook           'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.csh$"      . csh-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.sh$"       . sh-mode           )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.bash$"     . sh-mode           )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.bashrc$"   . sh-mode           )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.profile$"  . sh-mode           )) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.conf$"     . conf-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ini$"      . conf-mode         )) auto-mode-alist))

