;; -*-emacs-lisp-*-
;;
;; Text mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode hook section, called on entry of Text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook 
          '(lambda ()
             (auto-fill-mode                              1)    
             (setq fill-column                           80)
             (setq tab-width                              4)
             (local-set-key "^C-^t"   'ispell-complete-word)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook         'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.bib$"      . bibtex-mode       )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.tex$"      . TeX-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.txi$"      . Texinfo-mode      )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.txt$"      . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.README$"   . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("README$"      . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '((".*READ\\.ME$" . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.doc$"      . text-mode         )) auto-mode-alist))


