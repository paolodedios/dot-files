;; -*- mode: emacs-lisp -*-
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
             (linum-mode                             1)
             ;; set fill-column option
             (setq fill-column                     100)
             ;; disable auto-fill-mode after setting fill-column
             (auto-fill-mode                        -1)
             (local-set-key "^C-^t"   'ispell-complete-word))
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.bib$"      . bibtex-mode       )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.tex$"      . TeX-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.txi$"      . Texinfo-mode      )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.txt$"      . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.README$"   . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.doc$"      . text-mode         )) auto-mode-alist))
