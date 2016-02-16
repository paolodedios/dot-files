;; -*- mode: emacs-lisp -*-
;;
;; Text mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace-mode is a minor mode to visualize blanks
;; http://www.emacswiki.org/emacs/WhiteSpace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)

(define-key global-map "\C-c_w"   'whitespace-mode                 )
(define-key global-map "\C-c_t"   'whitespace-toggle-options       )
(define-key global-map "\C-c=w"   'global-whitespace-mode          )
(define-key global-map "\C-c=t"   'global-whitespace-toggle-options)

;; make whitespace-mode use just basic coloring
(setq whitespace-style
      '(spaces tabs newline space-mark tab-mark newline-mark empty trailing)
      )

(setq whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark    32 [183] [46]    )  ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark  10 [182 10]      )  ; 10 LINE FEED
        (tab-mark      9 [9655 9] [92 9])  ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        )
      )

;; enable automatic white space cleanup when buffer is written while in whitespace mode
(setq whitespace-action '(auto-cleanup))

;; enable white space cleanup to global write hook
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode hook section, called on entry of Text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode                         1)
             (setq fill-column                      80)
             (setq tab-width                         4)
             (local-set-key "^C-^t"   'ispell-complete-word)
             ;; For some reason, global-linum-mode does not work in text mode
             (linum-mode                             1)
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
(setq auto-mode-alist (append '(("NOTICE$"      . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("CHANGELOG$"   . text-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.doc$"      . text-mode         )) auto-mode-alist))
