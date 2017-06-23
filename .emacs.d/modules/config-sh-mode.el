;; -*- mode: emacs-lisp -*-
;;
;; Shell mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Emacs Shell hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'shell-mode-hook                 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions  'comint-strip-ctrl-m          )

(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up]    'comint-previous-input)
     (define-key shell-mode-map [down]  'comint-next-input    )
     (define-key shell-mode-map "\C-p"  'comint-previous-input)
     (define-key shell-mode-map "\C-n"  'comint-next-input    )
     )
  )

;; Define shell mode to use ansi-term via bash
(defun sh ()
  (interactive)
  (ansi-term "/bin/bash")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-z")       'sh)

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
