;; -*- mode: emacs-lisp -*-
;;
;; Autocomplete Mode using Company
;; @see https://github.com/company-mode/company-mode/wiki/Switching-from-AC
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company (complete anything) mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust keybindings and behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; company-mode and auto-complete-mode behave differently in terms of candidate
;; selection and completion. There's a few tweaks you can do to make the switch
;; less jarring. First of all, to make TAB complete first, then cycle, rebind it
;; to company-complete-common-or-cycle
(eval-after-load 'company
  '(progn
     ;; Map tab to cycle through the completion options
     (define-key company-active-map (kbd "TAB"  ) 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

     ;; Map C-n and C-p to navigate the completion menu
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     )
  )

;; When using (company-ac-setup), the color of faces of the completions
;; (company-preview and company-preview-common) might be different. If this is
;; not preferred, to get matching colors, this is an option:
(eval-after-load 'company
  (lambda ()
    (set-face-attribute
     'company-preview
     nil
     :background (face-attribute 'company-preview-common :background))
    )
  )

;; The current candidate isn't displayed inline except when there's only one
;; left. You can rectify this by switching out company-preview-if-just-one-frontend
;; for company-preview-frontend:
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend)
      )


;; Finally, to cancel selections by typing non-matching characters, customize
;; company-require-match:
(setq company-require-match     'never)

;; Set up the idle delay in seconds until completion starts automatically
(setq company-idle-delay          0.25)

;; Set up the idle delay in seconds until tooltip is shown when using
;; company-pseudo-tooltip-unless-just-one-frontend-with-delay frontend
(setq company-tooltip-idle-delay  0.25)

;; Allow for a bigger popup window than the default
(setq company-tooltip-limit         20)

;; Remove blinking effect
(setq company-echo-delay             0)

;; Start autocompletion only after typing
(setq company-begin-commands '(self-insert-command))


;; Set up similar popup behavior to autocompelte
;; @see https://github.com/company-mode/company-mode/pull/524
(defun my-company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)
       )
  )

(defun company-ac-setup ()
  "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  (setq company-require-match nil)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend-with-delay
                            company-preview-frontend))

  ;; Bind TAB to company-complete-common-or-cycle instead
  ;; (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  ;; (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  )

(company-ac-setup)
