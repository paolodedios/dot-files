;; -*- mode: emacs-lisp -*-
;;
;; Typescript mode
;; @see https://github.com/ananthakumaran/typescript.el
;; @see https://github.com/ananthakumaran/tide
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'typescript-mode)
(require 'ts-comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-tide-mode ()
  "TypeScript Interactive Development Environment for Emacs"
  (interactive)
  (tide-setup                    )
  (eldoc-mode                  +1)
  (tide-hl-identifier-mode     +1)

  (flycheck-mode               +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode                +1)
  )

;; Align annotation to the right hand side
(setq company-tooltip-align-annotations  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript hook section, called on entry of typescript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'typescript-mode-hook
          (lambda ()
            (turn-on-font-lock                       )
            (c-toggle-auto-state                    1)
            (c-toggle-hungry-state                  1)
            (show-paren-mode                        t)
            (subword-mode                           1)

            (setq fill-column                     100)
            (setq c-basic-offset                    4)
            (setq tab-width                         4)
            (setq indent-tabs-mode                nil)
            (auto-fill-mode                         1)
            ;; Setup tide-mode
            (setup-tide-mode                         )
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add ts-comint key bindings for sending things to the Typescript
;; interpreter inside of typescript-mode
(add-hook 'typescript-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e")  'ts-send-last-sexp       )
            (local-set-key (kbd "C-M-x"  )  'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b"  )  'ts-send-buffer          )
            (local-set-key (kbd "C-c C-b")  'ts-send-buffer-and-go   )
            (local-set-key (kbd "C-c l"  )  'ts-load-file-and-go     )
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.ts$"         . typescript-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.typescript$" . typescript-mode     )) auto-mode-alist))
