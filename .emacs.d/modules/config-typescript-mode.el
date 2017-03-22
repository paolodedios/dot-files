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
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-tide-mode ()
  (interactive)
  (tide-setup                    )
  (eldoc-mode                  +1)
  (tide-hl-identifier-mode     +1)

  (flycheck-mode               +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode               +1)
  )

;; Align annotation to the right hand side
(setq company-tooltip-align-annotations  t)

;; Format the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; Setup tide-mode using the above initializer fun
(add-hook 'typescript-mode-hook  #'setup-tide-mode)

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


(add-hook 'typescript-mode-hook  'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.ts$"        . typescript-mode           )) auto-mode-alist))
