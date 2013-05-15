;; -*- mode: emacs-lisp -*-
;;
;; Matlab mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load matlab-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-vendor 'matlab)

(autoload 'matlab-mode  "matlab"  "Enter Matlab Mode."       t)
(autoload 'matlab-shell "matlab"  "Interactive Matlab Mode." t)

(setq matlab-shell-command                            "matlab")
(setq matlab-shell-command-switches        "-nodesktop -nojvm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab hook section, called on entry of Matlab mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'matlab-mode-hook
          (lambda ()
            (setq matlab-indent-level                      4)
            (setq fill-column                             80)
            (define-key matlab-mode-map "\M-;" 'comment-dwim)
            )
          )

;; indent function bodies
(setq matlab-indent-function-body    t)
;; turn off auto-verify on save
(setq matlab-verify-on-save-flag   nil)

(defun my-matlab-shell-mode-hook () '())

(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'matlab-mode-hook       'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.matlab$"   . matlab-mode       )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.octave$"   . matlab-mode       )) auto-mode-alist))
