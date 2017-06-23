;; -*- mode: emacs-lisp -*-
;;
;; CSS mode
;; @see https://julien.danjou.info/projects/emacs-packages#rainbow-mode
;; @see https://github.com/purcell/less-css-mode
;; @see https://github.com/nex3/sass-mode
;; @see https://github.com/antonj/scss-mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'css-mode)
(require 'less-css-mode)
(require 'sass-mode)
(require 'scss-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS hook section, called on entry of CSS mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode             1)
            (setq css-indent-offset   4)
            )
          )

(add-hook 'less-css-mode-hook
          (lambda ()
            (rainbow-mode             1)
            (setq css-indent-offset   4)
            )
          )


(add-hook 'sass-mode-hook
          (lambda ()
            (rainbow-mode             1)
            (setq sass-indent-offset  4)
            )
          )

(add-hook 'scss-mode-hook
          (lambda ()
            (rainbow-mode             1)
            (setq css-indent-offset   4)
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'css-mode-hook          'turn-on-font-lock)
(add-hook 'less-css-mode-hook     'turn-on-font-lock)
(add-hook 'sass-mode-hook         'turn-on-font-lock)
(add-hook 'scss-mode-hook         'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.css$"      . css-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.less$"     . less-css-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.sass$"     . sass-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.scss$"     . scss-mode         )) auto-mode-alist))
