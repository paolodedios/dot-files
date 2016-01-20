;; -*- mode: emacs-lisp -*-
;;
;; web template mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre-load variable declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq web-mode-enable-current-element-highlight   t)
(setq web-mode-enable-current-column-highlight    t)

(setq web-mode-ac-sources-alist
  '(
    ("css"    . (ac-source-css-property))
    ("html"   . (ac-source-words-in-buffer ac-source-abbrev))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-vendor 'web-mode)

(require 'web-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode hook section, called on entry of web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-web-mode-hook ()
  "Hooks for web-mode."
  (setq web-mode-markup-indent-offset             4)
  (setq web-mode-css-indent-offset                4)
  (setq web-mode-code-indent-offset               4)
  (setq web-mode-enable-auto-pairing              t)
  (setq web-mode-enable-css-colorization          t)
  (setq web-mode-enable-comment-keywords          t)
  (setq web-mode-enable-heredoc-fontification     t)
  (setq web-mode-use-tabs                         t)
  )

(add-hook 'web-mode-hook          'my-web-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'web-mode-hook          'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.htm$"      . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.html$"     . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.st$"       . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.phtml$"    . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.phps$"     . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.tmpl$"     . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.incl$"     . web-mode         )) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.[agj]sp$"  . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.as[cp]x$"  . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.erb$"      . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mustache$" . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.djhtml$"   . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.jsx$"      . web-mode         )) auto-mode-alist))
