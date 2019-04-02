;; -*- mode: emacs-lisp -*-
;;
;; Web template mode
;; @see https://github.com/fxbois/web-mode
;; @see https://github.com/osv/company-web/tree/master
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

(require 'web-mode)
(require 'company-web-html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode hook section, called on entry of web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset             2)
            (setq web-mode-css-indent-offset                4)
            (setq web-mode-code-indent-offset               4)
            (setq web-mode-indent-style                     4)
            (setq web-mode-style-paddding                   1)
            (setq web-mode-script-paddding                  1)
            (setq web-mode-block-paddding                   0)
            (setq web-mode-enable-auto-pairing              t)
            (setq web-mode-enable-css-colorization          t)
            (setq web-mode-enable-comment-keywords          t)
            (setq web-mode-enable-heredoc-fontification     t)
            (setq web-mode-use-tabs                         t)
            )
          )

;; Only use company-mode with company-web-html in web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-web-html company-yasnippet company-files)
                 )
            (company-mode t)
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'web-mode-hook          'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove HTML from the magic-mode-alist so that it can be bound to web-mode
;; @see https://stackoverflow.com/a/14059524
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'magic-mode-alist
    '("\\(?:<\\?xml\\s +[^>]*>\\)?\\s *<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\(?:!DOCTYPE\\s +[^>]*>\\s *<\\s *\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *\<\\)*\\)?[Hh][Tt][Mm][Ll]"
        . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.htm?\\'"   . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.html?\\'"  . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.st$"       . web-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.stg$"      . web-mode         )) auto-mode-alist))
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
