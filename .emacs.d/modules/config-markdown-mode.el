;; -*- mode: emacs-lisp -*-
;;
;; Markdown mode
;; @see https://github.com/defunkt/markdown-mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown hook section, called on entry of Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'markdown-mode-hook
          '(lambda ()
             (linum-mode                             1)
             ;; Disable auto-fill mode completely since spaces and newlines are
             ;; significant in Markdown formatting.
             (auto-fill-mode                        -1)
             ;; Disable auto-fill hot key
             (local-set-key (kbd "M-q")        'ignore)
             (local-set-key "^C-^t"   'ispell-complete-word))
          )


;; Enable native syntax highlighting of code blocks
(setq markdown-fontify-code-blocks-natively  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use github flavored markdown mode for *.md files
(setq auto-mode-alist (append '(("\\.markdown$" . markdown-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mdown$"    . markdown-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mdwn$"     . markdown-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.md$"       . gfm-mode          )) auto-mode-alist))
