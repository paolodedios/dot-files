;; -*- mode: emacs-lisp -*-
;;
;; YASnippet mode
;; @see https://github.com/capitaomorte/yasnippet
;; @see https://github.com/AndreaCrotti/yasnippet-snippets
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load snippets utility
;; @see https://github.com/capitaomorte/yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)

(setq yas-snippet-dirs
      (append yas-snippet-dirs
              '("~/.snippets"           ;; private snippets
                "~/.emacs.d/snippets"   ;; public contrib snippets
                )
              )
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable dropdown-prompt priority
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'popup)

(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))


(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))


(require 'dropdown-list)

(setq yas-prompt-functions
      '(yas-dropdown-prompt
        yas-x-prompt
        yas-popup-isearch-prompt
        yas-no-prompt
        )
      )

;; Set to auto indent first line
(setq yas-also-auto-indent-first-line             t  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebind trigger keys and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unbind yasnippet from using the TAB key to prevent it from conflicting
;; with TAB completion via the autocomplete.el module
(define-key yas-minor-mode-map [(tab)]            nil       )
(define-key yas-minor-mode-map (kbd "TAB")        nil       )

;; Bind the trigger to Shift-TAB, instead of just tab to allow for definitive
;; expansions and as an alternative mechanism for when minor modes also bind
;; to TAB or <tab>
(define-key yas-minor-mode-map (kbd "<S-tab>")   'yas-expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use YASnippet as a non-global minor mode, by replacing (yas-global-mode 1)
;; with (yas-reload-all) to load the snippet tables. Then add a call to
;; (yas-minor-mode) to the major-modes where you to enable YASnippet.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reload all snippet tables
(yas-reload-all)

;; Enable YAS mode everywhere
(yas-global-mode  t)
