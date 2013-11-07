;; -*- mode: emacs-lisp -*-
;;
;; YASnippet  mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load snippets utility
;; @see https://github.com/capitaomorte/yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                       ;; personal snippets
        "~/.emacs.d/vendor/yasnippet/snippets"      ;; the default collection
        )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable dropdown-prompt priority
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dropdown-list)

(setq yas/prompt-functions
      '(yas/dropdown-prompt
        yas/ido-prompt
        yas/x-prompt
        yas/completing-prompt
        yas/no-prompt
        )
      )


;; Use yas/completing-prompt ONLY via `M-x yas/insert-snippet'
(defadvice yas/insert-snippet (around use-completing-prompt activate)
    "Use `yas/completing-prompt' for `yas/prompt-functions' but only here..."
     (let ((yas/prompt-functions '(yas/completing-prompt)) )  ad-do-it)
     )

;; Set to auto indent first line
(setq yas/also-auto-indent-first-line             t  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebind trigger keys and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unbind yasnippet from using the TAB key to prevent it from conflicting
;; with TAB completion via the autocomplete.el module
(define-key yas-minor-mode-map [(tab)]            nil       )
(define-key yas-minor-mode-map (kbd "TAB")        nil       )

;; Rebind the trigger to Shift-TAB instead
(define-key yas-minor-mode-map (kbd "<S-tab>")   'yas/expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use YASnippet as a non-global minor mode, by replacing (yas-global-mode 1)
;; with (yas-reload-all) to load the snippet tables. Then add a call to
;; (yas-minor-mode) to the major-modes where you to enable YASnippet.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reload all snippet tables
(yas-reload-all)

;; enable yasnippet for generic cc-mode
(add-hook 'cc-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for objc-mode
(add-hook 'objc-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for java-mode
(add-hook 'java-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for scala-mode
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for python-mode
(add-hook 'python-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for html-mode
(add-hook 'html-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for psgml-mode
(add-hook 'psgml-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for sgml-mode
(add-hook 'sgml-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for xml-mode
(add-hook 'xml-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for css-mode
(add-hook 'css-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for sh-mode
(add-hook 'sh-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))
