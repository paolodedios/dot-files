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

(require 'dropdown-list)
(require 'popup)
(require 'dash)
(require 's)

;; Use incremental search to autocomplete the snippet key. Under incremental
;; search, the following keyboard shortcuts apply
;;
;; RET          select/finish completion  (also fn-ENTER on some keyboards)
;; C-m          select/finish completion
;; C-g          cancel isearch
;; C-h          delete isearch character
;;
;; @see https://github.com/auto-complete/popup-el
;; @see https://github.com/thomasf/emacs-yasnippet-popup
;;
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "A yasnipppet prompt based on popup.el"
  (let ((group-max-len 0)
        (key-max-len 0)
        (fmt "")
        (popup-items)
        )

    (mapcar #'(lambda (choice)
                (when (yas--template-p choice)
                  (setq group-max-len (max group-max-len
                                           (+ (length (yas--template-group choice) )
                                              (apply '+ (mapcar 'length (yas--template-group choice))))))
                  (setq key-max-len (max key-max-len (length (yas--template-key choice))))
                  )
                )
            choices)

    (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds│ %%s"
                      (if (> group-max-len 0 ) "" " ")
                      group-max-len group-max-len
                      (if (> group-max-len 0 ) " > " "")
                      key-max-len key-max-len
                      )
          )

    (setq popup-items
          (mapcar
           #'(lambda (choice)
               (popup-make-item
                (if (yas--template-p choice)
                    (format fmt
                            (if (yas--template-group choice)
                                (s-join "/" (yas--template-group choice))
                              "")
                            (if (yas--template-key choice)
                                (yas--template-key choice)
                              "")
                            (if (yas--template-name choice)
                                (yas--template-name choice)
                              ""))
                  (format " %s" choice)
                  )
                :value choice
                )
               )
           choices
           )
          )

    (popup-menu*
     popup-items
     :prompt prompt
     :max-width 80
     :isearch t
     :isearch-keymap yas-popup-isearch-keymap
     )
    )
  )

(setq yas-prompt-functions
       '(yas-popup-isearch-prompt
         yas-dropdown-prompt
         yas-x-prompt
         yas-no-prompt
         )
       )

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

;; Set to auto indent first line
(setq yas-also-auto-indent-first-line             t  )

;; Reload all snippet tables
(yas-reload-all)

;; Enable YAS mode everywhere
(yas-global-mode  t)
