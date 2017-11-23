;; -*- mode: emacs-lisp -*-
;;
;; Markdown mode
;; @see https://github.com/defunkt/markdown-mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load whitespace-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize whitespace mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark    32 [183] [46]    )  ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark  10 [182 10]      )  ; 10 LINE FEED
        (tab-mark      9 [9655 9] [92 9])  ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        )
      )

;; Highlight long lines that exceed the specified column
;;
;; Note: must enable "lines" in whitespace-style options.
(setq whitespace-line-column 120)

;; set whitespace mode style options and faces
(setq whitespace-style '(face spaces tabs newline space-mark tab-mark newline-mark empty trailing) )

(custom-set-faces
  '(whitespace-space ((t (:foreground "gray30"))))
  '(whitespace-empty ((t (:foreground "firebrick" :background "SlateGray1"))))
  '(whitespace-hspace ((t (:foreground "lightgray" :background "LemonChiffon3"))))
  '(whitespace-indentation ((t (:foreground "firebrick" :background "beige"))))
  '(whitespace-line ((t (:foreground "black" :background "red"))))
  '(whitespace-newline ((t (:foreground "orange" :background "gray20"))))
  '(whitespace-space-after-tab ((t (:foreground "black" :background "green"))))
  '(whitespace-space-before-tab ((t (:foreground "black" :background "DarkOrange"))))
  '(whitespace-tab ((t (:foreground "blue" :background "white"))))
  '(whitespace-trailing ((t (:foreground "red" :background "yellow"))))
  )

;; enable automatic white space cleanup when buffer is written while in whitespace mode
(setq whitespace-action '(auto-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace hook section, called on entry of whitespace mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable white space cleanup to global write hook
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Witespace mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-c_w")    'whitespace-mode                 )
(define-key global-map (kbd "C-c_t")    'whitespace-toggle-options       )
(define-key global-map (kbd "C-c=w")    'global-whitespace-mode          )
(define-key global-map (kbd "C-c=t")    'global-whitespace-toggle-options)
