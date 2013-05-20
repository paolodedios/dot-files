;; -*- mode: emacs-lisp -*-
;;
;; Custom color theme
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable color-themes for Emacs 23.X
;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-java.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(default-frame-alist '(
                         (tool-bar-lines         .        0)
                         (menu-bar-lines         .        1)
                         (foreground-color       .  "wheat")
                         (background-color       .  "black")
                         (cursor-type            .      box)
                         (cursor-color           . "yellow")
                         (vertical-scroll-bars   .    right)
                         (internal-border-width  .        0)
                         )
    )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default color theme and font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 '(custom-mode-default ((t (:inherit        autoface-default
                            :background     "black"
                            :foreground     "wheat"
                            :strike-through  nil
                            :underline       nil
                            :slant           normal
                            :weight          normal
                            :height          120
                            :width           normal
                            :family          "Monaco")))  t)

 ;; text mode files must inherit the custom-mode-default setting
 '(text-mode-default ((t (:inherit autoface-default))) t)

 ;; highlight current line settings
 '(highlight-current-line-face ((t (:background "gray10"))))

 ;; line number column custom face
 '(linum ((t (:inherit (shadow default) :background "gray10" :foreground "dimgray"))))

 ;; mode line active face
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))

 ;; mode line inactive face
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set common font lock and color decorations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

(defun my-common-font-lock()
  "font-lock settings"
  (setq-default
   font-lock-auto-fontify                              t
   font-lock-use-fonts                                 nil
   font-lock-use-maximal-decoration                    t
   font-lock-use-colors                                t
   font-lock-mode-enable-list                          nil
   font-lock-mode-disable-list                         nil
   )

  ;; set-font-lock color scheme
  (set-face-foreground 'font-lock-builtin-face         "lightsteelblue")
  (set-face-foreground 'font-lock-comment-face         "orangered"     )
  (set-face-foreground 'font-lock-doc-face             "orangered"     )
  (set-face-foreground 'font-lock-constant-face        "aquamarine"    )
  (set-face-foreground 'font-lock-function-name-face   "lightblue"     )
  (set-face-foreground 'font-lock-keyword-face         "turquoise"     )
  (set-face-foreground 'font-lock-string-face          "tan"           )
  (set-face-foreground 'font-lock-type-face            "palegreen"     )
  (set-face-foreground 'font-lock-variable-name-face   "khaki"         )
  (set-face-foreground 'font-lock-warning-face         "pink"          )
  )

(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode           t)
       (my-common-font-lock              )

       ;; Maximum colors
       (setq font-lock-maximum-decoration
             '(
               (java-mode             . t)
               (scala-mode            . t)
               (clojure-mode          . t)
               (groovy-mode           . t)
               (jflex-mode            . t)
               (c++-mode              . t)
               (c-mode                . t)
               (objc-mode             . t)
               (javascript-mode       . t)
               (js2-mode              . t)
               (coffee-mode           . t)
               (css-mode              . t)
               (python-mode           . t)
               (php-mode              . t)
               (sql-mode              . t)
               (R-mode                . t)
               (matlab-mode           . t)
               (lisp-mode             . t)
               (emacs-lisp-mode       . t)
               (t                     . 1)
               )
             ))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 22.X and later use the new jit-lock package for fontification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'jit-lock)

(setq font-lock-support-mode          'jit-lock-mode)

(setq lazy-lock-defer-on-scrolling               nil)
(setq lazy-lock-defer-time                         1)
(setq lazy-lock-stealth-time                      20)
(setq lazy-lock-stealth-lines                     25)
(setq lazy-lock-stealth-verbose                  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set buffer menu highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ; Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)       ; Read only
        )
      )


(defun buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
         (lambda (start end)
           (remove-text-properties start end '(font-lock-face nil))
           )
         )
        )
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
         '(buffer-menu-buffer-font-lock-keywords t)
         )
    (font-lock-fontify-buffer)
    )
  )

(add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the max buffer size (in bytes) before fontification is suppressed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default font-lock-maximum-size         8388608)
