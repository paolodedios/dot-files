;; -*-emacs-lisp-*-
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
 '(default-frame-alist (quote (
                               (tool-bar-lines         .        0)
                               (menu-bar-lines         .        1)
                               (foreground-color       .  "wheat")
                               (background-color       .  "black")
                               (cursor-type            .      box)
                               (cursor-color           . "purple")
                               (vertical-scroll-bars   .    right)
                               (internal-border-width  .        0)
                               )
                              )
    )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set custom color theme and font
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

 ;; custom font-lock settings
 '(font-lock-builtin-face       ((((class color) (min-colors 88) (background light)) (:foreground "lightsteelblue"   ))))
 '(font-lock-comment-face       ((((class color) (min-colors 88) (background light)) (:foreground "orangered"        ))))
 '(font-lock-doc-face           ((((class color) (min-colors 88) (background light)) (:foreground "orangered"        ))))
 '(font-lock-constant-face      ((((class color) (min-colors 88) (background light)) (:foreground "aquamarine"       ))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "lightblue"        ))))
 '(font-lock-keyword-face       ((((class color) (min-colors 88) (background light)) (:foreground "turquoise"        ))))
 '(font-lock-string-face        ((((class color) (min-colors 88) (background light)) (:foreground "tan"              ))))
 '(font-lock-type-face          ((((class color) (min-colors 88) (background light)) (:foreground "palegreen"        ))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "khaki"            ))))
 '(font-lock-warning-face       ((((class color) (min-colors 88) (background light)) (:foreground "pink" :weight bold))))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global font lock and color decorations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (global-font-lock-mode        t)
       (my-common-font-lock           )
       ;; default color scheme
       (set-background-color             "black"    )
       (set-foreground-color             "wheat"    )
       (set-cursor-color                 "purple"   )
       (set-face-foreground 'modeline    "firebrick")
       (set-face-background 'modeline    "wheat"    )
       
       ;; Maximum colors
       (setq font-lock-maximum-decoration
             '((java-mode          . t)
               (c++-mode           . t)
               (c-mode             . t)
               (objc-mode          . t)
               (javascript-mode    . t)
               (js2-mode           . t)
               (coffee-mode        . t)
               (css-mode           . t)
               (scala-mode         . t)
               (clojure-mode       . t)
               (python-mode        . t)
               (sql-mode           . t)
               (R-mode             . t)
               (emacs-lisp-mode    . t)
               (t                  . 1))
             ))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global font lock settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

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
;; Set the max buffer size (in bytes) before fontification is suppressed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default font-lock-maximum-size         8388608)

