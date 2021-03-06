;; -*- mode: emacs-lisp -*-
;;
;; Java mode
;; @see http://emacswiki.org/emacs/JavaDevelopmentEnvironment
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java mode. Esp for identation settings, require Java-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)

(autoload 'java-mode "cc-mode" "Java Editing Mode"          t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Programming constants
;;
;; Note - default code identation styles
;; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linux, python, java
;;
;; @see http://www.gnu.org/software/emacs/manual/html_node/ccmode/Indentation-Engine-Basics.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java mode style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-java-mode-programming-style
  ;; hanging brace setup
  '((c-hanging-braces-alist .
                            ((brace-list-open                    after)
                             (brace-entry-open                   after)
                             (substatement-open                 before)
                             (block-close            . c-snug-do-while)
                             (extern-lang-open                   after)
                             (inexpr-class-open                  after)
                             (inexpr-class-close                before)
                             ))
    ;; cleanup shortcuts
    (c-cleanup-list         .
                            ((brace-else-brace                        )
                             (brace-elseif-brace                      )
                             (brace-catch-brace                       )
                             (list-close-comma                        )
                             ))
    ;; indentation offsets
    ;; +   'c-basic-offset' times    1
    ;; -   'c-basic-offset' times   -1
    ;; ++  'c-basic-offset' times    2
    ;; --  'c-basic-offset' times   -2
    ;; *   'c-basic-offset' times  0.5
    ;; /   'c-basic-offset' times -0.5
    (c-offsets-alist        .
                            ((access-label                         . 0)
                             (inline-open                          . 0)
                             (substatement-open                    . 0)
                             (statement-block-intro                . +)
                             (block-close                          . 0)
                             (do-while-closure                     . 0)
                             (case-label                           . +)
                             (statement-case-intro                 . +)
                             (statement-cont c-lineup-cascaded-calls +)
                             (stream-op                            . c-lineup-streamop)
                             ))
    (c-lineup-math                   1)
    (c-lineup-inexpr-block           1)

    ) "My Java Programming Style")


(defconst my-knr13-java-mode-programming-style
  ;; hanging brace setup
  '((c-hanging-braces-alist .
                            ((brace-list-open                    after)
                             (brace-entry-open                   after)
                             (substatement-open                  after)
                             (block-close            . c-snug-do-while)
                             (extern-lang-open                   after)
                             (inexpr-class-open                  after)
                             (inexpr-class-close                before)

                             (defun-open                         after)
                             (defun-close                        after)
                             (class-open                         after)
                             (class-close                        after)
                             (inline-open                        after)
                             (inline-close                       after)
                             ))
    ;; cleanup shortcuts
    (c-cleanup-list         .
                            ((brace-else-brace                        )
                             (brace-elseif-brace                      )
                             (brace-catch-brace                       )
                             (list-close-comma                        )
                             ))
    ;; indentation offsets
    ;; +   'c-basic-offset' times    1
    ;; -   'c-basic-offset' times   -1
    ;; ++  'c-basic-offset' times    2
    ;; --  'c-basic-offset' times   -2
    ;; *   'c-basic-offset' times  0.5
    ;; /   'c-basic-offset' times -0.5
    (c-offsets-alist        .
                            ((access-label                         . 0)
                             (inline-open                          . 0)
                             (substatement-open                    . 0)
                             (statement-block-intro                . +)
                             (block-close                          . 0)
                             (do-while-closure                     . 0)
                             (case-label                           . +)
                             (statement-case-intro                 . +)
                             (statement-cont c-lineup-cascaded-calls +)
                             (stream-op                            . c-lineup-streamop)
                             ))
    (c-lineup-math                   1)
    (c-lineup-inexpr-block           1)

    ) "My K&R Java Programming Style")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java mode hack to get a little better Java 5+ style support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* (
       ;; declare keywords
       (java-keywords
        (eval-when-compile
          (regexp-opt
           '("catch"
             "do"
             "else"
             "super"
             "this"
             "finally"
             "for"
             "if"
             "implements"
             "extends"
             "throws"
             "instanceof"
             "new"
             "interface"
             "return"
             "switch"
             "throw"
             "try"
             "while")
           )
          )
        )
       ;; classes immediately followed by an object name.
       (java-type-names
        `(mapconcat 'identity
                    (cons
                     ,(eval-when-compile
                        (regexp-opt '("boolean"
                                      "char"
                                      "byte"
                                      "short"
                                      "int"
                                      "long"
                                      "float"
                                      "double"
                                      "void"
                                      )
                                    )
                        )
                     java-font-lock-extra-types)
                    "\\|"
                    )
        )
       (java-type-names-depth `(regexp-opt-depth ,java-type-names))

       ;; These are eventually followed by an object name.
       (java-type-specs
        (eval-when-compile
          (regexp-opt
           '("abstract"
             "const"
             "final"
             "synchronized"
             "transient"
             "static"
             "volatile"
             "enum"
             "public"
             "private"
             "protected"
             "native"
             "strictfp"
             )
           )
          )
        )
       )

  (setq java-font-lock-keywords-3
        (append
         (list
          ;; support static import statements
          '("\\<\\(import\\)\\>\\s-+\\(static\\)\\s-+\\(\\sw+\\)"
            (1 font-lock-keyword-face)
            (2 font-lock-keyword-face)
            (3 (if (equal (char-after (match-end 0)) ?\.)
                   'jde-java-font-lock-package-face
                 'font-lock-type-face))
            ("\\=\\.\\(\\sw+\\)" nil nil
             (1 (if (and (equal (char-after (match-end 0)) ?\.)
                         (not (equal (char-after (+ (match-end 0) 1)) ?\*)))
                    'jde-java-font-lock-package-face
                  'font-lock-type-face)
                )
             )
            )
          )

         java-font-lock-keywords-2

         ;; More complicated regexps for more complete highlighting for types.
         ;; We still have to fontify type specifiers individually, as Java is hairy.
         (list
          ;; Fontify class names with ellipses
          `(eval .
                 (cons (concat "\\<\\(" ,java-type-names "\\)\\>\\.\\.\\.[^.]")
                       '(1 font-lock-type-face)
                       )
                 )
          ;; Fontify random types immediately followed by an item or items.
          `(eval .
                 (list (concat "\\<\\(\\(?:" ,java-type-names "\\)"
                               "\\(?:\\(?:<.*>\\)\\|\\>\\)\\(?:\\.\\.\\.\\)?\\)"
                               "\\([ \t]*\\[[ \t]*\\]\\)*"
                               "\\([ \t]*\\sw\\)")
                       ;; Fontify each declaration item.
                       (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                             ;; Start and finish with point after the type specifier.
                             (list 'goto-char (list 'match-beginning
                                                    (+ ,java-type-names-depth 3)))
                             (list 'goto-char (list 'match-beginning
                                                    (+ ,java-type-names-depth 3)))
                             ;; Fontify as a variable or function name.
                             '(1 (if (match-beginning 2)
                                     font-lock-function-name-face
                                   font-lock-variable-name-face)
                                 )
                             )
                       )
                 )
          ;; Fontify those that are eventually followed by an item or items.
          (list (concat "\\<\\(" java-type-specs "\\)\\>"
                        "\\([ \t]+\\sw+\\>"
                        "\\([ \t]*\\[[ \t]*\\]\\)*"
                        "\\)*")
                ;; Fontify each declaration item.
                '(font-lock-match-c-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  (goto-char (or (match-beginning 5) (match-end 1)))
                  ;; Finish with point after first type specifier.
                  (goto-char (match-end 1))
                  ;; Fontify as a variable or function name.
                  (1 (if (match-beginning 2)
                         font-lock-function-name-face
                       font-lock-variable-name-face
                       )
                     )
                  )
                )
          )
         )
        )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java hook section, called on entry of Java mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'java-mode-hook
          '(lambda ()
             ;; toggle major mode editor options
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (show-paren-mode                        t)
             (subword-mode                           1)
             (setq fill-column                     100)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)
             (auto-fill-mode                         1)
             ;; set programming style
             (c-add-style "sourcery" my-java-mode-programming-style        t)
             (c-add-style "knr13"    my-knr13-java-mode-programming-style  t)
             ;; default style is modified K&R
             (c-set-style "sourcery")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'java-mode-hook         'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.java$"     . java-mode         )) auto-mode-alist))
