;; -*- mode: emacs-lisp -*-
;;
;; MMM (Multiple Major Modes) mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the updated mmm-library locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mmm-auto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the multi-mode library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'multi-mode "multi-mode" "Allowing multiple major modes in a buffer" t)

(setq mmm-global-mode               'auto)
(setq mmm-submode-decoration-level      2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up a multi major mode group for each markup type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HTML with inline Javascript markup mode groups
(mmm-add-group
 'html-js
 '(
   (js-script
    :submode js
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-script-cdata
    :submode js
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-cdata-only
    :submode js
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode js
    :face mmm-code-submode-face
    :front "on\w+=\""
    :back "\""
    )
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @))
    )
   )
 )

;; HTML with inline CSS markup mode groups
(mmm-add-group
 'html-css
 '(
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @))
    )
   )
 )
;; PHP with inline HTML markup mode groups
(mmm-add-group
 'html-php
 '(
   (html-php-tagged
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")
   )
 )

;; Mozilla XUL markup mode groups
(mmm-add-group
 'xul-js
 '(
   (js-methodbody-cdata
    :submode js
    :face mmm-code-submode-face
    :front "<body[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</body>"
    :insert ((?j js-tag nil @ "<method type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</method>" @))
    )
   (js-methodbody
    :submode js
    :face mmm-code-submode-face
    :front "<body[^>]*>[ \t]*\n?"
    :back "[ \t]*</body>"
    :insert ((?j js-tag nil @ "<method type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</method>" @))
    )
   (js-script-cdata
    :submode js
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-script
    :submode js
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-getter-cdata
    :submode js
    :face mmm-code-submode-face
    :front "<getter[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</getter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-setter-cdata
    :submode js
    :face mmm-code-submode-face
    :front "<setter[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</setter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-getter
    :submode js
    :face mmm-code-submode-face
    :front "<getter[^>]*>[ \t]*\n?"
    :back "[ \t]*</getter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-setter
    :submode js
    :face mmm-code-submode-face
    :front "<setter[^>]*>[ \t]*\n?"
    :back "[ \t]*</setter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-cdata-only
    :submode js
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode js
    :face mmm-code-submode-face
    :front "on\w+=\""
    :back "\"")
   )
 )


;; JSP markup mode groups
(mmm-add-group
 'html-jsp
 '(
   (html-jsp
    :submode java-mode
    :match-face (("<%!" . mmm-declaration-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[!=]?"
    :back "%>"
    :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
             (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
             (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (jsp-directive
    :submode text-mode
    :face mmm-special-submode-face
    :front "<%@"
    :back "%>"
    :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
    )
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the javascript-mode package (for mmm-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'js)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript style (for mmm-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-js-mode-programming-style
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

    ) "My Javascript Programming Style")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set js-mode MMM submode indentation function override
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js-mode indentation breaks under Emacs 22 and MMM
;; This is a custom indentation function for exclusive use with
;; js-mode in MMM

(defun my-js-mmm-indent-sexp ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)

      (save-excursion
        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2))))
        )

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset))

      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript hook section, called on entry of js-mode in MMM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'js-mode-hook
          '(lambda ()
             (require 'js)
             ;; toggle major mode editor options
             (show-paren-mode                        t)
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (subword-mode                           1)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)
             (set (make-local-variable 'indent-line-function) 'my-js-mmm-indent-sexp)
             ;; set programming style and force c-mode so that Emacs allows c-set-style
             (c-add-style "sourcery" my-js-mode-programming-style t)
             (c-set-style "sourcery")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMM hook section, called on entry of mmm-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind modes with mmm groups
(add-to-list 'mmm-mode-ext-classes-alist '(xml-mode   nil   xul-js   ))
(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode  nil   html-php ))
(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode  nil   html-jsp ))
(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode  nil   html-js  ))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode  nil   html-js  ))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode  nil   html-css ))

;; What features should be turned on in this multi-mode
(mmm-add-mode-ext-class  'sgml-mode  "\\.html\\'"      'html-js  )
(mmm-add-mode-ext-class  'sgml-mode  "\\.vm\\'"        'html-js  )
(mmm-add-mode-ext-class  'xml-mode   "\\.xul\\'"       'xul-js   )
(mmm-add-mode-ext-class  'xml-mode   "\\.xml\\'"       'xul-js   )
(mmm-add-mode-ext-class  'xml-mode   "\\.xbl\\'"       'xul-js   )
