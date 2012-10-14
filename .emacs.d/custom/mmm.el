;; -*-emacs-lisp-*-
;;
;; MMM (Multiple Major Modes) mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the updated mmm-library locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mmm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the multi-mode library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'multi-mode "multi-mode" "Allowing multiple major modes in a buffer" t)

(setq mmm-global-mode 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up a multi major mode group for each markup type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HTML with inline Javascript markup mode groups
(mmm-add-group
 'html-js
 '(
   (js-script
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-script-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-cdata-only
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode javascript-mode
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
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<body[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</body>"
    :insert ((?j js-tag nil @ "<method type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</method>" @))
    )
   (js-methodbody
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<body[^>]*>[ \t]*\n?"
    :back "[ \t]*</body>"
    :insert ((?j js-tag nil @ "<method type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</method>" @))
    )
   (js-script-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-script
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-getter-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<getter[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</getter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-setter-cdata
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<setter[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*</setter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-getter
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<getter[^>]*>[ \t]*\n?"
    :back "[ \t]*</getter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-setter
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<setter[^>]*>[ \t]*\n?"
    :back "[ \t]*</setter>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-cdata-only
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*]]>[ \t\n]*"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">"
                 @ "\n" _ "\n" @ "</script>" @))
    )
   (js-inline
    :submode javascript-mode
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
(mmm-add-mode-ext-class  'sgml-mode  "\\.php[34]?\\'"  'html-php )
(mmm-add-mode-ext-class  'sgml-mode  "\\.jsp\\'"       'html-jsp )
(mmm-add-mode-ext-class  'sgml-mode  "\\.html\\'"      'html-js  )
(mmm-add-mode-ext-class  'sgml-mode  "\\.vm\\'"        'html-js  )
(mmm-add-mode-ext-class  'xml-mode   "\\.xul\\'"       'xul-js   )
(mmm-add-mode-ext-class  'xml-mode   "\\.xml\\'"       'xul-js   )
(mmm-add-mode-ext-class  'xml-mode   "\\.xbl\\'"       'xul-js   )



