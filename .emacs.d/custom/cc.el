;; -*-emacs-lisp-*-
;;
;; C/C++ mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode. Esp for identation settings, require CC-mode
;; Uses latest cc-mode in vendor/cc
;; @see http://cc-mode.sourceforge.net/index.php
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode"           t)
(autoload 'c-mode    "cc-mode" "C Editing Mode"             t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode"   t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define Programming constants
;;
;; Note - default code identation styles
;; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linux, python, java
;;
;; auto identation constant definition
;; explicit declaration of all syntactic attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-cc-mode-programming-style
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
                            ((access-label                        . -2)
                             (inline-open                         .  0)
                             (substatement-open                   .  0)
                             (statement-block-intro               .  +)
                             (block-close                         .  0)
                             (do-while-closure                    .  0)
                             (case-label                          .  *)
                             (statement-case-intro                .  +)
                             (statement-cont c-lineup-cascaded-calls +)
                             (stream-op                           . c-lineup-streamop)
                             ;; Don't indent inside namespaces, extern, etc
                             (incomposition                        . 0)
                             (inextern-lang                        . 0)
                             (inmodule                             . 0)
                             (innamespace                          . 0)
                             ;; Preprocessor macros
                             (cpp-define-intro                  c-lineup-cpp-define +)
                             (cpp-macro                            . [ 0 ])
                             (cpp-macro-cont                       . +)
                             ))
    (c-lineup-math                   1)
    (c-lineup-inexpr-block           1)

    ) "My C/C++ Programming Style")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ mode hook section, called on entry of C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function to lineup stream operators in c++
(defun c-lineup-streamop (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
    (goto-char (match-beginning 0))
    (vector (current-column))
    )
  )

(add-hook 'c++-mode-hook
          '(lambda ()
             ;; toggle major mode editor options
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)
             ;; bind buffer local keys
             (local-set-key (quote [C-f12]) (quote compile))
             (local-set-key "" (quote compile))
             (local-set-key (quote [C-f11]) (quote gdb))
             ;; set programming style
             (c-add-style "my-cc-mode-programming-style" my-cc-mode-programming-style t)
             (c-set-style "my-cc-mode-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C hook section, called on entry of C mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook
          '(lambda ()
             ;; toggle major mode editor options
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)
             ;; bind buffer local keys
             (local-set-key (quote [C-f12]) (quote compile))
             (local-set-key "" (quote compile))
             (local-set-key (quote [C-f11]) (quote gdb))
             ;; set programming style
             (c-add-style "my-cc-mode-programming-style" my-cc-mode-programming-style t)
             (c-set-style "my-cc-mode-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objective-C hook section, called on entry of C mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'objc-mode-hook
          '(lambda ()
             ;; toggle major mode editor options
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)
             ;; bind buffer local keys
             (local-set-key (quote [C-f12]) (quote compile))
             (local-set-key "" (quote compile))
             (local-set-key (quote [C-f11]) (quote gdb))
             ;; set programming style
             (c-add-style "my-cc-mode-programming-style" my-cc-mode-programming-style t)
             (c-set-style "my-cc-mode-programming-style")
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set C-mode font lock extra types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mozilla/NSPR specific extra font-lock data types

(setq c-font-lock-extra-types
      (append
       '("nsresult"
         "PRStatus"
         "PRBool"
         "PRPackedBool"
         "PRFloat64"
         "PRIntn"
         "PRUWord"
         "PRInt8"
         "PRInt16"
         "PRInt32"
         "PRInt64"
         "PRUint"
         "PRUint8"
         "PRUint16"
         "PRUnichar"
         "PRUint32"
         "PRUint64"
         "PLOptStatus"
         "PLOptState"
         "PRThread"
         "PRThreadType"
         "PRThreadScope"
         "PRThreadState"
         "PRThreadPriority"
         "PRLock"
         "PRCondVar"
         "PRMonitor"
         "PRRWLock"
         "PRDir"
         "PRPollDesc"
         "PRFileDesc"
         "PRIOMethods"
         "PRFilePrivate"
         "PRDescIdentity"
         "PRFileInfo"
         "PRFileInfo64"
         "PRFileType"
         "PRNetAddr"
         "PRIPv6Addr"
         "PRSocketOptionData"
         "PRSockOption"
         "PRLinger"
         "PRMcastRequest"
         "PRFileMap"
         "PRHostEnt"
         "PRProtoEnt"
         "PRIntervalTime"
         "PRTime"
         "PRTimeParameters"
         "PRExplodedTime"
         "prbitmap_t"
         "PRCList"
         "PRLibrary"
         "PRStaticLinkTable"
         "PRProcess"
         "PRProcessAttr"
         "PRSysInfo"
         "PRCounterHandle"
         "PRJobIoDesc"
         "PRJobFn"
         "PRThreadPool"
         "PRJob"
         "PLHashEntry"
         "PLHashTable"
         "PLHashNumber"
         "PLHashFunction"
         "PLHashComparator"
         "PLHashEnumerator"
         "PLHashAllocOps"
         "PRCallOnceType"
         )
       c-font-lock-extra-types)
      )



;; Win32 specific extra font-lock data types

(setq c-font-lock-extra-types
      (append
       '("LPC?\\(W\\|T\\|OLE\\)?\\STR" "HRESULT"
         "BOOL"
         "BYTE"
         "DWORD"
         "SOCKET"
         "idl_char"
         "idl_boolean"
         "idl_byte"
         "idl_\\(short\\|long\\)_float"
         "idl_u?\\(small\\|short\\|long\\)_int"
         "boolean32"
         "unsigned\\(32\\|16\\)"
         "SAFEARRAY"
         "boolean"
         "UINT"
         "ULONG"
         "VARIANT"
         )
       c-font-lock-extra-types)
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set C++ mode font lock extra types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c++-font-lock-extra-types
      (append
       c-font-lock-extra-types
       c++-font-lock-extra-types
       )
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook            'turn-on-font-lock)
(add-hook 'objc-mode-hook         'turn-on-font-lock)
(add-hook 'c++-mode-hook          'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.c$"        . c-mode            )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.m$"        . objc-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.h$"        . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.hpp$"      . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.H$"        . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cpp$"      . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cxx$"      . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.C$"        . c++-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.idl$"      . c++-mode          )) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.ac$"       . makefile-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mk$"       . makefile-mode     )) auto-mode-alist))
(setq auto-mode-alist (append '(("Makefile$"    . makefile-mode     )) auto-mode-alist))
