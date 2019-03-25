;; -*- mode: emacs-lisp -*-
;;
;; SQL mode
;; @see https://github.com/alex-hhh/emacs-sql-indent
;; @see https://github.com/Trevoke/sqlup-mode.el
;; @see https://emacs.stackexchange.com/a/19805
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load sql-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sql)
(require 'sql-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL hook section, called on entry of SQL mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-indent Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-sql-indentation-offsets-alist
  `( ;; Put new syntactic symbols here, and add the default ones at the end.
     ;; If there is no value specified for a syntactic symbol, the default
     ;; will be picked up.
    ,@sqlind-default-indentation-offsets-alist))

;; Arrange for the new indentation offset to be set up for each SQL buffer.
(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            ;; Set tab-width to 4 spaces
            (setq sqlind-basic-offset           4 )
            (setq sqlind-indentation-offsets-alist
                  my-sql-indentation-offsets-alist)))

;; Enable sqlind-minor-mode when sql-mode major mode is enabled
(add-hook 'sql-mode-hook               'sqlind-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sqlup-mode Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook               'sqlup-mode)

;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook   'sqlup-mode)

;; Set a global keyword to use sqlup on a region
(global-set-key (kbd "C-c u")          'sqlup-capitalize-keywords-in-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sql-mode-hook               'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.sql$"      . sql-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.plsql$"    . sql-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mysql$"    . sql-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.epl$"      . sql-mode          )) auto-mode-alist))
