;; -*- mode: emacs-lisp -*-
;;
;; SQL mode
;; @see https://github.com/alex-hhh/emacs-sql-indent
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

(defvar my-sql-indentation-offsets-alist
  `( ;; Put new syntactic symbols here, and add the default ones at the end.
     ;; If there is no value specified for a syntactic symbol, the default
     ;; will be picked up.
    ,@sqlind-default-indentation-offsets-alist))

;; Arrange for the new indentation offset to be set up for each SQL buffer.
(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-indentation-offsets-alist
                  my-sql-indentation-offsets-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sql-mode-hook          'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.sql$"      . sql-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.plsql$"    . sql-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mysql$"    . sql-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.epl$"      . sql-mode          )) auto-mode-alist))
