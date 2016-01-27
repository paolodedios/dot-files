;; -*- mode: emacs-lisp -*-
;;
;; DTD mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTD mode for editing SGML-DTDs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'dtd-mode  "tdtd" "Major mode for SGML and XML DTDs."             )
(autoload 'dtd-etags "tdtd" "Execute etags on FILESPEC"                    t)
(autoload 'dtd-grep  "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTD hook section, called on entry of dtd-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'dtd-mode-hooks         'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.dtd$"      . dtd-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.dcl$"      . dtd-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.dec$"      . dtd-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ele$"      . dtd-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ent$"      . dtd-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.mod$"      . dtd-mode          )) auto-mode-alist))
