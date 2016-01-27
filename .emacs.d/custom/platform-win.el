;; -*- mode: emacs-lisp -*-
;;
;; Windows specific settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows specific encoding system configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; On Windows, it's best to avoid (set-selection-coding-system 'utf-8) as
;; it alters the clipboard behavior in Emacs. The correct value for
;; selection-coding-system (aka clipboard-coding-system) on Windows
;; should be 'utf-16-le in order to allow Emacs to interoperate with othe
;; Unicode programs
(set-selection-coding-system      'utf-16-le)
