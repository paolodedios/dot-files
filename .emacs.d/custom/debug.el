;; -*-emacs-lisp-*-
;;
;; Emacs/Elisp debugging
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Debugging Configuration Commands and Variables
;; ----------------------------------------------
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html#Help
;; Emacs provides extensive help features, all accessible through the help character
;;   C-h
;; Examples:
;;  C-h k key        Gives more information about a key/command: it displays the documentation
;;                   string of the command as well as its name
;;  C-h K key        Finds the documentation of a key sequence key
;;  C-h w command    Lists the keys that are bound to command.
;;  C-h f function   Describes a function. Displays the documentation of Lisp function
;;  C-h v variable   Similar to "C-h f" but describes Lisp variables instead of Lisp functions
;;
;; elisp stacktrace mode
;; ---------------------
;; M-x set-variable RET debug-on-error  RET t RET
;; M-x set-variable RET debug-on-signal RET t RET
;;
;; This should give a backtrace the next time the error happens, which may show
;; where the real problem lies.
;;
;; elisp byte compiler
;; -------------------
;; The emacs byte compiler can be used to speed up processing various elisp packages
;;
;; Example:
;;   M-x byte-compile-file RET js2.el RET
;;
;; To byte compile everything
;;   C-u 0 M-x byte-recompile-directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set to "t" instead of "nil" to enter the elisp debugger when an error occurs
;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Error-Debugging.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error      nil)
(setq debug-on-signal     nil)
(setq debug-on-quit       nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile entire .emacs or multiple elisp files loaded at startup using the
;; dope profiler
;;
;; M-x dope-quick-start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dope)
