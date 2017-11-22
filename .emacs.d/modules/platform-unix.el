;; -*- mode: emacs-lisp -*-
;;
;; Unix specific settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux specific encoding system configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-selection-coding-system      'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Platform Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the correct environment for bash commands
(setq shell-file-name             "bash")
(setq shell-command-switch        "-ic" )

;; Inherit the $PATH environment variable from shell configuration
;;
;; Due to the special way Gnome starts GUI programs a windowed Emacs instance
;; such as Aquamacs does not inherit the environment variables from the shell
;; configuration. As a result, Emacs will lack some important entries specified
;; in the $PATH, most notably /usr/local/bin/, where Homebrew, NPM and many
;; other package managers put binaries in.
;;
;; The exec-path-from-shell works around this issue by extracting environment
;; variables from a shell session and injecting them into the environment of
;; the running Emacs instance.
;;
;; @see https://github.com/purcell/exec-path-from-shell
;;
(exec-path-from-shell-initialize)
