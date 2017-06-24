;; -*- mode: emacs-lisp -*-
;;
;; Emacs configuration file. Configured for Emacs 23.X and later.
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;
;; M-x load-file RET .emacs RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading Emacs customization files via init.el.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the correct environment for bash commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq shell-file-name          "bash")
(setq shell-command-switch     "-ic" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set language constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require common lisp package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the load path for emacs lisp customization packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the load path for custom themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-theme-directory "~/.emacs.d/themes/")

;; Always trust custom themes (and don't prompt)
(setq custom-safe-themes t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure subdirectories are scanned, since Emacs doesn't include subdirectories
;; of a directory in the load-path by default.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let*
            ((my-elisp-dir "~/.emacs.d") (default-directory my-elisp-dir))
          (setq load-path (cons my-elisp-dir load-path))
          (normal-top-level-add-subdirs-to-load-path)
          )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppress function redefinition warnings from `defadvice` usage by customized
;; Emacs functions or third-party packages.
;;
;; @see http://andrewjamesjohnson.com/
;;      suppressing-ad-handle-definition-warnings-in-emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ad-redefinition-action  'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load debugger settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "modules/debug")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "modules/packages")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom configuration settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun configure-emacs ()
  (interactive)
  ;; Load package configuration files
  (load "modules/functions")
  (load "modules/key-bindings")
  (load "modules/encodings")
  (load "modules/editor")
  (load "modules/modeline")
  (load "modules/linenum")
  (load "modules/modes")
  (load "modules/backups")

  ;; Load platform specific configuration files
  (when (string-equal system-type "darwin")
    (message "Loading Mac OS X specific settings")
    (load "modules/platform-mac")
    )

  (when (string-equal system-type "gnu/linux")
    (message "Loading GNU/Linux specific settings")
    (load "modules/platform-unix")
    )

  (when (string-equal system-type "windows-nt")
    (message "Loading Windows specific settings")
    (load "modules/platform-win")
    )

  ;; Load custom theme
  (load-theme   'candycrush  t)
  (enable-theme 'candycrush   )
  )

;; Load custom package configurations after packages and init.el load. The
;; after-init-hook runs only once, when emacs starts, so in order to do a
;; hot reload of configuration files, the configure-emacs function will
;; need to be executed from a REPL session via:
;;
;;  M-x: configure-emacs
;;
(add-hook 'after-init-hook 'configure-emacs)
