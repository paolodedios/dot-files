;; -*- mode: emacs-lisp -*-
;;
;; Emacs configuration file. Configured for Emacs 23.X and later.
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;
;; M-x load-file RET .emacs RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading Emacs customization files...")

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
;; Suppress dfunction redefinition warnings from `defadvice` usage by customized
;; Emacs functions or third-party packages.
;;
;; @see http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ad-redefinition-action 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Emacs 24 package management library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("gnu"          . "http://elpa.gnu.org/packages/"       ) t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"   ) t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"          ) t)
(add-to-list 'package-archives '("marmalade"    . "https://marmalade-repo.org/packages/") t)

(package-initialize)

(defvar local-package-deps
  '(dash
    deferred
    epc
    magit
    git-gutter+
    git-gutter-fringe+
    )
  "Packages to ensure are installed on startup.")

(defun local-package-deps-installed-p ()
  (loop for p in local-package-deps
        when (not (package-installed-p p)) do (return nil)
        finally (return t)
        )
  )

(unless (local-package-deps-installed-p)
  ;; Check for new packages (package versions)
  (message "%s" "Refreshing package database and checking for updates...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p local-package-deps)
    (when (not (package-installed-p p))
      (message "Installing package : %s" p)
      (package-install p)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom configuration settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "custom/debug")
(load "custom/functions")
(load "custom/key-bindings")
(load "custom/global")
(load "custom/theme")
(load "custom/backups")
(load "custom/modes")

(when (string-equal system-type "darwin")
  ;; Load Mac OS specific settings
  (message "Loading Mac OS X specific settings")
  (load "custom/platform-mac")
 )

(when (string-equal system-type "gnu/linux")
  ;; Load Unix/Linux specific settings
  (message "Loading GNU/Linux specific settings")
  (load "custom/platform-unix")
 )

(when (string-equal system-type "windows-nt")
  ;; Load Windows specific settings
  (message "Loading Windows specific settings")
  (load "custom/platform-win")
 )
