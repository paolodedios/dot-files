;; -*- mode: emacs-lisp -*-
;;
;; Emacs package management
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Initialize packages immediately and not after init.el is read post startup
(setq package-enable-at-startup  nil)
(package-initialize)

;; Declare package archive repositories
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/"      )
        ("melpa-stable" . "http://stable.melpa.org/packages/"  )
        ("melpa"        . "http://melpa.org/packages/"         )
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        )
      )


;; Declare packages to install and select which repositories
;; to source them from.
(defvar local-pinned-package-deps
  '((dash                  . "melpa"       )
    (deferred              . "melpa-stable")
    (epc                   . "melpa-stable")
    (git-gutter+           . "melpa"       )
    (git-gutter-fringe+    . "melpa"       )
    (magit                 . "melpa-stable")
    )
  )


;; Derive list of packages to install from the (package . repo) list
;; declared above.
(defvar local-package-deps (mapcar 'car local-pinned-package-deps))


;; On Emacs 24.4 or newer, use the pinned package feature to select
;; which repository to request packages from and enforce the
;; (package . repo) mapping declared above
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages local-pinned-package-deps)
  )

;; Check if any package on the list is not installed. Returns t if
;; the entire list is installed, nil otherwise.
(defun local-package-deps-installed-p ()
  (loop for p in local-package-deps
        when (not (package-installed-p p)) do (return nil)
        finally (return t)
        )
  )

;; Check for new packages (package versions) and install missing
;; packages from the list declared above.
(unless (local-package-deps-installed-p)
  (message "%s" "Refreshing package database and checking for updates...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p local-package-deps)
    (when (not (package-installed-p p))
      (message "Installing package : %s" p)
      (package-install p)
      )
    )
  )
