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
  '((auto-complete                   . "melpa-stable")
    (dash                            . "melpa"       )
    (deferred                        . "melpa-stable")
    (dropdown-list                   . "melpa"       )
    (epc                             . "melpa-stable")
    (git-gutter+                     . "melpa"       )
    (git-gutter-fringe+              . "melpa"       )
    (highlight-current-line          . "melpa"       )
    (linum-off                       . "melpa"       )
    (lorem-ipsum                     . "melpa"       )
    (paradox                         . "melpa-stable")
    (paredit                         . "melpa-stable")
    (pcre2el                         . "melpa"       )
    (powerline                       . "melpa-stable")
    (regex-dsl                       . "melpa"       )
    (s                               . "melpa-stable")
    (visual-regexp                   . "melpa"       )
    (visual-regexp-steroids          . "melpa"       )
    (wc-goal-mode                    . "melpa"       )
    ;; Major and minor mode packages
    (clojure-mode                    . "melpa-stable")
    (clojure-mode-extra-font-locking . "melpa-stable")
    (coffee-mode                     . "melpa-stable")
    (dockerfile-mode                 . "melpa"       )
    (format-sql                      . "melpa"       )
    (gist                            . "melpa"       )
    (go-mode                         . "melpa-stable")
    (groovy-mode                     . "melpa"       )
    (js2-mode                        . "melpa-stable")
    (js2-highlight-vars              . "melpa-stable")
    (json-mode                       . "melpa-stable")
    (json-reformat                   . "melpa-stable")
    (json-snatcher                   . "melpa-stable")
    (less-css-mode                   . "melpa-stable")
    (magit                           . "melpa-stable")
    (markdown-mode                   . "melpa-stable")
    (matlab-mode                     . "melpa"       )
    (mmm-mode                        . "melpa-stable")
    (php-mode                        . "melpa-stable")
    (python-mode                     . "melpa"       )
    (psgml                           . "marmalade"   )
    (rainbow-mode                    . "gnu"         )
    (sass-mode                       . "melpa-stable")
    (scala-mode                      . "melpa"       )
    (scss-mode                       . "melpa-stable")
    (sql-indent                      . "melpa"       )
    (tuareg                          . "melpa"       )
    (web-mode                        . "melpa-stable")
    (yaml-mode                       . "melpa-stable")
    (yasnippet                       . "melpa-stable")
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
