;; -*- mode: emacs-lisp -*-
;;
;; Emacs configuration file. Configured for Emacs 23.X and later.
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;
;; M-x load-file RET init.el RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require common lisp package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure GnuTLS
;;
;; GnuTLS requires additional configuration on Emacs 25+ on macOS to prevent it
;; from crashing when loading package repositories.
;;
;; @see https://github.com/davidswelt/aquamacs-emacs/issues/133
;; @see https://github.com/davidswelt/aquamacs-emacs/issues/149
;; @see https://www.reddit.com/r/emacs/comments/8sykl1/emacs_tls_defaults_are_downright_dangerous/
;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs-gnutls/Help-For-Users.html
;;
;; @note starttls.el and tls.el have been moved to obsolete in the master branch
;; (what will be Emacs 27).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tls)

(with-eval-after-load 'tls
  ;; Add the gnutls CA certificate file
  (push "/private/etc/ssl/cert.pem"                gnutls-trustfiles)
  ;; Add the curl CA certificate file
  (push "/opt/local/share/curl/curl-ca-bundle.crt" gnutls-trustfiles)
  )

;; Validate TLS certificates
(setq gnutls-verify-error           t)

;; Increase prime bits on TLS keys
(setq gnutls-min-prime-bits      2048)

;; Network Security Module settings
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Network-Security.html
(setq network-security-level  'medium)
(setq nsm-save-host-names           t)

;; Set tls-checktrust to `'ask` instead of `t` to allow user to determine
;; whether or not to trust a certificate.
(setq tls-checktrust             'ask)

;; Update the tls-program invocation command line string
;;
;; Add `--priority` flag to  prevents the 3des certificate from being used.
;; Add `:%%PROFILE_MEDIUM` to ban intermediate SHA1 certificates.
;; Add `--ocsp` flag to require certificate revocation check
;;
;; Optionally: Add `--insecure` flag as a temporary workaround for the expired certificate
(setq tls-program
      '("gnutls-cli -p %p --dh-bits=2048 --ocsp --x509cafile=%t \
--priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%%PROFILE_MEDIUM' %h"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize packages immediately and not after init.el is read post startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup  nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log configuration loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Loading Emacs customization files via init.el.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set language constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the load path for emacs lisp customization packages
;;
;; Ensure subdirectories are scanned, since Emacs doesn't include subdirectories
;; of a directory in the load-path by default.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let*
            ((my-elisp-dir "~/.emacs.d/config") (default-directory my-elisp-dir))
          (setq load-path (cons my-elisp-dir load-path))
          (normal-top-level-add-subdirs-to-load-path)
          )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the load path for custom themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-theme-directory "~/.emacs.d/config/themes/")

;; Always trust custom themes (and don't prompt)
(setq custom-safe-themes t)

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
    (message "Loading macOS specific settings...")
    (load "modules/platform-mac")
    )

  (when (string-equal system-type "gnu/linux")
    (message "Loading GNU/Linux specific settings...")
    (load "modules/platform-unix")
    )

  (when (string-equal system-type "windows-nt")
    (message "Loading Windows specific settings...")
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
