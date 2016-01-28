;; -*- mode: emacs-lisp -*-
;;
;; Javascript mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced JSON mode for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'json-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced js2-mode package for .js file editing.
;;
;; @see https://github.com/mooz/js2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'js2-mode "js2-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode imenu support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'js2-mode
  '(progn
     (require 'js2-imenu-extras)
     (js2-imenu-extras-setup)
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode on top of js2-mode which highlights all occurrences of the variable
;; under the cursor within its defining scope.
;; When variables are highlighted, you can use the following key bindings:
;;
;;  M-n or C-<down> - move to the next occurrence
;;  M-p or C-<up> - move to the previous occurrence
;;  M-r - rename the variable; this will ask you for confirmation for each
;;  occurrence, but you can press "!" to rename all.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'js2-highlight-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With c-mode there is a key binding M-C-q which indents the block starting with
;; the paren under the cursor.  The following function implements this generically
;; and can be used in js2-mode
;;
;; Unlike c-indent-exp, my-js2-indent-sexp above does not require the cursor to be over
;; a paren.  It looks up the innermost block using (syntax-ppss) which is a neat function
;; provided by Emacs, and reindents it.  It also highlights the block for half a second.

(defun my-js2-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript hook section, called on entry of js2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'js2-mode-hook
          '(lambda ()
             (require 'js)
             (setq mode-name                     "js2")
             ;; toggle major mode editor options; unlike other major modes,
             ;; js2-mode is not a cc-mode derivative
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (subword-mode                           1)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)

             ;; js2-mode specific options
             (setq js2-use-font-lock-faces           t)
             (setq js2-highlight-level               3)
             (setq js2-basic-offset                  4)
             (setq js2-idle-timer-delay            0.1)

             (setq js2-global-externs '("module"
                                        "require"
                                        "assert"
                                        "setTimeout"
                                        "clearTimeout"
                                        "setInterval"
                                        "clearInterval"
                                        "location"
                                        "__dirname"
                                        "console"
                                        "JSON"
                                        )
                   )

             ;; delegate syntax checking to external program like flycheck
             (setq js2-show-parse-errors                    nil)
             (setq js2-strict-missing-semi-warning          nil)
             (setq js2-strict-trailing-comma-warning          t)
             (setq js2-missing-semi-one-line-override         t)

             ;; indentation options for Emacs 24.5 and earlier.
             ;; all other indentation code has been ported to
             ;; js-mode for Emacs 25 and later.
             ;;
             ;; @see https://github.com/mooz/js2-mode/blob/master/js2-old-indent.el
             ;; for additional commentary on identation implementation.
             ;;
             ;; @see https://lists.gnu.org/archive/html/emacs-elpa-diffs/2015-07/msg00047.html
             ;; for commits migrating from js2-indent to js-indent
             ;;
             ;; @se https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/js.el
             ;; for list of new and migrated indent options
             (setq js2-indent-switch-body                     t)
             (setq js2-pretty-multiline-declarations   'dynamic)

             ;; enable highlight-vars-mode, which automatically highlights
             ;; all occurrences of the variable under the cursor within its
             ;; defining scope
             (if (featurep 'js2-highlight-vars)
                 (js2-highlight-vars-mode)
               )

             ;; bind buffer local keys
             (define-key js2-mode-map [(control meta |)]      'cperl-lineup             )
             (define-key js2-mode-map [return]                'newline-and-indent       )
             (define-key js2-mode-map [backspace]             'c-electric-backspace     )
             (define-key js2-mode-map [(control d)]           'c-electric-delete-forward)
             (define-key js2-mode-map [(control meta q)]      'my-js2-indent-sexp       )
             (define-key js2-mode-map [(control meta \;)]
               '(lambda()
                  (interactive)
                  (insert "/* ")
                  (save-excursion
                    (insert "  */"))
                  ))
             )
          )


;; Instead of using js2 as a major mode, it can optionally be
;; loaded as a minor mode to js-mode
;; (add-hook 'js-mode-hook  'js2-minor-mode)

;; @see https://github.com/ScottyB/ac-js2
;; (add-hook 'js2-mode-hook 'ac-js2-mode   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace anonymous function declarations with the lambda symbol.
;;
;; (font-lock-add-keywords
;;  'js2-mode `(("\\<\\(function\\) *("
;;               (0 (progn (compose-region (match-beginning 1)
;;                                         (match-end 1) "\u0192")
;;                         nil))))
;;  )

;; Replace return keyword in one-line functions with the right arrow symbol.
;;
;; (font-lock-add-keywords
;;  'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
;;               (0 (progn (compose-region (match-beginning 1)
;;                                         (match-end 1) "\u2190")
;;                         nil))))
;; )

(add-hook 'js2-mode-user-hook     'turn-on-font-lock)
(add-hook 'javascript-mode-hook   'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.js$"       . js2-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.jsx$"      . js2-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.json$"     . json-mode         )) auto-mode-alist))

;; add node-js to interpreter list
(add-to-list 'interpreter-mode-alist '("node"   . js2-mode))
