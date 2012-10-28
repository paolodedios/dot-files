;; -*-emacs-lisp-*-
;;
;; Javascript mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode package for .js file editing.
;l @see https://github.com/mooz/js2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'js2-mode "js2-mode" nil t)
(require 'json)

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
;; Javascript style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-javascript-mode-programming-style
  ;; hanging brace setup
  '((c-hanging-braces-alist .
                            ((brace-list-open                    after)
                             (brace-entry-open                   after)
                             (substatement-open                 before)
                             (block-close            . c-snug-do-while)
                             (extern-lang-open                   after)
                             (inexpr-class-open                  after)
                             (inexpr-class-close                before)
                             ))
    ;; cleanup shortcuts
    (c-cleanup-list         .
                            ((brace-else-brace                        )
                             (brace-elseif-brace                      )
                             (brace-catch-brace                       )
                             (list-close-comma                        )
                             ))
    ;; indentation offsets
    (c-offsets-alist        .
                            ((access-label                         . 0)
                             (inline-open                          . 0)
                             (substatement-open                    . 0)
                             (statement-block-intro                . +)
                             (block-close                          . 0)
                             (do-while-closure                     . 0)
                             (case-label                           . *)
                             (statement-case-intro                 . +)
                             (statement-cont c-lineup-cascaded-calls +)
                             (stream-op                            . c-lineup-streamop)
                             ))
    (c-lineup-math                   1)
    (c-lineup-inexpr-block           1)

    ) "My Javascript Programming Style")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With regular JS2-mode, pressing TAB at the beginning of a line won't move the
;; caret to the first non-space character if the line is already properly. This
;; indentation function fixes that.

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node
           )

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2)))
          )

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation)))
        )

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset))

      )
    )
  )

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
             ;; toggle major mode editor options
             ;; unlike other major modes, js2-mode is not
             ;; a cc-mode derivative
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (show-paren-mode                        t)
             (setq fill-column                      80)
             (setq c-basic-offset                    4)
             (setq tab-width                         4)
             (setq indent-tabs-mode                nil)

             (setq js2-use-font-lock-faces           t)
             (setq js2-highlight-level               3)

             ;; additional js2-mode indent sexp
             ;; incompatible with js2-mode from https://github.com/mooz/js2-mode
             ;;(set (make-local-variable 'indent-line-function) 'my-js2-indent-function)

             ;; enable highlight-vars-mode
             (if (featurep 'js2-highlight-vars)  (js2-highlight-vars-mode))

             ;; bind buffer local keys
             (define-key js2-mode-map [(meta control |)]      'cperl-lineup          )
             (define-key js2-mode-map [(meta control \;)]
               '(lambda()
                  (interactive)
                  (insert "/* -----[ ")
                  (save-excursion
                    (insert " ]----- */"))
                  ))
             (define-key js2-mode-map [return]             'newline-and-indent       )
             (define-key js2-mode-map [backspace]          'c-electric-backspace     )
             (define-key js2-mode-map [(control d)]        'c-electric-delete-forward)
             (define-key js2-mode-map [(control meta q)]   'my-js2-indent-sexp       )
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'js2-mode-user-hook     'turn-on-font-lock)
(add-hook 'javascript-mode-hook   'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.js$"       . js2-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.json$"     . json-mode         )) auto-mode-alist))

;; add node-js to interpreter list
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the javacsript-mode package (for mmm-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'javascript-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set javascript-mode MMM submode indentation function override
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; javascript-mode indentation breaks under Emacs 22 and MMM
;; This is a custom indentation function for exclusive use with
;; javascript-mode in MMM

(defun my-javascript-mmm-indent-sexp ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)

      (save-excursion
        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2))))
        )

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset))

      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript hook section, called on entry of javascript-mode in MMM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'javascript-mode-hook
          '(lambda ()
             (require 'js)
             ;; toggle major mode editor options
             (show-paren-mode                        t)
             (c-toggle-auto-state                    1)
             (c-toggle-hungry-state                  1)
             (auto-fill-mode                         1)
             (setq fill-column                      80)
             (setq indent-tabs-mode                nil)
             (set (make-local-variable 'indent-line-function) 'my-javascript-mmm-indent-sexp)
             ;; set programming style and force c-mode so that Emacs allows c-set-style
             (c-add-style "my-javascript-programming-style" my-javascript-mode-programming-style t)
             (c-set-style "my-javascript-programming-style")
             )
          )
