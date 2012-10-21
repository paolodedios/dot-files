;; -*-emacs-lisp-*-
;;
;; linum mode settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on line/column number modes
;; @see http://www.emacswiki.org/LineNumbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'linum)
(require 'linum-off)

(if (equal linum-mode nil)
    (global-linum-mode)
    ;; Enable linum for all modes except those on this list
    (linum-on)
  )

(column-number-mode                           t)
(fringe-mode                                  0)

;; Set linum-format to pad for up to 4 digit line numbers, followed by a space
;; then a solid '|' character and then another space
;;
;; Normally, the following would be sufficient
;; (setq linum-format                "%4d \u2502 ")
;;
;; But with whitespace mode enabled, linum-mode must use Unicode x2007 character,
;; FIGURE SPACE, a space that always has the same width as a digit to ensure
;; that whitespace-mode does not pickup any formatting from linum-mode
;;
(add-hook 'linum-before-numbering-hook
          (lambda ()
            (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
              (setq linum-format
                    `(lambda (line)
                       (propertize (concat
                                    "\u2007"
                                    (truncate-string-to-width
                                     "" (- ,w (length (number-to-string line)))
                                     nil ?\x2007)
                                    (number-to-string line)
                                    "\u2007"
                                    "\u2502"
                                    "\u2007"
                                    )
                                   'face 'linum)
                       )
                    )
              )
            )
          )

;; Select lines by click dragging on the margin
(defvar *linum-mdown-line* nil)

(defun line-at-click ()
  (save-excursion
	(let ((click-y (cdr (cdr (mouse-position))))
		  (line-move-visual-store line-move-visual))
	  (setq line-move-visual t)
	  (goto-char (window-start))
	  (next-line (1- click-y))
	  (setq line-move-visual line-move-visual-store)
	  ;; If you are using tabbar substitute the next line with
	  ;; (line-number-at-pos))))
	  (1+ (line-number-at-pos))
      )
    )
  )

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
		(line-number-at-pos)
        )
  )

(defun mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
	(let (mu-line)
	  ;; (goto-line (line-at-click))
	  (setq mu-line (line-at-click))
	  (goto-line (max *linum-mdown-line* mu-line))
	  (set-mark (line-end-position))
	  (goto-line (min *linum-mdown-line* mu-line))
	  (setq *linum-mdown* nil)
      )
    )
  )

(global-set-key (kbd "<left-margin> <down-mouse-1>")  'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>")       'mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>")  'mu-select-linum)



