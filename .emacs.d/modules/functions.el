;; -*- mode: emacs-lisp -*-
;;
;; User defined definitions
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load libraries in from the vendor directory and any similarly named
;; customizations in the custom directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/config/vendor/" file))
         (suffix (concat normal ".el"))
         )
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library))
     )
    )
  )

(defun load-module (library)
  (let* ((file (symbol-name library))
         (custom (concat "~/.emacs.d/config/modules/" file)))
    (when (file-exists-p (concat custom ".el"))
      (load custom)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reload .emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-dot-emacs ()
  "Reload the .emacs.d/config/init.el file."
  (interactive)
  (load "~/.emacs.d/config/init.el"))


(defun edit-dot-emacs ()
  "Edit the .emacs.d/config/init.el file."
  (interactive)
  (find-file "~/.emacs.d/config/init.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for toggling end of line encoding
;;
;; M-x set-buffer-file-coding-system utf-8-unix (for LF)
;; M-x set-buffer-file-coding-system utf-8-mac  (for CR)
;; M-x set-buffer-file-coding-system utf-8-dos  (for CR+LF)
;;
;; With Emacs 21 and later, you can preserve the current text coding system and
;; change end-of-line conversion by running the command set-buffer-file-coding-system
;; (C-x RET f) and specifying "unix", "dos", or "mac"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unix-file ()
  "Change the current buffer to UTF-8 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

(defun dos-file ()
  "Change the current buffer to UTF-8 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

(defun mac-file ()
  "Change the current buffer to UTF-8 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))

(defun remove-double-newline-unix ()
  "Delete doubled newlines from the current buffer."
  (interactive)
  (save-excursion
    (replace-regexp "\n\n" "\n")))

(defun remove-double-newline-dos ()
  "Delete doubled newlines from the current buffer."
  (interactive)
  (save-excursion
    (replace-regexp "\r\n\r\n" "\r\n")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: M-x dos2unix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS eolformat to UNIX eol format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: M-x cut-ctrlM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cut-ctrlM ()
  "Eliminate all visible ^M characters. "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean out buffers except shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restart ()
  "Clean out all editing buffers except for shell."
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
             (name (buffer-name buffer)))
        (and (not (string-equal name "*shell*"))
             (kill-buffer buffer)))
      (setq list (cdr list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trim trailing whitespace from the buffer before going to EOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trim-end-of-line ()
  "Trim trailing whitespace before going to EOL."
  (interactive)
  (if buffer-read-only
      (end-of-line)
    (progn
      (trim-trailing-spaces)
      (end-of-line))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts to project directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goto-homedir ()
  "Switch to project directory."
  (interactive)
  (cd "~/"))

(defun goto-projects ()
  "Switch to project directory."
  (interactive)
  (cd "~/Projects"))

(defun goto-aftertext ()
  "Switch to Squarespace project directory."
  (interactive)
  (cd "~/Projects/AfterText"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can also use CTRL-x CTRL-v to do a “Find alternative file” and choose
;; the same file that you are currently editing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-file ()
  "Reload the file you're currently editing."
  (interactive)
  (revert-buffer t t t)
  )


(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name)))
              )
        )
      )
    )
  )


(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle line wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-line-wrapping ()
  "Toggles between line wrapping in the current buffer."
  (interactive)
  (if (eq truncate-lines nil)
      (progn
        (setq truncate-lines t)
        (redraw-display)
        (message "Setting truncate-lines to t"))
    (setq truncate-lines nil)
    (redraw-display)
    (message "Setting truncate-lines to nil"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle line/column number modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-linum-on ()
  "Turn line-number column ON. "
  (interactive)
  (unless (minibufferp)
    (linum-mode 1)))

(defun turn-linum-off ()
  "Turn line-number column OFF. "
  (interactive)
  (unless (minibufferp)
    (linum-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to matching parenthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert formatted date/timestamp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%d.%m.%Y %H:%M")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (region-beginning) and (region-end) are not saved in
;; variables since they can change after each clean step.
(defun indent-clean-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertical alignment functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-active-region-pos ()
  "Return position of the current word or the current text selection"
  (interactive)
  (let (pos1 pos2 bds)
    (if (region-active-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds  (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;; Now, pos1 and pos2 are the starting and ending positions of the
    ;; current word, or current text selection if exist.
    (list pos1 pos2)
    )
  )

(defun align-stacked-c-list (current-region-begin current-region-end)
  "Vertically align region - used to align stacked list commas"
  (interactive "r")
  (align-regexp current-region-begin
                current-region-end
                (concat "\\(\\s-*\\)"
                        "\\(,\\|;\\)\\(\\s-+\\|$\\)"
                        )
                1 1 t
                )
  )


(defun align-stacked-c-funcall (current-region-begin current-region-end)
  "Vertically align region - used to align stacked function calls"
  (interactive "r")
  (align-regexp current-region-begin
                current-region-end
                (concat "\\(\\s-*\\)"
                        "\\(,\\|)\\)\\(;\\)?\\(\\s-+\\|$\\)"
                        )
                1 1 t
                )
  )

(defun align-stacked-c-param-delimiters (current-region-begin current-region-end)
  "Vertically align region - used to align stacked parameter commas"
  (interactive "r")
  (align-regexp current-region-begin
                current-region-end
                (concat "\\(\\s-*\\)"
                        "\\(,\\|)\\)\\(\\s-+\n\\|\\s-+{\\|$\\)"
                        )
                1 1 t
                )
  )

(defun align-stacked-c-param-names (current-region-begin current-region-end)
  "Vertically align region - used to align stacked arg list parameter names"
  (interactive "r")
  (align-regexp current-region-begin
                current-region-end
                (concat "\\(\\s-*\\)"
                        "\\([a-zA-Z0-9_]+[a-zA-Z0-9_]+\\)+\\(\\s-*\\)\\(,\\|)\\)\\(\\s-*\\)"
                        )
                1 1 t
                )
  )

(defun align-stacked-c-params (current-region-begin current-region-end)
  "Vertically align region - used to align arg list variables and commas"
  (interactive "r")

  ;; Properly indent region before vertically aligning names
  (indent-clean-region)

  ;; Align param names
  (align-stacked-c-param-names (region-beginning) (region-end))

  ;; Obtain updated active region point and mark and align param delimiters
  (align-stacked-c-param-delimiters (region-beginning) (region-end))
  )


(defun align-stacked-c-lvalues (current-region-begin current-region-end)
  "Vertically align region - used to align variable l-values"
  (interactive "r")
  (align-regexp current-region-begin
                current-region-end
                (concat "\\(\\s-*\\)"
                        "\\([a-zA-Z0-9_]+[a-zA-Z0-9_]+\\)+\\(\\s-*\\)\\(=\\|<-\\|=>\\)\\(\\s-*\\)"
                        )
                1 1 t
                )
  )

(defun align-stacked-c-assignment-equals (current-region-begin current-region-end)
  "Vertically align region - used to align variable assignments"
  (interactive "r")
  (align-regexp current-region-begin
                current-region-end
                (concat "\\(\\s-*\\)"
                        "\\(=\\|<-\\|=>\\)"
                        )
                1 1 t)
  )

(defun align-stacked-c-assignments (current-region-begin current-region-end)
  "Vertically align region - used to align variable assignments"
  (interactive "r")

  ;; Properly indent region before vertically aligning names
  (indent-clean-region)

  ;; Align variable names
  (align-stacked-c-lvalues (region-beginning) (region-end))

  ;; Obtain updated active region point and mark and align equal signs
  (align-stacked-c-assignment-equals (region-beginning) (region-end))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-comment-current-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position)))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line and region editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-current-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))
    )
  )


(defun paste-default-c-file-style ()
  "Insert c-file-style local variable header. "
  (interactive)
  (insert
   "/* -*- c-file-style: \"sourcery\" -*- */"
   )
  )

(defun paste-knr-c-file-style ()
  "Insert c-file-style local variable header. "
  (interactive)
  (insert
   "/* -*- c-file-style: \"knr13\" -*- */"
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle moving margin to fill column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-margin-to-fill-column ()
  "Toggle the right margin between `fill-column' or window width.
   This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )
