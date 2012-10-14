;; -*-emacs-lisp-*-
;;
;; Mac OS X specific settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs specific window settings
;; see http://www.emacswiki.org/emacs/CustomizeAquamacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check for aquamacs
(defvar aquamacs-p (boundp 'aquamacs-version))

;; Define aquamacs specific settings
(when aquamacs-p
  (defun transparency-set-initial-value ()
    "Set initial value of alpha parameter for the current frame"
    (interactive)
    (if (equal (frame-parameter nil 'alpha) nil)
        (set-frame-parameter nil 'alpha 100)))
 
  (defun transparency-set-value (numb)
    "Set level of transparency for the current frame"
    (interactive "Enter transparency level in range 0-100: ")
    (if (> numb 100)
        (message "Error. The maximum value for transparency is 100.")
      (if (< numb 0)
          (message "Error. The minimum value for transparency is 0.")
        (set-frame-parameter nil 'alpha numb))))
 
  (defun transparency-increase ()
    "Increase level of transparency for the current frame"
    (interactive)
    (transparency-set-initial-value)
    (if (> (frame-parameter nil 'alpha) 0)
        (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) -1))
      (message "This is the minimum value for transparency")))
 
  (defun transparency-decrease ()
    "Decrease level of transparency for the current frame"
    (interactive)
    (transparency-set-initial-value)
    (if (< (frame-parameter nil 'alpha) 100)
        (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) +1))
      (message "This is the minimum value for transparency")))
 
  ;; keybinding for transparency manipulation
  (define-key global-map (kbd "C-?") 'transparency-set-value)

  ;; bind transparency control
  (define-key global-map (kbd "C->") 'transparency-increase)
  (define-key global-map (kbd "C-<") 'transparency-decrease)
 
  ;; Set initial frame transparency
  (setq transparency-level 100)
  (transparency-set-value transparency-level)
  (add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))

  ;; DISABLED because of osx-key-mode
  ;;
  ;; Set the mac keyboard up for sane emacs usage.
  ;; [Ctrl][Option][Command/Alt]
  ;;
  ;; Within emacs only:
  ;;   "Alt/A-"   is mapped to the "Option" button
  ;;   "Meta/M-"  is mapped to the "Alt" button
  ;;   "Ctrl/C-"  is mapped to the "Ctrl" button (stays the same)
  ;;
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier  'alt)
  ;; (setq mac-control-modifier 'ctrl)

  ;; make C-v, M-v maintain the mark
  (define-key global-map "\C-v" 'scroll-up)
  (define-key global-map "\M-v" 'scroll-down)
  )

