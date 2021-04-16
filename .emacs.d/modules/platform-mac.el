;; -*- mode: emacs-lisp -*-
;;
;; macOS specific settings
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS specific encoding system configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-selection-coding-system      'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default Latin font (e.g. Consolas, Menlo, etc)
(set-face-attribute 'default nil   :family   "Monaco")

;; Default font size (point * 10)
;;
;; Depending on the default font, if the size is not supported very well,
;; the frame will be clipped so that the beginning of the buffer may not
;; be visible correctly.
(set-face-attribute 'default nil   :height   120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Platform Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the correct environment for bash commands
(setq shell-file-name             "bash")
(setq shell-command-switch        "-ic" )

;; Menu bar is not annoying in macOS
(menu-bar-mode 1)

;; Make ido-mode ignore .DS_Store files
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Make the browser the macOS default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; In dired, move deletions to trash
(setq delete-by-moving-to-trash t)

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))
    )
  )

;; DISABLED. Configured through macOS Keyboard Preferences
;;
;; Set the mac keyboard up for sane emacs usage.
;; [Ctrl][Option][Command/Alt]
;;
;; Within emacs only:
;;   "Alt/A-"   is mapped to the "Command" button
;;   "Meta/M-"  is mapped to the "Alt" button
;;   "Ctrl/C-"  is mapped to the "Caps-lock" button and the "Ctrl" button
;;
;; (setq mac-command-modifier  'alt)
;; (setq mac-option-modifier   'meta)
;; (setq mac-capslock-modifier 'ctrl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs specific window settings
;;
;; @see http://www.emacswiki.org/emacs/CustomizeAquamacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check for aquamacs
(defvar aquamacs-p (boundp 'aquamacs-version))

;; Define aquamacs specific settings
;;
;; @seehttps://www.emacswiki.org/emacs/CustomizeAquamacs
(when aquamacs-p

  ;; Enable tab bar globally
  ;;
  ;; @note Emacs 27.1 will come with two built-in tab modes, tab-bar-mode (per-frame) and
  ;; tab-line-mode (per-window). Tab-line-mode is similar to tabbar.el used by Aquamacs,
  ;; with one buffer per tab. Tab-bar-mode is for storing one window configuration per
  ;; tab.
  ;;
  ;; @see https://www.emacswiki.org/emacs/TabBarMode
  (tabbar-mode                 t)

  ;; Disable one-buffer-per-frame setting
  (one-buffer-one-frame-mode  -1)

  ;; Disable mode-specific faces, use the default font for everything
  (aquamacs-autoface-mode     -1)

  ;; Disable aquamacs auto-detect-wrap to allow text-mode-hook to control
  ;; line-wrapping and fill mode independently.
  (remove-hook 'text-mode-hook 'auto-detect-wrap)

  (defun transparency-set-initial-value ()
    "Set initial value of alpha parameter for the current frame"
    (interactive)
    (if (equal (frame-parameter nil 'alpha) nil)
        (set-frame-parameter nil 'alpha 100)
      )
    )

  (defun transparency-set-value (numb)
    "Set level of transparency for the current frame"
    (interactive "Enter transparency level in range 0-100: ")
    (if (> numb 100)
        (message "Error. The maximum value for transparency is 100.")
      (if (< numb 0)
          (message "Error. The minimum value for transparency is 0.")
        (set-frame-parameter nil 'alpha numb)
        )
      )
    )

  (defun transparency-increase ()
    "Increase level of transparency for the current frame"
    (interactive)
    (transparency-set-initial-value)
    (if (> (frame-parameter nil 'alpha) 0)
        (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) -1))
      (message "This is the minimum value for transparency")
      )
    )

  (defun transparency-decrease ()
    "Decrease level of transparency for the current frame"
    (interactive)
    (transparency-set-initial-value)
    (if (< (frame-parameter nil 'alpha) 100)
        (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) +1))
      (message "This is the minimum value for transparency")
      )
    )

  ;; keybinding for transparency manipulation
  (define-key global-map (kbd "C-?") 'transparency-set-value)

  ;; bind transparency control
  (define-key global-map (kbd "C->") 'transparency-increase)
  (define-key global-map (kbd "C-<") 'transparency-decrease)

  ;; Set initial frame transparency
  (setq transparency-level    100)
  (transparency-set-value     transparency-level)

  (add-hook 'after-make-frame-functions
            (lambda (selected-frame)
              (set-frame-parameter selected-frame 'alpha transparency-level)
              )
            )

  ;; Disable saving customizations to ~/Library/Preferences/.../customizations.el
  ;; (custom-set-variables '(aquamacs-save-options-on-quit nil) )
  (custom-set-variables '(aquamacs-save-options-on-quit nil) )

  )
