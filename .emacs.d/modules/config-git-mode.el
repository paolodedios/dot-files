;; -*- mode: emacs-lisp -*-
;;
;; Git integration modes
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable global-auto-revert-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A buffer can get out of sync with respect to its visited file on disk if that
;; file is changed by another program. To keep it up to date for all buffers, enable
;; global-auto-revert-mode with a non-nil value.
;;
;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
;;
(global-auto-revert-mode           t)

;; Update the vcs info in the modeline when auto revert runs. Running this with
;; any frequency is not good for performance.
;;
;; @see http://doc.endlessparentheses.com/Var/auto-revert-check-vc-info.html
;;
(setq auto-revert-check-vc-info  nil)

;; Disable the "reverting buffer ..." messages
(setq auto-revert-verbose        nil)

;; Helper function to generate random number to aid in the
;; configuration of a per-process auto revert interval.
(defun random-number-in-range (start end)
  (+ start (random (+ 1 (- end start)))) )

;; By default, Auto Revert mode will poll files for changes periodically even when
;; file notifications are used. Set file poll interval for 30 seconds to reduce
;; CPU utilization.
;;
;; Validate generated interval via (describe-variable VARIABLE)
(setq auto-revert-interval        (+ 30 (random-number-in-range 0 30)))

;; Disable polling and rely on filesystem notifications
(setq auto-revert-avoid-polling    t)

;; Enable filesystem notifications
(setq auto-revert-use-notify       t)

;;
;; Limit global-auto-revert-mode by reverting all buffers that have windows in the
;; current frame. Only the visible buffers are reverted.
;;
;; @see https://emacs.stackexchange.com/a/28899
;;
(require 'cl-lib)
(require 'autorevert)

(defvar auto-revert-some-buffers-filter #'get-buffer-window
  "Filter for the output of `buffer-list' in `auto-revert-buffers'.
The function is called with a buffer as argument.
It should return a non-nil value if this buffer should really be auto-reverted.")

(defun auto-revert-some-buffers-advice--buffer-list (ret)
  "Filter output of the first call of `buffer-list' in `auto-revert-buffers'.
This filter de-installs itself after this call."
  (advice-remove #'buffer-list #'auto-revert-some-buffers-advice--buffer-list)
  (cl-remove-if-not auto-revert-some-buffers-filter ret))

(defun auto-revert-some-buffers-advice (oldfun &rest args)
  "Filter the buffers to be auto-reverted through `auto-revert-some-buffers-filter' (which see)."
  (let (ret)
    (if global-auto-revert-mode
    (unwind-protect
        (progn
          (advice-add #'buffer-list :filter-return #'auto-revert-some-buffers-advice--buffer-list)
          (setq ret (apply oldfun args)))
      (advice-remove #'buffer-list #'auto-revert-some-buffers-advice--buffer-list) ;; being over-protective
      )
      (let ((old-auto-revert-buffer-list (cl-remove-if-not auto-revert-some-buffers-filter auto-revert-buffer-list))
        ;; Note: We interpret `auto-revert-remaining-buffers' as transient effect and don't filter this list.
        deleted-buffers)
    (let ((auto-revert-buffer-list old-auto-revert-buffer-list))
      (setq ret (apply oldfun args))
      (setq deleted-buffers (cl-set-difference old-auto-revert-buffer-list auto-revert-buffer-list)))
    (setq auto-revert-buffer-list (cl-set-difference auto-revert-buffer-list deleted-buffers))))
    ret))

(advice-add #'auto-revert-buffers :around #'auto-revert-some-buffers-advice)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable git diff indicators in the gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'git-gutter+)
(require 'git-gutter-fringe+)

(global-git-gutter+-mode)

(setq-default right-fringe-width   20          )
(setq git-gutter-fr+-side         'right-fringe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit mode hook section, called on entry of magit mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit no longer provides a hook that is run in each buffer that visits a
;; file that is being tracked in the current repository whenever it refreshes
;; the current Magit buffer. It used to provide such a hook and made use of
;; it itself, but that was highly inefficient and so it was removed.
;;
;; Such a hook could be implemented as follows.
;;
;; @see https://github.com/magit/magit/issues/2687
;; @see https://github.com/magit/magit/wiki/Code-snippets#magit-update-uncommitted-buffer-hook
(defvar magit--modified-files nil)

(defun magit-maybe-cache-modified-files ()
  "Maybe save a list of modified files.
   That list is later used by `magit-update-uncommitted-buffers',
   provided it is a member of `magit-post-refresh-hook'.  If it is
   not, then don't save anything here."
  (when (memq 'magit-update-uncommitted-buffers magit-post-refresh-hook)
    (setq magit--modified-files (magit-modified-files t))))

(add-hook 'magit-pre-refresh-hook   #'magit-maybe-cache-modified-files)
(add-hook 'magit-pre-call-git-hook  #'magit-maybe-cache-modified-files)
(add-hook 'magit-pre-start-git-hook #'magit-maybe-cache-modified-files)

(defun magit-update-uncommitted-buffers ()
  "Update some file-visiting buffers belonging to the current repository.
   Run `magit-update-uncommitted-buffer-hook' for each buffer
   which visits a file inside the current repository that had
   uncommitted changes before running the current Magit command
   and/or that does so now."
  (let ((topdir (magit-toplevel)))
    (dolist (file (delete-consecutive-dups
                   (sort (nconc (magit-modified-files t)
                                magit--modified-files)
                         #'string<)))
      (--when-let (find-buffer-visiting (expand-file-name file topdir))
        (with-current-buffer it
          (run-hooks 'magit-update-uncommitted-buffer-hook))))))

(add-hook 'magit-post-refresh-hook  #'magit-update-uncommitted-buffers)

;; Magit is not responsible for the version control information that is being
;; displayed in the mode-line. Emacs' own Version Control package, also known
;; as VC, displays something like Git-master in the mode-line. When using Magit
;; (and perhaps when using VC) this information is not always up to date, but
;; can be told to do so more often:
;;
;;  (setq auto-revert-check-vc-info t)
;;
;; But doing so isn’t good for performance so consider simply not displaying it in
;; the mode-line via:
;;
;;  (setq-default mode-line-format
;;              (delete '(vc-mode vc-mode) mode-line-format))
;;
;; If up-to-date VC mode-line information is important, but the VC setting
;; (setq auto-revert-check-vc-info t) is considered too expensive, then add the
;; above `magit-update-uncommitted-buffer-hook` instead and refresh the VC state
;; via the the 'magit-update-uncommitted-buffer-hook.
;;
;; This approach has the advantage that it doesn't create a constant load on the
;; cpu. Instead you will likely get a noticeable spike every time you run a Magit
;; command.
;;
;; @see https://github.com/magit/magit/wiki/Code-snippets#updating-vcs-mode-line-information
;;
(add-hook 'magit-update-uncommitted-buffer-hook 'vc-refresh-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-magit-file-mode)

(global-set-key  (kbd "C-x g")     'magit-status        )
(global-set-key  (kbd "C-x M-g")   'magit-dispatch-popup)
