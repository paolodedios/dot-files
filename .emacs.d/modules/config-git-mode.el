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
;; global-auto-revert-mode with a non-nil value
;;
;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
;;
(global-auto-revert-mode           t)

;; Update the vcs info in the modeline when auto revert runs
;;
;; @see http://doc.endlessparentheses.com/Var/auto-revert-check-vc-info.html
;;
(setq auto-revert-check-vc-info    t)

;; Disable the "reverting buffer ..." messages
(setq auto-revert-verbose        nil)

;; Set file poll interval for 10 seconds
;; (setq auto-revert-interval        10)

;; Disable polling and rely on filesystem notifications
(setq auto-revert-avoid-polling    t)

;; Enable filesystem notifications
(setq auto-revert-use-notify       t)

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

(defvar magit--modified-files nil)

(defun magit-maybe-cache-modified-files ()
  "Maybe save a list of modified files.
   That list is later used by `magit-update-uncommitted-buffers',
   provided it is a member of `magit-post-refresh-hook'.  If it is
   not, then don't save anything here."
  (when (memq 'magit-update-uncommitted-buffers magit-post-refresh-hook)
    (setq magit--modified-files (magit-modified-files t))))

(add-hook 'magit-pre-refresh-hook #'magit-maybe-cache-modified-files)
(add-hook 'magit-pre-call-git-hook #'magit-maybe-cache-modified-files)
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

(add-hook 'magit-post-refresh-hook #'magit-update-uncommitted-buffers)

;; Use the magit-update-uncommitted-buffer-hook to update the powerline
;; git informatiom provided by vc-mode. Cheaper to execute than using the
;; default vc-mode option
;;   (setq auto-revert-check-vc-info t)
;;
;; @see https://github.com/magit/magit/issues/2687
;; @see https://github.com/magit/magit/wiki/magit-update-uncommitted-buffer-hook
;;
(add-hook 'magit-update-uncommitted-buffer-hook 'vc-refresh-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-magit-file-mode)

(global-set-key  (kbd "C-x g")     'magit-status        )
(global-set-key  (kbd "C-x M-g")   'magit-dispatch-popup)
