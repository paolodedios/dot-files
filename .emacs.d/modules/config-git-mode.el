;; -*- mode: emacs-lisp -*-
;;
;; Git integration modes
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;(add-hook 'magit-update-uncommitted-buffer-hook 'vc-refresh-state)

(setq auto-revert-check-vc-info t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-magit-file-mode)

(global-set-key  (kbd "C-x g")     'magit-status        )
(global-set-key  (kbd "C-x M-g")   'magit-dispatch-popup)
