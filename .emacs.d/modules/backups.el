;; -*- mode: emacs-lisp -*-
;;
;; Backup file management
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs auto-save scheme configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prevent Emacs from cluttering source directories with autosave
;; files (filenames starting with #).
;;
;; Move autosave files elsewhere by setting  auto-save-file-name-transforms
(defvar user-autosave-file-directory "~/.emacs.backups.d/auto-save/")

(make-directory user-autosave-file-directory  t)

(setq auto-save-file-name-transforms
      `((".*" ,user-autosave-file-directory t)))

(setq auto-save-list-file-prefix
      (concat user-autosave-file-directory "auto-saves-"))

;; Automatically purge left over auto save files not accessed in a week
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files user-autosave-file-directory t))
    (message "Deleting old auto-save files in [%s]..." user-autosave-file-directory)
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file)))) week))
      (message file)
      (delete-file file)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs backup scheme configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable backup files
(setq make-backup-files      t)

;; Don't clobber symlinks
(setq backup-by-copying      t)

;; Delete old file versions
(setq delete-old-versions    t)

;; Enable versioning. Keep the last five versions and zero of the oldest
;; file versions.
(setq version-control        t)
(setq kept-new-versions      5)
(setq kept-old-versions      0)

;; By default, Emacs never backs up versioned files. Since we’re
;; going to be backing up intermediate copies, we want to backup
;; versioned files too, since we don’t commit on every save
(setq vc-make-backup-files   t)

;; Prevent Emacs from cluttering source directories with backup files (filenames
;; ending in ~)
;;
;; Move backup files elsewhere by setting backup-directory-alist
(defvar user-persave-backup-file-directory   "~/.emacs.backups.d/per-save/")
(defvar user-session-backup-file-directory   "~/.emacs.backups.d/per-session/")

(make-directory user-persave-backup-file-directory    t)

(setq backup-directory-alist
      `((".*" . ,user-persave-backup-file-directory)
        (, tramp-file-name-regexp nil)
        )
      )

;; Automatically purge backup files not accessed in a week
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files user-persave-backup-file-directory t))
    (message "Deleting old per-save backup files in [%s]..." user-persave-backup-file-directory)
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file)))) week))
      (message file)
      (delete-file file)))
  (dolist (file (directory-files user-session-backup-file-directory t))
    (message "Deleting old per-session backup files in [%s]..." user-session-backup-file-directory)
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file)))) week))
      (message file)
      (delete-file file)))
  )

;; Tell emacs to make a backup on each save, not just the first, by unsetting
;; ‘buffer-backed-up’. We make two kinds of backups: per-session backups, once
;; on the first save of each emacs session, and per-save backups, once on every
;; save. The backups go in different places and Emacs creates the backup dirs
;; automatically if they don’t exist
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.backups.d/per-session/"))))
      (backup-buffer)
      )
    )
    ;; Make a per-save backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer)
      )
    )


(add-hook 'before-save-hook  'force-backup-of-buffer)
