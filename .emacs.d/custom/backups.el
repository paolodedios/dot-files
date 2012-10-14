;; -*-emacs-lisp-*-
;;
;; Backup file management
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prevent Emacs from cluttering source directories with backup files (filenames
;; ending in ~) and autosave files (filenames starting with #). Move backup 
;; elsewhere by setting backup-directory-alist and auto-save-file-name-transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable backup files.
;; (setq make-backup-files  t)
;; (setq backup-by-copying  t)

;; Enable versioning with default values (keep five last versions, I think!)
;; (setq version-control    t)

;; Save all backup files in this directory.
;; (setq backup-directory-alist (quote ((".*" . "~/.emacs.backups.d/"))))

(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (message "Deleting old backup files...")
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file)))
  )

 
;; Save all autosave-* backup files to this directory.
;; Example:
;; (defvar user-backup-file-directory
;;   (concat temporary-file-directory user-login-name "/"))
;;
(defvar user-backup-file-directory "~/.emacs.backups.d/")

(make-directory user-backup-file-directory t)

(setq backup-directory-alist
      `(("." . , user-backup-file-directory)
        (, tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-backup-file-directory ".auto-saves-"))

(setq auto-save-file-name-transforms
      `((".*" ,user-backup-file-directory t)))

;; delete old file versions
(setq delete-old-versions  t)
