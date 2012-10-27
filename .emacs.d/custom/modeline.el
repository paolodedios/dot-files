;; -*-emacs-lisp-*-
;;
;; Modeline customizations
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'powerline)

;; get-scroll-bar-mode is not available in emacs 23.2
(if (not (functionp 'get-scroll-bar-mode))
    (defun get-scroll-bar-mode () scroll-bar-mode)
  )

(custom-set-faces
 )

(defvar buffer-count-chars
  nil
  "*Number of chars in the buffer."
  )

(defvar buffer-count-lines
  nil
  "*Number of lines in the buffer."
  )

(defvar buffer-count-words
  nil
  "*Number of words in the buffer."
  )

(defun buffer-count(expression)
  (how-many expression (point-min) (point-max))
  )

(defun buffer-update-mode-line()
  (setq buffer-count-lines
        (number-to-string
         (+ (buffer-count "\n") 1)
         )

        buffer-count-words
        (number-to-string (buffer-count "\\w+"))

        buffer-count-chars
        (number-to-string (buffer-count ".\\|\n"))
        )
  (force-mode-line-update)
  )

(unless buffer-count-lines
  (run-with-idle-timer 1 t 'buffer-update-mode-line)
  (buffer-update-mode-line)
  )

(setq global-mode-string
      (append '(" Lines: " buffer-count-lines
                " Words: " buffer-count-words
                " Chars: " buffer-count-chars
                " ")
              )
      )

(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq (frame-selected-window) (selected-window)) )
                        (face1  (if active 'powerline-inactive1 'powerline-inactive1))
                        (face2  (if active 'powerline-inactive2 'powerline-inactive2))
                        (lhs (list
                              (powerline-raw                  "%*" nil 'l)
                              (powerline-buffer-size               nil 'l)
                              (powerline-buffer-id                 nil 'l)
                              (powerline-raw                          " ")
                              (powerline-arrow-right            nil face1)

                              (powerline-raw mode-line-process   face1 'l)
                              (powerline-narrow                  face1 'l)
                              (powerline-vc                         face1)
                              ))

                        (center (list
                                 (powerline-raw                 " " face1)
                                 (powerline-arrow-right       face1 face2)
                                 (powerline-major-mode           face2 'l)
                                 (powerline-raw               " : " face2)
                                 (powerline-minor-modes          face2 'l)
                                 (powerline-raw                 " " face2)
                                 (powerline-arrow-left        face2 face1)
                                 ))

                        (rhs (list
                              (powerline-raw  global-mode-string face1 'l)

                              (powerline-raw            "[%4l  " face1 'l)
                              (powerline-raw                    ":" face1)
                              (powerline-raw            "%3c ] " face1 'l)

                              (powerline-arrow-left             face1 nil)
                              (powerline-raw                          " ")
                              (powerline-raw                 "%6p" nil 'l)
                              (powerline-hud                  face2 face1)
                              ))
                        )

                   (concat
                    (powerline-render                                            lhs)
                    (powerline-fill-center    face1 (/ (powerline-width center) 2.0))
                    (powerline-render                                         center)
                    (powerline-fill                      face1 (powerline-width rhs))
                    (powerline-render                                            rhs)
                    )
                   )
                 )
                )
              )

