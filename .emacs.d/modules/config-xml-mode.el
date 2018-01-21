;; -*- mode: emacs-lisp -*-
;;
;; Markup (XML, HTML, XHTML, SGML) mode
;; @see https://www.emacswiki.org/emacs/PsgmlMode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use psgml for sgml and xml mode editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files."  t)
(autoload 'xml-mode  "psgml" "Major mode to edit XML files."   t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SGML and XML global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sgml-validate-command                "nsgmls -s %s %s"
      sgml-catalog-files                   (list "catalog" "CATALOG" "~/.emacs.d/config/vendor/dtd/catalog")
      sgml-trace-entity-lookup             t
      sgml-set-face                        t
      sgml-auto-insert-required-elements   t
      sgml-auto-activate-dtd               t
      sgml-indent-data                     t
      sgml-indent-step                     2
      sgml-live-element-indicator          nil
      sgml-list-attributes                 nil
      sgml-insert-missing-element-comment  nil
      sgml-warn-about-undefined-entities   nil
      sgml-warn-about-undefined-elements   nil
      sgml-trace-entity-lookup             nil
      nxml-child-indent                    2
      nxml-attribute-indent                2
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML hook section, called on entry of a markup mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable auto fill mode for all markup major/minor modes
(add-hook 'html-mode-hook
          '(lambda ()
             (auto-fill-mode    -1)
             (setq tab-width     2)
             )
          )

(add-hook 'psgml-mode-hook
          '(lambda ()
             (auto-fill-mode    -1)
             (setq tab-width     2)
             )
          )

(add-hook 'sgml-mode-hook
          '(lambda ()
             (auto-fill-mode    -1)
             (setq tab-width     2)
             )
          )

(add-hook 'xml-mode-hook
          '(lambda ()
             (auto-fill-mode    -1)
             (setq tab-width     2)
             )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground (if (>= (apply '+ (x-color-values
                                                          (match-string-no-properties 0)))
                                               (* (apply '+ (x-color-values "white")) .6))
                                           "black" ;; light bg, dark text
                                         "white" ;; dark bg, light text
                                         )
                           )
               )
            )
          )
        append)
     )
    )
  )



(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords t)
  )

;; Add CSS colorization to relevant major/minor modes
(add-hook 'html-mode-hook     'hexcolor-add-to-font-lock)
(add-hook 'psgml-mode-hook    'hexcolor-add-to-font-lock)
(add-hook 'sgml-mode-hook     'hexcolor-add-to-font-lock)
(add-hook 'xml-mode-hook      'hexcolor-add-to-font-lock)

(add-hook 'html-mode-hook     'turn-on-font-lock)
(add-hook 'psgml-mode-hook    'turn-on-font-lock)
(add-hook 'sgml-mode-hook     'turn-on-font-lock)
(add-hook 'xml-mode-hook      'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close XML tag function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-close-tag ()
  "Close the previously defined XML tag"
  (interactive)
  (let ((tag nil)
        (quote nil))
    (save-excursion
      (do ((skip 1))
          ((= 0 skip))
        (re-search-backward "</?[a-zA-Z0-9_-]+")
        (cond ((looking-at "</")
               (setq skip (+ skip 1)))
              ((not (looking-at "<[a-zA-Z0-9_-]+[^>]*?/>"))
               (setq skip (- skip 1)))))
      (when (looking-at "<\\([a-zA-Z0-9_-]+\\)")
        (setq tag (match-string 1)))
      (if (eq (get-text-property (point) 'face)
              'font-lock-string-face)
          (setq quote t)))
    (when tag
      (setq quote (and quote
                       (not (eq (get-text-property (- (point) 1) 'face)
                                'font-lock-string-face))))
      (if quote
          (insert "\""))
      (insert "</" tag ">")
      (if quote
          (insert "\""))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML mode key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To close a tag, press "C-c /".  It will look for the closest opened tag and
;; it will close it.  The above function is not bullet-proof. It just counts
;; closing tags and discards as many open tags, before finding the one that you
;; need to close.  This means, if you have: "<foo><bar></foo>|" and press "C-c /",
;; it will still enter "</foo>" although your XML is obviously invalid.

(define-key global-map (kbd "C-c /")  'xml-close-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.htm$"      . html-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.html$"     . html-mode         )) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.st$"       . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.incl$"     . sgml-mode         )) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.rng$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xml$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xul$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xbl$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xhtml$"    . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.rdf$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xsd$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xsl$"      . xml-mode          )) auto-mode-alist))
