;; -*- mode: emacs-lisp -*-
;;
;; Markup (XML, HTML, XHTML, SGML) mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use psgml for sgml and xml mode editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files."  t)
(autoload 'xml-mode  "psgml" "Major mode to edit XML files."   t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nXhtml mode loader (Breaks MMM mode customizations and file associations)
;;
;; (requre 'autostart.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'xxml)

(autoload 'xxml-mode-routine "xxml")

(require 'yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SGML and XML global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sgml-validate-command                "nsgmls -s %s %s"
      sgml-catalog-files                   (list "catalog" "CATALOG" "~/.emacs.d/vendor/dtd/catalog")
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
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML hook section, called on entry of a markup mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable xxml mode for SGML and XML modes
(add-hook 'sgml-mode-hook     'xxml-mode-routine)
(add-hook 'xml-mode-hook      'xxml-mode-routine)


;; Disable auto fill mode for all markup major/minor modes
(add-hook 'html-mode-hook
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'psgml-mode-hook
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'sgml-mode-hook
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'xml-mode-hook
          '(lambda ()
             (auto-fill-mode     nil)
             (setq tab-width     2)
             )
          )

(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)
        )
      )

;; Add CSS colorization to relevant major/minor modes
(add-hook 'html-mode-hook     'hexcolor-add-to-font-lock)
(add-hook 'psgml-mode-hook    'hexcolor-add-to-font-lock)
(add-hook 'xml-mode-hook      'hexcolor-add-to-font-lock)

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

;; To close a tag, press "C-c /".  It will look for the closest opened tag and
;; it will close it.  The above function is not bullet-proof. It just counts
;; closing tags and discards as many open tags, before finding the one that you
;; need to close.  This means, if you have: "<foo><bar></foo>|" and press "C-c /",
;; it will still enter "</foo>" although your XML is obviously invalid.

(define-key global-map [(control c) (/)] 'xml-close-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on font-lock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'html-mode-hook         'turn-on-font-lock)
(add-hook 'psgml-mode-hook        'turn-on-font-lock)
(add-hook 'sgml-mode-hook         'turn-on-font-lock)
(add-hook 'xml-mode-hook          'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist (append '(("\\.htm$"      . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.html$"     . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.vrml$"     . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.st$"       . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.jsp$"      . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.phtml$"    . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.phps$"     . sgml-mode         )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.incl$"     . sgml-mode         )) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.rng$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xml$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xul$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xbl$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xhtml$"    . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.rdf$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xsd$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.xsl$"      . xml-mode          )) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.yml$"      . yaml-mode         )) auto-mode-alist))
