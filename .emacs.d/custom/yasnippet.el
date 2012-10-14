;; -*-emacs-lisp-*-
;;
;; YASnippet  mode
;;
;; Paolo de Dios <paolodedios@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load snippets utility
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet-bundle)

(setq yas/root-directory "~/.emacs.d/snippets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attach to various prog hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable yasnippet for generic cc-mode
(add-hook 'cc-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for objc-mode
(add-hook 'objc-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for java-mode
(add-hook 'java-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for scala-mode
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for python-mode
(add-hook 'python-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for html-mode
(add-hook 'html-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for psgml-mode
(add-hook 'psgml-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for sgml-mode
(add-hook 'sgml-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for xml-mode
(add-hook 'xml-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; enable yasnippet for css-mode
(add-hook 'css-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))


