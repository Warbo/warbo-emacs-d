;;; warbo --- Entry point for Warbo's Emacs config
;;; Commentary:
;;; We want everything to be loaded and configured with use-package, but we need
;;; to transition ourselves to that gradually.
;;;
;;; The first step is to make a "proper" package for our config; that's what
;;; this file contains.  We should be able to (use-package warbo) to load it.
;;;
;;; We need to be careful that our config doesn't conflict with the name of some
;;; existing package, e.g. we don't want to (use-package writing) for our
;;; writing customisations, since there might be an existing package called
;;; 'writing' on ELPA, etc.
;;;
;;; To avoid this we'll prefix everything with 'warbo-'.  This also gives us a
;;; way to gradually transition our config across: any file 'personal/foo.el'
;;; will be loaded as "raw" Emacs Lisp, whilst files 'personal/warbo-foo.el'
;;; will be ignored.  This lets us put '(use-package warbo-foo)' in here to load
;;; it in our preferred way.
;;;
;;; Once everything's been transitioned, we can simplify things a bit more.  I'm
;;; thinking that this file should *only* contain '(use-package ...)' calls;
;;; anything extra, like function definitions, should go into a 'warbo-foo'
;;; package.
;;;
;;; Code:

(use-package warbo-documents)
(use-package warbo-fixes)
(use-package warbo-haskell)
(use-package warbo-lisp)
(use-package warbo-mail)
(use-package warbo-mercury)
(use-package warbo-programming)
(use-package warbo-provers)
(use-package warbo-python)
(use-package warbo-scala)
(use-package warbo-tramp)
(use-package warbo-web)
(use-package warbo-windows)
(use-package warbo-writing)

(provide 'warbo)
;;; warbo.el ends here
