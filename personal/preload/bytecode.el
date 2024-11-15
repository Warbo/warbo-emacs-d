;; Always load newest byte code
(setq load-prefer-newer t)

;; Ensure all of our custom code is byte-compiled, for speed
;; FIXME: Disabled since this makes it harder to switch Emacs version
;;(byte-recompile-directory (expand-file-name "~/.emacs.d/personal") 0)
