;; Settings for W3M browser
(setq w3m-use-cookies t)                   ; Use cookies
(setq w3m-default-display-inline-images t) ; Show images

;; Set the default browser to Debian's default.
;; W3M and Conkeror are good choices.
                                        ;(setq browse-url-browser-function 'w3m-browse-url)
                                        ;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")
