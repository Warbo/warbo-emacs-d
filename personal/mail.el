(require 'gnus)
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))
(add-hook 'gnus-group-mode-hook
          ;; List all the subscribed groups even they contain zero un-read
          ;; messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups)))

;; Make Gnus NOT ignore [Gmail]/... mailboxes
(setq gnus-ignored-newsgroups "")

;; Posting styles, to make Gnus behave differently for each account
(setq mail-signature nil)
(setq gnus-parameters
      '(("home:.*"
         (posting-style
          (address "chriswarbo@gmail.com")
          (name "Chris Warburton")
          (body "\n\nCheers,\nChris")
          (eval (setq message-sendmail-extra-arguments '("-a" "gmail" "--read-envelope-from" "--read-recipients")))
          (user-mail-address "chriswarbo@gmail.com")))
        ("dd:.*"
         (posting-style
          (address "cmwarburton@dundee.ac.uk")
          (body "\n\nRegards,\nChris")
          (eval (setq message-sendmail-extra-arguments '("-a" "dd" "--read-envelope-from" "--read-recipients")))
          (user-mail-address "cmwarburton@dundee.ac.uk")))))

;; Always show inboxes, even when empty
(setq gnus-permanently-visible-groups "INBOX")

;; Discourage HTML emails
(eval-after-load "gnus-sum"
  '(add-to-list
    'gnus-newsgroup-variables
    '(mm-discouraged-alternatives
      . '("text/html" "image/.*"))))

;; Encourage HTML when reading RSS
(add-to-list
 'gnus-parameters
 '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;; Ignore updates to already-read articles
(setq nnrss-ignore-article-fields '(slash:comments dc:date pubDate))

;; If an RSS feed is actually ATOM, convert it
(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
                           nil t)
    (message "Converting Atom to RSS... ")
    (goto-char (point-min))
    (call-process-region (point-min) (point-max)
                         "xsltproc"
                         t t nil
                         (expand-file-name "~/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))
(ad-activate 'mm-url-insert)
