;; Assumes we're running on NixOS with mu installed system-wide
(add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-maildir "~/Mail")

;; We use mbsync to fetch mail periodically, but it's still useful to have mu4e
;; update its index periodically
(setq mu4e-get-mail-command "true")
(setq mu4e-update-interval 300)

;; Apparently needed when using mbsync
(setq mu4e-change-filenames-when-moving t)

;; Convenience
(setq message-kill-buffer-on-exit t)
(setq mu4e-confirm-quit nil)

;; Show us full addresses, like 'Bob <bob@example.com>', rather than just 'Bob'
(setq mu4e-view-show-addresses t)

;; Make M move to spaM
(defun mu4e-headers-move-to-spam ()
  "Move message at point to spam folder in mu4e header mode."
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))

  (let* ((msg   (mu4e-message-at-point))
         (docid (mu4e-message-field msg :docid)))
    (if docid
        (mu4e~proc-move docid
                        (mu4e~mark-check-target "/gmail/[Google Mail]/.Spam")
                        "-N")
      (mu4e-error "No message at point"))))

(define-key mu4e-headers-mode-map "M" 'mu4e-headers-move-to-spam)

;; Prevents Emacs giving a "-f" (from) argument to msmtp, since we're reading it
;; from the message headers instead
(setq message-sendmail-f-is-evil 't)

;; Home and work accounts
(setq mu4e-contexts
  `(,(make-mu4e-context
      :name "Home"
      :enter-func (lambda ()
                    (mu4e-message "Switch to the Home context"))
      :leave-func (lambda ()
                    ;; Try to prevent sending mail out of context
                    (setq sendmail-program nil))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches
                       msg :to "chriswarbo.*@gmail.com")))
      :vars '((user-mail-address                . "chriswarbo@gmail.com"  )
              (user-full-name                   . "Chris Warburton" )
              (mu4e-compose-signature           . "Thanks,\nChris")
              (message-send-mail-function       . message-send-mail-with-sendmail)
              (sendmail-program                 . "msmtp")
              (message-sendmail-extra-arguments . ("-a" "gmail"
                                                   "--read-envelope-from"
                                                   "--read-recipients"))
              (mu4e-trash-folder                . "/gmail/[Google Mail]/.Trash")
              (mu4e-drafts-folder               . "/gmail/[Google Mail]/.Drafts")
              (mu4e-sent-folder                 . "/gmail/[Google Mail]/.Sent Mail")

              ;; don't save message to Sent Messages, GMail/IMAP will take
              ;; care of this
              (mu4e-sent-messages-behavior      . delete)))

    ,(make-mu4e-context
      :name "Dundee"
      :enter-func (lambda ()
                    (mu4e-message "Switch to the Dundee context")
                    (setq message-send-mail-function
                          'message-send-mail-with-sendmail)
                    (setq sendmail-program "msmtp")
                    (setq message-sendmail-extra-arguments
                          '("-a" "dd"
                            "--read-envelope-from" "--read-recipients"))
                    (setq user-mail-address "cmwarburton@dundee.ac.uk")
                    (setq mu4e-sent-messages-behavior 'sent))
      :leave-func (lambda ()
                    ;; Try to prevent sending mail out of context
                    (setq sendmail-program nil))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches
                       msg :to ".*warburton@dundee.ac.uk")))
      :vars '((user-mail-address      . "cmwarburton@dundee.ac.uk")
              (user-full-name         . "Chris Warburton")
              (mu4e-compose-signature . "Regards,\nChris Warburton")))))

;; mu4e uses database queries rather than hierarchical structure, so we use
;; "bookmarks" to create pseudo-folders
(setq mu4e-bookmarks
      `(("maildir:/gmail/INBOX OR maildir:/dundee/INBOX"   "Inboxen"    ?i)
        ("maildir:/feeds* AND flag:unread"                 "News"       ?n)
        ("maildir:/gmail/[Google Mail]/.* AND flag:unread" "New tagged" ?t)
        ("flag:unread"                                     "All Unread" ?u)
        ,(letrec ((f (lambda (xs)
                       (if (cdr xs)
                           (concat "maildir:/feeds/" (car xs)
                                   " OR "
                                   (funcall f (cdr xs)))
                           (concat "maildir:/feeds/" (car xs))))))
           (list (funcall f '("50Things"
                              "BornSmart,Equal,Different"
                              "CodePodcast"
                              "ComputingBritain"
                              "CrowdScience"
                              "FlyingColoursMaths"
                              "FunctionalGeekery"
                              "HaskellCast"
                              "HiddenHistoriesOfTheInformationAge"
                              "InfiniteMonkeyCage"
                              "InOurTimeScience"
                              "InsideScience"
                              "LifeScientific"
                              "MathFactor"
                              "MoreOrLess"
                              "NaturalHistories"
                              "PuttingScienceToWork"
                              "ReithLectures"
                              "RutherfordAndFry"
                              "ScienceInAction"
                              "ScienceStories"
                              "StronglyConnectedComponents"
                              "TalkingMachines"
                              "TravelsInAMathWorld"
                              "TypeTheory"))
                 "Podcasts"
                 ?p))))

;; Nicer HTML->text conversion, preserving links
(require 'mu4e-contrib)
;;(setq mu4e-html2text-command 'mu4e-shr2text)  ;; Slow, freezes Emacs
(setq mu4e-html2text-command "w3m -T text/html")

;; Nice idea, but loses our position in the list so disable it for now
(setq mu4e-headers-auto-update nil)

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; '(setq mu4e-compose-context-policy nil)

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

;; Fetching news (e.g. RSS) can lead to duplicates. Gnus treats these as threads
;; with 'replies', but we'd rather mark them as read automatically.
(add-hook 'gnus-after-getting-new-news-hook
          (lambda (&rest ignored)
            (save-excursion
              ;; Find "feeds" group
              (goto-char (point-min))
              (when (search-forward "nnmaildir+feeds:feeds" nil t)
                ;; Open group with all articles. "gnus-large-newsgroup" prevents
                ;; asking us how many to open.
                (let ((gnus-large-newsgroup nil))
                  (gnus-group-read-group t t))

                ;; Loop through all 'threaded replies', which we spot by their
                ;; indentation
                (goto-char (point-min))
                (while (re-search-forward "^ [ ]*<" nil t)

                  ;; Mark each 'reply' as being read
                  (gnus-summary-mark-as-read-forward 1))

                (goto-char (point-min))
                (while (re-search-forward "^ [ ]*. [ ]*<" nil t)
                  (gnus-summary-mark-as-read-forward 1))
                (gnus-summary-exit)))))
