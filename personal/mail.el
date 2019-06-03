;; Assumes we're running on NixOS with mu installed system-wide
(ignore-errors
  (add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e")
  (require 'mu4e))

(setq mu4e-maildir "~/Mail")

;; We use mbsync to fetch mail periodically, but it's still useful to have mu4e
;; update its index periodically
(setq mu4e-get-mail-command "true")
(setq mu4e-update-interval nil)

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
(ignore-errors
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
                    (mu4e-compose-signature . "Regards,\nChris Warburton"))))))

;; mu4e uses database queries rather than hierarchical structure, so we use
;; "bookmarks" to create pseudo-folders
(require 'cl-lib)
(let ((news-lists '("AGI"
                    "Bugs"
                    "CommentReplies"
                    "Coq"
                    "EFF"
                    "FONC"
                    "FSF"
                    "GeekUp"
                    "Gnus"
                    "HaskellCafe"
                    "HoTT"
                    "Identica"
                    "Idris"
                    "iPlayer"
                    "KDE"
                    "LambdaLounge"
                    "Nix"
                    "No2ID"
                    "ORG"
                    "People"
                    "Reprap"
                    "Societies"
                    "Sourceforge"
                    "Squeak"
                    "Uni")))
  (setq mu4e-bookmarks
        `(("maildir:/gmail/INBOX OR maildir:/dundee/INBOX"   "Inboxen"      ?i)
          (,(format "(maildir:/feeds* OR %s) AND flag:unread"
                    (mapconcat (lambda (dir)
                                 (format "maildir:\"/gmail/[Google Mail]/.%s\""
                                         dir))
                               news-lists
                               " OR "))                      "News"         ?n)
          ("maildir:/gmail/[Google Mail]/.* AND flag:unread" "New tagged"   ?t)
          ("flag:unread"                                     "All Unread"   ?u)
          )))

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
