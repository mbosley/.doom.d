;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mitchell Bosley"
      user-mail-address "mcbosley@umich.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd ;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(defun set-theme-type ()
  "Sets theme type to light or dark, depending on the hour emacs is started"
  (setq hour (nth 2 (parse-time-string (current-time-string))))
  (if (and (< hour 17) (> hour 6))
      (setq doom-theme 'doom-one-light)
    (setq doom-theme 'doom-dark+)))
(set-theme-type)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Desktop/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; disable the toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

;; (defun my/disable-scroll-bars (frame)
;;   (modify-frame-parameters frame
;;                            '((vertical-scroll-bars . nil)
;;                              (horizontal-scroll-bars . nil))))
;; (add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 500)
            (setq default-frame-alist
                  '(
                    (top . 0.5)(left . 0.5)
                    (width . 145)(height . 60)
                    ;;(font . "Menlo-13")
                    ))
          (setq default-frame-alist
                '((top . 0)(left . 0)
                  (width . 85)(height . 35)
                  ;;(font . "Menlo-13")
                  )))
        ))
)
(set-frame-size-according-to-resolution)

(map! :leader
      :prefix "w"
      :desc "ns-next-frame" "f" #'ns-next-frame)

(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; (setq use-package-verbose t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package org
  :after org
  :config
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-src-window-setup 'current-window)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HABIT(b)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))
        )
  (setq org-agenda-start-day "-1d")
  ;; (setq org-agenda-span 1)
  (setq org-agenda-start-on-weekday nil)
  (setq org-log-done 'time)    ;; logs time when completing item
  (setq org-image-actual-width nil)  ;; allows for rescaling of inline image size
  (setq org-cycle-separator-lines 1)
  (defvar +org-dir (expand-file-name "/Users/mitchellbosley/Desktop/org"))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "/Users/mitchellbosley/Desktop/org/inbox.org" "Tasks")
           "* TODO %?\n  Entered on: %U")
          ("n" "Note" entry (file+datetree "/Users/mitchellbosley/Desktop/org/notes.org" "Notes")
           "* %?\n\n")))
  (setq org-refile-targets '((nil :maxlevel . 6)
                             (org-agenda-files :maxlevel . 6)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python     . t)
                                 (R          . t)))
  (setq org-agenda-sorting-strategy ;
        '((agenda time-up priority-down category-keep)
          (todo   priority-down category-keep)
          (tags   priority-down category-keep)
          (search category-keep)))

   ;; setting time to display in time grid
  (setq org-habit-show-all-today t)
  (setq org-agenda-time-grid
        (quote
         ((daily today remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "......" "-------------------")))

  ;; (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)

  )

(use-package org-super-agenda
  :after (org-agenda evil-org-agenda)
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown nil
        org-agenda-span 'day
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-start-with-log-mode t
        org-agenda-start-on-weekday nil)
  (setq org-super-agenda-groups
        '(
          (:name "Today"
           :time-grid t
           :and (:date today :scheduled today :todo "TODO" :todo "NEXT")
           :order 1)
          (:log t)
          ;; (:auto-property "PROJECT"
          ;;  :order 5
          ;;  )
          (:name "To refile"
           :file-path "inbox.org")
          (:name "Next to do"
           :todo "NEXT"
           :order 2)
          (:name "Important"
           :and (:todo "TODO" :priority "A")
           :order 6)
          (:name "Habits"
           :habit t
           :order 99)
          (:name "Due Today"
           :deadline today
           :order 2)
          ;; (:name "Scheduled Today"
          ;;  :scheduled today
          ;;  :order 1)
          (:name "Scheduled Soon"
           :scheduled future
           :order 12)
          (:name "Deadline Soon"
           :deadline future
           :order 8)
          (:name "Overdue"
           :deadline past
           :order 7)
          ;; (:name "Meetings"
          ;;  :and (:todo "MEET" :scheduled future)
          ;;  :order 10)
          (:discard (:not (:todo "TODO")))
          ;; (:discard (:anything))
          ))
  :config
  (org-super-agenda-mode)
  ;; (require 'evil)
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))
  )

(use-package org-ref    ;; references
  :after org
  ;; :hook (org-mode . org-ref) ;; try this out
  :config
  (setq reftex-default-bibliography '("/Users/mitchellbosley/library.bib")
        ;; org-ref-bibliography-notes "/Users/mitchellbosley/Desktop/org/paper_notes.org"
        org-ref-default-bibliography '("/Users/mitchellbosley/library.bib")
        ;; org-ref-notes-directory "/Users/mitchellbosley/Desktop/org/roam"
        bibtex-completion-bibliography '("/Users/mitchellbosley/library.bib")
        bibtex-completion-notes-path "/Users/mitchellbosley/Desktop/org/roam"
        bibtex-completion-pdf-field "file"
        ;; bibtex-completion-notes-template-multiple-files
        ;; (concat
        ;;  "#+TITLE: ${title}\n"
        ;;  "#+ROAM_KEY: cite:${=key=}\n"
        ;;  "* TODO Notes\n"
        ;;  ":PROPERTIES:\n"
        ;;  ":Custom_ID: ${=key=}\n"
        ;;  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
        ;;  ":AUTHOR: ${author-abbrev}\n"
        ;;  ":JOURNAL: ${journaltitle}\n"
        ;;  ":DATE: ${date}\n"
        ;;  ":YEAR: ${year}\n"
        ;;  ":DOI: ${doi}\n"
        ;;  ":URL: ${url}\n"
        ;;  ":END:\n\n")
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-notes-function 'orb-edit-notes
        org-latex-prefer-user-labels t
        ;; org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
        org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f")
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
        org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
        )

  ;; (setq org-ref-notes-function
  ;;       (lambda (thekey)
  ;;         (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
  ;;           (bibtex-completion-edit-notes
  ;;            (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))

  )

;; (setq package-check-signature nil)
(use-package org-gcal
  :after org
  :config
  (setq org-gcal-client-id "787977855889-libbeje55t66hi60f0amb1sdnjn0mkt8.apps.googleusercontent.com"
        org-gcal-client-secret "0xYv3BfA_6sY0Ss2gH-VIEkp"
        org-gcal-file-alist '(("mitchellbosley@gmail.com" .  "/Users/mitchellbosley/Desktop/org/schedule.org")
                              ("mcbosley@umich.edu" .  "/Users/mitchellbosley/Desktop/org/schedule.org"))
        ;; org-gcal-header-alist '(("mitchellbosley@gmail.com" . "#+PROPERTY: TIMELINE_FACE \"pink\"\n"))
        org-gcal-auto-archive nil
        org-gcal-notify-p nil)
   ;; (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
   ;; (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch)
  )

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; the WM can handle splits
   ;; org-noter-notes-window-location 'other-frame
   ;; stop opening frames
   org-noter-always-create-frame nil
   ;; see complete file
   org-noter-hide-other nil))

(use-package org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "/Users/mitchellbosley/Desktop/org/roam/")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam )
  (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox")
  (setq org-roam-link-title-format "§%s")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n
- tags ::"
           :unnarrowed t)))
  :config
  (setq org-roam-dailies-capture-templates
        (let ((head "#+title: %<%Y-%m-%d (%A)>\n#+startup: showall\n* [/] Do Today\n* [/] Maybe Do Today\n* Journal\n"))
          `(("j" "journal" entry
             #'org-roam-capture--get-point
             "* %<%H:%M> %?"
             :file-name "daily/%<%Y-%m-%d>"
             :head ,head
             :olp ("Journal"))
            ("t" "do today" item
             #'org-roam-capture--get-point
             "[ ] %(princ as/agenda-captured-link)"
             :file-name "daily/%<%Y-%m-%d>"
             :head ,head
             :olp ("Do Today")
             :immediate-finish t)
            ("m" "maybe do today" item
             #'org-roam-capture--get-point
             "[ ] %(princ as/agenda-captured-link)"
             :file-name "daily/%<%Y-%m-%d>"
             :head ,head
             :olp ("Maybe Do Today")
             :immediate-finish t))))

  (org-roam-mode +1))

;; (use-package company-org-roam
;;   :after org-roam company org
;;   :config
;;   (push 'company-org-roam company-backends))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "Orb-note-actions" "a" #'orb-note-actions )
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}  ; <== special file keyword: if more than one filename
:NOTER_PAGE:              ;     is available, the user will be prompted to choose
:END:")))




  (setq orb-preformat-keywords
        '("citekey" "title" "url" "file" "author-or-editor" "keywords"))

    (setq orb-templates
          '(("r" "ref" plain (function org-roam-capture--get-point)
             ""
             :file-name "${slug}"
             :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

  - tags ::
  - keywords :: ${keywords}

  * Notes
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author-or-editor}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
  :NOTER_PAGE:
  :END:
  ** Argument
  ** Methods
  ** Results
  ** Contribution
  ** Critique")))
  )

;; (use-package org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 40
;;         org-roam-server-network-label-wrap-length 20))

(use-package deft
  ;; :after org
  :bind
  ("C-c n d" . deft)
  :init
  (setq deft-recursive t
        deft-use-filter-string-for-filename t
        deft-default-extension "org"
        deft-directory "/Users/mitchellbosley/Desktop/org/roam/"
        ;; limits number of displayed files to speed things up
        deft-file-limit 30
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase))))

;; (use-package mathpix.el
;;   :init
;;   (setq mathpix-screenshot-method "screencapture -i %s")
;;   :custom
;;   ((mathpix-app-id "app-id")
;;    (mathpix-app-ky "app-key"))
;;   :bind
;;   ("C-x m" . mathpix-screenshot))

;; (use-package org-fragtog
;;   :after org
;;   :init
;;   (add-hook 'org-mode-hook 'org-fragtog-mode)
;;   )

;; (use-package geiser
;;   :init (setq geiser-active-implementations '(mit))
;;   )

;; (use-package lsp-julia
;;   :config
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.5")
;;   ;; (setq lsp-julia-package-dir nil))
;;   (setq lsp-enable-folding t)
;;   (setq lsp-julia-package-dir "~/.emacs.d/.local/straight/repos/lsp-julia/languageserver")
;;   (require 'lsp-julia)
;;   (add-hook 'ess-julia-mode-hook #'lsp-mode))

;; (setq org-babel-clojure-backend `cider)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (julia . t)
;;    (python . t)
;;    (jupyter . t)))
;; (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
;;                                                     (:session . "jl")
;;                                                     (:kernel . "julia-1.5")))

;; (use-package pinentry
;;   :after mu4e
;;   )

;; (use-package mu4e
;;   :config
;;   ;; setup specs
;;   (setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a" emacs-version)
;;         epa-pinentry-mode 'ask
;;         mail-user-agent `mu4e-user-agent
;;         mu4e-use-fancy-chars t
;;         mu4e-view-prefer-html t
;;         mu4e-attachment-dir "~/Downloads"
;;         mu4e-update-interval nil
;;         mu4e-view-show-addresses t
;;         ;; enable inline images
;;         mu4e-view-show-images t
;;         mu4e-confirm-quit nil
;;         mu4e-split-view 'vertical
;;         ;; every new email composition gets its own frame!
;;         mu4e-compose-in-new-frame t
;;         mu4e-headers-auto-update t
;;         mu4e-compose-signature-auto-include nil
;;         mu4e-compose-format-flowed t
;;         mu4e-change-filenames-when-moving t)

;;   (pinentry-start)

;;   ;; to view selected message in the browser, no signin, just html mail
;;   (add-to-list 'mu4e-view-actions
;;                '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;;   ;; use imagemagick, if available
;;   (when (fboundp 'imagemagick-register-types)
;;     (imagemagick-register-types))

;;   ;; don't save message to Sent Messages, IMAP takes care of this
;;   (setq mu4e-sent-messages-behavior 'delete)

;;   (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;;   ;; configure email sending
;;   (setq message-send-mail-function 'smtpmail-send-it
;;         smtpmail-default-smtp-server "smtp.gmail.com"
;;         smtpmail-smtp-server "smtp.gmail.com")

;;   ;; ;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
;;   ;; (add-hook 'mu4e-headers-mode-hook
;;   ;;           (defun my/mu4e-change-headers ()
;;   ;;             (interactive)
;;   ;;             (setq mu4e-headers-fields
;;   ;;                   `((:human-date . 25) ;; alternatively, use :date
;;   ;;       	      (:flags . 6)
;;   ;;       	      (:from . 22)
;;   ;;       	      (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
;;   ;;       	      (:size . 7)))))

;;   ;; setting contexts
;;   ;; mu4e-context
;;   (setq mu4e-context-policy 'pick-first)
;;   (setq mu4e-compose-context-policy 'always-ask)
;;   (setq mu4e-contexts
;;         (list
;;          (make-mu4e-context
;;           :name "work" ;;for mcbosley-gmail
;;           :enter-func (lambda () (mu4e-message "Entering context work"))
;;           :leave-func (lambda () (mu4e-message "Leaving context work"))
;;           :match-func (lambda (msg)
;; 		        (when msg
;; 		          (mu4e-message-contact-field-matches
;; 		           msg '(:from :to :cc :bcc) "acc1@gmail.com")))
;;           :vars '((user-mail-address . "mcbosley@umich.edu")
;; 	          (user-full-name . "Mitchell Bosley")
;; 	          (mu4e-sent-folder . "/mcbosley-gmail/[mcbosley].Sent Mail")
;; 	          (mu4e-drafts-folder . "/mcbosley-gmail/[mcbosley].drafts")
;; 	          (mu4e-trash-folder . "/mcbosley-gmail/[mcbosley].Bin")
;; 	          (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
;; 	          (mu4e-compose-format-flowed . t)
;; 	          (smtpmail-queue-dir . "~/Maildir/mcbosley-gmail/queue/cur")
;; 	          (message-send-mail-function . smtpmail-send-it)
;; 	          (smtpmail-smtp-user . "mcbosley")
;;                   (auth-sources . (expand-file-name "~/.authinfo.gpg"))
;; 	          ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
;;                   ;; (smtpmail-auth-credentials . (("smtp.gmail.com" 587 "mcbosley@umich.edu" nil)))
;; 	          ;; (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
;; 	          ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
;; 	          (smtpmail-default-smtp-server . "smtp.gmail.com")
;; 	          (smtpmail-smtp-server . "smtp.gmail.com")
;; 	          (smtpmail-smtp-service . 587)
;; 	          (smtpmail-debug-info . t)
;; 	          ;; (smtpmail-debug-verbose . t)
;; 	          ;; (mu4e-maildir-shortcuts . ( ("/mcbosley-gmail/INBOX"            . ?i)
;; 		  ;;       		      ("/mcbosley-gmail/[mcbosley].Sent Mail" . ?s)
;; 		  ;;       		      ("/mcbosley-gmail/[mcbosley].Bin"       . ?t)
;; 		  ;;       		      ("/mcbosley-gmail/[mcbosley].All Mail"  . ?a)
;; 		  ;;       		      ("/mcbosley-gmail/[mcbosley].Starred"   . ?r)
;; 		  ;;       		      ("/mcbosley-gmail/[mcbosley].drafts"    . ?d)
;; 		  ;;       		      ))
;;                   ))
;;          (make-mu4e-context
;;           :name "personal" ;;for acc2-gmail
;;           :enter-func (lambda () (mu4e-message "Entering context personal"))
;;           :leave-func (lambda () (mu4e-message "Leaving context personal"))
;;           :match-func (lambda (msg)
;; 		        (when msg
;; 		          (mu4e-message-contact-field-matches
;; 		           msg '(:from :to :cc :bcc) "mitchellbosley@gmail.com")))
;;           :vars '((user-mail-address . "mitchellbosley@gmail.com")
;; 	          (user-full-name . "Mitchell Bosley")
;; 	          (mu4e-sent-folder . "/mitchellbosley-gmail/[mitchellbosley].Sent Mail")
;; 	          (mu4e-drafts-folder . "/mitchellbosley-gmail/[mitchellbosley].drafts")
;; 	          (mu4e-trash-folder . "/mitchellbosley-gmail/[mitchellbosley].Trash")
;; 	          (mu4e-compose-signature . (concat "Informal Signature\n" "Emacs is awesome!\n"))
;; 	          (mu4e-compose-format-flowed . t)
;; 	          (smtpmail-queue-dir . "~/Maildir/mitchellbosley-gmail/queue/cur")
;; 	          (message-send-mail-function . smtpmail-send-it)
;; 	          (smtpmail-smtp-user . "mitchellbosley")
;; 	          ;; (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
;; 	          ;; (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
;;                   ;; (smtpmail-auth-credentials . (("smtp.gmail.com" 587 "mitchellbosley@gmail.com" nil)))
;;                   (auth-sources . (expand-file-name "~/.authinfo.gpg"))
;; 	          (smtpmail-default-smtp-server . "smtp.gmail.com")
;; 	          (smtpmail-smtp-server . "smtp.gmail.com")
;; 	          (smtpmail-smtp-service . 587)
;; 	          (smtpmail-debug-info . t)
;; 	          ;; (smtpmail-debug-verbose . t)
;; 	          ;; (mu4e-maildir-shortcuts . ( ("/mitchellbosley-gmail/INBOX"            . ?i)
;; 		  ;;       		      ("/mitchellbosley-gmail/[mitchellbosley].Sent Mail" . ?s)
;; 		  ;;       		      ("/mitchellbosley-gmail/[mitchellbosley].Trash"     . ?t)
;; 		  ;;       		      ("/mitchellbosley-gmail/[mitchellbosley].All Mail"  . ?a)
;; 		  ;;       		      ("/mitchellbosley-gmail/[mitchellbosley].Starred"   . ?r)
;; 		  ;;       		      ("/mitchellbosley-gmail/[mitchellbosley].drafts"    . ?d)
;; 		  ;;       		      ))
;;                   ))))

;;   ;; (set-email-account! "mcbosley-gmail"
;;   ;;                     '((mu4e-sent-folder       . "/mcbosley-gmail/[mcbosley].Sent Mail")
;;   ;;                       ;; (mu4e-drafts-folder     . "/Lissner.net/Drafts")
;;   ;;                       (mu4e-trash-folder      . "/mcbosley-gmail/[mcbosley].Trash")
;;   ;;                       (mu4e-refile-folder     . "/mcbosley-gmail/[mcbosley].All Mail")
;;   ;;                       (smtpmail-smtp-user     . "mcbosley@umich.edu")
;;   ;;                       (mu4e-compose-signature . "---\nMitchell Bosley"))
;;   ;;                     t)
;;   ;; (set-email-account! "mitchellbosley-gmail"
;;   ;;                     '((mu4e-sent-folder       . "/mitchellbosley-gmail/[mitchellbosley].Sent Mail")
;;   ;;                       ;; (mu4e-drafts-folder     . "/Lissner.net/Drafts")
;;   ;;                       (mu4e-trash-folder      . "/mitchellbosley-gmail/[mitchellbosley].Trash")
;;   ;;                       (mu4e-refile-folder     . "/mitchellbosley-gmail/[mitchellbosley].All Mail")
;;   ;;                       (smtpmail-smtp-user     . "mitchellbosley@gmail.com")
;;   ;;                       (mu4e-compose-signature . "---\nMitchell Bosley"))
;;   ;;                     t)
;;   )

;; (use-package mu4e-views
;;   :after mu4e
;;   :defer nil
;;   :bind (:map mu4e-headers-mode-map
;; 	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
;; 	    ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
;; 	    ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
;;         ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
;; 	    )
;;   :config
;;   (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
;;   (setq mu4e-views-default-view-method "html") ;; make xwidgets default
;;   (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
;;   (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
;;   (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view

;; (use-package org-mime
;;   :after mu4e
;;   )

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo.gpg")))
