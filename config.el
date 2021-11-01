;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mitchell Bosley"
      user-mail-address "mcbosley@umich.edu")
(setq use-package-verbose t) (setq comint-scroll-to-bottom-on-output 'others)
(setq comint-scroll-to-bottom-on-input t)
(setq ispell-dictionary "en")
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
      (setq doom-theme 'modus-operandi)
    (setq doom-theme 'modus-vivendi)))
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

(use-package pdf-tools
  :defer t
  :init (setq pdf-view-use-scaling t))

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
        (if (> (x-display-pixel-width) 700)
            (setq default-frame-alist
                  '(
                    (top . 0.5)(left . 0.5)
                    (width . 200)(height . 80)
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

;; (use-package exec-path-from-shell
;;   :config
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize))
;;   )

(use-package org-roam
  :config
  (setq org-roam-directory "/Users/mitchellbosley/Desktop/org/roam/")
  (setq org-roam-completion-everywhere nil)
  )

;; (use-package! websocket
;;     :after org-roam)

;; (use-package! org-roam-ui
;;     :after org-roam ;; or :after org
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))
;; (use-package org-roam
;;   ;; :commands (org-roam-insert org-roam-find-file org-roam)
;;   ;; :after org
;;   ;; :defer t
;;   ;; :init
;;   ;; (setq org-roam-directory "/Users/mitchellbosley/Desktop/org/roam/")
;;   ;; (map! :leader
;;   ;;       :prefix "n"
;;   ;;       :desc "Org-Roam-Insert" "i" #'org-roam-insert
;;   ;;       :desc "Org-Roam-Find"   "/" #'org-roam-find-file
;;   ;;       :desc "Org-Roam-Buffer" "r" #'org-roam )
;;   ;; (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox")
;;   ;; (setq org-roam-link-title-format "§%s")
;;   ;; (setq org-roam-completion-everywhere t)
;;   ;; (setq org-roam-v2-ack t)
;; ;;   (setq org-roam-capture-templates
;; ;;         '(("d" "default" plain (function org-roam--capture-get-point)
;; ;;            "%?"
;; ;;            :file-name "%<%Y%m%d%H%M%S>-${slug}"
;; ;;            :head "#+TITLE: ${title}\n
;; ;; - tags ::"
;; ;;            :unnarrowed t)))
;; ;;   :config
;; ;;   (setq org-roam-dailies-capture-templates
;; ;;         (let ((head "#+title: %<%Y-%m-%d (%A)>\n#+startup: showall\n* [/] Do Today\n* [/] Maybe Do Today\n* Journal\n"))
;; ;;           `(("j" "journal" entry
;; ;;              #'org-roam-capture--get-point
;; ;;              "* %<%H:%M> %?"
;; ;;              :file-name "daily/%<%Y-%m-%d>"
;; ;;              :head ,head
;; ;;              :olp ("Journal"))
;; ;;             ("t" "do today" item
;; ;;              #'org-roam-capture--get-point
;; ;;              "[ ] %(princ as/agenda-captured-link)"
;; ;;              :file-name "daily/%<%Y-%m-%d>"
;; ;;              :head ,head
;; ;;              :olp ("Do Today")
;; ;;              :immediate-finish t)
;; ;;             ("m" "maybe do today" item
;; ;;              #'org-roam-capture--get-point
;; ;;              "[ ] %(princ as/agenda-captured-link)"
;; ;;              :file-name "daily/%<%Y-%m-%d>"
;; ;;              :head ,head
;; ;;              :olp ("Maybe Do Today")
;; ;;              :immediate-finish t))))

;; ;;   (org-roam-mode +1)
;;   )

;; ;; (use-package company-org-roam
;; ;;   :after org-roam company org
;; ;;   :config
;; ;;   (push 'company-org-roam company-backends))

;; (use-package org-roam-bibtex
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :init
;;   (map! :leader
;;         :prefix "n"
;;         :desc "Orb-note-actions" "a" #'orb-note-actions )
;;   (setq orb-preformat-keywords
;;         '("citekey" "title" "url" "author-or-editor" "keywords" "file")
;;         orb-process-file-keyword t
;;         orb-file-field-extensions '("pdf"))

;;   (setq orb-templates
;;         `(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "${citekey}"
;;            :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

;; - tags ::
;; - keywords :: ${keywords}

;; * ${title}
;; :PROPERTIES:
;; :Custom_ID: ${citekey}
;; :URL: ${url}
;; :AUTHOR: ${author-or-editor}
;; :NOTER_DOCUMENT: ${file}  ; <== special file keyword: if more than one filename
;; :NOTER_PAGE:              ;     is available, the user will be prompted to choose
;; :END:")))




;;   (setq orb-preformat-keywords
;;         '("citekey" "title" "url" "file" "author-or-editor" "keywords"))

;;     (setq orb-templates
;;           '(("r" "ref" plain (function org-roam-capture--get-point)
;;              ""
;;              :file-name "${slug}"
;;              :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

;;   - tags ::
;;   - keywords :: ${keywords}

;;   * Notes
;;   :PROPERTIES:
;;   :Custom_ID: ${citekey}
;;   :URL: ${url}
;;   :AUTHOR: ${author-or-editor}
;;   :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
;;   :NOTER_PAGE:
;;   :END:
;;   ** Argument
;;   ** Methods
;;   ** Results
;;   ** Contribution
;;   ** Critique")))
;;   )

(use-package org
  ;; :after org
  :defer t
  :config
  (setq org-preview-latex-default-process 'dvisvgm)
  (add-to-list 'org-latex-packages-alist
               '("" "tikz" t))
  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
  (setq org-src-window-setup 'current-window)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HABIT(b)" "PROJ(p)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
  (setq org-todo-keyword-faces
        '(("HABIT" . "dark magenta") ("NEXT" . "light green") ("WAIT" . "goldenrod2") ("PROJ" . "DarkOrange1")))
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
  (setq org-agenda-custom-commands
        '(("W" "Weekly review"
           agenda ""
           ((org-agenda-start-day "-14d")
            (org-agenda-span 14)
            (org-agenda-start-on-weekday 1)
            (org-agenda-start-with-log-mode '(closed))
            (org-agenda-archives-mode t)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))
          ("n" "Agenda and all TODOs"
           agenda ""
           ((alltodo "")))
          ))
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
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (timeline . "  % s")
                                   (todo .
                                         " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
                                   (tags .
                                         " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
                                   (search . " %i %-12:c"))
        )
  ;; (setq org-super-agenda-keep-order t)
  )

(use-package vulpea
  :ensure t
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
;;   :config
;;   (setq org-agenda-prefix-format
;;         '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
;;           (todo . " %i %(vulpea-agenda-category 12) ")
;;           (tags . " %i %(vulpea-agenda-category 12) ")
;;           (search . " %i %(vaulpea-agenda-category 12) ")))

;;   (defun vulpea-agenda-category (&optional len)
;;     "Get category of item at point for agenda.

;; Category is defined by one of the following items:

;; - CATEGORY property
;; - TITLE keyword
;; - TITLE property
;; - filename without directory and extension

;; When LEN is a number, resulting string is padded right with
;; spaces and then truncated with ... on the right if result is
;; longer than LEN.

;; Usage example:

;;   (setq org-agenda-prefix-format
;;         '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

;; Refer to `org-agenda-prefix-format' for more information."
;;     (let* ((file-name (when buffer-file-name
;;                         (file-name-sans-extension
;;                          (file-name-nondirectory buffer-file-name))))
;;            (title (vulpea-buffer-prop-get "title"))
;;            (category (org-get-category))
;;            (result
;;             (or (if (and
;;                      title
;;                      (string-equal category file-name))
;;                     title
;;                   category)
;;                 "")))
;;       (if (numberp len)
;;           (s-truncate len (s-pad-right len " " result))
;;         result)))
  )

(use-package org-super-agenda
  :after (org org-agenda evil-org-agenda)
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
           :order 0)
          (:log t)
          (:name "Next to do"
           :todo "NEXT"
           :order 0)
          (:auto-category)
          ;; ;; (:auto-property "PROJECT"
          ;; ;;  :order 5
          ;; ;;  )
          ;; (:name "To refile"
          ;;  :file-path "inbox.org")
          ;; (:name "Important"
          ;;  :and (:todo "TODO" :priority "A")
          ;;  :order 6)
          ;; (:name "Habits"
          ;;  :habit t
          ;;  :order 99)
          ;; (:name "Due Today"
          ;;  :deadline today
          ;;  :order 2)
          ;; ;; (:name "Scheduled Today"
          ;; ;;  :scheduled today
          ;; ;;  :order 1)
          ;; (:name "Scheduled Soon"
          ;;  :scheduled future
          ;;  :order 12)
          ;; (:name "Deadline Soon"
          ;;  :deadline future
          ;;  :order 8)
          ;; (:name "Overdue"
          ;;  :deadline past
          ;;  :order 7)
          ;; ;; (:name "Meetings"
          ;; ;;  :and (:todo "MEET" :scheduled future)
          ;; ;;  :order 10)
          ;; (:discard (:not (:todo "TODO")))
          ;; ;; (:discard (:anything))
          ))
  :config
  (org-super-agenda-mode)
  ;; (require 'evil)
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))
  )

;; (use-package ivy-bibtex
;;   ;; :defer t
;;   :after org
;;   :config
;;   (setq bibtex-completion-format-citation-functions
;;         '((org-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;           (latex-mode . bibtex-completion-format-citation-cite)
;;           (default . bibtex-completion-format-citation-default))
;;         bibtex-completion-pdf-field "file"
;;         bibtex-completion-additional-search-fields '("journaltitle")
;;         bibtex-completion-pdf-symbol "@"
;;         bibtex-completion-notes-symbol "#"
;;         bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${author:20} ${year:4} ${title:*} ${=type=:3} ${journaltitle:10}")))
;;   (setq bibtex-completion-bibliography '("/Users/mitchellbosley/library.bib")
;;         bibtex-completion-library-path "/Users/mitchellbosley/"
;;         bibtex-completion-notes-path org-directory)
;;   (ivy-set-display-transformer
;;    'org-ref-ivy-insert-cite-link
;;    'ivy-bibtex-display-transformer))

;; (use-package org-ref    ;; references
;;   :after org
;;   :init
;;   (setq org-ref-completion-library 'org-ref-ivy-cite)
;;   :config
;;   (setq
;;    org-ref-default-bibliography '("/Users/mitchellbosley/library.bib")
;;    org-ref-notes-function 'orb-edit-notes
;;    org-latex-prefer-user-labels t
;;    org-latex-pdf-process
;;    '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;      "bibtex %b"
;;      "pdflatex -interaction nonstopmode -output-directory %o %f"
;;      "pdflatex -interaction nonstopmode -output-directory %o %f")
;;    org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
;;    org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;;   ;; :hook (org-ref . ivy-bibtex)
;;   (defun my/org-ref-open-pdf-at-point ()
;;     "Open the pdf for bibtex key under point if it exists."
;;     (interactive)
;;     (let* ((results (org-ref-get-bibtex-key-and-file))
;;            (key (car results))
;;            (pdf-file (car (bibtex-completion-find-pdf key))))
;;       (if (file-exists-p pdf-file)
;;           (org-open-file pdf-file)
;;         (message "No PDF found for %s" key)))))

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

(use-package deft
  ;; :after org
  :bind
  ("C-c n d" . deft)
  :config
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))
  
  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)
  
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                "\\)"))
  (setq deft-recursive t
        deft-use-filter-string-for-filename t
        ;; deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
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

(use-package lsp-mode
  :config
  (setq lsp-auto-guess-root t))

(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.5")
  ;; (setq lsp-julia-package-dir nil))
  (setq lsp-enable-folding t)
  ;; (setq lsp-julia-package-dir "~/.emacs.d/.local/straight/repos/lsp-julia/languageserver")
  ;; (require 'lsp-julia)
  (add-hook 'ess-julia-mode-hook #'lsp-mode))

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

(use-package ox-jekyll-lite
  :after org
  :config (setq org-jekyll-project-root "/Users/mitchellbosley/mbosley.github.io")
  )

;; (use-package! bibtex-completion
;;   :defer t
;;   :config
;;   (setq bibtex-completion-pdf-field "file"
;;         bibtex-completion-additional-search-fields '("journaltitle")
;;         bibtex-completion-pdf-symbol "@"
;;         bibtex-completion-notes-symbol "#"
;;         bibtex-completion-display-formats '((t . "${=has-pdf=:1}${=has-note=:1} ${author:20} ${year:4} ${title:*} ${=type=:3} ${journaltitle:10}")))
;;         bibtex-completion-bibliography '("/Users/mitchellbosley/library.bib")
;;         bibtex-completion-library-path "/Users/mitchellbosley/"
;;         bibtex-completion-notes-path '("/Users/mitchellbosley/Desktop/org/roam"))

;; (use-package! ivy-bibtex
;;   :when (featurep! :completion ivy)
;;   :defer t
;;   :config
;;   (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

;; (use-package! bibtex-actions
;;   :when (featurep! :completion vertico)
;;   :after embark bibtex-completion
;;   :config
;;   (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map)))

;; (use-package! citeproc
;;   :defer t)

;; ;; ;; ;;; Org-Cite configuration

;; (use-package! oc
;;   :after org bibtex-completion bibtex-actions
;;   :config
;;   (require 'ox)
;;   (map! :map org-mode-map
;;         :localleader
;;         :desc "Insert citation" "@" #'org-cite-insert)
;;   (setq org-cite-global-bibliography
;;         (let ((paths (or bibtex-actions-bibliography
;;                          bibtex-completion-bibliography)))
;;           ;; Always return bibliography paths as list for org-cite.
;;           (if (stringp paths) (list paths) paths)))
;;   ;; setup export processor; default csl/citeproc-el, with biblatex for latex
;;   (setq org-cite-export-processors
;;         '((latex biblatex)
;;           (t csl))))

;; (use-package! oc-bibtex-actions
;;   :when (featurep! :completion vertico)
;;   :after (oc bibtex-actions)
;;   :config
;;   (setq org-cite-insert-processor 'oc-bibtex-actions
;;         org-cite-follow-processor 'oc-bibtex-actions))

;; ;; Set bibliography paths so they are the same.
;; (defvar my/bibs '("/Users/mitchellbosley/library.bib"))

;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode))

;; (use-package embark
;;   :ensure t

;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init

;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))


;; (use-package oc-bibtex-actions
;;   :bind (("C-c b" . org-cite-insert)
;;          ("M-o" . org-open-at-point)
;;          :map minibuffer-local-map
;;          ("M-b" . bibtex-actions-insert-preset))
;;   :after (embark oc)
;;   :config
;;   (setq bibtex-actions-bibliography my/bibs
;;         org-cite-global-bibliography my/bibs
;;         org-cite-insert-processor 'oc-bibtex-actions
;;         org-cite-follow-processor 'oc-bibtex-actions
;;         org-cite-activate-processor 'basic))
;;   (setq bibtex-actions-templates
;;         '((main . "${author editor:30}     ${date year issued:4}     ${title:48b}")
;;         (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
;;         (note . "#+title: Notes on ${author editor}, ${title}")))
;; ;; Use consult-completing-read for enhanced interface.
;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)



;;   ;;; Org-cite processors
;; (use-package! oc-basic
;;   :after oc)

;; (use-package! oc-biblatex
;;   :after oc)

;; (use-package! oc-csl
;;   :after oc)

;; (use-package! oc-natbib
;;   :after oc)

;; ;; ;;;; Third-party

;; (use-package! oc-bibtex-actions
;;   :when (featurep! :completion vertico)
;;   :after oc
;;   :demand t
;;   :config
;;   (setq org-cite-insert-processor 'oc-bibtex-actions
;;         org-cite-follow-processor 'oc-bibtex-actions
;;         org-cite-activate-processor 'basic))

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"));; This tell bibtex-completion to look at the File field of the bibtex to figure out which pdf to open

(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

(use-package! bibtex-actions
  :when (featurep! :completion vertico)
  :after embark bibtex-completion
  :config
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map)))

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(use-package! oc
  :after org bibtex-completion bibtex-actions
  :config
  (require 'ox)
  (map! :map org-mode-map
        :localleader
        :desc "Insert citation" "@" #'org-cite-insert)
  (setq org-cite-global-bibliography
        (let ((paths (or bibtex-actions-bibliography
                         bibtex-completion-bibliography)))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((latex biblatex)
          (t csl))))

(use-package! oc-bibtex-actions
  :when (featurep! :completion vertico)
  :after (oc bibtex-actions)
  :config
  (setq bibtex-actions-file-note-org-include '(org-id org-roam-ref))
  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions)
  (setq bibtex-actions-symbols
        `((pdf . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                  ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)
  (setq bibtex-actions-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
          (note . "#+title: Notes on ${author editor} ${year}, ${title}"))))

  ;;; Org-cite processors
(use-package! oc-basic
  :after oc)

(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc)

(use-package! oc-natbib
  :after oc)

;; Use consult-completing-read for enhanced interface.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)


;;;; Third-party

(use-package! oc-bibtex-actions
  :when (featurep! :completion vertico)
  :after oc
  :demand t
  :config
  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions
        org-cite-activate-processor 'basic))

(setq! bibtex-actions-bibliography '("/Users/mitchellbosley/library.bib"))
(setq! bibtex-actions-library-paths '("/Users/mitchellbosley/")
       bibtex-actions-notes-paths '("/Users/mitchellbosley/Desktop/org/roam/"))
(setq! org-cite-csl-styles-dir "/Users/mitchellbosley/Zotero/styles")
