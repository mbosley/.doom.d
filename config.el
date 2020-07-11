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
    (setq doom-theme 'doom-one)))
(set-theme-type)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Desktop/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

(use-package org
:after org
:config
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-src-window-setup 'current-window)
(setq org-habit-show-all-today t)
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)
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
(setq org-refile-targets '((nil :maxlevel . 5)
                            (org-agenda-files :maxlevel . 5)))
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
)

(use-package org-ref    ;; references
:after org
;; :hook (org-mode . org-ref) ;; try this out
:config
(setq reftex-default-bibliography '("/Users/mitchellbosley/library.bib")
    org-ref-bibliography-notes "/Users/mitchellbosley/Desktop/org/paper_notes.org"
    org-ref-default-bibliography '("/Users/mitchellbosley/library.bib")
    org-ref-notes-directory "/Users/mitchellbosley/Desktop/org/roam"
    bibtex-completion-bibliography '("/Users/mitchellbosley/library.bib")
    bibtex-completion-notes-path "/Users/mitchellbosley/Desktop/org/roam"
    bibtex-completion-pdf-field "file"
    bibtex-completion-notes-template-multiple-files
     (concat
      "#+TITLE: ${title}\n"
      "#+ROAM_KEY: cite:${=key=}\n"
      "* TODO Notes\n"
      ":PROPERTIES:\n"
      ":Custom_ID: ${=key=}\n"
      ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
      ":AUTHOR: ${author-abbrev}\n"
      ":JOURNAL: ${journaltitle}\n"
      ":DATE: ${date}\n"
      ":YEAR: ${year}\n"
      ":DOI: ${doi}\n"
      ":URL: ${url}\n"
      ":END:\n\n")
    org-ref-completion-library 'org-ref-ivy-cite
    org-ref-notes-function 'orb-edit-notes
    org-latex-prefer-user-labels t
    org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"


    org-latex-pdf-process
    '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f")
    org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
    org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")

    )

(setq org-ref-notes-function
    (lambda (thekey)
        (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
        (bibtex-completion-edit-notes
            (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

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

(use-package org-gcal
  :commands
  load-gcal
  :init
  (add-hook 'gcal-mode 'load-gcal)
  :config
  (setq org-gcal-client-id "787977855889-libbeje55t66hi60f0amb1sdnjn0mkt8.apps.googleusercontent.com"
        org-gcal-client-secret "0xYv3BfA_6sY0Ss2gH-VIEkp"
        org-gcal-file-alist '(("mitchellbosley@gmail.com" .  "/Users/mitchellbosley/Desktop/org/schedule.org")
                              ("mcbosley@umich.edu" .  "/Users/mitchellbosley/Desktop/org/schedule.org")))
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
  ;; (setq org-roam-directory "Users/mitchellbosley/Desktop/org/roam/")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam )
  (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox")
  (setq org-roam-link-title-format "§%s")
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n
- tags ::"
         :unnarrowed t)))
  :config
  (org-roam-mode +1))

(use-package company-org-roam
  :after org-roam company org
  :config
  (push 'company-org-roam company-backends))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "Orb-note-actions" "a" #'orb-note-actions )
  (setq orb-preformat-keywords
        '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

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
** Critique"))))

(use-package org-roam-server
  ;; :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 40
        org-roam-server-network-label-wrap-length 20))

(use-package deft
  ;; :after org
  :bind
  ("C-c n d" . deft)
  :init
  (setq deft-recursive t
        deft-use-filter-string-for-filename t
        deft-default-extension "org"
        deft-directory "/Users/mitchellbosley/Desktop/org/roam"
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase))))

(use-package mathpix.el
  :init
  (setq mathpix-screenshot-method "screencapture -i %s")
  :custom
  ((mathpix-app-id "app-id")
   (mathpix-app-ky "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

(use-package org-fragtog
  :after org
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  )
