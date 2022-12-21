;;; init.el --- init file for Emacs -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: Masayuki Hatta <mhatta@gnu.org>

;;; Code:

;;;
;;; Profiler (start)
;;;
;;(require 'profiler)
;;(profiler-start 'cpu)

;;;
;;; straight.el
;;;
(setq straight-repository-branch "develop") ;; use the develop branch of straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1) ;; shallow clone

;; See https://github.com/raxod502/straight.el#summary-of-options-for-package-modification-detection
(when (eq system-type 'windows-nt)
  (if (and (executable-find "watchexec")
           (executable-find "python3")
	   (executable-find "diff"))
      (setq straight-check-for-modifications '(watch-files find-when-checking))
    (setq straight-check-for-modifications '(check-on-save find-when-checking))))

;;;
;;; leaf.el
;;;
(eval-and-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (leaf-keywords-init)
  )
  
(leaf leaf
  :require t
  :init
  (leaf leaf-convert
    :straight t
    )
  (leaf leaf-tree
    :straight t
    :blackout t
    :custom
    (imenu-list-position . 'left)
    )
  )

;;;
;;; blackout
;;;
(leaf blackout
  :leaf-defer nil
  :straight t
  :config
  ;; shut up eldoc in modeline
  (leaf eldoc :blackout t)
  )

;;;
;;; Garbage Collector Magic Hack
;;;
(leaf gcmh
  :leaf-defer nil
  :straight t
  :blackout t
  :config
  (gcmh-mode t)
  )

;;;
;;; Language settings
;;;
(leaf Settings
  :config
  (leaf Language
    :config
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (set-default 'buffer-file-coding-system 'utf-8)
    )
  (leaf Fonts
    :config
    (when (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :family "Consolas" :height 110)
      (set-fontset-font 'nil 'japanese-jisx0208
			(font-spec :family "メイリオ")))
    (when (eq system-type 'gnu/linux)
      ;; Install e.g. fonts-inconsolata & fonts-ipaexfont on Debian/Ubuntu
      (set-frame-font "Inconsolata-14")
      (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAExGothic")))
    )
  (leaf Misc
    :config
    (define-key key-translation-map [?\C-h] [?\C-?])
    (column-number-mode t)
    :custom
    '((user-full-name . "Your Name") ;; CHANGEME
      (user-mail-address . "you@example.com") ;; CHANGEME
      (inhibit-startup-message . t)
      (kinsoku-limit . 10)
      ;;      (browse-url-browser-function . 'eww-browse-url)
    )
    )
  )

;;;
;;; Japanese IME
;;;
(leaf Japanese-IME
  :config
  ;; tr-ime (for Windows)
  (leaf tr-ime
    :if (eq system-type 'windows-nt)
    :straight t
    :config
    (tr-ime-advanced-install 'no-confirm)
    (setq default-input-method "W32-IME")
    (w32-ime-initialize)
    (setq-default w32-ime-mode-line-state-indicator "[--]")
    (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
    ;; Fonts used during henkan
    (modify-all-frames-parameters '((ime-font . "メイリオ-11")))
    ;; IME control
    (wrap-function-to-control-ime 'universal-argument t nil)
    (wrap-function-to-control-ime 'read-string nil nil)
    (wrap-function-to-control-ime 'read-char nil nil)
    (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
    (wrap-function-to-control-ime 'y-or-n-p nil nil)
    (wrap-function-to-control-ime 'yes-or-no-p nil nil)
    (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
    (wrap-function-to-control-ime 'register-read-with-preview nil nil)
    )
  ;; Mozc (for GNU/Linux)
  (leaf mozc
    :if (eq system-type 'gnu/linux)
    :straight t
    :config
    (setq default-input-method "japanese-mozc")
    ;; mozc-posframe
    (leaf mozc-cand-posframe
      :if (eq system-type 'gnu/linux)
      :after mozc
      :straight t
      :config
      (setq mozc-candidate-style 'posframe)
      )
    )
  ;; ddskk
  (leaf ddskk
    :straight t
    :bind
    (("C-x C-j" . skk-mode)
     ("C-x j"   . skk-mode))
    )
  ;; ddskk-posframe
  (leaf ddskk-posframe
    :straight t
    :blackout t
    :config
    (ddskk-posframe-mode t)
    )
  )

;;;
;;; Looks
;;;
(leaf Looks
  :config
  ;; Theme (Modus)
  (leaf modus-themes
    :straight t
    :init
    ;; Add all your customizations prior to loading the themes
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
          modus-themes-region '(bg-only no-extend))
    ;; Load the theme files before enabling a theme
    (modus-themes-load-themes)
    :config
    ;; Load the theme of your choice:
    (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
    :bind ("<f5>" . modus-themes-toggle)
    )
  :custom
  ;;  '((tool-bar-mode . nil)
  )

;;;
;;; minibuffer completion
;;;
(leaf Minibuf-completion
  :config
  ;; vertico
  (leaf vertico
    :straight t
    :init
    (vertico-mode)
    )
  ;; consult
  (leaf consult
    :straight t
    :bind
    (("C-s" . consult-line))
    )
  ;; orderless
  (leaf orderless
    :straight t
    :require t
    :after migemo
    :config
    ;; Using migemo with orderless
    (defun orderless-migemo (component)
      (let ((pattern (migemo-get-pattern component)))
	(condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    (orderless-define-completion-style orderless-default-style
      (orderless-matching-styles '(orderless-literal
				   orderless-regexp)))
    (orderless-define-completion-style orderless-migemo-style
      (orderless-matching-styles '(orderless-literal
				   orderless-regexp
				   orderless-migemo)))
    (setq completion-category-overrides
          '((command (styles orderless-default-style))
            (file (styles orderless-migemo-style))
            (buffer (styles orderless-migemo-style))
            (symbol (styles orderless-default-style))
            (consult-location (styles orderless-migemo-style))
            (consult-multi (styles orderless-migemo-style))
            (org-roam-node (styles orderless-migemo-style))
            (unicode-name (styles orderless-migemo-style))
            (variable (styles orderless-default-style))))
    ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))
    :custom
    (completion-styles . '(orderless))
    )
  ;; marginalia
  (leaf marginalia
    :straight t
    :init
    (marginalia-mode)
    )
  ;; migemo
  (leaf migemo
    :if (executable-find "cmigemo")
    :straight t
    :require t
    :config
    (setq migemo-command "cmigemo"
          migemo-options '("-q" "--emacs")
	  migemo-user-dictionary nil
          migemo-regex-dictionary nil
          migemo-coding-system 'utf-8-unix)
    (when (eq system-type 'gnu/linux)
      (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
    (when (eq system-type 'windows-nt)
      (setq migemo-dictionary "C:/cmigemo-mingw64/share/migemo/utf-8/migemo-dict"))
    (migemo-init)
    )
  )

;;;
;;; org-mode
;;;
(leaf Org
  :config
  (leaf org
    :straight t
    :leaf-defer t
    :bind
    (("\C-ca" . org-agenda)
     ("\C-cc" . org-capture)
     ("\C-ch" . org-store-link))
    :config
    (setq org-directory "~/Org" ;; CHANGEME
	  org-agenda-files (list org-directory)
	  org-default-notes-file "notes.org"
	  org-log-done 'time
	  org-startup-truncated nil
	  org-startup-folded 'content
	  org-use-speed-commands t
	  org-enforce-todo-dependencies t)
    (remove (concat org-directory "/archives") org-agenda-files)
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
    (setq org-refile-targets
	  (quote ((nil :maxlevel . 3)
		  (org-buffer-files :maxlevel . 1)
		  (org-agenda-files :maxlevel . 3))))
    (setq org-capture-templates
	  '(("t" "Todo" entry (file+headline "gtd.org" "Inbox")
	     "* TODO %?\n %i\n %a")
            ("n" "Note" entry (file+headline "notes.org" "Notes")
	     "* %?\nEntered on %U\n %i\n %a")
            ("j" "Journal" entry (function org-journal-find-location)
	     "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
	    ("h" "Hugo post" entry (file+olp "jamhattaorg.org" "Blog Ideas")
             (function org-hugo-new-subtree-post-capture-template))
	    ))
    ;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
	"Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
	(let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug title)))
	  (mapconcat #'identity
                     `(
                       ,(concat "*** TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
		   ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :share true :featured false :slug :image "
		   ":EXPORT_DESCRIPTION: "
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                     "\n"))))
    )
  ;; org-babel
  (leaf ob
    :after org
    :defun org-babel-do-load-languages
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (python . t)
       (R . t)
       (ditaa . t)
       ))
    (when (eq system-type 'windows-nt)
      (setq org-ditaa-jar-path "~/ownCloud/jditaa.jar") ;; CHANGEME
      )
    (when (eq system-type 'gnu/linux)
      (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
      )
    )
  
  ;; Opening Org notes by shortcuts
  (leaf Opening-org-notes
    :config
    (defun org-buffer-files ()
      "Return list of opened Org mode buffer files."
      (mapcar (function buffer-file-name)
	      (org-buffer-list 'files)))
    (defun show-org-buffer (file)
      "Show an org-file FILE on the current buffer."
      (interactive)
      (if (get-buffer file)
	  (let ((buffer (get-buffer file)))
	    (switch-to-buffer buffer)
	    (message "%s" file))
	(find-file (concat org-directory "/" file))))
    (global-set-key (kbd "C-M--") '(lambda () (interactive)
				     (show-org-buffer "gtd.org")))
    (global-set-key (kbd "C-M-^") '(lambda () (interactive)
				     (show-org-buffer "notes.org")))
    (global-set-key (kbd "C-M-~") '(lambda () (interactive)
				     (show-org-buffer "kb.org")))
    )
  
  ;; org-superstar
  (leaf org-superstar
    :after org
    :straight t
    :custom
    (org-superstar-headline-bullets-list . '("◉" "★" "○" "▷" "" ""))
    :hook
    (org-mode-hook (lambda () (org-superstar-mode 1)))
    )
  
  ;; org-journal
  (leaf org-journal
    :after org
    :straight t
    :config
    (setq org-journal-dir (concat org-directory "/journal")
	  org-journal-enable-agenda-integration t)
    (defun org-journal-find-location ()
      ;; Open today's journal, but specify a non-nil prefix argument in order to
      ;; inhibit inserting the heading; org-capture will insert the heading.
      (org-journal-new-entry t)
      ;; Position point on the journal's top-level heading so that org-capture
      ;; will add the new entry as a child entry.
      (goto-char (point-min))
      )
    )

  ;; org-web-tools
  (leaf org-web-tools
    :after org
    :straight t
    )

  ;; org-cliplink
  (leaf org-cliplink
    :after org
    :straight t
    :bind
    ("C-x p i" . org-cliplink)
    )

  ;; org-download
  (leaf org-download
    :after org
    :straight t
    :config
    (setq-default org-download-image-dir (concat org-directory "/pictures"))
    )
  
  ;; toc-org
  (leaf toc-org
    :after org markdown-mode
    :straight t
    ;;:commands toc-org-enable
    :config
    (add-hook 'org-mode-hook 'toc-org-enable)
    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode)
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
    )
  
  ;; ox-hugo
  (leaf ox-hugo
    :after ox
    :straight t
    :require t
    )

  ;; org2blog
  (leaf org2blog
    :after org
    :straight (org2blog :type git :host github :repo "sachac/org2blog")
    :leaf-autoload org2blog-autoloads
    :commands org2blog-user-login
    :config
    (setq org2blog/wp-blog-alist
          `(("wp"
             :url "https://www.example.com/xmlrpc.php" ;; CHANGEME
             :username ,(car (auth-source-user-and-password "wordpress")) ;; CHANGEME
             :password ,(cadr (auth-source-user-and-password "wordpress")) ;; CHANGEME
	     )
	    ))
    (setq org2blog/wp-buffer-template
	  "#+TITLE: 
#+CATEGORY: 
#+TAGS: 
#+OPTIONS:
#+PERMALINK: \n")
    )

;; ;; org-re-reveal
;; (when (eq system-type 'windows-nt)
;;   (use-package org-re-reveal
;;     :after org
;;     :defer t
;;     :config
;;     (setq org-re-reveal-root "file:///c:/Users/mhatta/ownCloud/reveal.js"))
;;   )

  ;; org-roam
  (leaf org-roam
    :after org
    :straight t
    :init
    ;;  (org-roam-db-update-method . 'immediate)
    (setq org-roam-v2-ack t)
    (setq org-roam-directory "~/ownCloud/Org/org-roam/")
    (setq org-roam-index-file "~/ownCloud/Org/org-roam/Index.org")
    :custom
    (org-roam-db-location . "~/.emacs.d/org-roam.db")
    :hook
    ;;  (after-init . org-roam-mode)
    :bind
    ((:org-roam-mode-map
      ("C-c n l" . org-roam)
      ("C-c n f" . org-roam-find-file)
      ("C-c n g" . org-roam-graph))
     (:org-mode-map
      ("C-c n i" . org-roam-insert)
      ("C-c n I" . org-roam-insert-immediate)))
    ;; :bind (("C-c n l" . org-roam-buffer-toggle)
    ;; 	 ("C-c n f" . org-roam-node-find)
    ;; 	 ("C-c n g" . org-roam-graph)
    ;; 	 ("C-c n i" . org-roam-node-insert)
    ;; 	 ("C-c n c" . org-roam-capture)
    ;; 	 ("C-c n j" . org-roam-dailies-capture-today))
    :config
    (org-roam-setup)
    (org-roam-db-autosync-mode)
    (when (eq system-type 'windows-nt)  
      (setq org-roam-graph-viewer
	    (lambda (file)
	      (let ((org-roam-graph-viewer "c:/Program Files/Mozilla Firefox/firefox.exe"))
		(org-roam-graph--open (concat "file:///" file))))))
    )
  
  ;; (use-package org-roam-ui
  ;;   :straight
  ;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  ;;     :after org-roam
  ;; ;;    :hook
  ;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; ;;         a hookable mode anymore, you're advised to pick something yourself
  ;; ;;         if you don't care about startup time, use
  ;; ;;  :hook (after-init . org-roam-ui-mode)
  ;;     :config
  ;;     (setq org-roam-ui-sync-theme t
  ;;           org-roam-ui-follow t
  ;;           org-roam-ui-update-on-save t
  ;;           org-roam-ui-open-on-start t))

)

(leaf Modes
  :config
  ;; rainbow-mode
  (leaf rainbow-mode
    :ensure t
    :leaf-defer t
    :hook
    (web-mode-hook . rainbow-mode)
    )
  ;; Markdown
  (leaf Markdown
    :config
    ;; markdown-mode
    (leaf markdown-mode
      :straight t
      :leaf-defer t
      :mode ("\\.md\\'" . gfm-mode)
      )
    ;; markdown-preview-mode
    (leaf markdown-preview-mode
      :straight t
      )
    )

  )

(leaf Tools
  :config
  ;; smartparens
  (leaf smartparens
    :straight t
    :blackout t
    :require smartparens-config
    :hook
    (prog-mode-hook . turn-on-smartparens-mode)
    :config
    (show-smartparens-global-mode t))
  
  ;; rainbow-delimiters
  (leaf rainbow-delimiters
    :straight t
    :hook
    (prog-mode-hook . rainbow-delimiters-mode)
    )

  ;; beacon
  (leaf beacon
    :straight t
    :blackout t
    :config
    (beacon-mode t)
    )
  
  ;; google-this
  (leaf google-this
    :straight t
    :bind
    ("M-s g" . google-this-noconfirm)
    )

  ;; free-keys
  (leaf free-keys
    :straight t
    )

  ;; popwin
  (leaf popwin
    :straight t
    :config
    (popwin t)
    )

  ;; ripgrep
  (leaf ripgrep
    :straight t
    :leaf-defer t
    :bind
    ("M-s r" . ripgrep-regexp)
    )

  ;; projectile
  (leaf projectile
    :straight t
    :blackout t
    :config
    (projectile-mode t)
    )

  ;; yasnippet
  (leaf yasnippet
    :straight t
    :blackout yas-minor-mode
    :commands yas-global-mode
    :hook ((after-init-hook . yas-global-mode))
    :custom ((yas-snippet-dirs . '("~/.emacs.d/snippets")))
    )

  ;; atomic-chrome
  (leaf atomic-chrome
    :straight t
    :config
    (atomic-chrome-start-server)
    )
  
  ;; restart-emacs
  (leaf restart-emacs
    :straight t
    )
  
  ;; magit
  (leaf magit
    :straight t
    :bind
    ("C-x g" . magit-status)
    )

  ;; easy-hugo
  (leaf easy-hugo
    :straight t
    :config
    (setq easy-hugo-basedir "~/Hugo") ;; CHANGEME
    (setq easy-hugo-url "https://www.example.com") ;; CHANGEME
    (setq easy-hugo-bloglist
	  '(((easy-hugo-basedir . "https://www2.example.com") ;; CHANGEME
	     (easy-hugo-url . "https://www2.example.com"))))
    )
  )
  
;;;
;;; exec-path-from-shell
;;;
(leaf exec-path-from-shell
  :require t
  :if (memq window-system '(mac ns x))
  :straight t
  :init
  (exec-path-from-shell-initialize)
  )

;;;
;;; Profiler (report)
;;;
;;(profiler-report)
;;(profiler-stop)

;;; init.el ends here
