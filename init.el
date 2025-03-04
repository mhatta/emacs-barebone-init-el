;;; init.el --- init file for Emacs -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: Masayuki Hatta <mhatta@gnu.org>

;;; Commentary:

;; A boilerplate configuration file for modern Emacs experience.

;;; Code:

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
;;; Blackout
;;;
(leaf blackout
  :leaf-defer nil
  :straight t
  :config
  ;; shut up eldoc in modeline
  (leaf eldoc :blackout t)
  )

;;;
;;; Defer loading several libraries (for speeding up)
;;;
(leaf Libraries
  :config
  (leaf cl-lib
    :leaf-defer t
    )
  (leaf dash
    :straight t
    :leaf-defer t
    )
  )

;;;
;;; Garbage Collector Magic Hack
;;;
(leaf gcmh
  :leaf-defer nil
  :straight t
  :blackout t
  :global-minor-mode gcmh-mode
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
    :if (display-graphic-p)
    :config
    ;; unicode-fonts
    (leaf unicode-fonts
      :straight t
      :config
      (unicode-fonts-setup)
      )
    (when (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :family "Consolas" :height 120) ;; CHANGEME
      (set-fontset-font 'nil 'japanese-jisx0208
			(font-spec :family "Yu Gothic UI")) ;; CHANGEME
      )
    (when (eq system-type 'gnu/linux)
      ;; Install e.g. fonts-inconsolata & fonts-ipaexfont packages on Debian/Ubuntu
      (set-frame-font "Inconsolata-14") ;; CHANGEME
      (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAExGothic"))) ;; CHANGEME
    )

  (leaf Misc
    :config
    (define-key key-translation-map [?\C-h] [?\C-?])
    (column-number-mode t)
    :custom
    '((user-full-name . "Your Name") ;; CHANGEME
      (user-login-name . "yourlogin") ;; CHANGEME
      (user-mail-address . "yourmail@example.org") ;; CHANGEME
      (inhibit-startup-message . t)
      (delete-by-moving-to-trash . t)
      (kinsoku-limit . 10)
      ;; For text-only web browsing
      ;; (browse-url-browser-function . 'eww-browse-url)
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
    ;; should work on terminal too
    :if (eq system-type 'windows-nt)
    :straight t
    :config
    ;; Set up w32 frame
    (defun my-w32-frame-setup ()
      (when (eq (framep (selected-frame)) 'w32)
	;; Only needs to be set up once, so remove it from the hook so that it will not be called in the future
	(remove-hook 'server-after-make-frame-hook #'my-w32-frame-setup)
	;; Font setting
	(set-frame-font "Yu Gothic UI-12" nil t) ;; CHANGEME
	(modify-all-frames-parameters '((ime-font . "Yu Gothic UI-12"))) ;; CHANGEME
	;; tr-ime
	(tr-ime-advanced-install 'no-confirm)
	;; w32-ime setting
	(when (featurep 'w32-ime)
	  (setq default-input-method "W32-IME")
	  (setq-default w32-ime-mode-line-state-indicator "[--]")
	  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
	  (w32-ime-initialize)
	  (wrap-function-to-control-ime 'universal-argument t nil)
	  (wrap-function-to-control-ime 'read-string nil nil)
	  (wrap-function-to-control-ime 'read-char nil nil)
	  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
	  (wrap-function-to-control-ime 'y-or-n-p nil nil)
	  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
	  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
	  (wrap-function-to-control-ime 'register-read-with-preview nil nil)))
      )
    ;; If the selected frame is w32 (normal startup), configure the frame
    ;;  If not w32 (console or daemon mode), add configuration function to hooks
    (if (eq (framep (selected-frame)) 'w32)
	(my-w32-frame-setup)
      (add-hook 'server-after-make-frame-hook #'my-w32-frame-setup))
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
      (setq mozc-candidate-style 'overlay)
      )
    )
  
  ;; ddskk
  (leaf ddskk
    :straight t
    :bind
    (("C-x C-j" . skk-mode)
     ("C-x j"   . skk-mode))
    )

  )

;;;
;;; Looks
;;;
(leaf Looks
  :config
  ;; No menu bar
  ;; (menu-bar-mode -1)
  ;; No tool bar
  ;; (tool-bar-mode -1)
  ;; No scroll bar
  ;; (scroll-bar-mode -1)

  ;; Theme (Modus)
  (leaf modus-themes
    :straight t
    :leaf-defer nil
    :init
    ;; Add all your customizations prior to loading the themes
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
          modus-themes-region '(bg-only no-extend))
    :config
    ;; Load the theme of your widget-choice-match
    (load-theme 'modus-vivendi :no-confirm) ;; OR modus-operandi
    :bind ("<f5>" . modus-themes-toggle)
    )
  
  ;; ;; Theme (zenburn)
  ;; (leaf zenburn-theme
  ;;   :straight t
  ;;   :config
  ;;   (load-theme 'zenburn t)
  ;;   )
  
  ;; dashboard
  (leaf dashboard
    :straight t
    :config
    (dashboard-setup-startup-hook)
    )
  
  ;; all-the-icons
  (leaf all-the-icons
    :if (display-graphic-p)
    :straight t
    :config
    ;; (all-the-icons-install-fonts)
    )
  
  )

;;;
;;; minibuffer completion
;;;
(leaf Minibuf-completion
  :config

  ;; corfu
  (leaf corfu
    :straight t
    :init
    (setq completion-cycle-threshold 3)
    (setq tab-always-indent 'complete)
    :global-minor-mode (global-corfu-mode)
    )
  
  ;; dabbrev
  (leaf dabbrev
    :straight t
    :blackout t
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    ;; Other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
    )

  ;; vertico
  (leaf vertico
    :straight (vertico :files (:defaults "extensions/*")
		       :includes (vertico-directory)
		       )
    :init
    (vertico-mode)
    (setq vertico-count 15)
    :config
    ;; vertico-directory
    (leaf vertico-directory
      :straight t
      :config
      (define-key vertico-map (kbd "C-l") #'vertico-directory-up)
      (define-key vertico-map "RET" #'vertico-directory-enter)  ;; enter dired
      (define-key vertico-map "DEL" #'vertico-directory-delete-char)
      (define-key vertico-map "M-DEL" #'vertico-directory-delete-word)
      :hook
      (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
      )
    )

  ;; consult
  (leaf consult
    :straight t
    :bind
    (("C-x b" . consult-buffer)
     ("M-g M-g" . consult-goto-line)
     ("C-c s" . consult-line)
     ("M-g o" .   consult-outline))
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
				       (orderless-matching-styles '(orderless-initialism
								    orderless-literal
								    orderless-regexp)))
    (orderless-define-completion-style orderless-migemo-style
				       (orderless-matching-styles '(orderless-initialism
								    orderless-literal
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
    (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))
    :custom
    (completion-styles . '(orderless basic))
    )

  ;; marginalia
  (leaf marginalia
    :straight t
    :global-minor-mode marginalia-mode
    :init
    (define-key minibuffer-local-map (kbd "C-M-a") #'marginalia-cycle)
    )
  
  ;;: all-the-icons-completion
  (leaf all-the-icons-completion
    :after (marginalia all-the-icons)
    :straight t
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :global-minor-mode all-the-icons-completion-mode
    )

  ;; cape
  (leaf cape
    :straight t
    :config
    :bind ("C-c p" . cape-prefix-map)
    :init
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-elisp-block)
    )

  ;; kind-icon
  (leaf kind-icon
    :straight t
    :config
    (setq kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    )

  ;; embark
  (leaf embark
    :straight t
    :bind
    (("C-." . embark-act)
     ("C-;" . embark-dwim)
     ("C-h B" . embark-bindings))
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    (leaf embark-consult
      :straight t
      :hook
      (embark-collect-mode . consult-preview-at-point-mode)
      )
    )

  ;; affe
  (leaf affe
    :straight t
    :after consult
    :config
    (consult-customize affe-grep :preview-key (kbd "M-."))
    (defvar affe-orderless-regexp "")
    (defun affe-orderless-regexp-compiler (input _type)
      (setq affe-orderless-regexp (orderless-pattern-compiler input))
      (cons affe-orderless-regexp
            (lambda (str) (orderless--highlight affe-orderless-regexp str))))
    (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
    )
  
  ;; migemo
  (leaf migemo
    :if (executable-find "cmigemo")
    :straight t
    :require t
    :config
    (setq migemo-command "cmigemo"
          migemo-options '("-q" "-e")
	  migemo-user-dictionary nil
          migemo-regex-dictionary nil
          migemo-coding-system 'utf-8-unix)
    (when (eq system-type 'gnu/linux)
      (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
    (when (eq system-type 'windows-nt)
      ;; needs absolute path
      (setq migemo-dictionary (expand-file-name "~/scoop/apps/cmigemo/current/cmigemo-mingw64/share/migemo/utf-8/migemo-dict")))
    (migemo-init)
    )
  
  )

;;;
;;; org-mode
;;;
(leaf Org-mode
  :config
  ;; org
  (leaf org
    :straight t
    :leaf-defer t
    :init
    (setq org-directory "~/Dropbox/Org") ;; CHANGEME
    (unless (file-exists-p org-directory)
      (make-directory org-directory))
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
    :bind
    (("\C-ca" . org-agenda)
     ("\C-cc" . org-capture)
     ("\C-ch" . org-store-link)
     ("C-M--" . #'(lambda () (interactive)
		    (show-org-buffer "gtd.org")))
     ("C-M-^" . #'(lambda () (interactive)
		    (show-org-buffer "notes.org")))
     ("C-M-~" . #'(lambda () (interactive)
    		    (show-org-buffer "kb.org")))
     )
    :config
    (setq  org-agenda-files (list org-directory)
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
	    ("h" "Hugo post" entry (file+headline "jamhattaorg.org" "Jazz")
             (function org-hugo-new-subtree-post-capture-template))
	    ))
    ;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
	"Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
	(let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug (concat (format-time-string "%Y-%m-%d") "-" title))))
	  (mapconcat #'identity
                     `(
                       ,(concat "** TODO " title)
                       "    :PROPERTIES:"
                       ,(concat "    :EXPORT_FILE_NAME: " fname)
		,(concat "    :EXPORT_HUGO_CUSTOM_FRONT_MATTER: :slug " (org-hugo-slug title) " :image ")
                       "    :END:"
                       "    %?\n")          ;Place the cursor here finally
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
       (plantuml . t)
       ))
    ;; Ditaa jar path
    ;; cf. https://tamura70.hatenadiary.org/entry/20100317/org
    (when (eq system-type 'windows-nt)
      (setq org-ditaa-jar-path (expand-file-name "~/Dropbox/jditaa.jar")) ;; CHANGEME
      )
    (when (eq system-type 'gnu/linux)
      (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
      )
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    ;; PlantUML jar path
    (when (eq system-type 'windows-nt)
      (setq org-plantuml-jar-path (expand-file-name "~/Dropbox/plantuml.jar")) ;; CHANGEME
      )
    (when (eq system-type 'gnu/linux)
      (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
      )
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

  ;; org-web-tools
  (leaf org-web-tools
    :after org
    :straight t
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

  ;; ox-qmd
  (leaf ox-qmd
    :after ox
    :straight t
    :require t
    )

;; org-re-reveal
  (leaf org-re-reveal
    :straight t
    :after ox
    :require t
    :config
    (setq org-re-reveal-root (expand-file-name "~/Dropbox/reveal.js"))
    )
  
  ;; org2blog
  (leaf org2blog
    :after org
    ;; If the latest version doesn't work
    ;;:straight (org2blog :type git :host github :repo "sachac/org2blog")
    :straight t
    :leaf-autoload org2blog-autoloads
    :commands org2blog-user-login
    :config
    (setq org2blog/wp-use-sourcecode-shortcode t)
    (setq org2blog/wp-blog-alist
          `(("wp"
	     :url "https://www.example..org/xmlrpc.php" ;; CHANGEME
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

  ;; org-roam
  (leaf org-roam
    :straight t
    :after org
    :bind
    ("C-c n l" . org-roam-buffer-toggle)
    ("C-c n f" . org-roam-node-find)
    ("C-c n g" . org-roam-graph)
    ("C-c n i" . org-roam-node-insert)
    ("C-c n c" . org-roam-capture)
    ("C-c n e" . org-roam-extract-subtree)
    ;; org-roam-dailies
    ("C-c n d n" . org-roam-dailies-capture-today)
    ("C-c n d d" . org-roam-dailies-goto-today)
    ("C-c n d Y" . org-roam-dailies-capture-yesterday)
    ("C-c n d y" . org-roam-dailies-goto-yesterday)
    ("C-c n d T" . org-roam-dailies-capture-yesterday)
    ("C-c n d t" . org-roam-dailies-goto-tomorrow)    
    ("C-c n d b" . org-roam-dailies-goto-next-note)
    ("C-c n d f" . org-roam-dailies-goto-previous-note)
    :config
    (setq org-roam-directory (concat org-directory "/org-roam"))
    (unless (file-exists-p org-directory)
      (make-directory org-roam-directory))
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    (require 'org-roam-protocol)
    )

  ;; org-roam-ui
  (leaf org-roam-ui
    :straight t
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    )

  )

;;;
;;; Modes
;;;

(leaf Modes
  :config
  ;; C/C++
  (leaf cc-mode
    :straight t
    :leaf-defer t
    )

  ;; Python
  (leaf python-mode
    :straight t
    :leaf-defer t
    )

  ;; Haskell
  (leaf haskell-mode
    :straight t
    :leaf-defer t
    :mode (("\\.hs\\'" . haskell-mode)
           ("\\.lhs\\'" . literate-haskell-mode)
           ("\\.cabal\\'" . haskell-cabal-mode))
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

  ;; web-mode
  (leaf web-mode
    :straight t
    :leaf-defer t
    :after flycheck
    :defun flycheck-add-mode
    :mode (("\\.html?\\'" . web-mode)
           ("\\js\\'" . web-mode)
           ("\\.jscss\\'" . web-mode)
           ("\\.css\\'" . web-mode)
           ("\\.scss\\'" . web-mode)
           ("\\.xml\\'" . web-mode))
    :config
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    )

  ;; plantuml-mode
  (leaf plantuml-mode
    :straight t
    :leaf-defer t
    :mode ("\\.plantuml\\'" . plantuml-mode)
    :config
    (setq plantuml-default-exec-mode 'jar)
    ;; PlantUML jar path
    (when (eq system-type 'windows-nt)
      (setq plantuml-jar-path (expand-file-name "~/Dropbox/plantuml.jar")) ;; CHANGEME
      )
    (when (eq system-type 'gnu/linux)
      (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
      )
    )
  
  ;; rainbow-mode
  (leaf rainbow-mode
    :straight t
    :leaf-defer t
    :blackout t
    :hook
    (web-mode-hook . rainbow-mode)
    )
  )

;;;
;;; flycheck
;;;
(leaf Flycheck
  :config
  ;; flycheck
  (leaf flycheck
    :straight t
    :blackout t
    :hook (prog-mode-hook . flycheck-mode)
    :custom ((flycheck-display-errors-delay . 0.3)
             (flycheck-indication-mode . 'left-margin))
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
    (leaf flycheck-posframe
      :straight t
      :after flycheck
      :hook (flycheck-mode-hook . flycheck-posframe-mode)
      :config
      (flycheck-posframe-configure-pretty-defaults)
      )
    )
  ;; checker for textlint
  (flycheck-define-checker textlint
    "A linter for text."
    :command ("textlint" "--format" "unix" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message (one-or-more not-newline)
		       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
	      line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode web-mode)
    )
  )

;;;
;;; Tree-sitter
;;;
(leaf Tree-sitter
  ;; tree-sitter
  (leaf tree-sitter
    :straight t
    )
  (leaf tree-sitter-langs
    :straight t
    )
  )

;;;
;;; Misc Tools
;;;
(leaf Tools
  :config
  ;; smartparens
  (leaf smartparens
    :straight t
    :blackout t
    :require smartparens-config
    :hook
    (prog-mode-hook . turn-on-smartparens-mode)
    :global-minor-mode show-smartparens-global-mode
    )

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
    :global-minor-mode beacon-mode
    )

  ;; google-this
  (leaf google-this
    :straight t
    :bind
    ("M-s g" . google-this-noconfirm)
    )

  ;; which-key
  (leaf which-key
    :straight t
    :blackout which-key-mode
    :config
    (which-key-mode)
    )

  ;; free-keys
  (leaf free-keys
    :straight t
    )

  ;; popwin
  (leaf popwin
    :straight t
    :global-minor-mode popwin-mode
    )

  ;; ripgrep
  (leaf ripgrep
    :straight t
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
    :hook (after-init-hook . yas-global-mode)
    )

  ;; yasnippet-snippets
  (leaf yasnippet-snippets
    :straight t
    :after yasnippet
    )
  
  ;; atomic-chrome
  (leaf atomic-chrome
    :straight t
    :config
    (atomic-chrome-start-server)
    )

  ;; twittering-mode
  (leaf twittering-mode
    :straight t
    :init
    (setq twittering-use-master-password t)
    (setq twittering-allow-insecure-server-cert t)
    (when (eq system-type 'windows-nt)
      (setq twittering-curl-program "~/scoop/apps/curl/current/bin/curl.exe")
      )
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
    (setq easy-hugo-basedir "~/example.org") ;; CHANGEME
    (setq easy-hugo-url "https://www.example.org") ;; CHANGEME
    (setq easy-hugo-bloglist
	  '(((easy-hugo-basedir . "~/secondblog.org") ;; CHANGEME
	     (easy-hugo-url . "https://www.secondblog.org")))) ;; CHANGEME
    )

  ;; go-translate
  (leaf go-translate
    :straight t
    :bind ("C-c t" . gt-do-translate)
    :config
    (setq gt-langs '(en ja))
    (setq gt-default-translator
	  (gt-translator
	   :taker (gt-taker :text 'buffer :pick 'paragraph)
	   :engines (list
		     (gt-deepl-engine :key "your-deepl-key") ;; CHANGEME
;;		     (gt-chatgpt-engine :key "your-chatgpt-key") ;; CHANGEME
		     )
 	   :render (gt-buffer-render)))
    )

  ;; elfeed
  (leaf elfeed
    :straight t
    :bind ("C-x w" . elfeed)
    :config
    (setq elfeed-feeds
	  '("http://nullprogram.com/feed/"
            "https://planet.emacslife.com/atom.xml"
	    "https://feeds.arstechnica.com/arstechnica/index"
	    "https://theconversation.com/articles.atom?language=en"
	    "https://www.technologyreview.jp/feed/")) ;; CHANGEME
    )
  
  ;; smart-jump
  (leaf smart-jump
    :straight t
    :config
    (smart-jump-setup-default-registers)
    )
  
  ;; dumb-jump
  (leaf dumb-jump
    :straight t
    :config
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    )

  ;; recentf
  (leaf recentf
    :straight t
    :global-minor-mode recentf-mode
    )

  ;; savehist
  (leaf savehist
    :straight t
    :global-minor-mode savehist-mode
    )
  
  ;; esup
  (leaf esup
    :straight t
    )
  
  )

;;;
;;; Server
;;;
(leaf server
  :straight t
  :require t
  :config
  (server-start)
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
;;; Local packages
;;;
(leaf Local
  :config
  ;; word-count-mode
  (leaf word-count
    :straight '(word-count :type git :host github
			   :repo "mhatta/word-count-mode")
    :bind ("\M-+" . word-count-mode)
    )

  ;; lookup-el
  (leaf lookup
    :leaf-defer t
    :straight nil
    :commands (lookup lookup-region lookup-pattern)
    :init
    (when (eq system-type 'windows-nt)
      (add-to-list 'load-path "~/.emacs.d/site-lisp/lookup")
      )
    :bind
    ("\C-cw" . lookup-pattern)
    ("\C-cW" . lookup)
    :init
    (setq lookup-enable-splash nil)
    )
  )

;;;
;;; Profiler (report)
;;;
;;(profiler-report)
;;(profiler-stop)

;;; init.el ends here
