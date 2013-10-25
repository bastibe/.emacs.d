(add-to-list 'load-path "~/.emacs.d/")

;; -----------------------------------------------------------------------------
;; set up OS dependent stuff
;; -----------------------------------------------------------------------------
(setq my-font-height (cond ((eq system-type 'darwin) 120)
                           ((eq system-type 'windows-nt) 100)
                           ((eq system-type 'gnu/linux) 110)))

(when (and (eq system-type 'darwin) (string= (user-login-name) "bb"))
  (add-to-list 'exec-path "/usr/local/bin/") ; homebrew bin path
  (add-to-list 'exec-path "/usr/texbin/")    ; tex bin path
  (setenv "PATH" (concat "/usr/local/bin:/usr/texbin:" (getenv "PATH")))
  (setq eshell-path-env
        (concat "/opt/local/Library/Frameworks/Python.framework/Versions/3.2/bin:"
                "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin"))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setq org-agenda-files (quote ("~/Documents/journal/"
                                 "~/Dropbox/Elements/arbeit.org"
                                 "~/Dropbox/Elements/life.org"
                                 "~/Dropbox/Elements/uni.org")))
  (setq python-shell-interpreter "/Users/bb/.virtualenvs/numerics/bin/ipython3"
        jedi:server-args
        (quote ("--sys-path" "/Users/bb/.virtualenvs/numerics/lib/python3.3/site-packages/"))
        jedi:server-command
        (quote ("/Users/bb/.virtualenvs/numerics/bin/python"
                "/Users/bb/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))))

(when (and (eq system-type 'windows-nt) (string= (system-name) "L4490002"))
  (setq magit-git-executable "C:/Users/449BBechtold/AppData/Local/Programs/Git/bin/git.exe")
  (setq org-agenda-files (quote ("d:/Time Tracking/Sennheiser.org"
                                 "D:/Time Tracking/Journal/")))
  (setq org-journal-dir "d:/Time Tracking/Journal/")
  ;; so git opens emacs if invoked from emacs
  (setenv "EDITOR" "C:/PROGRA~2/emacs/bin/emacs.exe")
  ;; set the home directory to the user directory instead of %appdata%
  (setenv "HOME" "C:/Users/449BBechtold/")
  ;; so eshell uses git.exe instead of git.cmd
  (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH")))
  ;; so shell finds all the msys binaries
  (setenv "PATH" (concat "C:\\MinGW\\msys\\1.0\\bin;" (getenv "PATH")))
  ;; so shell uses the MinGW bash shell
  (setq explicit-shell-file-name "C:/MinGW/msys/1.0/bin/bash")
  (setq ispell-dictionary "german")
  (setq ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
  ;; so emacs finds the vital binaries like diff.exe
  (add-to-list 'exec-path "C:/MinGW/msys/1.0/bin")
  ;; set correct python interpreter
  (setq python-shell-interpreter "C:/WinPython/python-3.3.2.amd64/Scripts/ipython3.exe"
        jedi:server-args
        (quote ("--sys-path" "C:/WinPython/python-3.3.2.amd64/Lib/site-packages"))
        jedi:server-command
        (quote ("C:/WinPython/python-3.3.2.amd64/python.exe"
                "C:/Users/449BBechtold/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py")))
  (fset 'EasyCODEDelete
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 69 97 115 121 67 79 68 69 13 18 47 42 13 67108896 19 42 47 13 23] 0 "%d")) arg))))

(server-start)

;; -----------------------------------------------------------------------------
;; auto-install packages
;; -----------------------------------------------------------------------------

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
				  ("elpa" . "http://tromey.com/elpa/")
				  ("gnu" . "http://elpa.gnu.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(auto-complete auctex color-theme-sanityinc-tomorrow company dash
    expand-region htmlize ido-ubiquitous iy-go-to-char jedi magit
    main-line markdown-mode multiple-cursors org-plus-contrib org-journal
    popup pymacs undo-tree wrap-region)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
	(package-install p)))

;; -----------------------------------------------------------------------------
;; Make Emacs look good
;; -----------------------------------------------------------------------------

;; load my favourite theme of the day
(load-theme 'sanityinc-tomorrow-day t)

;; set a nice looking font
(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :width 'normal
                    :height my-font-height
                    :weight 'normal)

;; Use Pragmata for Unicode, too
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "PragmataPro"
                               :width 'normal
                               :size (/ my-font-height 10)
                               :weight 'normal)))
;; For testing purposes: →„Σ“←

;; don't show hat pesky toolbar
(if window-system
	(tool-bar-mode -1)
    (menu-bar-mode -1))

;; use mainline
(require 'cl) ;; main-line uses cl
(require 'main-line)

;; make org-mode fontify source code
(setq org-src-fontify-natively t)
(setq org-clock-mode-line-total 'current)

;; set window size to an arbitrary number of pixels
(defun set-frame-pixel-size (frame width height)
  "Sets size of FRAME to WIDTH by HEIGHT, measured in pixels."
  (let ((pixels-per-char-width (/ (frame-pixel-width) (frame-width)))
		(pixels-per-char-height (/ (frame-pixel-height) (frame-height))))
	(set-frame-size frame
					(floor (/ width pixels-per-char-width))
					(floor (/ height pixels-per-char-height)))))

;; set window size to the left half of the screen
(defun use-left-half-screen ()
  (interactive)
  (let* ((excess-width 32)
		 (excess-height 56)
         (half-screen-width (- (/ (x-display-pixel-width) 2) excess-width))
         (screen-height (- (x-display-pixel-height) excess-height)))
	(set-frame-pixel-size (selected-frame) half-screen-width screen-height)
    (set-frame-position (selected-frame) 0 0)))

(defun use-full-screen ()
  (interactive)
  (let* ((excess-width 56)
         (excess-height 48)
         (screen-width (- (x-display-pixel-width) excess-width))
         (screen-height (- (x-display-pixel-height) excess-height)))
  (set-frame-pixel-size (selected-frame) screen-width screen-height)
  (set-frame-position (selected-frame) 0 0)))

(if window-system
    (use-left-half-screen))

;; highlight matching parenthesis
(show-paren-mode t)

;; enable column number in info area
(column-number-mode t)

;; in magit and diary mode, use word wrap
(add-hook 'magit-mode-hook (lambda () (visual-line-mode t)) t)
(add-hook 'diary-mode-hook (lambda () (visual-line-mode t)) t)

;; -----------------------------------------------------------------------------
;; Make Emacs behave nicely
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-x c") 'ns-do-hide-emacs)
(global-set-key (kbd "s-w") 'ns-do-hide-emacs)

;; enable "dangerous" features
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; don't ever minimize Emacs on C-z again. Ever.
(define-key (current-global-map) [remap suspend-frame] 'yank)

;; recursive minibuffers are essential for ucs-insert in the minibuffer
(setq enable-recursive-minibuffers t)

;; enter German special characters using the default OSX key combination
(global-set-key (kbd "M-°") "“")
(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "M-8") "{")
(global-set-key (kbd "M-9") "}")
(global-set-key (kbd "M-2") "„")
(global-set-key (kbd "C-M-\"") "“")
(global-set-key (kbd "M-|") "”")
;; Make backward-paragraph and forward-paragraph work the same on EN and DE key maps
(global-set-key (kbd "M-Ü") 'backward-paragraph)
(global-set-key (kbd "M-*") 'forward-paragraph)
(global-set-key (kbd "C-c [") (defun insert-ue () (interactive) (insert-char 252))) ; ü
(global-set-key (kbd "C-c ;") (defun insert-oe () (interactive) (insert-char 246))) ; ö
(global-set-key (kbd "C-c '") (defun insert-ae () (interactive) (insert-char 228))) ; ä
(global-set-key (kbd "C-c {") (defun insert-UE () (interactive) (insert-char 220))) ; Ü
(global-set-key (kbd "C-c :") (defun insert-OE () (interactive) (insert-char 214))) ; Ö
(global-set-key (kbd "C-c \"") (defun insert-AE () (interactive) (insert-char 196))) ; Ä
(setq default-input-method 'german-postfix)

;; Make command history persistent
(savehist-mode t)

;; Make ispell and flyspell work with aspell instead of ispell
(setq ispell-prefer-aspell t)

;; make clock persistent
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-time-stamp-rounding-minutes '(5 5))

;; detect external file changes automatically
(global-auto-revert-mode t)

;; NO TABS. EVER.
(setq-default indent-tabs-mode nil)

;; Auto-resize eshell or Python windows to 15 lines of height
(add-hook 'post-command-hook
		  (lambda ()
            ;; prevent infinite loop if there is only one window
            (unless (= 1 (length (window-list nil -1)))
              (when (or (string-equal (buffer-name) "*Python*")
                        (string-equal (buffer-name) "*eshell*")
                        (string-equal (buffer-name) "*tex-shell*"))
                (if (not (eq (window-height) 15))
                    (enlarge-window (- 15 (window-height))))))))

;; turn off the splash screen on startup
(setq inhibit-startup-message t)

;; turn off the scratch buffer header
(setq initial-scratch-message nil)

;; store backup files where they don't bother me
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; disable yank on mouse-2 (middle-click)
(global-set-key [mouse-2] nil)

;; enable ido mode and fuzzy matching
(ido-mode t)
(setq ido-enable-flex-matching t)
(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-auto-merge-delay-time 1)

;; join lines using keyboard shortcut
(global-set-key (kbd "M-j") 'join-line)

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda () (when (not (eq major-mode 'markdown-mode))
					 (delete-trailing-whitespace))))

;; enable backwards delete
(global-set-key [kp-delete] 'delete-char)

;; enable fast character search
(global-set-key (kbd "M-s") 'iy-go-to-char)
(global-set-key (kbd "M-r") 'iy-go-to-char-backward)

;; enable mark-multiple
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "M-C-*") 'mc/mark-all-like-this-in-defun)

;; enable sensible undo
(global-undo-tree-mode)
;; make undo work the same way on the EN and DE keymap
(define-key undo-tree-map (kbd "C--") 'undo-tree-undo)
(define-key undo-tree-map (kbd "C-_") 'undo-tree-redo)

;; don't sound that bloody chime
(setq ring-bell-function #'ignore)

;; delete region when I start to type
;; (pending-delete-mode t)

;; Easily wrap statements in delimiters
(wrap-region-global-mode t)

;; always ask for `y` or `n` instead of `yes` or `no`
(defalias 'yes-or-no-p 'y-or-n-p)

;; use C-o and M-o to create a new line above or below point, somewhat like in vi
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "M-o") 'vi-open-line-above)

(defun bb/mark-line ()
  "Mark the entire line where point is"
  (interactive)
  (end-of-line)
  (set-mark (point))
  (beginning-of-line))

;; mark stuff semantically
(global-set-key (kbd "C-j") 'er/expand-region)
(global-set-key (kbd "C-M-j") 'bb/mark-line)
(global-set-key (kbd "M-<return>") 'indent-new-comment-line)

;; quick access to org-agenda and org-todo
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c t") 'org-todo-list)

;; quick access to magit-status
(global-set-key (kbd "C-c m") 'magit-status)

;; quick access to list-packages
(global-set-key (kbd "C-c p") 'list-packages)

;; quick access to the calendar
(global-set-key (kbd "C-c c") 'calendar)

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))
(global-set-key (kbd "C-c v") 'halve-other-window-height)

(setq view-diary-entries-initially t
      mark-diary-entries-in-calendar t
      number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; set up the calendar to look German
(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

(defun bb/beginning-of-indentation ()
  "Return the position of the first non-whitespace character in the line"
  (save-excursion
    (back-to-indentation)
    (point)))

(defun bb/beginning-of-line ()
  "Return the position of the first character in the line"
  (save-excursion
    (move-beginning-of-line nil)
    (point)))

(defun bb/end-of-line ()
  "Return the position of the last character in the line"
  (save-excursion
    (move-end-of-line nil)
    (point)))

(defun bb/move-backward ()
  "Move point backwards to the first non-whitespace character in the line
   or the first character in the line or the beginning of the buffer,
   whichever comes first"
  (interactive)
  (cond
   ((> (point) (bb/beginning-of-indentation))
    (back-to-indentation))
   ((> (point) (bb/beginning-of-line))
    (move-beginning-of-line nil))
   (t (beginning-of-buffer))))

(defun bb/move-forward ()
  "Move point forward to the end of the line or the end of the buffer,
   whichever comes first"
  (interactive)
  (if (< (point) (bb/end-of-line))
      (move-end-of-line nil)
    (end-of-buffer)))

(global-set-key (kbd "C-a") 'bb/move-backward)
(global-set-key (kbd "C-e") 'bb/move-forward)

;; set up ansi-term to work with unicode correctly
(add-hook 'term-exec-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
          t)

;; -----------------------------------------------------------------------------
;; Set a sane indentation style
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)

;; always indent automatically
(global-set-key (kbd "RET") 'newline-and-indent)

;; C/C++
(setq c-default-style "linux"
	  c-basic-offset 4)

;; Ruby
(setq-default ruby-indent-level 4)

;; Lua
(setq lua-indent-level 4)

;; -----------------------------------------------------------------------------
;; Set up some language specific stuff
;; -----------------------------------------------------------------------------

;; set up python to use ipython and working auto-completion
(setq
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 jedi:setup-keys t
 jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'jedi:setup)
(require 'auto-complete)
(add-hook 'python-mode-hook 'auto-complete-mode)

;; open *.md files as markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; open *.m files as octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; open *.pdf files as images
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode))

;; start up markdown-mode with visual-line-mode
(add-hook 'markdown-mode-hook
		  (lambda ()
			(visual-line-mode t))
		  t)

;; start up org mode with visual-line-mode and org-indent-mode
(add-hook 'org-mode-hook
		  (lambda ()
			(visual-line-mode t))
		  t)
(setq org-startup-indented t)

;; start up latex mode with visual-line-mode
(add-hook 'latex-mode-hook
		  (lambda ()
			(visual-line-mode t)
			(turn-on-reftex))
		  t)

;; open/show pdf file within Emacs using doc-view-mode
(defun open-show-pdf ()
  (interactive)
  (let ((tex-buffer-name (buffer-name))
		(pdf-buffer-name (concat (TeX-master-file) ".pdf")))
	(if (get-buffer pdf-buffer-name)
		(switch-to-buffer-other-window pdf-buffer-name)
	  (find-file-other-window pdf-buffer-name))
	(if (not (eq major-mode 'doc-view-mode))
		(doc-view-mode))
	(doc-view-revert-buffer t t)
	(switch-to-buffer-other-window tex-buffer-name)))

(add-hook 'org-mode-hook
          (lambda ()
            ;; make DONE use strike through
            (set-face-attribute 'org-done nil :strike-through t)
            (set-face-attribute 'org-headline-done nil :strike-through t)
            ;; overload C-j in org-mode, too
            (define-key org-mode-map (kbd "C-j") 'er/expand-region))
            t)

(require 'ox-latex)
(setq org-latex-listings 'minted
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(add-to-list 'org-latex-packages-alist '("" "minted"))

(add-hook 'LaTeX-mode-hook
		  (lambda ()
			(define-key LaTeX-mode-map (kbd "C-c C-v") 'open-show-pdf)
            (define-key LaTeX-mode-map (kbd "C-j") 'er/expand-region)
			(visual-line-mode t)
			(turn-on-reftex)
            (local-unset-key (kbd "\""))
            (local-set-key (kbd "M-\"") "“"))
		  t)

(setq TeX-PDF-mode t)
(setq TeX-view-program-list '(("Preview" "open -a Preview.app %o")))
(setq TeX-view-program-selection '((output-pdf "Preview")))

;; Make doc-view-mode scroll sanely with the mouse wheel
(add-hook 'doc-view-mode-hook
		  (lambda ()
			(define-key doc-view-mode-map [wheel-down]
			  'doc-view-next-line-or-next-page)
			(define-key doc-view-mode-map [double-wheel-down]
			  (lambda () (interactive) (doc-view-next-line-or-next-page 2)))
			(define-key doc-view-mode-map [triple-wheel-down]
			  (lambda () (interactive) (doc-view-next-line-or-next-page 3)))
			(define-key doc-view-mode-map [wheel-up]
			  'doc-view-previous-line-or-previous-page)
			(define-key doc-view-mode-map [double-wheel-up]
			  (lambda () (interactive) (doc-view-previous-line-or-previous-page 2)))
			(define-key doc-view-mode-map [triple-wheel-up]
			  (lambda () (interactive) (doc-view-previous-line-or-previous-page 3))))
			t)


;; -----------------------------------------------------------------------------
;; Make Emacs scroll somewhat nicely
;; -----------------------------------------------------------------------------

;; sadly, Emacs does not handle all scroll events on OSX. Hence, inertia
;;   scrolling does not work properly. This is the closest approximation I could
;;   come up with.
(setq mouse-wheel-progressive-speed nil)
(setq redisplay-dont-pause t)
(defun up-single () (interactive) (scroll-up 1))
(defun down-single () (interactive) (scroll-down 1))
(defun up-double () (interactive) (scroll-up 2))
(defun down-double () (interactive) (scroll-down 2))
(defun up-triple () (interactive) (scroll-up 5))
(defun down-triple () (interactive) (scroll-down 5))

(global-set-key [wheel-down] 'up-single)
(global-set-key [wheel-up] 'down-single)
(global-set-key [double-wheel-down] 'up-double)
(global-set-key [double-wheel-up] 'down-double)
(global-set-key [triple-wheel-down] 'up-triple)
(global-set-key [triple-wheel-up] 'down-triple)

;; -----------------------------------------------------------------------------
;; Extend browse-url to be able to search for stuff on the web
;; -----------------------------------------------------------------------------

(defun search (url)
  "Opens a browser and searches DuckDuckGo for the given string"
  (interactive "sSearch for: ")
  (browse-url (concat "http://www.duckduckgo.com/?q="
					  (url-hexify-string url))))
(global-set-key (kbd "C-c C-s") 'search)


;; -----------------------------------------------------------------------------
;; emacs's own customizations
;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|[0-9]+")
 '(safe-local-variable-values (quote ((org-startup-folded "content") (org-set-startup-cisibility (quote content)) (backup-inhibited . t))))
 '(sentence-end-double-space nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))) t)
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))) t)
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))) t)
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :underline t :height 1.2))) t)
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :underline t :height 1.1))) t)
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :underline t))) t)
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))) t))
