;; -----------------------------------------------------------------------------
;; set up OS dependent stuff
;; -----------------------------------------------------------------------------

(when (and (eq system-type 'gnu/linux) (string= (user-login-name) "bb"))
  (setq org-agenda-files (quote ("~/Documents/journal/"))
        org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+"))

(when (and (eq system-type 'darwin) (string= (user-login-name) "bb"))
  (add-to-list 'exec-path "/usr/local/bin/") ; homebrew bin path
  (add-to-list 'exec-path "/Library/TeX/texbin/")    ; tex bin path
  (add-to-list 'exec-path "/Users/bb/miniconda3/bin/") ; python path
  (setenv "PATH" (concat "/usr/local/bin:/Library/TeX/texbin/:" (getenv "PATH")))
  (setq eshell-path-env
        (concat "/opt/local/Library/Frameworks/Python.framework/Versions/3.2/bin:"
                "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin"))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setq org-agenda-files (quote ("~/Documents/journal/"))
        org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+")
  (setq org-babel-python-command "/Users/bb/miniconda3/envs/emacs/bin/python")
  (setq python-shell-interpreter "/Users/bb/miniconda3/envs/emacs/bin/ipython"
        conda-env-path "/Users/bb/miniconda3/envs/"
        jedi:server-args
        (quote ("--sys-path" "/Users/bb/miniconda3/envs/emacs/lib/python3.4/site-packages/"))
        jedi:server-command
        `("/Users/bb/miniconda3/envs/emacs/bin/python"
          ;; grab whichever version of jediepcserver is installed
          ,(concat "/Users/bb/.emacs.d/elpa/"
                   (car (directory-files "~/.emacs.d/elpa" nil "jedi"))
                   "/jediepcserver.py")))
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq default-directory "~")
  (global-set-key (kbd "H-f") isearch-forward))
(when (and (eq system-type 'gnu/linux) (string= (user-login-name) "bb"))
  (setq org-agenda-files (quote ("~/Documents/journal/"))
        org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+")
  (setq default-directory "~"))

(require 'server)
(unless (server-running-p)
  (server-start))

;; -----------------------------------------------------------------------------
;; auto-install packages
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-archive-exclude-alist '(("melpa")))
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
				  ("elpa" . "http://tromey.com/elpa/")
				  ("gnu" . "http://elpa.gnu.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(auto-complete auctex color-theme-sanityinc-tomorrow
    concurrent dash elpy ess expand-region flyspell-popup htmlize
    idomenu ido-ubiquitous ido-vertical-mode iy-go-to-char
    magit markdown-mode multiple-cursors org-journal popup s
    smartparens undo-tree wrap-region yaml-mode yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
	(package-install p)))

;; -----------------------------------------------------------------------------
;; Make Emacs look good
;; -----------------------------------------------------------------------------

;; load my favourite theme of the day
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'my-eink-theme)
(load-theme 'my-eink t)

;; set a nice looking font
(setq my-font-height (cond ((eq system-type 'darwin) 130)
                           ((eq system-type 'windows-nt) 100)
                           ((eq system-type 'gnu/linux) 100)))


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
                               :height my-font-height
                               :weight 'normal)))
;; For testing purposes: →„Σ“←

;; don't show hat pesky toolbar
(if window-system
	(tool-bar-mode -1)
    (menu-bar-mode -1))

;; make org-mode fontify source code
(setq org-src-fontify-natively t)

;; highlight matching parenthesis
(show-smartparens-mode)

;; enable column number in info area
(column-number-mode t)

;; in magit and diary mode, use word wrap
(add-hook 'magit-mode-hook (lambda () (visual-line-mode t)) t)
(add-hook 'magit-log-edit-mode-hook (lambda () (visual-line-mode t)) t)
(add-hook 'diary-mode-hook (lambda () (visual-line-mode t)) t)

;; make emacs visual-line wrap at some column
(defvar visual-wrap-column 0)

(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil 1)
    (let* ((current-margins (window-margins))
           (left-margin (or (car current-margins) 1))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil left-margin)
        (set-window-margins nil left-margin
                            (- current-available visual-wrap-column))))))

;; make fringes more beautiful
(if window-system
    (set-fringe-mode 0)) ; disable fringes
;; ensure that the left margin is always 1
(set-display-table-slot standard-display-table 'wrap ?→) ; eol wrap character
(set-display-table-slot standard-display-table 'truncation ?→) ; bol wrap char
;; TODO clicking the margin should position cursor at eol/bol

;; ----------------------------------------------------------------------------
;; Make Emacs behave nicely
;; ----------------------------------------------------------------------------

(setq org-journal-file-pattern "%Y%m%d.org")

(add-hook 'python-mode-hook '(lambda ()
                               (company-mode t)
                               (add-to-list 'company-backends 'company-jedi)))

(setq ns-pop-up-frames nil)
(global-set-key (kbd "H-h") 'ns-do-hide-emacs)
(global-set-key (kbd "H-w") 'delete-window)

;; enable "dangerous" features
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; don't ever minimize Emacs on C-z again. Ever.
(define-key (current-global-map) [remap suspend-frame] 'yank)

;; recursive minibuffers are essential for ucs-insert in the minibuffer
(setq enable-recursive-minibuffers t)

;; enter German special characters using the default OSX key combination
(global-set-key (kbd "M-\"") "“")
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
(setq ispell-really-hunspell t)
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;; Allow quotes in org source blocks
(setq org-emphasis-regexp-components '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n," "." 1))

;; detect external file changes automatically
(global-auto-revert-mode t)

;; NO TABS. EVER.
(setq-default indent-tabs-mode nil)

;; turn off the splash screen on startup
(setq inhibit-startup-message t)

;; turn off the scratch buffer header
(setq initial-scratch-message nil)

;; store backup files where they don't bother me
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
;; don't create #autosave-files#
(setq auto-save-default nil)

;; disable yank on mouse-2 (middle-click)
(global-set-key [mouse-2] nil)

;; enable ido mode and fuzzy matching
(ido-mode t)
(setq ido-enable-flex-matching t)
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
(global-set-key (kbd "C-'") 'mc/edit-lines)

;; enable sensible undo
(global-undo-tree-mode)
;; make undo work the same way on the EN and DE keymap
(define-key undo-tree-map (kbd "C--") 'undo-tree-undo)
(define-key undo-tree-map (kbd "C-_") 'undo-tree-redo)

;; don't sound that bloody chime
(setq ring-bell-function #'ignore)

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
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(defun term-other-window ()
  "Create or switch to a terminal in another window"
  (interactive)
  (switch-to-buffer-other-window "*terminal*")
  (term "fish"))
(eval-after-load 'term
  '(term-set-escape-char ?\C-x))

;; quick access to a terminal
(global-set-key (kbd "C-c t") 'term-other-window)

;; quick access to magit-status
(global-set-key (kbd "C-c m") 'magit-status)

;; quick access to list-packages
(global-set-key (kbd "C-c p") 'list-packages)

;; quick access to the calendar
(global-set-key (kbd "C-c c") 'calendar)

;; quickly jump to imenu locations
(global-set-key (kbd "C-c i") 'idomenu)

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

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(autoload 's-trim "s")

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

;; allow org-mode to use alphabetical lists
(setq org-list-allow-alphabetical t)

;; open *.md files as markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; open *.m files as octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(setq octave-block-offset 4)
;; use single-percent comments
(setq octave-comment-char ?%)
(add-hook 'octave-mode-hook
          (lambda ()
            (setq-local comment-add 0)
            ;; overwrite octave indentation logic
            (defun octave-indent-comment ()
              "A function for `smie-indent-functions' (which see)."
              (save-excursion
                (back-to-indentation)
                (cond
                 ((octave-in-string-or-comment-p) nil)
                 ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}")
                  0)
                 ;; keep documentation comments at bol
                 ((= (current-column) 0) 0)
                 ;; no more special-casing for double-comments
                 )))))

;; open *.pdf files as images
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode))

;; open *.yaml files as yaml
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; open *.jl files as julia
(autoload 'julia-mode "ess-site")
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

;; open *.py files as python
(add-to-list 'auto-mode-alist '("\\.py\\'" . (lambda () (elpy-enable) (python-mode))))

(defun select-python ()
  "Select appropriate venv"
  (interactive)
  (let ((git-path (s-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
    (when (and (> (length git-path) 0)
               (file-directory-p git-path)
               (file-directory-p (concat git-path "/.env/bin/" )))
      (make-local-variable 'python-shell-interpreter)
      (make-local-variable 'python-shell-interpreter-args)
      (setq elpy-rpc-python-command (concat git-path "/.env/bin/python")
            python-shell-interpreter (concat git-path "/.env/bin/ipython")
            python-shell-interpreter-args "-i --simple-prompt"))))
(add-hook 'python-mode-hook 'select-python)

;; start up markdown-mode with visual-line-mode
(add-hook 'markdown-mode-hook
		  (lambda ()
			(visual-line-mode t))
		  t)

(setq org-startup-indented t)
(setq org-export-allow-bind-keywords t)

;; start up latex mode with visual-line-mode
(add-hook 'latex-mode-hook
		  (lambda ()
			(visual-line-mode t)
			(turn-on-reftex)
            (yas-minor-mode t))
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
(setq TeX-view-program-list '(("Preview" "open -a Skim.app %o")))
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

(autoload 'ox-latex "org-mode" "Org Mode." t)
(autoload 'ox-html "org-mode" "Org Mode." t)
(autoload 'ox-rss "org-mode" "Org Mode." t)
(autoload 'ox-publish "org-mode" "Org Mode." t)
(autoload 'yas-reload-all "yasnippet")
(add-hook 'org-load-hook (lambda () (yas-reload-all)))
(add-hook 'org-mode-hook
          (lambda ()            ;; yasnippet
            (yas-minor-mode t)
            ;; convert cite: links to \cite{...}
            (org-add-link-type "cite"
                 (defun follow-cite (name)
                   "Open bibliography and jump to appropriate entry.
                    The document must contain \bibliography{filename}
                    somewhere for this to work"
                   (find-file-other-window
                    (save-excursion
                      (beginning-of-buffer)
                      (save-match-data
                        (re-search-forward "\\\\bibliography{\\([^}]+\\)}")
                        (concat (match-string 1) ".bib"))))
                   (beginning-of-buffer)
                   (search-forward name))
                 (defun export-cite (path desc format)
                   "Export [[cite:cohen93]] as \cite{cohen93} in LaTeX."
                   (if (eq format 'latex)
                       (if (or (not desc) (equal 0 (search "cite:" desc)))
                           (format "\\cite{%s}" path)
                         (format "\\cite[%s]{%s}" desc path)))))
            ;; set up org-babel so it uses the correct python version
            (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook
                         (lambda ()
                           (let ((yas/fallback-behavior 'return-nil)) (yas/expand))))
            (define-key yas/keymap [tab] 'yas/next-field)
            ;; turn on visual-line-mode
            (visual-line-mode t)
            ;; make DONE use strike through
            (set-face-attribute 'org-done nil :strike-through t)
            (set-face-attribute 'org-headline-done nil :strike-through t)
            ;; overload C-j in org-mode, too
            (define-key org-mode-map (kbd "C-j") 'er/expand-region)
            (setq org-latex-listings 'minted
                  org-latex-pdf-process
                  '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "bibtex %b"
                    "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
            (add-to-list 'org-latex-packages-alist '("" "minted"))
            (set-face-attribute 'variable-pitch nil
                                :family "Calibri"
                                :height (+ my-font-height 20))
            (set-face-attribute 'fixed-pitch nil
                                :family "PragmataPro"
                                :height my-font-height)
            (mapc (lambda (face)
                    (set-face-attribute face nil
                                        :family (face-attribute 'fixed-pitch :family)
                                        :height (face-attribute 'fixed-pitch :height)))
                  '(org-block-begin-line
                    org-code org-link org-meta-line
                    ;;org-block-background
                    org-document-info-keyword
                    font-lock-comment-face
                    org-table
                    org-special-keyword
                    org-property-value))
            (set-face-attribute 'org-level-1 nil :height (+ my-font-height 40))
            (set-face-attribute 'org-level-2 nil :height (+ my-font-height 30))
            (set-face-attribute 'org-level-3 nil :weight 'bold)

            (setq org-confirm-babel-evaluate nil)))


;; -----------------------------------------------------------------------------
;; Make Emacs scroll somewhat nicely
;; -----------------------------------------------------------------------------

;; sadly, Emacs does not handle all scroll events on OSX. Hence, inertia
;;   scrolling does not work properly. This is the closest approximation I could
;;   come up with.
(setq mouse-wheel-progressive-speed nil)
(setq redisplay-dont-pause t)

;; Make clicking and scrolling work in the margin
(global-set-key (kbd "<right-margin> <wheel-down>") 'mwheel-scroll)
(global-set-key (kbd "<right-margin> <wheel-up>") 'mwheel-scroll)
(defun bastibe-margin-click (event)
  (interactive "e")
  (mouse-set-point event)
  (unless (eolp)
    (left-char)))
(global-set-key (kbd "<right-margin> <mouse-1>") 'bastibe-margin-click)

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
;; Set up blogging in Emacs
;; -----------------------------------------------------------------------------

(setq org-static-blog-publish-title "Bastibe.de")
(setq org-static-blog-publish-url "http://bastibe.de/")
(setq org-static-blog-publish-directory "~/projects/blog/")
(setq org-static-blog-posts-directory "~/projects/blog/posts/")
(setq org-static-blog-drafts-directory "~/projects/blog/drafts/")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(setq org-static-blog-page-header
"<meta  name=\"author\" content=\"Bastian Bechtold\" />
<link href='http://fonts.googleapis.com/css?family=Roboto&subset=latin' rel='stylesheet' type='text/css'>
<link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono' rel='stylesheet' type='text/css'>
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
<link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
<link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML\"> </script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"http://bastibe.de\">Basti's Scratchpad on the Internet</a>
  <div class=\"sitelinks\">
    <a href=\"http://alpha.app.net/bastibe\">alpha.app.net</a> | <a href=\"http://github.com/bastibe\">Github</a>
  </div>
</div>")

(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"archive.html\">Other posts</a>
</div>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
  var disqus_shortname = 'bastibe';
  (function() {
    var dsq = document.createElement('script');
    dsq.type = 'text/javascript';
    dsq.async = true;
    dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
  })();
</script>
<noscript>Please enable JavaScript to view the
<a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
<a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
<center><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"http://purl.org/dc/terms/\" href=\"http://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"http://creativecommons.org/ns#\" href=\"http://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

(defadvice org-preview-latex-fragment (around non-xelatex-org-preview-latex-fragment)
  "Strip down the LaTeX process to the bare minimum when compiling fragments"
  (let ((org-latex-default-packages-alist
        '((""     "amsmath"   t)
          (""     "amssymb"   t)))
        (org-latex-pdf-process
         '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
       ad-do-it))
(ad-activate 'org-preview-latex-fragment)

;; This slows down org-publish to a crawl, and it is not needed since
;; I use magit anyway.
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; -----------------------------------------------------------------------------
;; emacs's own customizations
;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex -shell-escape")
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(custom-safe-themes
   (quote
    ("21fb497b14820147b2b214e640b3c5ee19fcadc15bc288e3c16c9c9575d95d66" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(delete-selection-mode nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-yasnippet elpy-module-sane-defaults)))
 '(magit-diff-refine-hunk t)
 '(magit-push-always-verify nil)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote hyper))
 '(org-agenda-files
   (quote
    ("~/Documents/journal/20150917" "/Users/bb/Documents/journal/20140508" "/Users/bb/Documents/journal/20121227" "/Users/bb/Documents/journal/20121228" "/Users/bb/Documents/journal/20121229" "/Users/bb/Documents/journal/20121230" "/Users/bb/Documents/journal/20121231" "/Users/bb/Documents/journal/20130101" "/Users/bb/Documents/journal/20130102" "/Users/bb/Documents/journal/20130103" "/Users/bb/Documents/journal/20130104" "/Users/bb/Documents/journal/20130105" "/Users/bb/Documents/journal/20130106" "/Users/bb/Documents/journal/20130604" "/Users/bb/Documents/journal/20130605" "/Users/bb/Documents/journal/20130606" "/Users/bb/Documents/journal/20130607" "/Users/bb/Documents/journal/20130608" "/Users/bb/Documents/journal/20130611" "/Users/bb/Documents/journal/20130612" "/Users/bb/Documents/journal/20130623" "/Users/bb/Documents/journal/20130625" "/Users/bb/Documents/journal/20130627" "/Users/bb/Documents/journal/20130629" "/Users/bb/Documents/journal/20130705" "/Users/bb/Documents/journal/20130707" "/Users/bb/Documents/journal/20130711" "/Users/bb/Documents/journal/20130722" "/Users/bb/Documents/journal/20130724" "/Users/bb/Documents/journal/20130729" "/Users/bb/Documents/journal/20130809" "/Users/bb/Documents/journal/20130811" "/Users/bb/Documents/journal/20130812" "/Users/bb/Documents/journal/20130813" "/Users/bb/Documents/journal/20130814" "/Users/bb/Documents/journal/20130815" "/Users/bb/Documents/journal/20130816" "/Users/bb/Documents/journal/20130817" "/Users/bb/Documents/journal/20130818" "/Users/bb/Documents/journal/20130820" "/Users/bb/Documents/journal/20130821" "/Users/bb/Documents/journal/20130822" "/Users/bb/Documents/journal/20130823" "/Users/bb/Documents/journal/20130825" "/Users/bb/Documents/journal/20130826" "/Users/bb/Documents/journal/20130827" "/Users/bb/Documents/journal/20130828" "/Users/bb/Documents/journal/20130829" "/Users/bb/Documents/journal/20130830" "/Users/bb/Documents/journal/20130831" "/Users/bb/Documents/journal/20130901" "/Users/bb/Documents/journal/20130902" "/Users/bb/Documents/journal/20130903" "/Users/bb/Documents/journal/20130904" "/Users/bb/Documents/journal/20130909" "/Users/bb/Documents/journal/20130910" "/Users/bb/Documents/journal/20130911" "/Users/bb/Documents/journal/20130912" "/Users/bb/Documents/journal/20130914" "/Users/bb/Documents/journal/20130916" "/Users/bb/Documents/journal/20130917" "/Users/bb/Documents/journal/20130918" "/Users/bb/Documents/journal/20130919" "/Users/bb/Documents/journal/20130927" "/Users/bb/Documents/journal/20130929" "/Users/bb/Documents/journal/20131001" "/Users/bb/Documents/journal/20131002" "/Users/bb/Documents/journal/20131008" "/Users/bb/Documents/journal/20131009" "/Users/bb/Documents/journal/20131011" "/Users/bb/Documents/journal/20131015" "/Users/bb/Documents/journal/20131016" "/Users/bb/Documents/journal/20131020" "/Users/bb/Documents/journal/20131021" "/Users/bb/Documents/journal/20131023" "/Users/bb/Documents/journal/20131024" "/Users/bb/Documents/journal/20131025" "/Users/bb/Documents/journal/20131027" "/Users/bb/Documents/journal/20131030" "/Users/bb/Documents/journal/20131031" "/Users/bb/Documents/journal/20131103" "/Users/bb/Documents/journal/20131105" "/Users/bb/Documents/journal/20131106" "/Users/bb/Documents/journal/20131109" "/Users/bb/Documents/journal/20131111" "/Users/bb/Documents/journal/20131112" "/Users/bb/Documents/journal/20131117" "/Users/bb/Documents/journal/20131118" "/Users/bb/Documents/journal/20131120" "/Users/bb/Documents/journal/20131202" "/Users/bb/Documents/journal/20131208" "/Users/bb/Documents/journal/20140107" "/Users/bb/Documents/journal/20140112" "/Users/bb/Documents/journal/20140113" "/Users/bb/Documents/journal/20140119" "/Users/bb/Documents/journal/20140121" "/Users/bb/Documents/journal/20140123" "/Users/bb/Documents/journal/20140126" "/Users/bb/Documents/journal/20140128" "/Users/bb/Documents/journal/20140203" "/Users/bb/Documents/journal/20140210" "/Users/bb/Documents/journal/20140212" "/Users/bb/Documents/journal/20140221" "/Users/bb/Documents/journal/20140304" "/Users/bb/Documents/journal/20140305" "/Users/bb/Documents/journal/20140306" "/Users/bb/Documents/journal/20140307" "/Users/bb/Documents/journal/20140310" "/Users/bb/Documents/journal/20140311" "/Users/bb/Documents/journal/20140312" "/Users/bb/Documents/journal/20140313" "/Users/bb/Documents/journal/20140314" "/Users/bb/Documents/journal/20140318" "/Users/bb/Documents/journal/20140319" "/Users/bb/Documents/journal/20140321" "/Users/bb/Documents/journal/20140324" "/Users/bb/Documents/journal/20140326" "/Users/bb/Documents/journal/20140327" "/Users/bb/Documents/journal/20140328" "/Users/bb/Documents/journal/20140331" "/Users/bb/Documents/journal/20140401" "/Users/bb/Documents/journal/20140403" "/Users/bb/Documents/journal/20140408" "/Users/bb/Documents/journal/20140421" "/Users/bb/Documents/journal/20140424" "/Users/bb/Documents/journal/20140425" "/Users/bb/Documents/journal/20140509" "/Users/bb/Documents/journal/20140512" "/Users/bb/Documents/journal/20140513")))
 '(org-export-babel-evaluate nil)
 '(org-export-latex-classes
   (quote
    (("article" "\\documentclass[11pt,a4paper]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 0.9 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(org-latex-default-packages-alist
   (quote
    (("" "microtype" nil)
     ("" "polyglossia" nil)
     "\\setdefaultlanguage{german}" "\\setotherlanguage{english}"
     ("" "fontspec" nil)
     "\\setmainfont{Calibri}"
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("" "hyperref" nil)
     "\\tolerance=1000")))
 '(org-latex-default-table-environment "longtable")
 '(org-latex-listings (quote minted) t)
 '(org-latex-tables-centered nil)
 '(python-check-command "pyflakes3")
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter . "/Users/bb/miniconda3/envs/stretch-correlation/bin/ipython")
     (org-startup-folded "content")
     (org-set-startup-cisibility
      (quote content))
     (backup-inhibited . t))))
 '(send-mail-function (quote mailclient-send-it))
 '(sentence-end-double-space nil)
 '(writeroom-disable-fringe nil)
 '(writeroom-disable-mode-line nil)
 '(writeroom-fullscreen-effect (quote maximized))
 '(writeroom-global-effects nil)
 '(writeroom-maximize-window nil)
 '(writeroom-width 120))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :underline t :height 1.2))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :underline t :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :underline t))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))))
 '(markdown-header-rule-face ((t (:inherit markdown-header-face :height 1.5)))))
