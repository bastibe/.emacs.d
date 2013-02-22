(add-to-list 'load-path "~/.emacs.d/")

;; -----------------------------------------------------------------------------
;; set up OS dependent stuff
;; -----------------------------------------------------------------------------
(setq my-font-height (cond ((eq system-type 'darwin) 120)
                           ((eq system-type 'windows-nt) 100)
                           ((eq system-type 'gnu/linux) 110)))

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/local/bin/") ; MacPorts bin path
  (add-to-list 'exec-path "/usr/texbin/")    ; tex bin path
  (add-to-list 'exec-path "/Applications/MATLAB_R2012a_Student.app/bin/") ; Matlab
  (setenv "PATH" (concat "/opt/local/bin:/usr/texbin:" (getenv "PATH")))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8"))
(when (eq system-type 'windows-nt)
  (setq magit-git-executable "C:/Users/449BBechtold/AppData/Local/Programs/Git/bin/git.exe")
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
  ;; so emacs finds the vital binaries like diff.exe
  (add-to-list 'exec-path "C:/MinGW/msys/1.0/bin"))

;; -----------------------------------------------------------------------------
;; auto-install packages
;; -----------------------------------------------------------------------------

(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
				  ("marmalade" . "http://marmalade-repo.org/packages/")
				  ("elpa" . "http://tromey.com/elpa/")
				  ("gnu" . "http://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(auto-complete expand-region fill-column-indicator htmlize ido-ubiquitous
                  iy-go-to-char magit markdown-mode mark-multiple undo-tree
                  color-theme-sanityinc-tomorrow wrap-region iedit popup)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
	(package-install p)))

;; -----------------------------------------------------------------------------
;; Make Emacs look good
;; -----------------------------------------------------------------------------

(load-theme 'sanityinc-tomorrow-day t)

;; set a nice looking font
(set-face-attribute 'default nil :height my-font-height :family "DejaVu Sans Mono")

;; don't show hat pesky toolbar
(if window-system
	(tool-bar-mode -1)
    (menu-bar-mode -1))


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
		 (excess-height (cond ((eq system-type 'darwin) 48)
                              ((eq system-type 'windows-nt) 20)))
		 (half-screen-width (- (/ (x-display-pixel-width) 2) excess-width))
		 (screen-height (- (x-display-pixel-height) excess-height)))
	(set-frame-pixel-size (selected-frame) half-screen-width screen-height)
    (set-frame-position (selected-frame) 0 0)))

(defun use-full-screen ()
  (interactive)
  (let* ((excess-width 36)
         (excess-height (cond ((eq system-type 'darwin) 48)
                              ((eq system-type 'windows-nt) 20)))
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

;; recursive minibuffers are essential for ucs-insert in the minibuffer
(setq enable-recursive-minibuffers t)

;; enter German special characters using the default OSX key combination
(global-set-key (kbd "M-°") "“")
(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "M-8") "{")
(global-set-key (kbd "M-9") "}")
(global-set-key (kbd "M-2") "„")
(global-set-key (kbd "M-\"") "“")

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

;; simplify window-movements
(global-unset-key (kbd "C-c f"))
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)

;; NO TABS. EVER.
(setq-default indent-tabs-mode nil)

;; Auto-resize eshell or Python windows to 15 lines of height
(add-hook 'window-configuration-change-hook
		  (lambda ()
			(when (or (string-equal (buffer-name) "*Python*")
					  (string-equal (buffer-name) "*eshell*")
					  (string-equal (buffer-name) "*tex-shell*"))
			  (if (not (eq (window-height) 15))
				  (enlarge-window (- 15 (window-height)))))))

;; turn off the splash screen on startup
(setq inhibit-startup-message t)

;; turn off the splash screen on startup
(setq initial-scratch-message nil)

;; store backup files where they don't bother me
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))


;; disable yank on mouse-2 (middle-click)
(global-set-key [mouse-2] nil)

;; enable ido mode and fuzzy matching
(ido-mode t)
(setq ido-enable-flex-matching t)
(load "ido-goto-symbol")
(global-set-key "\C-ci" 'ido-goto-symbol)

;; join lines using keyboard shortcut
(global-set-key (kbd "M-j") 'join-line)

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda () (when (not (eq major-mode 'markdown-mode))
					 (delete-trailing-whitespace))))

;; enable backwards delete
(global-set-key [kp-delete] 'delete-char)

;; enable fast search
(global-set-key (kbd "M-s") 'iy-go-to-char)
(global-set-key (kbd "M-r") 'iy-go-to-char-backward)

;; enable mark-multiple
(require 'mark-more-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; enable sensible undo
(require 'undo-tree)
(global-undo-tree-mode)

;; don't sound that bloody chime
(setq ring-bell-function #'ignore)

;; enable global autocomplete mode
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(global-auto-complete-mode t)

;; delete region when I start to type
(pending-delete-mode t)

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

(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of 'iedit-start'.
        (narrow-to-defun)
        (if iedit-mode
            (iedit-done)
          ;; 'current-word' can of course be replaced by other function
          (iedit-start (current-word)))))))

(global-set-key (kbd "C-:") 'iedit-dwim)

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

(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

;; -----------------------------------------------------------------------------
;; Set a sane indentation style
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)

;; always indent automatically
(electric-indent-mode t)

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

;; enable a few modules in Haskell mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(diary-file "~/Dropbox/Elements/diary")
 '(ecb-options-version "2.40")
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(sentence-end-double-space nil))

;; enable code folding for python
(add-hook 'python-mode-hook
		  (lambda ()
			(hs-minor-mode t))
		  t)
(global-set-key (kbd "C-c @ @") 'hs-toggle-hiding)
(setq python-remove-cwd-from-path nil)

;; set the default scheme implementation
(setq scheme-program-name "csi")

;; open *.pyx files as python
;(require 'cython-mode)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . python-mode))

;; open *.peproj files as python
(add-to-list 'auto-mode-alist '("\\.peproj\\'" . python-mode))

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

(add-hook 'LaTeX-mode-hook
		  (lambda ()
			(define-key LaTeX-mode-map (kbd "C-c C-v") 'open-show-pdf)
			(visual-line-mode t)
			(turn-on-reftex))
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
;; ECB customization
;; -----------------------------------------------------------------------------

;; for some reason, ECB demands this do be set in order to start up correctly
(setq stack-trace-on-error t)
(semantic-mode t)
;; fix typo in ecb.el (for ecb 2.40)
(defun ecb-enable-own-temp-buffer-show-futition (arg)
  (ecb-enable-own-temp-buffer-show-function arg))

(setq
 ecb-activate-before-layout-draw-hook 'use-full-screen
 ecb-deactivate-hook 'use-left-half-screen
 ecb-layout-name "leftright-basic"
 ecb-source-file-regexps '((".*"
							("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\|exe\\|sublime-workspace\\|sublime-project\\|org_archive\\)$\\)\\)")
							("^\\.\\(emacs\\|gnus\\)$")))
 ecb-tip-of-the-day nil
 ecb-windows-width 40)
(global-set-key (kbd "C-x c") 'ecb-goto-window-compilation)


;; -----------------------------------------------------------------------------
;; Extend browse-url to be able to search for stuff on the web
;; -----------------------------------------------------------------------------

(defun search (url)
  "Opens a browser and searches DuckDuckGo for the given string"
  (interactive "sSearch for: ")
  (browse-url (concat "http://www.duckduckgo.com/?q="
					  (url-hexify-string url))))
(global-set-key (kbd "C-c C-s") 'search)

(defun chicken-doc (url)
  "Opens a browser and searches the Scheme documentation for the given string"
  (interactive "sDocumentation for: ")
  (browse-url (concat "http://api.call-cc.org/cdoc?q="
					  (url-hexify-string url))))
(add-hook 'scheme-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c s") 'chicken-doc)))


;; -----------------------------------------------------------------------------
;; emacs's own customizations
;; -----------------------------------------------------------------------------


(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(setq org-fontify-done-headline t)

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
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))) t)
 '(org-done ((t (:strike-through t))))
 '(org-headline-done ((t (:strike-through t)))))
(put 'set-goal-column 'disabled nil)
