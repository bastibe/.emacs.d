;; -----------------------------------------------------------------------------
;; set up OS dependent stuff
;; -----------------------------------------------------------------------------

(when (and (eq system-type 'windows-nt) (string= (user-login-name) "basti"))
  (setq org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+"
        org-journal-dir "C:/Users/basti/Documents/Journal"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-enable-agenda-integration t
        dired-listing-switches "-ahl"
        LaTeX-command "wsl xelatex -shell-escape")
  (cd "C:/Users/basti/"))

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
                  ("melpa" . "https://melpa.org/packages/")
                  ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("marmalade" . 9)
        ("org" . 9)
        ("elpa" . 8)
        ("gnu" . 8)
        ("melpa" . 0)))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(auto-complete auctex color-theme-sanityinc-tomorrow concurrent
    dash dumb-jump elpy ess expand-region flyspell-popup htmlize
    idomenu ido-vertical-mode magit pyvenv
    markdown-mode multiple-cursors org-journal org-ref popup s
    smartparens undo-tree wrap-region yaml-mode yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -----------------------------------------------------------------------------
;; Make Emacs look good
;; -----------------------------------------------------------------------------

;; set a nice looking font
(setq my-font-height (cond ((eq system-type 'darwin) 130)
                           ((eq system-type 'windows-nt) 100)
                           ((eq system-type 'gnu/linux) 100)))


(set-face-attribute 'default nil
                    :height my-font-height)

;; force consistent font height by using the biggest font for spaces:
(global-whitespace-mode t)
(setq whitespace-style '(face tabs spaces trailing empty newline))

;; Use Pragmata for Unicode, too
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "PragmataPro"
                               :height my-font-height)))
;; For testing purposes: →„Σ“←

;; load my favourite theme of the day
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'typo-theme)
(load-theme 'typo t)
(require 'sleep-table)

;; don't show hat pesky toolbar
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

;; make org-mode fontify source code
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

;; highlight matching parenthesis
(show-smartparens-mode)

;; enable column number in info area
(column-number-mode t)

(global-display-line-numbers-mode t)

(setq-default frame-title-format '("Emacs (%b%&)"))

;; set up a pretty mode line (old version)
;; (setq-default mode-line-format
;;               '(((:eval (let* ((buffer-name (concat
;;                                              (propertize (buffer-name) 'face '(:weight bold))
;;                                              ":" (propertize (format-mode-line "%l,%c") 'face '(:weight light))))
;;                                (left (concat (format-mode-line mode-line-front-space)
;;                                              "(" (if (buffer-modified-p) "⋯" "✓") ")"
;;                                              " "
;;                                              (format "%-30s" buffer-name)
;;                                              "    "
;;                                              (if vc-mode (concat "" vc-mode " (" (symbol-name (vc-state (buffer-file-name))) ")") "")
;;                                              "  "
;;                                              (format-mode-line mode-line-misc-info)))
;;                                (right (concat "("
;;                                               (propertize (format-mode-line mode-name) 'face '(:weight bold))
;;                                               (format-mode-line minor-mode-alist)
;;                                               ")"
;;                                               (format-mode-line mode-line-end-spaces)))
;;                                (padding (make-string (max 0 (- (window-width) 4 (length left) (length right))) ? )))
;;                           (format "%s %s %s" left padding right))))))

;; set up a very simple mode-line at the top
(setq-default x-underline-at-descent-line t)  ; give the file name a bit of room to breathe
(setq-default header-line-format
              '("" (:eval
                    (let* ((left (concat (format-mode-line mode-line-front-space)
                                         "  " (propertize (if (buffer-file-name)
                                                              (buffer-name)
                                                            (format-mode-line "%b"))
                                                          'face '(:weight bold))
                                         " " (if (buffer-modified-p) "(⋯)" "(✓)")))
                           (right (concat (if vc-mode (concat "" vc-mode " (" (symbol-name (vc-state (buffer-file-name))) ")") "")
                                          "  " (propertize (format-mode-line "%l:%c") 'face '(:weight light))
                                          (format-mode-line mode-line-end-spaces)))
                           (padding (make-string (max 0 (- (window-width) (length left) (length right))) ? )))
                      (concat left padding right)))))

;; repurpose the mode line as a simple divider between buffer content and minibuffer:
(setq-default mode-line-format " ")
(set-face-attribute 'mode-line nil
                    :background "#fffff8"
                    :foreground "#111111"
                    :height 10
                    :box '(:line-width 8 :color "#fffff8")
                    :strike-through "#111111")
(set-face-attribute 'mode-line-inactive nil
                    :background "#fffff8"
                    :foreground "#111111"
                    :height 10
                    :box '(:line-width 8 :color "#fffff8")
                    :strike-through "#111111")

(set-face-attribute 'header-line nil
                    :background "#fffff8"
                    :foreground "#111111"
                    :height 100
                    :box '(:line-width 8 :color "#fffff8")
                    :underline "#111111"
                    :strike-through nil)
(set-face-attribute 'minibuffer-prompt nil
                    :background "#fffff8"
                    :foreground "#111111"
                    :height 100)

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

(set-display-table-slot standard-display-table 'wrap ?→) ; eol wrap character
(set-display-table-slot standard-display-table 'truncation ?→) ; bol wrap char

;; ----------------------------------------------------------------------------
;; Make Emacs behave nicely
;; ----------------------------------------------------------------------------

(setq org-journal-file-pattern "%Y%m%d.org")

(add-hook 'prog-mode-hook '(lambda ()
                             (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate)
                             (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))


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
(global-set-key (kbd "C-M-\"") "”")
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

(global-set-key (kbd "M-g") 'goto-line)

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
;; don't create .#files
(setq create-lockfiles nil)

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
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c e") 'mc/edit-lines)

;; enable sensible undo
(require 'undo-tree)
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
(require 'expand-region)
(define-prefix-command 'mark-semantically)
(global-set-key (kbd "C-j") 'mark-semantically)
(define-key mark-semantically (kbd "w") 'er/mark-word)
(define-key mark-semantically (kbd "s") 'er/mark-symbol)
(define-key mark-semantically (kbd "f") 'er/mark-method-call)
(define-key mark-semantically (kbd "d") 'er/mark-defun)
(define-key mark-semantically (kbd "c") 'er/mark-comment)
(define-key mark-semantically (kbd "p") 'mark-paragraph)
(define-key mark-semantically (kbd "'") 'er/mark-inside-quotes)
(define-key mark-semantically (kbd "\"") 'er/mark-outside-quotes)
(define-key mark-semantically (kbd "[") 'er/mark-inside-pairs)
(define-key mark-semantically (kbd "]") 'er/mark-outside-pairs)
(define-key mark-semantically (kbd "l") 'bb/mark-line)

(global-set-key (kbd "M-<return>") 'indent-new-comment-line)

;(global-set-key (kbd "M-.") 'dumb-jump-go)
;(global-set-key (kbd "M-,") 'dumb-jump-back)

;; quick access to org-agenda and org-todo
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(global-set-key (kbd "C-c s") 'org-journal-new-scheduled-entry)
(global-set-key (kbd "C-c S") 'org-journal-schedule-view)

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
(setq paradox-github-token "7df093f60968600ae8bfd0d41959653dad25d19e")
(global-set-key (kbd "C-c p") 'package-list-packages)

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

;; ----------------------------------------------------------------------------
;; Work around issues
;; ----------------------------------------------------------------------------

;; Add high-resolution bitmaps for flyspell-mode et al on a high-DPI screen

(fringe-mode '(16 . 0))

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'exclamation-mark
    (vector #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000001111000000
            #b0000000000000000
            #b0000000000000000
            #b0000001111000000
            #b0000001111000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000)))


(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'flymake-double-exclamation-mark
    (vector #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0011110000111100
            #b0000000000000000
            #b0000000000000000
            #b0011110000111100
            #b0011110000111100
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000)))


;; -----------------------------------------------------------------------------
;; Set a sane indentation style
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)

;; always indent automatically
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x w") 'ido-switch-buffer-other-window)

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
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . (lambda ()
                                                (require 'pdf-tools)
                                                (pdf-tools-install)
                                                (pdf-view-mode))))


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
    ;; (make-local-variable 'elpy-rpc-python-command)
    ;; (make-local-variable 'python-shell-interpreter)
    ;; (make-local-variable 'python-shell-interpreter-args)
    (cond ((file-directory-p ".venv/bin/")
           (setq elpy-rpc-python-command (expand-file-name ".venv/bin/python")
                 python-shell-interpreter (expand-file-name ".venv/bin/python")
                 python-shell-interpreter-args "-i")
           (pyvenv-activate (expand-file-name ".venv")))
          ((and (> (length git-path) 0)
                (file-directory-p git-path)
                (file-directory-p (concat git-path "/.venv/bin/" )))
           (setq elpy-rpc-python-command (concat git-path "/.venv/bin/python")
                 python-shell-interpreter (concat git-path "/.venv/bin/python")
                 python-shell-interpreter-args "-i")
           (pyvenv-activate (concat git-path "/.venv"))))))
(add-hook 'python-mode-hook 'select-python)

;; start up markdown-mode with visual-line-mode
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t))
          t)

(setq org-startup-indented t)
(setq org-export-allow-bind-keywords t)
(setq org-ref-ref-html "[<a class='org-ref-reference' href=\"#%s\">%s</a>]")

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
            (require 'org-ref)
            ;; set up org-babel so it uses the correct python version
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((python . t)))
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
            (setq org-latex-listings 'minted
                  org-latex-pdf-process
                  '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "bibtex %b"
                    "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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
(setq org-static-blog-publish-url "https://bastibe.de/")
(setq org-static-blog-publish-directory "C:/Users/basti/projects/blog/")
(setq org-static-blog-posts-directory "C:/Users/basti/projects/blog/posts/")
(setq org-static-blog-drafts-directory "C:/Users/basti/projects/blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(setq org-static-blog-page-header
"<meta name=\"author\" content=\"Bastian Bechtold\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
<link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
<link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
<script src=\"static/katex.min.js\"></script>
<script src=\"static/auto-render.min.js\"></script>
<script src=\"static/lightbox.js\"></script>
<link rel=\"stylesheet\" href=\"static/katex.min.css\">
<script>document.addEventListener(\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"https://bastibe.de\">Basti's Scratchpad on the Internet</a>
  <div class=\"sitelinks\">
    <a href=\"https://github.com/bastibe\">Github</a> | <a href=\"https://bastibe.de/projects.html\">Projects</a>
  </div>
</div>")

(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"https://bastibe.de/archive.html\">Other posts</a>
</div>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

(setq org-static-blog-post-comments
"<div id=\"hyvor-talk-view\"></div>
<script type=\"text/javascript\">
    var HYVOR_TALK_WEBSITE = 3390;
    var HYVOR_TALK_CONFIG = {
        url: false,
        id: location.pathname
    };
</script>
<script async type=\"text/javascript\" src=\"//talk.hyvor.com/web-api/embed\"></script>")

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
 '(TeX-PDF-mode t t)
 '(TeX-engine 'xetex)
 '(custom-safe-themes
   '("1da1b169142666783bcb535e2275f5ebe70ece84b725a75e474ddfd4691709c1" "21fb497b14820147b2b214e640b3c5ee19fcadc15bc288e3c16c9c9575d95d66" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))
 '(delete-selection-mode nil)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-yasnippet elpy-module-sane-defaults))
 '(magit-diff-refine-hunk t)
 '(magit-push-always-verify nil)
 '(ns-alternate-modifier 'meta)
 '(ns-command-modifier 'hyper)
 '(org-agenda-files '("c:/Users/basti/Documents/Journal/2021-09-22.org"))
 '(org-export-latex-classes
   '(("article" "\\documentclass[11pt,a4paper]{article}"
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
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning)))
 '(org-export-use-babel nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 0.9 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-default-packages-alist
   '(("" "microtype" nil)
     ("" "polyglossia" nil)
     "\\setdefaultlanguage{english}" "\\setotherlanguage{german}"
     ("" "fontspec" nil)
     "\\setmainfont{Latin Modern Roman}"
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
     ("" "unicode-math" t)
     ("hidelinks" "hyperref" nil)
     "\\tolerance=1000"))
 '(org-latex-default-table-environment "longtable")
 '(org-latex-listings nil)
 '(org-latex-tables-centered nil)
 '(org-preview-latex-default-process 'imagemagick)
 '(package-selected-packages
   '(org-static-blog pyvenv flycheck annotate wc-mode lua-mode virtualenvwrapper traad evil yaml-mode wrap-region undo-tree smartparens org-journal multiple-cursors markdown-mode magit iy-go-to-char idomenu ido-vertical-mode ido-ubiquitous htmlize flyspell-popup fish-mode expand-region ess dumb-jump concurrent color-theme-sanityinc-tomorrow auto-complete all-the-icons-dired))
 '(python-check-command "pyflakes3")
 '(safe-local-variable-values
   '((python-shell-interpreter . "/Users/bb/miniconda3/envs/stretch-correlation/bin/ipython")
     (org-startup-folded "content")
     (org-set-startup-cisibility 'content)
     (backup-inhibited . t)))
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
