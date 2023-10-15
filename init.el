; -*- coding: utf-8 -*-

;; -----------------------------------------------------------------------------
;; set up OS dependent stuff
;; -----------------------------------------------------------------------------

(when (and (eq system-type 'gnu/linux) (string= (user-login-name) "bastibe"))
  (setq org-agenda-files (quote ("~/Documents/Journal/"))
        org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "~/Documents/Journal"
        org-agenda-diary-file 'diary-file
        org-autowiki-directory "~/Documents/Autowiki"
        default-directory "~"
	server-name "/home/bastibe/.emacs.d/currentserver")
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (require 'org-autowiki))

(when (and (eq system-type 'darwin) (string= (user-login-name) "bastibe"))
  (setq org-agenda-files (quote ("~/Documents/Journal/"))
        org-agenda-file-regexp "'\\`[^.].*\\.org'\\|[0-9]+"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "~/Documents/Journal"
        org-agenda-diary-file 'diary-file
        org-autowiki-directory "~/Documents/Autowiki"
        default-directory "~"
        ispell-program-name "enchant-2"
        server-name "~/.emacs.d/currentserver")
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (require 'org-autowiki))

(when (and (eq system-type 'windows-nt) (string= (user-login-name) "btd"))
  (setq org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "C:/Users/btd/Documents/Journal"
        org-autowiki-directory "C:/Users/btd/Documents/Autowiki")
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (require 'org-autowiki)
  (prefer-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (setq default-directory "//wsl$/Ubuntu-20.04/home/btd/")
  (setq ispell-program-name "hunspell"
        ispell-local-dictionary "de_DE_frami"
        ispell-local-dictionary-alist '(("de_DE_frami" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

  (defun file-extended-attributes (filename)
    "Return an alist of extended attributes of file FILENAME.
   Extended attributes are platform-specific metadata about
   the file, such as SELinux context, list of ACL entries, etc."
    (cond
     ((fboundp 'ignore-errors)
      (ignore-errors
        `((acl . ,(file-acl filename))
          (selinux-context .
                           ,(file-selinux-context filename) ) ) ) )
     (t
      `((acl . ,(file-acl filename))
        (selinux-context .
                         ,(file-selinux-context filename) ) ) ) ) ))

(require 'server)
(unless (server-running-p)
  (server-start))


;; -----------------------------------------------------------------------------
;; auto-install packages
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-archive-exclude-alist '(("melpa")))
(dolist (source '(("elpa" . "http://tromey.com/elpa/")
                  ("gnu" . "http://elpa.gnu.org/packages/")
                  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                  ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(setq package-archive-priorities
      '(("nongnu" . 10)
        ("melpa-stable" . 9)
        ("gnu" . 8)
        ("elpa" . 7)))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(company company-posframe concurrent dash
  dumb-jump expand-region flyspell-popup ido-vertical-mode
  idomenu magit markdown-mode multiple-cursors org org-journal
  popup s smartparens wrap-region)
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

(global-display-fill-column-indicator-mode t)
(setq display-fill-column-indicator-column 100)
(setq display-fill-column-indicator-character ?|)

;; Use Pragmata for Unicode and Segoe for Emoji
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "PragmataPro"
                               :height my-font-height)))
  ;;(set-fontset-font t 'emoji "Segoe UI Emoji"))
;; For testing purposes: →„Σ💩“←

;; load my favourite theme of the day
(require 'typo-theme)
(load-theme 'typo t)

;; don't show hat pesky toolbar
(when (display-graphic-p)
  (tool-bar-mode -1)
  (when (not (eq system-type 'darwin))  ; it's not attached to the window anyway
    (menu-bar-mode -1)))

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
                           (right (concat (propertize (format-mode-line "Line %l Col %c") 'face '(:weight light)) "  "
                                          (if (and vc-mode (buffer-file-name))
                                              (concat "" vc-mode " (" (symbol-name (vc-state (buffer-file-name))) ")") "")
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
                    :height 100
                    :box '(:line-width 8 :color "#fffff8"))

;; in magit and diary mode, use word wrap
(add-hook 'magit-mode-hook (lambda () (visual-line-mode t)) t)
(add-hook 'magit-log-edit-mode-hook (lambda () (visual-line-mode t)) t)

(set-display-table-slot standard-display-table 'wrap ?→) ; eol wrap character
(set-display-table-slot standard-display-table 'truncation ?→) ; bol wrap char

;; (require 'echo-bar)

;; (defun my-echobar-function ()
;;   (concat (replace-regexp-in-string
;;            "utf 8" "utf-8"
;;            (replace-regexp-in-string "-" " " (symbol-name buffer-file-coding-system))) " "
;;           (replace-regexp-in-string "-mode" "" (symbol-name major-mode))))
;; (setq echo-bar-function #'my-echobar-function)
;; (echo-bar-enable)


;; ----------------------------------------------------------------------------
;; Make Emacs behave nicely
;; ----------------------------------------------------------------------------

(setq org-journal-file-pattern "%Y%m%d.org")

(add-hook 'prog-mode-hook '(lambda ()
                             (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
                             (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

;; enable global completion
(global-company-mode t)
(company-posframe-mode 1)

(setq ns-pop-up-frames nil)
(global-set-key (kbd "H-h") 'ns-do-hide-emacs)
(global-set-key (kbd "H-w") 'delete-window)

(setq mc/always-run-for-all t)

;; enable "dangerous" features
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; don't ever minimize Emacs on C-z again. Ever.
(define-key (current-global-map) [remap suspend-frame] 'yank)

;; recursive minibuffers are essential for ucs-insert in the minibuffer
(setq enable-recursive-minibuffers t)

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

;; enable mark-multiple
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c e") 'mc/edit-lines)

;; make undo work the same way on the EN and DE keymap
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-_") 'undo-redo)

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

;; quick access to org-agenda and org-todo
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

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

;; start up latex mode with visual-line-mode
(add-hook 'latex-mode-hook
          (lambda ()
            (visual-line-mode t))
          t)

(setq TeX-PDF-mode t)

(autoload 'ox-latex "org-mode" "Org Mode." t)
(autoload 'ox-html "org-mode" "Org Mode." t)
(autoload 'ox-rss "org-mode" "Org Mode." t)
(autoload 'ox-publish "org-mode" "Org Mode." t)
(add-hook 'org-mode-hook
          (lambda ()
            ;; set up org-babel so it uses the correct python version
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((python . t)))
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
(setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll) ((meta)) ((control) . text-scale)))

;; -----------------------------------------------------------------------------
;; Set up blogging in Emacs
;; -----------------------------------------------------------------------------

(setq org-static-blog-publish-title "bastibe.de")
(setq org-static-blog-publish-url "https://bastibe.de/")
(setq org-static-blog-publish-directory "/Users/bastibe/Projects/blog/")
(setq org-static-blog-posts-directory "/Users/bastibe/Projects/blog/posts/")
(setq org-static-blog-drafts-directory "/Users/bastibe/Projects/blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-static-blog-no-comments-tag "nocomments")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(defun with-c-locale (orig-fun &rest args)
  (let ((system-time-locale "C")) (apply orig-fun args)))
(advice-add 'org-static-blog-publish :around #'with-c-locale)

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
    <a href=\"https://github.com/bastibe\">Github</a> | <a href=\"https://bastibe.de/projects.html\">Projects</a> | <a href=\"https://bastibe.de/uses.html\">Uses</a> | <a href=\"https://bastibe.de/reviews.html\">Reviews</a> | <a href=\"https://bastibe.de/about.html\">About</a>
  </div>
</div>")

(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"https://bastibe.de/archive.html\">Other posts</a>
</div>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

(setq org-static-blog-post-comments
"<script async src=\"https://talk.hyvor.com/embed/embed.js\" type=\"module\"></script>
<hyvor-talk-comments id=\"hyvorcomments\" website-id=\"3390\" page-id=\"\"></hyvor-talk-comments>
<script type=\"text/javascript\">
    document.getElementById(\"hyvorcomments\").setAttribute(\"page-id\", location.pathname);
</script>")

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
 '(TeX-PDF-mode t t)
 '(TeX-engine 'xetex)
 '(custom-safe-themes
   '("21fb497b14820147b2b214e640b3c5ee19fcadc15bc288e3c16c9c9575d95d66" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))
 '(delete-selection-mode nil)
 '(display-fill-column-indicator-column 100)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-yasnippet elpy-module-sane-defaults))
 '(magit-diff-refine-hunk t)
 '(magit-push-always-verify nil)
 '(mc/always-run-for-all t t)
 '(ns-alternate-modifier 'meta)
 '(ns-command-modifier 'hyper)
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
 '(org-latex-listings nil t)
 '(org-latex-tables-centered nil)
 '(org-preview-latex-default-process 'imagemagick)
 '(package-selected-packages
   '(zig-mode dumb-jump lua-mode cmake-mode s popup company-posframe ido-completing-read+ xref flycheck-pycheckers annotate flycheck org-static-blog virtualenvwrapper traad evil wrap-region smartparens org-journal multiple-cursors markdown-mode magit iy-go-to-char idomenu ido-vertical-mode ido-ubiquitous flyspell-popup fish-mode expand-region concurrent all-the-icons-dired))
 '(python-check-command "pyflakes3")
 '(safe-local-variable-values
   '((python-shell-interpreter . "/Users/bb/miniconda3/envs/stretch-correlation/bin/ipython")
     (org-startup-folded "content")
     (org-set-startup-cisibility 'content)
     (backup-inhibited . t)))
 '(send-mail-function 'mailclient-send-it)
 '(sentence-end-double-space nil)
 '(tramp-password-prompt-regexp
   "^.*\\(\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|p\\(?:a\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|in\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|btd@olserv04's password:\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\).*: ? *")
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
