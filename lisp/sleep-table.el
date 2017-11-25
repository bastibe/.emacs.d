(make-face 'sleep-table-date-face)
(set-face-attribute 'sleep-table-date-face nil :slant 'oblique)

(make-face 'sleep-table-time-highlight-face)
(set-face-attribute 'sleep-table-time-highlight-face nil :background "#FFEECC")

(defun sleep-table-mode-newline ()
  (interactive)
  (if (save-excursion
        (end-of-line)
        (eq (point) (point-max)))
      (sleep-table-mode-insert-new-line)
    (next-line)))

(defun sleep-table-mode-insert-new-line ()
  (let* ((col (current-column))
         (date (buffer-substring-no-properties
                (progn (beginning-of-line) (point)) ; start of date
                (progn (forward-word 3) (point))))  ; end of date
         (day-month-year (mapcar 'string-to-number (split-string date "[-/\\.]")))
         (new-time (encode-time 0 0 0
                                (1+ (nth 0 day-month-year)) ; increment day
                                (nth 1 day-month-year)
                                (nth 2 day-month-year))))
    (end-of-line)
    (newline)
    ;; insert new time header at month boundaries
    (when (not (= (nth 4 (decode-time new-time)) ; month
                  (nth 1 day-month-year)))
      (insert "             0:00        6:00        12:00       18:00")
      (newline))
    (insert (format-time-string "%d-%m-%Y" new-time))
    (insert " |                                                  |")
    (beginning-of-line)
    (forward-char col)))

(defun sleep-table-print-current-time ()
  (interactive)
  (let ((hour (/ (float (- (current-column) 13))
                 2.0)))
    (when (and (>= hour 0) (< hour 24))
      (message "%i:%s"
               (- hour (mod hour 1))
               (if (= (mod hour 1) 0.5)
                   "30"
                 "00")))))

(defun sleep-table-forward-char (&optional arg)
  (interactive "^p")
  (sleep-table-print-current-time)
  (forward-char arg))

(defun sleep-table-backward-char (&optional arg)
  (interactive "^p")
  (sleep-table-print-current-time)
  (backward-char arg))

(defun sleep-table-previous-line (&optional arg try-vscroll)
  (interactive "^p\np")
  (sleep-table-print-current-time)
  (previous-line arg try-vscroll))

(defun sleep-table-next-line (&optional arg try-vscroll)
  (interactive "^p\np")
  (sleep-table-print-current-time)
  (next-line arg try-vscroll))

(defun sleep-table-forward-word (&optional arg)
  (interactive "^p")
  (sleep-table-print-current-time)
  (forward-word arg))

(defun sleep-table-backward-word (&optional arg)
  (interactive "^p")
  (sleep-table-print-current-time)
  (backward-word arg))

(defun sleep-table-beginning-of-table ()
  (interactive)
  (beginning-of-line)
  (forward-char 13))

(defun sleep-table-end-of-table ()
  (interactive)
  (beginning-of-line)
  (forward-char 60))

(defvar sleep-table-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'sleep-table-mode-newline)
    (define-key map (kbd "C-f") 'sleep-table-forward-char)
    (define-key map (kbd "<right>") 'sleep-table-forward-char)
    (define-key map (kbd "C-b") 'sleep-table-backward-char)
    (define-key map (kbd "<left>") 'sleep-table-backward-char)
    (define-key map (kbd "C-n") 'sleep-table-next-line)
    (define-key map (kbd "<down>") 'sleep-table-next-line)
    (define-key map (kbd "C-p") 'sleep-table-previous-line)
    (define-key map (kbd "<up>") 'sleep-table-previous-line)
    (define-key map (kbd "M-f") 'sleep-table-forward-word)
    (define-key map (kbd "<M-right>") 'sleep-table-forward-word)
    (define-key map (kbd "M-b") 'sleep-table-backward-word)
    (define-key map (kbd "<M-left>") 'sleep-table-backward-word)
    (define-key map (kbd "M-a") 'sleep-table-beginning-of-table)
    (define-key map (kbd "M-e") 'sleep-table-end-of-table)
    map)
  "Keymap for Sleep Table Mode")

(define-generic-mode 'sleep-table-mode
  nil
  '("|")
  '(("[0-9]+[\-/][0-9]+[\-/][0-9]+" . 'sleep-table-date-face)
    ("       \\([0-9:]+\\)" 1 'sleep-table-time-highlight-face)
    ("^[0-9]+[\-/][0-9]+[\-/][0-9]+ | .\\{00\\}\\(.\\)" 1 'sleep-table-time-highlight-face)
    ("^[0-9]+[\-/][0-9]+[\-/][0-9]+ | .\\{12\\}\\(.\\)" 1 'sleep-table-time-highlight-face)
    ("^[0-9]+[\-/][0-9]+[\-/][0-9]+ | .\\{24\\}\\(.\\)" 1 'sleep-table-time-highlight-face)
    ("^[0-9]+[\-/][0-9]+[\-/][0-9]+ | .\\{36\\}\\(.\\)" 1 'sleep-table-time-highlight-face)
    ("^[0-9]+[\-/][0-9]+[\-/][0-9]+ | .\\{36\\}\\(.\\)" 1 'sleep-table-time-highlight-face))
  '("\\.slp\\'")
  '(overwrite-mode
    (lambda () (use-local-map sleep-table-map))
    (lambda ()
      (make-variable-buffer-local 'post-self-insert-hook)
      (add-hook 'post-self-insert-hook 'sleep-table-print-current-time)))
  "A mode for keeping track of when a baby sleeps or feeds")

;; DONE: override RET to run next-line or auto-create a new line
;; DONE: enable overwrite-mode
;; DONE: auto-insert time separators on month borders
;; TODO: pre-fill empty file
;; TODO: protect date and table borders from overwrite-mode

(provide 'sleep-table)
