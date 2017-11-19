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

(defvar sleep-table-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'sleep-table-mode-newline)
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
    (lambda () (use-local-map sleep-table-map)))
  "A mode for keeping track of when a baby sleeps or feeds")

;; DONE: override RET to run next-line or auto-create a new line
;; DONE: enable overwrite-mode
;; DONE: auto-insert time separators on month borders
;; TODO: pre-fill empty file
;; TODO: protect date and table borders from overwrite-mode

(provide 'sleep-table)
