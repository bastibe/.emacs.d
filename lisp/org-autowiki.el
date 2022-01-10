;;; org-autowiki.el - Links that automatically create targets
;;
;; Information on custom hyperlinks: https://orgmode.org/manual/Adding-Hyperlink-Types.html

(require 'ol)


(defgroup org-autowiki nil
  "Settings for autowiki"
  :group 'org
  :group 'org-autowiki)


(defcustom org-autowiki-dir "~/Documents/Autowiki/"
  "Directory containing autowiki files"
  :type 'directory)


(org-link-set-parameters "wiki"
                         :follow #'org-autowiki-open)

(defun org-autowiki-open (path arg)
  "Opens a wiki:name::suffix link as if it were a file:path-to-wiki/name.org::suffix

   The path may not contain colons before the '::foo' suffix.

   If the wiki entry does not yet exist, create it with #+TITLE: name."

  (let ((separator-idx (string-match ":" path)) link)
    (if separator-idx
      (setq link (concat org-autowiki-dir "/"
                         (substring path 0 separator-idx) ".org"
                         (substring path separator-idx (length path))))
      (setq link (concat org-autowiki-dir "/" path ".org")))
    (org-link-open-as-file link arg))

  ;; enter title if empty:
  (if (= (point-min) (point-max))
      (insert "#+TITLE: " path)))


(org-link-set-parameters "journal"
                         :follow #'org-journal-link-open)

(defvar org-journal-link-format "\\(?1:[0-9]\\{4\\}\\)-\\(?2:[0-9]\\{2\\}\\)-\\(?3:[0-9]\\{2\\}\\)")

(defun org-journal-link-open (path arg)
  "Opens a journal:yyyy-mm-dd::suffix link as if it were file:path-to-journal/journal-entry.org::suffix

   The path may not contain colons before the '::foo' suffix."

  (let ((separator-idx (string-match ":" path)) datestring path-rest filename)

    ;; separate path into datestring and path-rest:
    (cond (separator-idx
           (setq datestring (substring path 0 separator-idx))
           (setq path-rest (substring path separator-idx (length path))))
          (t
           (setq datestring (concat org-autowiki-dir "/" path ".org"))
           (setq path-rest "")))

    ;; split datestring into year, month, day:
    (let ((year (string-to-number
                 (replace-regexp-in-string org-journal-link-format "\\1" datestring)))
          (month (string-to-number
                  (replace-regexp-in-string org-journal-link-format "\\2" datestring)))
          (day (string-to-number
                (replace-regexp-in-string org-journal-link-format "\\3" datestring))))

      ;; ask org-journal to assemble appropriate file name from date:
      (setq filename (org-journal--get-entry-path (encode-time 0 0 0 day month year))))

    ;; open appropriate org-journal entry:
    (org-link-open-as-file (concat filename path-rest) arg)))


(defvar autowiki-search-history nil)

(defun org-autowiki-search (what)
  "Search things in journal and autowiki

   Search results are displayed in a new buffer, and use
   wiki:/journal: links to the appropriate line."

  (interactive (list (read-string "Enter a string to search for: " nil 'autowiki-search-history)))
  (let (files results)

    ;; get a list of all wiki files and all journal files
    (setq files (directory-files org-autowiki-dir t ".*\.org"))
    (setq files (append files (org-journal--list-files)))

    ;; search for `what` in all files
    (dolist (fname files)
      ;; assemble list of (fname beg line)
      (with-temp-buffer
        (insert-file-contents fname)
        (goto-char (point-min))
        (while (search-forward what nil t)
          (push (list (substring fname 0 -4)
                      (line-number-at-pos)
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))
                results))))

    ;; assemble links in new buffer
    (let ((buf (get-buffer-create "*autowiki-search*")))
      ;; prepare buffer:
      (unless (get-buffer-window buf 0)
        (switch-to-buffer buf))
      (with-current-buffer buf
        (view-mode-disable)
        (erase-buffer)
        (insert "Search results for \"" what "\":\n\n")

        ;; insert links:
        (dolist (result results)
          (cond ((string-match org-autowiki-dir (nth 0 result))
                 (insert "[[wiki:" (file-name-base (nth 0 result)) "::" (number-to-string (nth 1 result)) "]]"))
                ((string-match org-journal-dir (nth 0 result))
                 (insert "[[journal:" (file-name-base (nth 0 result)) "::" (number-to-string (nth 1 result)) "]]"))
                (t  ; default to file: links if not in journal or wiki for some reason
                 (insert "[[file:" (nth 0 result) "::" (number-to-string (nth 1 result)) "]]")))
          (insert "\t" (nth 2 result) "\n"))

        ;; highlight target:
        (goto-char (point-min))
        (while (search-forward what nil t)
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'org-journal-highlight))

        (goto-char (point-min))
        (next-line 2)  ; go to first search result
        (org-mode)
        (view-mode t)))))


(defun org-autowiki-backlinks ()
  "Search for backlinks to the current file."
  (interactive)
  ;; TODO: rebind TAB to go to next link

  (cond
   ;; we are in an autowiki file: produce wiki:link
   ((string-match org-autowiki-dir (buffer-file-name))
    (org-autowiki-search (concat "wiki:" (file-name-base (buffer-file-name)))))
   ;; we are in a journal file: produce journal:link
   ((string-match org-journal-dir (buffer-file-name))
    (let ((time (org-journal--calendar-date->time (org-journal--file-name->calendar-date (buffer-file-name)))))
      (org-autowiki-search (concat "journal:" (format-time-string "%Y-%m-%d" time)))))
   ;; otherwise:
   (t (message "not an org-autowiki file"))))

(provide 'org-autowiki)
;;; org-autowiki.el ends here
