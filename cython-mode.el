;;; cython-mode.el --- support for Cython sources
;;
;; Copyright (C) 2010 Georg Brandl
;;
;; Author: Georg Brandl <georg@python.org>
;; Created: Jan 2010
;; Keywords: languages
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Major mode for editing Cython.
;;
;;; Code:

;; I don't know how to properly depend on either python-mode, but one
;; of them has to be loaded.
(unless (featurep 'python)
  (require 'python-mode))

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))


(defun cython-compile ()
  "Compile the file via Cython."
  (interactive)
  (let ((cy-buffer (current-buffer)))
    (with-current-buffer
        (compile compile-command)
      (set (make-local-variable 'cython-buffer) cy-buffer)
      (add-to-list (make-local-variable 'compilation-finish-functions)
                   'cython-compilation-finish))))

(defun cython-compilation-finish (buffer how)
  "Called when Cython compilation finishes."
  (if (and cython-annotation
           (string-equal how "finished\n"))
      (with-current-buffer cython-buffer
        (cython-load-annotations)
        (cython-annotation-minor-mode 1))))

(defun cython-load-annotations ()
  "Load annotation data for current buffer."
  (let ((anno-file (concat (file-name-sans-extension buffer-file-name) ".el")))
    (unless (file-exists-p anno-file)
      (error "No annotations file found"))
    (setq cython-buffer-annotations
          (with-temp-buffer
            (insert-file-contents anno-file)
            (let ((form (read (current-buffer))))
              (eval form))))))

(defun cython-unannotate ()
  "Remove annotations from current buffer."
  (interactive)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(point-entered nil)))
  (save-excursion
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'cython-lineno)
        (delete-overlay ov))))
  (setq cython-buffer-annotated nil))

(defun cython-annotate ()
  "Add annotations to current buffer."
  (interactive)
  (let (color ovl (line 1))
    (unless cython-buffer-annotations
      (cython-load-annotations))
    (cython-unannotate)
    (save-excursion
      (goto-char (point-min))
      (dolist (line-anno cython-buffer-annotations)
        (let ((ovl (make-overlay (point) (line-end-position)))
              (color (format "#FFFF%02X"
                             (truncate (/ 255 (+ 1 (/ (car line-anno) 10.0)))))))
          (overlay-put ovl 'face (list ':background color))
          (if cython-annotation-auto-show
              ;; this doesn't seem to work with overlays
              (add-text-properties (point) (line-end-position)
                                   '(point-entered cython--point-move-hook)))
          (overlay-put ovl 'cython-lineno line)
          (forward-line)
          (setq line (1+ line))))
    (setq cython-buffer-annotated t))))

(defun cython-toggle-annotations ()
  "Toggle Cython annotations in current buffer."
  (interactive)
  (if cython-buffer-annotated
      (cython-unannotate)
    (cython-annotate)))

(defun cython--point-move-hook (from to)
  "Show C code for current line, called from `point-entered' text property."
  (ignore-errors
   (cython-show-c-code)))

(defun cython-show-c-code ()
  "Show C code for current line in another window."
  (interactive)
  (unless cython-buffer-annotations
    (error "No annotations loaded for buffer"))
  (let* ((linum (catch 'found
                  (dolist (ov (overlays-at (point)))
                    (if (overlay-get ov 'cython-lineno)
                        (throw 'found (overlay-get ov 'cython-lineno))))
                  (error "No annotation for this line")))
         (code (cdr (nth (1- linum) cython-buffer-annotations))))
    (with-current-buffer (get-buffer-create
                          (concat "*" buffer-file-name " C code*"))
      (unless (eq major-mode 'c-mode)
        ;; set up buffer for showing Cython C code snippets
        (c-mode)
        ;; highlight Python and Cython API
        (font-lock-add-keywords nil
         '(("\\(__Pyx_[A-Za-z_]+\\)(" 1
            font-lock-builtin-face prepend)
           ("\\(Py[A-Z][a-z]+_[A-Za-z][A-Za-z_]+\\)(" 1
            font-lock-builtin-face prepend)
           ("\\(__Pyx_X?\\(GOT\\|GIVE\\)REF\\|__Pyx_RefNanny[A-Za-z]+\\)" 1
            font-lock-preprocessor-face prepend)
           )))
      (buffer-disable-undo)
      (setq buffer-read-only nil)
      (kill-region (point-min) (point-max))
      (insert code)
      (setq buffer-read-only t)
      (display-buffer (current-buffer) t))))


(defgroup cython nil "Cython mode.")

(defcustom cython-annotation t
  "If non-nil, annotate Cython compilations and show them in the source buffer."
  :group 'cython :type 'boolean)

(defcustom cython-annotation-auto-show t
  "If non-nil, automatically show Cython C code when moving around
in a buffer with compilation annotations."
  :group 'cython :type 'boolean)

(defvar cython-buffer-annotated nil
  "Whether the buffer has Cython annotations enabled.")
(defvar cython-buffer-annotations nil
  "The Cython annotations for the buffer.")

(make-variable-buffer-local 'cython-buffer-annotated)
(make-variable-buffer-local 'cython-buffer-annotations)

(define-minor-mode cython-annotation-minor-mode
  "Minor mode for showing Cython C code annotations."
  nil
  "/Ann"
  '(("\C-c\C-a" . cython-toggle-annotations)
    ("\C-c\C-v" . cython-show-c-code))
  (if cython-annotation-minor-mode
      (cython-annotate)
    (cython-unannotate)))

(defvar cython-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Will inherit from `python-mode-map' thanks to define-derived-mode.
    (define-key map "\C-c\C-c" 'cython-compile)
    map)
  "Keymap used in `cython-mode'.")

(defvar cython-font-lock-keywords
  `(;; new keywords in Cython language
    (,(regexp-opt '("api" "by" "cdef" "cimport" "cpdef" "ctypedef" "enum" "except?"
                    "extern" "gil" "include" "inline" "nogil" "property" "public"
                    "readonly" "struct" "union" "DEF" "IF" "ELIF" "ELSE") 'words)
     1 font-lock-keyword-face)
    ;; C and Python types (highlight as builtins)
    (,(regexp-opt '("NULL" "bint" "char" "dict" "double" "float" "int" "list"
                    "long" "object" "Py_ssize_t" "short" "size_t" "void") 'words)
     1 font-lock-builtin-face)
    ;; cdef is used for more than functions, so simply highlighting the next
    ;; word is problematic. struct, enum and property work though.
    ("\\<\\(?:struct\\|enum\\|union\\|cppclass\\)[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     1 font-lock-type-face)
    ("\\<property[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     1 font-lock-function-name-face))
  "Additional font lock keywords for Cython mode.")

(define-derived-mode cython-mode python-mode "Cython"
  "Major mode for Cython development, derived from Python mode.

\\{cython-mode-map}"
  (setcar font-lock-defaults
          (append python-font-lock-keywords cython-font-lock-keywords))
  (set (make-local-variable 'compile-command)
       (if cython-annotation
           (concat "cython-el-annotate -a " buffer-file-name)
         (concat "cython " buffer-file-name)))
  (add-to-list (make-local-variable 'compilation-finish-functions)
               'cython-compilation-finish))

(provide 'cython-mode)
;;; cython-mode.el ends here
