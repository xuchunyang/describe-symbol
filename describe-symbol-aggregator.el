;;; describe-symbol-aggregator.el --- Aggregate symbols' documentation from M-x describe-symbol  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/describe-symbol
;; Keywords: help, docs
;; Created: 2020-11-01T15:04:08+08:00
;; Package-Requires: ((emacs "25.1"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package collects documentation of all kinds of symbols via the command
;; describe-symbol.

;;; Code:

(require 'cl-lib)
(require 'help-fns)                     ; `describe-symbol' (added in 25.1)
(require 'help-mode)                    ; `describe-symbol-backends'
(require 'json)
(require 'subr-x)                       ; `when-let'

;; Load some libraries for its docstring
(require 'auth-source)
(require 'avl-tree)
(require 'chart)
(require 'checkdoc)
(require 'eieio)
(require 'ert)
(require 'ewoc)
(require 'let-alist)
(require 'map nil t)
(require 'radix-tree nil t)
(require 'rx)
(require 'seq)
(require 'thunk nil t)
(require 'tq)
(require 'url)
(require 'url-http)

(defalias 'help-window-display-message (symbol-function 'ignore))

;; Make help show eval.c instead of C-source, it creates the " *DOC*" buffer,
;; otherwises `find-lisp-object-file-name' (will be used by the help commands)
;; returns `C-source' rather "eval.c"
;;
;;   (find-lisp-object-file-name 'if (symbol-function 'if))
;;
(help-C-file-name (symbol-function 'if) 'subr)

;; Prefer `grave' over ‘curve'
(setq text-quoting-style 'grave)

;; Avoid spliting the first line
(setq-default fill-column 100)

(setq debug-on-error t)

;; TODO: link to source code, one idea is TAGS
;; `emacs-version', `xref-location', `auth-source-backend'
(defun describe-symbol-aggregator--find-delimiters ()
  (let (res)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (let ((val (get-text-property (point) 'face)))
        (when (and val
                   (listp val)
                   (memq :inverse-video val))
          (push (point) res))))
    (nreverse res)))

(defun describe-symbol-aggregator--dummy ()
  "Dummy function for showing various links that docstring supports.
For testing `describe-symbol-aggregator--find-links'.

Some symbols such as `car' and `foobarbaz' (undefined).
Variable `emacs-version',
Function `emacs-version'.
Option `user-full-name'.
Face `default'.
Info node `(elisp) Property Search'.
URL `http://example.com'."
  nil)

;; Emacs 25.1 lacks the argument REGEXP
(defsubst describe-symbol-aggregator--string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") string)
      (substring string (match-end 0))
    string))

(defsubst describe-symbol-aggregator--string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (let ((i (string-match-p (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
                           string)))
    (if i (substring string 0 i) string)))

(defun describe-symbol-aggregator--sans-path (path)
  "Convert absolute PATH to relative path to emacs source root."
  ;; car   "/Users/xcy/src/emacs/src/data.c"
  ;; pcase "/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp/pcase.el.gz"
  ;; when  "/Applications/Emacs.app/Contents/Resources/lisp/subr.el.gz"
  (if (string-suffix-p ".c" path)
      (concat "src/"
              (describe-symbol-aggregator--string-trim-left
               path (rx bos (* nonl) "/src/")))
    (concat "lisp/"
            (describe-symbol-aggregator--string-trim-right
             (describe-symbol-aggregator--string-trim-left
              path
              (rx bos (* nonl) "/lisp/"))
             (rx ".gz")))))

;; corresponding to `help-make-xrefs'
(defun describe-symbol-aggregator--find-links ()
  "Extract links (that is useful for WWW) in the *Help* buffer.
Return a list of links, each link is an alist."
  (let (res)
    (goto-char (point-min))
    (while
        ;; 27.1 supports NO-ERROR but 26.1/25.1 does not
        (ignore-errors
          (forward-button 1 nil nil))
      (when-let ((category (get-text-property (point) 'category))
                 (category-to-type-alist
                  '(("help-news-button"          . news)
                    ("help-symbol-button"        . symbol)
                    ("help-variable-button"      . variable)
                    ("help-function-button"      . function)
                    ("help-face-button"          . face)
                    ("help-info-button"          . info)
                    ("help-url-button"           . url)
                    ("help-function-def-button"  . function-def)
                    ("help-variable-def-button"  . variable-def)
                    ("help-face-def-button"      . face-def)
                    ("cl-type-definition-button" . cl-type-def)
                    ("cl-help-type-button"       . cl-type))) ; `cl-describe-type'
                 ;; the value of `category' is uninterned symbol, so `eq' (thus `alist-get') does
                 ;; not work, e.g.,
                 ;;
                 ;; (get-text-property (point) 'category)
                 ;; => help-function-def-button
                 ;;
                 ;; (eq (get-text-property (point) 'category) 'help-function-def-button)
                 ;; => nil
                 ;;
                 (type (assoc-default (symbol-name category) category-to-type-alist))
                 (beg (point))
                 (end (let ((limit (+ (point) 100)))
                        ;; ignore links longer than 100 characters
                        (let ((pt (next-property-change (point) nil limit)))
                          (when (/= limit pt)
                            pt)))))
        (let ((args (get-text-property (point) 'help-args)))
          (let ((data (pcase-exhaustive type
                        ('url `((href . ,(car args))))  ; ("http://example.com")
                        ('info `((node . ,(car args)))) ; ("(elisp) Property Search")
                        ('news    ; ("/Applications/Emacs.app/Contents/Resources/etc/NEWS.25" 47804)
                         (with-current-buffer (find-file-noselect (car args))
                           `((file . ,(file-name-nondirectory buffer-file-name))
                             (linum . ,(line-number-at-pos (cadr args)))))) 
                        ((or 'symbol 'function 'variable 'face 'cl-type)
                         nil)
                        ('function-def
                         ;; (car "src/data.c")
                         ;; (when "/Applications/Emacs.app/Contents/Resources/lisp/subr.el")
                         (let* ((fun (car args))
                                (pair (condition-case err
                                          (find-definition-noselect fun nil (cadr args))
                                        (error (message "Error: %s" err)
                                               nil)))
                                (buf (car pair))
                                (pos (cdr pair)))
                           (when buf
                             (with-current-buffer buf
                               `((file . ,(describe-symbol-aggregator--sans-path buffer-file-name))
                                 (linum . ,(line-number-at-pos pos)))))))
                        ('cl-type-def
                         ;; (avl-tree- "/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp/avl-tree.el" define-type)
                         (let* ((fun (car args))
                                (pair (condition-case err
                                          (find-definition-noselect
                                           fun (nth 2 args) (nth 1 args))
                                        (error (message "Error: %s" err)
                                               nil)))
                                (buf (car pair))
                                (pos (cdr pair)))
                           (when buf
                             (with-current-buffer buf
                               `((file . ,(describe-symbol-aggregator--sans-path buffer-file-name))
                                 (linum . ,(line-number-at-pos pos)))))))
                        ('variable-def
                         ;; (emacs-version "src/emacs.c")
                         (let* ((var (car args))
                                (pair (condition-case err
                                          (find-definition-noselect var 'defvar)
                                        (error (message "Error: %s" err)
                                               nil)))
                                (buf (car pair))
                                (pos (cdr pair)))
                           (when buf
                             (with-current-buffer buf
                               `((file . ,(describe-symbol-aggregator--sans-path buffer-file-name))
                                 (linum . ,(line-number-at-pos pos)))))))
                        ('face-def
                         ;; (default "/Applications/Emacs.app/Contents/Resources/lisp/faces.el")
                         (let* ((face (car args))
                                (pair (condition-case err
                                          (find-definition-noselect face 'defface (cadr args))
                                        (error (message "Error: %s" err)
                                               nil)))
                                (buf (car pair))
                                (pos (cdr pair)))
                           (when buf
                             (with-current-buffer buf
                               `((file . ,(describe-symbol-aggregator--sans-path buffer-file-name))
                                 (linum . ,(line-number-at-pos pos))))))))))
            (push `((beg . ,beg)
                    (end . ,end)
                    (type . ,(symbol-name type)) ; `json-serialize' does not like symbol
                    ,@(when data `((data . ,data))))
                  res)))))
    (nreverse res)))

(defun describe-symbol-aggregator (&optional count symbols)
  "Dump all symbols's documentation as a JSON file.

COUNT and SYMBOLS are for debugging/testing, COUNT is a number
and limits to the first specific number of symbols, SYMBOLS is a
list of symbols to use."
  (let* ((all-sym-names (if symbols
                            (mapcar #'symbol-name symbols)
                          (all-completions
                           ""
                           obarray
                           (lambda (vv)
                             (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                      describe-symbol-backends)))))
         (sym-names (cl-loop for name in all-sym-names
                             unless (string-match
                                     (rx (or "--"
                                             ;; Ignore (intern-soft "(setf seq-elt)")
                                             " "))
                                     name)
                             collect name))
         (sym-names (if count
                        (cl-subseq sym-names 0 count)
                      sym-names))
         (sym-names (sort sym-names #'string<))
         (sym-names
          ;; `json-serialize' complains not utf-8
          (dolist (sym '(composition-function-table
                         language-info-alist
                         tibetan-composable-pattern
                         tibetan-precomposed-transcription-alist
                         tibetan-precomposition-rule-alist
                         tibetan-vowel-transcription-alist
                         ;; skip some errors in 26.1
                         global-eldoc-mode-buffers ; not show error, but large buffer with lots of #<killed buffer>
                         menu-bar-options-menu
                         menu-bar-line-wrapping-menu
                         bookmark-map
                         ;; 7.2M, emacs 25.1 takes too long
                         translation-table-vector)
                       sym-names)
            (setq sym-names (delete (symbol-name sym) sym-names))))
         (total (length sym-names))
         (idx 0)
         (skipped (list))
         (output-buffer (generate-new-buffer " *temp*"))
         (output-filename (format "emacs-%s-%s.json"
                                  emacs-version
                                  (format-time-string "%s")))
         (my-json-encode (if (fboundp 'json-serialize)
                             #'json-serialize
                           #'json-encode-alist))
         (t0 (current-time)))
    (dolist (name sym-names)
      (cl-incf idx)
      (message "[%d/%d] %s" idx total name)
      (when (get-buffer "*Help*")
        (kill-buffer "*Help*"))
      (when (condition-case err
                (progn
                  (describe-symbol (intern-soft name))
                  t)
              (error
               (message "ERROR: %s, skip %s" (error-message-string err) name)
               (push name skipped)
               nil))
        (let (doc
              delimiters
              links
              (limit 10240)             ; 10240 characters
              truncated)
          (with-current-buffer "*Help*"
            (when (> (point-max) limit)
              (setq truncated (- (point-max) limit))
              (narrow-to-region (point-min) limit))
            (setq doc (buffer-substring-no-properties (point-min) (point-max)))
            (when truncated
              (setq doc (concat doc (format "...(omitted %d chars)" truncated))))
            (setq delimiters (describe-symbol-aggregator--find-delimiters))
            (setq links (describe-symbol-aggregator--find-links)))
          (with-current-buffer output-buffer
            (goto-char (point-max))
            (insert (funcall my-json-encode
                             (append `((sym . ,name)
                                       (doc . ,doc))
                                     (and delimiters
                                          `((delimiters . ,(vconcat delimiters))))
                                     (and links
                                          `((links . ,(vconcat links)))))))
            (insert ",\n")))))
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (insert (format "{\"emacs-version\": \"%s\", \"timestamp\": %s, \"count\": %d, \"data\": [\n"
                      emacs-version
                      (format-time-string "%s")
                      total))
      (goto-char (point-max))
      (delete-char -2)
      (insert "\n]}")
      (write-region nil nil output-filename)
      (message "Wrote to %s" output-filename))
    (message "Total %d, Processed %d, Skipped %d, %s, in %f seconds"
             total
             (- total (length skipped))
             (length skipped)
             skipped
             (float-time (time-since t0)))))

(provide 'describe-symbol-aggregator)
;;; describe-symbol-aggregator.el ends here
