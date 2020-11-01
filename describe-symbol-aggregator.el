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

(defun describe-symbol-aggregator (&optional count)
  (let* ((all-sym-names (all-completions
                         ""
                         obarray
                         (lambda (vv)
                           (cl-some (lambda (x) (funcall (nth 1 x) vv))
                                    describe-symbol-backends))))
         (sym-names (cl-loop for name in all-sym-names
                             unless (string-match "--" name)
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
        (let (doc delimiters)
          (with-current-buffer "*Help*"
            (setq
             doc
             (let ((limit 10240))
               (if (> (point-max) limit)
                   (concat
                    (buffer-substring-no-properties (point-min) limit)
                    (format "...(omitted %d chars)" (- (point-max) limit)))
                 (buffer-substring-no-properties (point-min) (point-max)))))
            (setq delimiters (describe-symbol-aggregator--find-delimiters)))
          (with-current-buffer output-buffer
            (goto-char (point-max))
            (insert (funcall my-json-encode
                             `((sym . ,name)
                               (doc . ,doc)
                               ,@(and delimiters
                                      `((delimiters . ,(vconcat delimiters)))))))
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
