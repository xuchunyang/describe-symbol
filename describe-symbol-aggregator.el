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

(defalias 'help-window-display-message (symbol-function 'ignore))

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
                         tibetan-vowel-transcription-alist)
                       sym-names)
            (setq sym-names (delete (symbol-name sym) sym-names))))
         (total (length sym-names))
         (idx 0)
         (skipped 0)
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
      (condition-case err
          (progn
            (describe-symbol (intern-soft name))
            (let ((doc
                   (with-current-buffer "*Help*"
                     (let ((limit 10240))
                       (if (> (point-max) limit)
                           (concat
                            (buffer-substring-no-properties (point-min) limit)
                            (format "...(omitted %d chars)" (- (point-max) limit)))
                         (buffer-substring-no-properties (point-min) (point-max)))))))
              (with-current-buffer output-buffer
                (goto-char (point-max))
                (insert (funcall my-json-encode `((sym . ,name)
                                                  (doc . ,doc))))
                (insert ",\n")))
            t)
        (error
         (message "ERROR: %s, skip %s" (error-message-string err) name)
         (cl-incf skipped))))
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
    (message "Total %d, Processed %d, Skipped %d, in %f seconds"
             total
             (- total skipped)
             skipped
             (float-time (time-since t0)))))

(provide 'describe-symbol-aggregator)
;;; describe-symbol-aggregator.el ends here
