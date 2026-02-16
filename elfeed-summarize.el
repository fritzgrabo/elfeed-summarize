;;; elfeed-summarize.el --- Add LLM-powered inline summaries to elfeed -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Fritz Grabo

;; Author: Fritz Grabo <hello@fritzgrabo.com>
;; URL: https://github.com/fritzgrabo/elfeed-summarize
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (elfeed "3.4.1") (llm "0.28.4"))
;; Keywords: comm

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Add LLM-powered inline summaries to elfeed.

;;; Code:

(require 'elfeed)
(require 'elfeed-search)
(require 'elfeed-show)
(require 'llm)
(require 'seq)
(require 'shr)

;;;; Customization

(defgroup elfeed-summarize nil
  "Add LLM-powered inline summaries to elfeed."
  :group 'elfeed)

(defcustom elfeed-summarize-system-prompt
  "You write one-sentence summaries for an RSS feed reader.
The summary appears below the article headline and helps the user decide
whether to read the full article. You MUST respond with exactly one
short sentence. No preamble, no quotation marks, no bullet points, no
multiple paragraphs."
  "System prompt sent to the LLM for summary generation."
  :type 'string
  :group 'elfeed-summarize)

(defcustom elfeed-summarize-expand-prompt
  "You are extending an existing summary of an article for an RSS feed reader.
The current summary is shown below, followed by the full article text.
You MUST respond with exactly one short paragraph of one to two short
sentences that adds the next most important detail not yet covered. Keep
sentences brief and concise; the user can request further expansion.
Return ONLY the new paragraph, not the existing summary. No quotation
marks, no bullet points, no multiple paragraphs."
  "System prompt sent to the LLM when expanding a summary."
  :type 'string
  :group 'elfeed-summarize)

(defcustom elfeed-summarize-max-entry-length 1500
  "Maximum number of characters of entry text to send to the LLM.
Set to nil to disable truncation."
  :type '(choice (integer :tag "Max characters")
                 (const :tag "No limit" nil))
  :group 'elfeed-summarize)

(defface elfeed-summarize-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for inline summary text in the elfeed search buffer."
  :group 'elfeed-summarize)

(defcustom elfeed-summarize-show-insert-summary-function
  #'elfeed-summarize-show-insert-summary-mail-style
  "Function called to insert the summary in elfeed-show buffers.

Called with no arguments after the buffer has been rendered."
  :type 'function
  :group 'elfeed-summarize)

(defvar elfeed-summarize-llm-provider nil
  "LLM provider to use for generating summaries.

Must be set to an `llm' provider instance, e.g.:

  (setq elfeed-summarize-llm-provider
        (make-llm-ollama :chat-model \"mistral:latest\"))")

;;;; Shared helpers

(defvar-local elfeed-summarize--fill-width nil
  "Cached window width for summary formatting.

Captured when the user initiates a summary request so that async
callbacks format to the correct width even if the window is no longer
selected or visible.")

(defun elfeed-summarize--format-summary (text)
  "Format TEXT for display as an inline summary."
  (let ((result (with-temp-buffer
                  (insert (string-trim text))
                  (let ((fill-column (or elfeed-summarize--fill-width
                                         (1- (window-width)))))
                    (fill-region (point-min) (point-max)))
                  (buffer-string))))
    (propertize result 'face 'elfeed-summarize-summary-face)))

(defun elfeed-summarize--entry-text (entry)
  "Extract and optionally truncate the plain text content of ENTRY."
  (when-let* ((raw (elfeed-entry-content entry))
              (content (elfeed-deref raw)))
    (let ((text (if (eq (elfeed-entry-content-type entry) 'html)
                    (with-temp-buffer
                      (insert content)
                      (shr-render-region (point-min) (point-max))
                      (buffer-string))
                  content)))
      (if (and elfeed-summarize-max-entry-length
               (> (length text) elfeed-summarize-max-entry-length))
          (substring text 0 elfeed-summarize-max-entry-length)
        text))))

(defun elfeed-summarize--generate-summary (entry callback &optional on-error)
  "Generate a summary for ENTRY, calling CALLBACK on success or ON-ERROR on error."
  (if-let* ((cached (elfeed-meta entry :summary)))
      (funcall callback cached)
    (when (elfeed-meta entry :summary-pending)
      (user-error "Summary generation already in progress"))
    (unless elfeed-summarize-llm-provider
      (user-error "Please set `elfeed-summarize-llm-provider'"))
    (let ((text (elfeed-summarize--entry-text entry)))
      (unless text
        (user-error "Entry has no content to summarize"))
      (setf (elfeed-meta entry :summary-pending) t)
      (llm-chat-async elfeed-summarize-llm-provider
                      (llm-make-chat-prompt
                       text :context elfeed-summarize-system-prompt)
                      (lambda (summary)
                        (setf (elfeed-meta entry :summary-pending) nil)
                        (setf (elfeed-meta entry :summary) summary)
                        (funcall callback summary))
                      (lambda (_type message)
                        (setf (elfeed-meta entry :summary-pending) nil)
                        (when on-error (funcall on-error))
                        (message "elfeed-summarize: %s" message))))))

(defun elfeed-summarize--expand-summary (entry callback &optional on-error)
  "Extend the summary for ENTRY, calling CALLBACK on success or ON-ERROR on error."
  (when (elfeed-meta entry :summary-pending)
    (user-error "Summary generation already in progress"))
  (unless elfeed-summarize-llm-provider
    (user-error "Please set `elfeed-summarize-llm-provider'"))
  (let ((text (elfeed-summarize--entry-text entry))
        (existing (elfeed-meta entry :summary)))
    (unless text
      (user-error "Entry has no content to summarize"))
    (unless existing
      (user-error "No summary to expand; generate one first"))
    (setf (elfeed-meta entry :summary-pending) t)
    (llm-chat-async elfeed-summarize-llm-provider
                    (llm-make-chat-prompt
                     (concat "CURRENT SUMMARY:\n" existing
                             "\n\nFULL ARTICLE:\n" text)
                     :context elfeed-summarize-expand-prompt)
                    (lambda (extra)
                      (setf (elfeed-meta entry :summary-pending) nil)
                      (let ((expanded (concat existing "\n\n" extra)))
                        (setf (elfeed-meta entry :summary) expanded)
                        (funcall callback expanded)))
                    (lambda (_type message)
                      (setf (elfeed-meta entry :summary-pending) nil)
                      (when on-error (funcall on-error))
                      (message "elfeed-summarize: %s" message)))))

;;;; Search buffer

(defun elfeed-summarize--search-overlay (entry)
  "Return the summary overlay for ENTRY, or nil."
  (seq-find (lambda (ov) (eq (overlay-get ov 'elfeed-summarize-entry) entry))
            (overlays-in (point-min) (point-max))))

(defun elfeed-summarize--search-show (entry text)
  "Display TEXT below ENTRY's line in the elfeed search buffer."
  (elfeed-summarize--search-hide entry)
  (let ((pos (seq-position elfeed-search-entries entry)))
    (when pos
      (save-excursion
        (elfeed-goto-line (+ pos elfeed-search--offset))
        (let ((ov (make-overlay (line-end-position) (1+ (line-end-position)))))
          (overlay-put ov 'elfeed-summarize-entry entry)
          (overlay-put ov 'after-string
                       (concat "\n"
                               (elfeed-summarize--format-summary text)
                               "\n\n")))))))

(defun elfeed-summarize--search-hide (entry)
  "Remove the summary overlay for ENTRY."
  (when-let* ((ov (elfeed-summarize--search-overlay entry)))
    (delete-overlay ov)))

(defun elfeed-summarize--search-hide-all ()
  "Remove all summary overlays in the current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'elfeed-summarize-entry)
      (delete-overlay ov))))

(defun elfeed-summarize-search-toggle-summary (entry)
  "Toggle an inline summary for ENTRY in the elfeed search buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (setq elfeed-summarize--fill-width (1- (window-width)))
    (if (elfeed-summarize--search-overlay entry)
        (elfeed-summarize--search-hide entry)
      (elfeed-summarize--search-show entry "Generating summary...")
      (elfeed-summarize--generate-summary
       entry
       (lambda (summary)
         (when (elfeed-summarize--search-overlay entry)
           (elfeed-summarize--search-show entry summary)))
       (lambda () (elfeed-summarize--search-hide entry))))))

(defun elfeed-summarize-search-expand-entry (entry)
  "Expand the inline summary for ENTRY with an additional paragraph.
If no summary exists yet, generate one first."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (setq elfeed-summarize--fill-width (1- (window-width)))
    (if (elfeed-meta entry :summary)
        (let ((existing (elfeed-meta entry :summary)))
          (elfeed-summarize--search-show entry (concat existing "\n\nExtending summary..."))
          (elfeed-summarize--expand-summary
           entry
           (lambda (expanded)
             (when (elfeed-summarize--search-overlay entry)
               (elfeed-summarize--search-show entry expanded)))
           (lambda () (elfeed-summarize--search-show entry existing))))
      (elfeed-summarize--search-show entry "Generating summary...")
      (elfeed-summarize--generate-summary
       entry
       (lambda (summary)
         (when (elfeed-summarize--search-overlay entry)
           (elfeed-summarize--search-show entry summary)))
       (lambda () (elfeed-summarize--search-hide entry))))))

;;;; Show buffer

(defvar-local elfeed-summarize--show-summary-visible-p nil
  "Whether the summary is currently shown in an elfeed-show buffer.")

(defvar-local elfeed-summarize--show-pending-message nil
  "Message to show while an LLM request is in-flight for this show buffer.")

(defun elfeed-summarize--show-summary-value ()
  "Return the string to display as the summary.

Depends on the state of the current show buffer's entry."
  (let* ((entry elfeed-show-entry)
         (cached (elfeed-meta entry :summary))
         (key (substitute-command-keys
               "\\<elfeed-summarize-show-mode-map>\\[elfeed-summarize-show-toggle-summary]")))
    (cond
     ((and elfeed-summarize--show-pending-message cached
           elfeed-summarize--show-summary-visible-p)
      (concat (elfeed-summarize--format-summary cached)
              "\n\n"
              (propertize elfeed-summarize--show-pending-message
                          'face 'elfeed-summarize-summary-face)))
     (elfeed-summarize--show-pending-message
      (propertize elfeed-summarize--show-pending-message
                  'face 'elfeed-summarize-summary-face))
     ((not cached)
      (concat (propertize "Press " 'face 'elfeed-summarize-summary-face)
              (propertize key 'face 'help-key-binding)
              (propertize " to generate" 'face 'elfeed-summarize-summary-face)))
     ((not elfeed-summarize--show-summary-visible-p)
      (concat (propertize "Press " 'face 'elfeed-summarize-summary-face)
              (propertize key 'face 'help-key-binding)
              (propertize " to show" 'face 'elfeed-summarize-summary-face)))
     (t
      (elfeed-summarize--format-summary cached)))))

(defun elfeed-summarize-show-insert-summary-mail-style ()
  "Insert the summary into an elfeed-show buffer using mail-style layout."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      ;; Insert after the Link: header line, which is always present
      ;; and exactly one line long.
      (when (re-search-forward "^Link: .*\n" nil t)
        (insert (format (propertize "Summary: %s\n" 'face 'message-header-name)
                        (elfeed-summarize--show-summary-value)))))))

(defun elfeed-summarize--show-insert-summary ()
  "Call `elfeed-summarize-show-insert-summary-function' if appropriate."
  (when (bound-and-true-p elfeed-summarize-show-mode)
    (funcall elfeed-summarize-show-insert-summary-function)))

(defun elfeed-summarize--show-request (entry message llm-fn)
  "Request a summary for ENTRY via LLM-FN, show MESSAGE while waiting."
  (setq elfeed-summarize--show-summary-visible-p t)
  (setq elfeed-summarize--show-pending-message message)
  (elfeed-show-refresh)
  (let* ((buf (current-buffer))
         (refresh (lambda ()
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (setq elfeed-summarize--show-pending-message nil)
                        (elfeed-show-refresh))))))
    (funcall llm-fn entry
             (lambda (_result) (funcall refresh))
             refresh)))

(defun elfeed-summarize-show-toggle-summary ()
  "Toggle the summary display in the current elfeed-show buffer."
  (interactive)
  (setq elfeed-summarize--fill-width (1- (window-width)))
  (let ((entry elfeed-show-entry))
    (cond
     ;; No cached summary — generate one.
     ((not (elfeed-meta entry :summary))
      (elfeed-summarize--show-request
       entry "Generating summary..."
       #'elfeed-summarize--generate-summary))
     ;; Cached — toggle visibility.
     (t
      (setq elfeed-summarize--show-summary-visible-p
            (not elfeed-summarize--show-summary-visible-p))
      (elfeed-show-refresh)))))

(defun elfeed-summarize-show-expand-entry ()
  "Expand the summary in the current elfeed-show buffer."
  (interactive)
  (setq elfeed-summarize--fill-width (1- (window-width)))
  (let ((entry elfeed-show-entry))
    (cond
     ;; No summary yet — generate.
     ((not (elfeed-meta entry :summary))
      (elfeed-summarize--show-request
       entry "Generating summary..."
       #'elfeed-summarize--generate-summary))
     ;; Hidden — just show it.
     ((not elfeed-summarize--show-summary-visible-p)
      (setq elfeed-summarize--show-summary-visible-p t)
      (elfeed-show-refresh))
     ;; Shown — expand.
     (t
      (elfeed-summarize--show-request
       entry "Extending summary..."
       #'elfeed-summarize--expand-summary)))))

;;;; Shared interactive commands

(defun elfeed-summarize-remove-summary (entry)
  "Remove the cached summary for ENTRY in the elfeed database."
  (interactive
   (list (cond
          ((derived-mode-p 'elfeed-search-mode)
           (elfeed-search-selected :ignore-region))
          ((derived-mode-p 'elfeed-show-mode)
           elfeed-show-entry)
          (t (user-error "Not in an elfeed buffer")))))
  (when (and (elfeed-entry-p entry) (elfeed-meta entry :summary))
    (setf (elfeed-meta entry :summary) nil)
    (cond
     ((derived-mode-p 'elfeed-search-mode)
      (elfeed-summarize--search-hide entry))
     ((derived-mode-p 'elfeed-show-mode)
      (setq elfeed-summarize--show-summary-visible-p nil)
      (elfeed-show-refresh)))))

(defun elfeed-summarize-remove-all-summaries ()
  "Remove cached summaries from all entries in the elfeed database."
  (interactive)
  (let ((count 0))
    (with-elfeed-db-visit (entry _feed)
      (when (elfeed-meta entry :summary)
        (setf (elfeed-meta entry :summary) nil)
        (setq count (1+ count))))
    ;; Update visible buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond
         ((derived-mode-p 'elfeed-search-mode)
          (elfeed-summarize--search-hide-all))
         ((and (derived-mode-p 'elfeed-show-mode)
               (bound-and-true-p elfeed-summarize-show-mode))
          (setq elfeed-summarize--show-summary-visible-p nil)
          (elfeed-show-refresh)))))
    (message "Cleared %d summaries" count)))

;;;; Minor modes

(defvar elfeed-summarize-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "z" #'elfeed-summarize-search-toggle-summary)
    (define-key map "Z" #'elfeed-summarize-search-expand-entry)
    map)
  "Keymap for `elfeed-summarize-search-mode'.")

(define-minor-mode elfeed-summarize-search-mode
  "Buffer-local minor mode for elfeed search buffers."
  :keymap elfeed-summarize-search-mode-map
  (if elfeed-summarize-search-mode
      (add-hook 'elfeed-search-update-hook #'elfeed-summarize--search-hide-all nil t)
    (elfeed-summarize--search-hide-all)
    (remove-hook 'elfeed-search-update-hook #'elfeed-summarize--search-hide-all t)))

(defvar elfeed-summarize-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "z" #'elfeed-summarize-show-toggle-summary)
    (define-key map "Z" #'elfeed-summarize-show-expand-entry)
    map)
  "Keymap for `elfeed-summarize-show-mode'.")

(define-minor-mode elfeed-summarize-show-mode
  "Buffer-local minor mode for elfeed show buffers."
  :keymap elfeed-summarize-show-mode-map
  (if elfeed-summarize-show-mode
      (when (> (buffer-size) 0)
        (elfeed-show-refresh))
    (setq elfeed-summarize--show-summary-visible-p nil)
    (when (> (buffer-size) 0)
      (elfeed-show-refresh))))

;;;###autoload
(define-minor-mode elfeed-summarize-mode
  "Toggle inline LLM summaries in elfeed buffers."
  :global t
  (if elfeed-summarize-mode
      (progn
        (add-hook 'elfeed-search-mode-hook #'elfeed-summarize-search-mode)
        (add-hook 'elfeed-show-mode-hook #'elfeed-summarize-show-mode)
        (advice-add 'elfeed-show-refresh :after
                    #'elfeed-summarize--show-insert-summary)
        ;; Enable in existing elfeed buffers.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (cond
             ((derived-mode-p 'elfeed-search-mode)
              (elfeed-summarize-search-mode 1))
             ((derived-mode-p 'elfeed-show-mode)
              (elfeed-summarize-show-mode 1))))))
    (remove-hook 'elfeed-search-mode-hook #'elfeed-summarize-search-mode)
    (remove-hook 'elfeed-show-mode-hook #'elfeed-summarize-show-mode)
    (advice-remove 'elfeed-show-refresh
                   #'elfeed-summarize--show-insert-summary)
    ;; Disable in existing elfeed buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond
         ((derived-mode-p 'elfeed-search-mode)
          (elfeed-summarize-search-mode -1))
         ((derived-mode-p 'elfeed-show-mode)
          (elfeed-summarize-show-mode -1)))))))

(provide 'elfeed-summarize)
;;; elfeed-summarize.el ends here
