;;; gptel-quick.el --- Fast look-up using gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Version: 0.0.5
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.1") (gptel "0.8.0"))
;; Keywords: convenience, help, extensions
;; URL: https://github.com/karthink/gptel-quick

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; gptel-quick is a tiny everyday helper for looking up or summarizing text
;; using an LLM (Large Language Model) with low friction.
;;
;; It provides one command.  Call =gptel-quick= to show a short summary or
;; explanation of the word at point, or an active region, in a popup.  This is
;; useful for quickly looking up names, words, phrases, or
;; summarizing/explaining prose or snippets of code, with minimal friction:
;;
;; When the popup is active, press "+" to get a longer summary, M-w (or
;; `kill-ring-save') to copy the text, or q/C-g (`keyboard-quit') to clear it.
;; If you have follow-up questions you can press M-RET to switch to a chat
;; buffer and continue the conversation.
;;
;; If the posframe package is not installed (optional), the response is messaged
;; to the echo area instead.
;;
;; You can control the response word count (approximate) via
;; `gptel-quick-word-count', and the popup timeout via `gptel-quick-timeout'.
;;
;; Set `gptel-quick-use-context' if you have added context to gptel (other
;; regions, buffers or files) and want that to be included in the queries.
;;
;; ----
;;
;; It is intended as a demonstration of `gptel-request', gptel's general purpose
;; API for constructing prompts and querying LLMs from Emacs.

;;; Code:
(require 'gptel)
(require 'thingatpt)

(declare-function pdf-view-active-region-p "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")

(defcustom gptel-quick-display (and (fboundp 'posframe-workable-p)
                                    'posframe)
  "How to display `gptel-quick' results.

Set to `posframe' to use posframe, if available.  Any other value
will cause the result to be displayed in the echo area."
  :type '(choice
          (const :tag "In posframe" posframe)
          (const :tag "Echo area" nil))
  :group 'gptel)

(defvar gptel-quick-system-message
  (lambda (count)
    (format "Explain in %d words or fewer." count))
  "System message for `gptel-quick'.  It is a function called with
one argument, the desired word count -- see
`gptel-quick-word-count'.

WARNING: This variable is experimental and the calling convention is
subject to change in the future.")

(defvar gptel-quick-word-count 12
  "Approximate word count of LLM summary.")
(defvar gptel-quick-timeout 10
  "Time in seconds before dismissing the summary.")
(defvar gptel-quick-use-context nil
  "Whether to use gptel's active context.

This can include other regions, buffers or files added by
`gptel-add'.")
(defvar gptel-quick-backend nil
  "Set `gptel-quick-backend' to use a dedicated model. Require
`gptel-quick-model' to be configured.")
(defvar gptel-quick-model nil
  "Set `gptel-quick-model' to use a dedicated model. Must be one of
`gptel-quick-backend''s models. Require `gptel-quick-backend' to
be configured.")

(defsubst gptel-quick--buffered-callback (callback)
  "Adapt streaming responses for non-streaming CALLBACK."
  (let (chunks)
    (lambda (response info)
      (cond
       ((stringp response) (push response chunks))
       ((eq response t)
        (funcall callback (apply #'concat (nreverse chunks)) info))
       (t
        (when-let* ((error (plist-get info :error)))
          (setf (plist-get info :status) error))
        (funcall callback response info))))))

;;;###autoload
(defun gptel-quick (query-text &optional count options)
  "Explain or summarize region or thing at point with an LLM.

QUERY-TEXT is the text being explained.  COUNT is the approximate
word count of the response.

OPTIONS is a plist of per-request overrides.  The supported keys are:

  :system      a system message accepted by `gptel-request'
  :max-tokens  an integer token limit, or nil for no limit

OPTIONS is retained when requesting a longer response with `+'."
  (interactive
   (list (cond
          ((use-region-p) (buffer-substring-no-properties (region-beginning)
                                                          (region-end)))
          ((and (derived-mode-p 'pdf-view-mode)
                (pdf-view-active-region-p))
           (mapconcat #'identity (pdf-view-active-region-text) "\n\n"))
          (t (thing-at-point 'sexp)))
         current-prefix-arg))

  (when (xor gptel-quick-backend gptel-quick-model)
    (error "gptel-quick-backend and gptel-quick-model must be both set or unset"))

  (let* ((count (or count gptel-quick-word-count))
         (gptel-max-tokens
          (if (plist-member options :max-tokens)
              (plist-get options :max-tokens)
            (floor (+ (sqrt (length query-text))
                      (* count 2.5)))))
         (gptel-use-context (and gptel-quick-use-context 'system))
         (gptel-backend (or gptel-quick-backend gptel-backend))
         (gptel-model (or gptel-quick-model gptel-model))
         (stream (and (fboundp 'gptel-openai-oauth-p)
                      (gptel-openai-oauth-p gptel-backend)))
         (callback (if stream
                       (gptel-quick--buffered-callback
                        #'gptel-quick--callback-posframe)
                     #'gptel-quick--callback-posframe)))
    (gptel-request query-text
      :system (if (plist-member options :system)
                  (plist-get options :system)
                (funcall gptel-quick-system-message count))
      :context (list query-text count
                     (posn-at-point (and (use-region-p) (region-beginning)))
                     options)
      :stream stream
      :callback callback)))

;; From (info "(elisp) Accessing Mouse")
(defun gptel-quick--frame-relative-coordinates (position)
  "Return frame-relative coordinates from POSITION.

POSITION is assumed to lie in a window text area."
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (or (car x-y) 0) (car edges))
          (+ (or (cdr x-y) 0) (cadr edges)))))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function posframe-poshandler-window-center "posframe")
(declare-function markdown-ts-mode "markdown-ts-mode")
(declare-function outline-show-all "outline")
(defvar markdown-ts-default-folding)

(defun gptel-quick--initialize-posframe ()
  "Initialize the result buffer for unfolded Markdown rendering."
  (if (require 'markdown-ts-mode nil t)
      (let ((markdown-ts-default-folding 'show-all))
        (markdown-ts-mode))
    (text-mode))
  (visual-line-mode 1))

(defun gptel-quick--callback-posframe (response info)
  "Show RESPONSE appropriately, in a popup if possible.

Uses the buffer context from INFO.  Set up a buffer-local map for
quick actions when the popup has focus."
  (pcase response
    ('nil (message "Response failed with error: %s" (plist-get info :status)))
    ((pred stringp)
     (pcase-let ((`(,query ,count ,pos ,options) (plist-get info :context)))
       (let ((display-frame (gptel-quick--update-posframe response pos)))
         (cl-labels ((clear-response ()
                       (interactive)
                       (when-let ((buffer (get-buffer " *gptel-quick*")))
                         (with-current-buffer buffer
                           (kill-local-variable 'overriding-local-map)))
                       (and (eq gptel-quick-display 'posframe)
                            (fboundp 'posframe-hide)
                            (posframe-hide " *gptel-quick*")))
                     (quit-response ()
                       (interactive)
                       (clear-response)
                       (message nil))
                     (more-response ()
                       (interactive)
                       (gptel-quick--update-posframe
                        "...generating longer summary..." pos)
                       (gptel-quick query (* count 4) options))
                     (copy-response ()
                       (interactive)
                       (kill-new response)
                       (message "Copied summary to kill-ring."))
                     (create-chat ()
                       (interactive)
                       (gptel (generate-new-buffer-name "*gptel-quick*") nil
                              (concat query "\n\n"
                                      (propertize response 'gptel 'response) "\n\n")
                              t)))
           (when (and (framep display-frame) (frame-live-p display-frame))
             (let ((map (make-sparse-keymap)))
               (define-key map [remap keyboard-quit] #'clear-response)
               (define-key map (kbd "q") #'quit-response)
               (define-key map (kbd "+") #'more-response)
               (define-key map [remap kill-ring-save] #'copy-response)
               (define-key map (kbd "M-RET") #'create-chat)
               (with-current-buffer " *gptel-quick*"
                 (setq-local overriding-local-map map))))))))
    (`(tool-call . ,tool-calls)
     (gptel--display-tool-calls tool-calls info 'minibuffer))))

(defun gptel-quick--update-posframe (response pos)
  "Show RESPONSE at in a posframe (at POS) or the echo area."
  (if (and (eq gptel-quick-display 'posframe)
           (require 'posframe nil t)
           (posframe-workable-p))
      (let ((fringe-indicator-alist nil)
            (coords) (poshandler))
        (if (and pos (not (equal (posn-x-y pos) '(0 . 0))))
            (setq coords (gptel-quick--frame-relative-coordinates pos))
          (setq poshandler #'posframe-poshandler-window-center))
        (let ((frame
               (posframe-show " *gptel-quick*"
                              :string response
                              :position coords
                              :border-width 2
                              :border-color (face-attribute 'vertical-border :foreground)
                              :initialize #'gptel-quick--initialize-posframe
                              :poshandler poshandler
                              :left-fringe 8
                              :right-fringe 8
                              :min-width 36
                              :max-width fill-column
                              :min-height 1
                              :timeout gptel-quick-timeout
                              :accept-focus t
                              :window-point 1)))
          (when (frame-live-p frame)
            (with-current-buffer " *gptel-quick*"
              (when (derived-mode-p 'markdown-ts-mode)
                (outline-show-all)
                (font-lock-ensure)))
            (posframe-refresh " *gptel-quick*")
            (set-window-point (frame-selected-window frame) 1)
            (select-frame-set-input-focus frame)
            frame)))
    (message response)))

(provide 'gptel-quick)
;;; gptel-quick.el ends here
