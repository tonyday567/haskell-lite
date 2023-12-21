;;; haskell-lite.el --- haskell-lite package -*- lexical-binding: t -*-

;; Copyright © 2022 Tony Day.  All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'haskell-ng-mode)


;; Customize

(defgroup haskell-lite nil
  "Haskell, but lite."
  :prefix "haskell-lite-"
  :group 'haskell)

(defcustom haskell-lite-eval-result-prefix "-- "
  "The prefix displayed in the minibuffer before a result value."
  :group 'haskell-lite
  :type 'string
  :package-version '(haskell-lite "0.1.0"))

(defface haskell-lite-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `haskell-lite-overlays-use-font-lock' is non-nil, this face is applied
with lower priority than the syntax highlighting."
  :group 'haskell-lite
  :package-version '(haskell-lite "0.1.0"))

(defcustom haskell-lite-overlays-use-font-lock t
  "If non-nil, results overlays are font-locked as Haskell code.
If nil, apply `haskell-lite-result-overlay-face'
to the entire overlay instead of
font-locking it."
  :group 'haskell-lite
  :type 'boolean
  :package-version '(haskell-lite "0.1.0"))

(defcustom haskell-lite-eval-result-duration 'command
  "Duration, in seconds, of eval-result overlays.

If nil, overlays last indefinitely.

If the symbol `command', they're erased before the next command."
  :group 'haskell-lite
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Last indefinitely" nil))
  :package-version '(haskell-lite "0.1.0"))



(defcustom haskell-lite-prompt-input-regexps
  '("> ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :group 'haskell-comint
  :version "24.4")

(defcustom haskell-lite-prompt-regexp "> "
  "Regular expression matching top level input prompt of Haskell shell.
It should not contain a caret (^) at the beginning."
  :group 'haskell-comint
  :type 'string)

(defcustom haskell-lite-prompt-block-regexp "| "
  "Regular expression matching block input prompt of Haskell shell.
It should not contain a caret (^) at the beginning."
  :group 'haskell-comint
  :type 'string)

(defcustom haskell-lite-prompt-debug-regexp "> "
  "Regular expression matching debugging input prompt of Haskell shell.
It should not contain a caret (^) at the beginning."
  :group 'haskell-comint
  :type 'string)

(defcustom haskell-lite-first-prompt-hook nil
  "Hook run upon first shell prompt detection.
This is the place for shell setup functions that need to wait for
output.  Since the first prompt is ensured, this helps the
current process to not hang while waiting.  This is useful to
safely attach setup code for long-running processes that
eventually provide a shell."
  :version "25.1"
  :type 'hook
  :group 'haskell-comint)

(defcustom haskell-lite-compilation-regexp-alist
  '(("^\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\(-[0-9]*\\|\\)\\): error:$" 2 3 4 2 1)
    ("^\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\(-[0-9]*\\|\\)\\): warning:" 2 3 4 1 1)
    ("^[[:space:]]+[[:word:]]+, called at \\(\\([[:word:]]*/\\)*[[:word:]]+\\.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) in "
    1 3 4))
  "`compilation-error-regexp-alist' for inferior Haskell."
  :type '(alist string)
  :group 'haskell-comint)

(defvar haskell-lite-output-filter-in-progress nil)
(defvar haskell-lite-output-filter-buffer nil)

(defvar haskell-lite--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for repl.
Do not set this variable directly, instead use
`haskell-lite-prompt-set-calculated-regexps'.")

(defvar haskell-lite--block-prompt nil
  "Input block prompt for repl.
Do not set this variable directly, instead use
`haskell-lite-prompt-set-calculated-regexps'.")

(defun haskell-lite-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.
Build and set the values for `haskell-lite-input-prompt-regexp'
and `haskell-lite-output-prompt-regexp' using the values from
`haskell-lite-prompt-regexp', `haskell-lite-prompt-block-regexp',
`haskell-lite-prompt-debug-regexp',
`haskell-lite-prompt-input-regexps' and detected prompts from
`haskell-lite-prompt-detect'."
  (when (not haskell-lite--prompt-calculated-input-regexp)
    (let* ((input-prompts nil)
           (build-regexp
            (lambda (prompts)
              (concat "^\\("
                      (mapconcat #'identity
                                 (sort prompts
                                       (lambda (a b)
                                         (let ((length-a (length a))
                                               (length-b (length b)))
                                           (if (= length-a length-b)
                                               (string< a b)
                                             (> (length a) (length b))))))
                                 "\\|")
                      "\\)"))))
      ;; Validate ALL regexps
      (haskell-lite-prompt-validate-regexps)
      ;; Collect all user defined input prompts
      (dolist (prompt (append haskell-lite-prompt-input-regexps
                              (list haskell-lite-prompt-regexp
                                    haskell-lite-prompt-block-regexp
                                    haskell-lite-prompt-debug-regexp)))
        (cl-pushnew prompt input-prompts :test #'string=))
      ;; Set input and output prompt regexps from collected prompts
      (setq haskell-lite--prompt-calculated-input-regexp
            (funcall build-regexp input-prompts)))))

(defun haskell-lite-prompt-validate-regexps ()
  "Validate all user provided regexps for prompt.
Signals `user-error' if any of these vars contain invalid
regexps: `haskell-lite-prompt-regexp',
`haskell-lite-prompt-block-regexp',
`haskell-lite-prompt-debug-regexp',
`haskell-lite-prompt-input-regexps'"
  (dolist (symbol (list 'haskell-lite-prompt-input-regexps
                        'haskell-lite-prompt-regexp
                        'haskell-lite-prompt-block-regexp
                        'haskell-lite-prompt-debug-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (haskell-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in `%s'"
                    regexp symbol)))))

(defun haskell-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))

(defvar haskell-lite--interpreter nil)
(defvar haskell-lite--interpreter-args nil)

(defvar haskell-lite--block-prompt nil
  "Input block prompt for inferior haskell shell.
Do not set this variable directly, instead use
`haskell-lite-prompt-set-calculated-regexps'.")

(defvar haskell-lite--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior haskell shell.
Do not set this variable directly, instead use
`haskell-lite-prompt-set-calculated-regexps'.")


(defvar haskell-lite--first-prompt-received-output-buffer nil)
(defvar haskell-lite--first-prompt-received nil)
(defun haskell-lite-comint-watch-for-first-prompt-output-filter (output)
  "Run `haskell-lite-first-prompt-hook' when first prompt is found in OUTPUT."
  (when (not haskell-lite--first-prompt-received)
    (set (make-local-variable 'haskell-lite--first-prompt-received-output-buffer)
         (concat haskell-lite--first-prompt-received-output-buffer
                 (ansi-color-filter-apply output)))
    (when (haskell-lite-comint-end-of-output-p
           haskell-lite--first-prompt-received-output-buffer)
      (set (make-local-variable 'haskell-lite--first-prompt-received) t)
      (setq-local haskell-lite--first-prompt-received-output-buffer nil)
      (save-current-buffer
        (let ((inhibit-quit nil))
          (run-hooks 'haskell-lite-first-prompt-hook)))))
  output)

;;;###autoload
(defun haskell-lite-repl-show ()
  "Show the global repl."
  (interactive)
  (when (comint-check-proc haskell-ng-repl-buffer-name)
    (delete-other-windows)
    (with-current-buffer haskell-ng-repl-buffer-name
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun haskell-lite-comint-end-of-output-p (output)
  "Return if OUTPUT ends with input prompt return the input prompt."
  (let ((start
         (string-match
          ;; XXX: It seems on macOS an extra carriage return is attached
          ;; at the end of output, this handles that too.
          (concat
           "\r?\n?"
           ;; Remove initial caret from calculated regexp
           (replace-regexp-in-string
            (rx string-start ?^) ""
            haskell-lite--prompt-calculated-input-regexp)
           (rx eos))
          output)))
    (when start (substring output start))))

(defun haskell-lite-output-filter (string)
  "Filter used in `haskell-lite-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`haskell-lite-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   haskell-lite-output-filter-buffer
   (concat haskell-lite-output-filter-buffer string))
  (when (haskell-lite-comint-end-of-output-p
         haskell-lite-output-filter-buffer)
    ;; Output ends when `haskell-lite-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq haskell-lite-output-filter-in-progress nil
          haskell-lite-output-filter-buffer
          (substring haskell-lite-output-filter-buffer
                     0 (match-beginning 0))))
  "")

(defun haskell-lite-repl-eval-sync (string)
  "Evaluate STRING in PROCESS, wait and return the output."
  (save-excursion
    (let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
          (comint-preoutput-filter-functions '(haskell-lite-output-filter))
        (haskell-lite-output-filter-in-progress t)
        (inhibit-quit t))
      (or
       (with-local-quit
         (haskell-lite-repl-input string)
         (haskell-lite-repl-wait-for-output)
         (haskell-lite-repl-get-output))
       (with-current-buffer buffer)
         (comint-interrupt-subjob)))))

(defun haskell-lite-repl-input (string)
  "Send STRING to a repl, via the PROCESS buffer."
  (let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
	  (process (get-buffer-process buffer)))
      (with-current-buffer buffer)
      (goto-char (process-mark process))
      (insert string)
      (comint-send-input)))

(defun haskell-lite-repl-wait-for-output ()
  "Wait until output arrives from the repl.
Note: this is only safe when waiting for the result of a single
statement (not large blocks of code)."
  (let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
         (process (get-buffer-process buffer)))
  (save-excursion
    (with-current-buffer buffer)
    (while (progn
             (goto-char comint-last-input-end)
             (not (and (re-search-forward comint-prompt-regexp nil t)
                       (goto-char (match-beginning 0)))))
      (accept-process-output process)))))

(defun haskell-lite-repl-get-output ()
  "Get output from the repl."
  (let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
         (process (get-buffer-process buffer)))
        (save-excursion
        (with-current-buffer buffer)
                (goto-char (process-mark process))
                (forward-line 0)
                (backward-char)
                (buffer-substring comint-last-input-end (point)))))

(defun haskell-lite-repl-eval-region ()
  "Send the current region to PROCESS, wait and return the result."
  (haskell-lite-repl-eval-sync
   (buffer-substring-no-properties (region-beginning) (region-end))))

;;;###autoload
(defun haskell-lite-repl-overlay ()
  "Send the region to the repl and return the answer via an overlay."
  (interactive)
  (haskell-lite--eval-overlay
   (format "%s" (substring-no-properties (haskell-lite-repl-eval-region)))
   (point)))

;; Overlay

(defun haskell-lite--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.

TYPE is a symbol put on the overlay category property.  It is
used to easily remove all overlays from a region with:

    (remove-overlays start end category TYPE)

PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'haskell-lite-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'haskell-lite--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun haskell-lite--delete-overlay (ov &rest _)
  "Safely delete overlay OV.

Never throws errors, and can be used in an overlay's
modification-hooks."
  (ignore-errors (delete-overlay ov)))

(cl-defun haskell-lite--make-result-overlay (value &rest props &key where duration (type 'result)
                                           (format (concat " " haskell-lite-eval-result-prefix "%s "))
                                           (prepend-face 'haskell-lite-result-overlay-face)
                                           &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.

VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.

Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it
failed.

This function takes some optional keyword arguments:

- If WHERE is a number or a marker, apply the overlay over the
  entire line at that place (defaulting to `point').  If it is a
  cons cell, the car and cdr determine the start and end of the
  overlay.

- DURATION takes the same possible values as the
  `haskell-lite-eval-result-duration' variable.

- TYPE is passed to `haskell-lite--make-overlay' (defaults to `result').

- FORMAT is a string passed to `format'.  It should have exactly
  one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on
the overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cddr props)))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((beg (if (consp where)
                        (car where)
                      (save-excursion
                        (backward-sexp 1)
                        (point))))
               (end (if (consp where)
                        (cdr where)
                      (line-end-position)))
               (display-string (format format value))
               (o nil))
          (remove-overlays beg end 'category type)
          (funcall (if haskell-lite-overlays-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face prepend-face
                   display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property 0 1 'cursor 0 display-string)
          (when (> (string-width display-string) (* 3 (window-width)))
            (setq display-string
                  (concat (substring display-string 0 (* 3 (window-width)))
                          "...\nResult truncated.")))
          ;; Create the result overlay.
          (setq o (apply #'haskell-lite--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase duration
            ((pred numberp) (run-at-time duration nil #'haskell-lite--delete-overlay o))
            (`command (if this-command
                          (add-hook 'pre-command-hook
                                    #'haskell-lite--remove-result-overlay
                                    nil 'local)
                        (haskell-lite--remove-result-overlay))))
          (let ((win (get-buffer-window buffer)))
            ;; Left edge is visible.
            (when (and win
                       (<= (window-start win) (point))
                       ;; In 24.3 `<=' is still a binary predicate.
                       (<= (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))

(defun haskell-lite--remove-result-overlay ()
  "Remove result overlay from current buffer.

This function also removes itself from `pre-command-hook'."
  (remove-hook 'pre-command-hook #'haskell-lite--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun haskell-lite--eval-overlay (value point)
  "Make overlay for VALUE at POINT."
  (haskell-lite--make-result-overlay (format "%S" value)
    :where point
    :duration haskell-lite-eval-result-duration)
  value)

;; result history

(defvar haskell-lite-repl-result-history nil
  "Repl result history for current session."
)

(defun haskell-lite-repl-result-save (string)
  "Save STRING to repl result history."
  (push string haskell-lite-repl-result-history)
  string)

(defun haskell-lite-repl-error (response)
  "Look for a compile warning or error in RESPONSE.
If there is one, pop that up in a buffer.
Return the remaining output, if any"
  (when (string-match "^\n<interactive>:[-0-9]+:[-0-9]+:" response)
    (haskell-lite-popup-error response))
  response
)

(defcustom haskell-lite-popup-errors
  t
  "Popup errors in a separate buffer."
  :type 'boolean
  :group 'haskell-lite)

(define-derived-mode haskell-lite-error-mode
  special-mode "Error"
  "Major mode for viewing Haskell compile errors.")

(defun haskell-lite-popup-error (response)
  "Pop up RESPONSE as an error."
  (if haskell-lite-popup-errors
      (let ((buf (get-buffer-create "*HS-Error*")))
        (pop-to-buffer buf nil t)
        (with-current-buffer buf
          (haskell-lite-error-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize response
                                'font-lock-face
                                'haskell-interactive-face-compile-error))
            (goto-char (point-min))
            (delete-blank-lines)
            (let ((start-comment (propertize "-- " 'font-lock-face 'font-lock-comment-delimiter-face)))
              (insert start-comment (propertize "Hit `q' to close this window.\n\n" 'font-lock-face 'font-lock-comment-face))
              (save-excursion
                (goto-char (point-max))
                (insert "\n" start-comment
                        (propertize "To disable popups, customize `haskell-interactive-popup-errors'.\n\n"
                                    'font-lock-face 'font-lock-comment-face)))))))
    (haskell-lite-mode-insert-error response)))

(defun haskell-lite-mode-insert-error (response)
  "Insert RESPONSE as an error message."
  (insert "\n"
          (haskell-lite-fontify-as-mode
           response
           'haskell-mode))
  (haskell-lite-prompt))

(defun haskell-lite-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

;;;autoload
(defun haskell-lite-prompt ()
  "Goto the current repl prompt."
  (interactive)
  (pop-to-buffer haskell-ng-repl-buffer-name)
  (goto-char (point-max)))

;;;###autoload
(defun haskell-lite-repl-restart ()
  "Restart the repl process."
  (interactive)
  (let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
         (process (get-buffer-process buffer)))
    (when process
      (with-current-buffer buffer
        (comint-kill-subjob))))
  (sleep-for 1)
  (haskell-ng-repl-run))

;;;autoload
(defun haskell-lite-repl-quit ()
  "Quit the repl buffer & kill the process."
  (interactive)
  (let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
         (process (get-buffer-process buffer)))
    (when process
      (with-current-buffer buffer
        (comint-kill-subjob)))
    (sleep-for 1)
    (kill-buffer buffer)))

(provide 'haskell-lite)
;;; haskell-lite.el ends here
