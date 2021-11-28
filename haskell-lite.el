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
(require 'haskell-mode)
(require 'haskell-hoogle)
(require 'haskell-commands)
(require 'haskell-string)
(require 'haskell-completions)
(require 'haskell-utils)
(require 'haskell-customize)
(require 'haskell-compile)
(require 'fd-haskell-comint)


;; Customize

(defgroup haskell-lite nil
  "Haskell, but lite"
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
If nil, apply `haskell-lite-result-overlay-face' to the entire overlay instead of
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


;;;###autoload
(defun haskell-lite-repl-start (&optional cmd)
  "Start the global haskell repl, in the nearest cabal directory and switch to it."
  (interactive)
  (setq default-directory (haskell-cabal-find-dir))
  (run-haskell-comint cmd nil t))

;;;###autoload
(defun haskell-lite-repl-kill (&optional process)
  "Kill the global haskell repl."
  (interactive)
  (let ((proc (or process (haskell-shell-get-process))))
    (when proc
      (with-current-buffer (process-buffer proc)
        (comint-kill-subjob)
        (kill-buffer)))))

;;;###autoload
(defun haskell-lite-repl-buffer-show ()
  "Show the global repl."
  (interactive)
  (when (comint-check-proc "*haskell*")
    (delete-other-windows)
    (with-current-buffer "*haskell*"
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

;;;###autoload
(defun haskell-lite-repl-load-file ()
  "Load the current buffer in the global repl."
  (interactive)
  (save-buffer)
  (haskell-shell-load-file (buffer-file-name)))

;;;###autoload
(defun haskell-lite-repl-region ()
  "Send the region to the repl."
  (interactive)
;;  (haskell-lite--send-string (buffer-substring (region-beginning) (region-end)))
;;  (message "%s" (buffer-substring (region-beginning) (region-end))))
  (message "%s"
           (haskell-shell-send-string-no-output
            (concat (buffer-substring-no-properties (region-beginning) (region-end)) "\n"))))

;;;###autoload
(defun haskell-lite-repl-overlay ()
  "Send the region to the repl and return the answer via an overlay."
  (interactive)
  (haskell-lite--eval-overlay
   (format "%s" (substring-no-properties (haskell-lite-repl-eval-region)))
   (point)))

(defun haskell-lite--send-string (s)
  (if (comint-check-proc "*haskell*")
      (let ((cs (haskell-lite--chunk-string 64 (concat s "\n"))))
        (mapcar (lambda (c) (comint-send-string "*haskell*" c)) cs))
    (error "no haskell process running")))

(defun haskell-lite--chunk-string (n s)
  "Split a string S into chunks of N characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (tidal-chunk-string n (substring s n))))))

;;;###autoload
(defun haskell-lite-repl-echo ()
  "Echo repl output in the minibuffer"
  (interactive)
  (add-hook 'comint-output-filter-functions #'(lambda (txt) (message "%s" txt)))
  (message "%s" "*haskell* is echoing"))

(defun haskell-lite--get-string ()
  (if (comint-check-proc "*haskell*")
      (with-current-buffer "*haskell*"
            (goto-char (process-mark (get-buffer-process (current-buffer))))
            (forward-line 0)
            (backward-char)
            (buffer-substring comint-last-input-end (point)))
    (error "no haskell process running")))

;;;###autoload
(defun haskell-lite-repl-input (string &optional process)
  "Send a string to a repl, via the process buffer."
  (interactive)
  (let ((process (or process (haskell-shell-get-process-or-error))))
    (with-current-buffer (process-buffer process)
      (goto-char (process-mark process))
      (insert string)
      (comint-send-input))))

(defun haskell-lite-repl-wait-for-output (process)
  "Wait until output arrives from a repl.
Note: this is only safe when waiting for the result of a single
statement (not large blocks of code)."
  (save-excursion
    (with-current-buffer (process-buffer process)
    (while (progn
             (goto-char comint-last-input-end)
             (not (and (re-search-forward comint-prompt-regexp nil t)
                       (goto-char (match-beginning 0)))))
      (accept-process-output process)))))

;;;###autoload
(defun haskell-lite-repl-eval-sync (string &optional process)
  "Evaluate STRING to PROCESS, wait, and return the output."
  (interactive)
  (save-excursion
    (let ((process (or process (haskell-shell-get-process-or-error)))
          (comint-preoutput-filter-functions '(haskell-shell-output-filter))
        (haskell-shell-output-filter-in-progress t)
        (inhibit-quit t))
      (or
       (with-local-quit
         (haskell-lite-repl-input string process)
         (haskell-lite-repl-wait-for-output process)
         (haskell-lite-repl-get-output process))
       (with-current-buffer (process-buffer process)
         (comint-interrupt-subjob))))))

(defun haskell-lite-repl-wait (string &optional process)
  "Evaluate STRING to PROCESS and wait for output."
  (save-excursion
    (let ((process (or process (haskell-shell-get-process-or-error)))
        (comint-preoutput-filter-functions '(haskell-shell-output-filter))
        (haskell-shell-output-filter-in-progress t)
        (inhibit-quit t))
      (or
       (with-local-quit
         (haskell-lite-repl-input string process)
         (accept-process-output process)
         (prog1
             haskell-shell-output-filter-buffer
           (setq haskell-shell-output-filter-buffer nil)))
       (with-current-buffer (process-buffer process)
         (comint-interrupt-subjob))))))

(defun haskell-lite-repl-get-output (&optional process)
  (let ((process (or process (haskell-shell-get-process-or-error))))
    (save-excursion
      (with-current-buffer (process-buffer process)
        (goto-char (process-mark process))
        (forward-line 0)
        (backward-char)
        (buffer-substring comint-last-input-end (point))))))

;;;###autoload
(defun haskell-lite-repl-eval-region (&optional process)
  "Send the current region to a repl, via the process buffer."
  (interactive)
  (haskell-lite-repl-eval-sync
   (buffer-substring-no-properties (region-beginning) (region-end))
   process))


;; Overlay

(defun haskell-lite--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.

TYPE is a symbol put on the overlay's category property.  It is
used to easily remove all overlays from a region with:

    (remove-overlays start end 'category TYPE)

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

(provide 'haskell-lite)
;;; haskell-lite.el ends here
