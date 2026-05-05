;;; haskell-ng-repl-mode.el --- A mode for Haskell REPL interaction -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; Commentary:
;;; Code:

(require 'comint)
(require 'projectile)

(defvar haskell-ng-repl-buffer-name "*haskell-ng-repl*"
  "Name of the buffer to use for the `haskell-ng-repl-run' comint instance.")

(defvar haskell-ng-repl-prompt-regexp (rx (seq bol "ghci" "> "))
  "Prompt for Haskell REPL.")

;;;###autoload
(defun haskell-ng-repl-run (&optional and-go)
  "Run an inferior instance of Haskell REPL process AND-GO."
  (interactive)
  (let* ((buffer (get-buffer-create haskell-ng-repl-buffer-name))
	 (proc-alive (comint-check-proc buffer))
	 (process (get-buffer-process buffer)))
    (unless proc-alive
      (with-current-buffer buffer
	    (setq default-directory (projectile-project-root))
	    (make-comint-in-buffer "HsRepl" buffer "cabal" nil "repl")
	    (haskell-ng-repl-mode)))
    (when (and buffer and-go)
	  (pop-to-buffer buffer)
	  (goto-char (point-max)))))

(defun haskell-ng-repl--send-string (text &optional and-go)
  "Send `TEXT' to the inferior Haskell REPL process"
  (if-let* ((buffer (get-buffer haskell-ng-repl-buffer-name))
	    (process (get-buffer-process buffer)))
      (progn
	(comint-send-string process (concat text "\n"))
	(when and-go
	  (pop-to-buffer buffer)
	  (goto-char (point-max))))
    (warn "No Haskell REPL process is running")))

;;;###autoload
(defun haskell-ng-repl-send-region (start end)
  "Send the current region to the inferior Haskell REPL process."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (deactivate-mark)
    (haskell-ng-repl--send-string (format ":{\n%s\n:}" text))))

;;;###autoload
(defun haskell-ng-repl-send-region-and-go (start end)
  "Send the current region to the inferior Haskell REPL process, and switch."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (deactivate-mark)
    (haskell-ng-repl--send-string (format ":{\n%s\n:}" text) t)))

;;;###autoload
(defun haskell-ng-repl-load-buffer ()
  "Load the current buffer to the inferior Haskell REPL process."
  ;; TODO check if the file needs saving first
  (interactive)
  (let* ((fn (buffer-file-name))
	 (repl-cmd (concat ":l " fn)))
    (haskell-ng-repl--send-string repl-cmd)))

;;;###autoload
(defun haskell-ng-repl-load-buffer-and-go ()
  "Load the current buffer to the inferior Haskell REPL process, and switch."
  ;; TODO check if the file needs saving first
  (interactive)
  (let* ((fn (buffer-file-name))
	 (repl-cmd (concat ":l " fn)))
    (haskell-ng-repl--send-string repl-cmd t)))

;;;###autoload
(define-derived-mode haskell-ng-repl-mode comint-mode "Haskell NG Repl"
  "Major mode for interacting with an inferior Haskell process (Haskell REPL)."
  :group 'haskell-ng-repl

  (setq comint-prompt-regexp haskell-ng-repl-prompt-regexp))

(defun haskell-ng-repl--initialize ()
  "Helper function to initialize Haskell REPL."
  (setq comint-process-echoes t)
  (setq comint-prompt-read-only t)
  (setq comint-use-prompt-regexp t))

(add-hook 'haskell-ng-repl-mode-hook #'haskell-ng-repl--initialize)

;; TODO
;; - completion
;; - fontlock
;; - send expression/buffer
;; - load file
;; - handle sessions/targets

(provide 'haskell-ng-repl-mode)
;;; haskell-ng-repl-mode.el ends here
