;;; ob-haskell-lite.el --- Org-babel functions for Haskell evaluation -*- lexical-binding: t; -*-

;; Author: Tony Day
;; Keywords: languages
;; Homepage: https://github.com/tonyday567/haskell-lite
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (haskell-lite "0.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the ed warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; Commentary:
;; non-empty

;;; Code:

(require 'ob)
;;(require 'ob-ref)
;;(require 'ob-comint)
;;(require 'ob-eval)

(require 'haskell-ng-mode)

(add-to-list 'org-babel-tangle-lang-exts '("haskell-ng" . "hs"))

(defun org-babel-variable-assignments:haskell-ng (params)
  "Return Haskell representing the block's variables contained in PARAMS."
  (mapconcat 'identity
             (mapcar (lambda (pair) (format "%s=%s"
                            (car pair)
                            (ob-haskell-lite-var-to-haskell-ng (cdr pair))))
          (org-babel--get-vars params))
             "\n"))

(defun ob-haskell-lite-var-to-haskell-ng (var)
  "Convert VAR into a string of implicit let assignments.
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'ob-haskell-lite-var-to-haskell-ng var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defvar ob-haskell-lite-eoe "ob-haskell-lite-eoe")

(defun org-babel-execute:haskell-ng (body params)
  "Execute haskell code (BODY) and org-babel parameters (PARAMS) with org-babel."
  (message "executing Haskell source code block")
  (unless (org-babel-comint-buffer-livep haskell-ng-repl-buffer-name)
    (haskell-ng-repl-run))
  (let* ((result-type (cdr (assq :result-type params)))
         (vars (org-babel-variable-assignments:haskell-ng params))
         ;; FIXME: ignoring the :session parameter as haskell-ng-repl-buffer-name is hardcoded
         (session haskell-ng-repl-buffer-name)
         (full-body (concat body "\n" vars))
         (eoe (concat "putStrLn \"" ob-haskell-lite-eoe "\"\n")))
    (pcase result-type
      (`output
       (mapconcat
	#'identity
	(butlast
	 (split-string
	  (mapconcat
	   #'org-trim
	   (org-babel-comint-with-output
	       (session ob-haskell-lite-eoe t full-body)
             (insert (org-babel-chomp full-body) "\n" eoe)
             (comint-send-input nil t))
	   "\n") "[\r\n]")) "\n"))
      (`value
       (mapconcat
	#'identity
	(butlast
	 (split-string
	  (mapconcat
	   #'org-trim
           (progn
           (org-babel-comint-with-output
	       (session ob-haskell-lite-eoe t full-body)
             (insert (org-babel-chomp full-body) "\n" "_VALUE=it" "\n" eoe)
             (comint-send-input nil t))
           (org-babel-comint-with-output
	       (session ob-haskell-lite-eoe t full-body)
             (insert "_VALUE" "\n" eoe)
             (comint-send-input nil t)))
	   "\n") "[\r\n]")) "\n")))))

(provide 'ob-haskell-lite)

;;; ob-haskell-lite.el ends here
