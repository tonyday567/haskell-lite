;;; cabal-ng-mode.el --- A mode for Cabal -*- lexical-binding: t -*-

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

(require 'treesit)
(require 'rx)

(defvar cabal-ng--treesit-font-lock-setting
  (when (treesit-available-p)
    (treesit-font-lock-rules
     :feature 'comment
     :language 'cabal
     '((comment) @font-lock-comment-face)

     :feature 'cabal-version
     :language 'cabal
     '((cabal_version _) @font-lock-constant-face)

     :feature 'field-name
     :language 'cabal
     '((field_name) @font-lock-keyword-face)

     :feature 'section-name
     :language 'cabal
     '((section_name) @font-lock-variable-name-face)))
  "Tree-sitter font-lock settings.")

(defvar cabal-ng--indent-rules
  `((cabal
     (no-node parent 0)))
  "Tree-sitter indent rules")

;;;###autoload
(define-derived-mode cabal-ng-mode fundamental-mode "Cabal NG Mode"
  "A modern mode for Cabal files"
  :group 'haskell

  (unless (treesit-ready-p 'cabal)
    (error "Tree-sitter for Cabal is not available"))

  (treesit-parser-create 'cabal)
  ;; set up treesit
  (setq-local treesit-font-lock-feature-list
              '((comment field-name section-name)
                (cabal-version)
                () ()))
  (setq-local treesit-font-lock-settings cabal-ng--treesit-font-lock-setting)
  ;; - treesit-simple-indent-rules
  ;; - treesit-defun-type-regexp
  ;; - treesit-defun-name-function
  ;; - treesit-simple-imenu-settings
  (treesit-major-mode-setup)
  (setq-local treesit-simple-indent-rules cabal-ng--indent-rules)

  ;; comment setup
  (setq-local comment-start "-"
	      comment-end ""
	      comment-add 1
	      comment-column 40
	      comment-padding " "
	      comment-start-skip (rx bol (zero-or-more space) "--" (zero-or-more space))
	      comment-end-skip (rx (zero-or-more space) (or (syntax comment-end) eol)))

  (setq-local indent-tabs-mode nil)

  ;; thing setup
  (setq-local treesit-thing-settings
              '((cabal (section "section_type")
                       (properties cabal-ng--node-properties-p)
                       (defun (or properties section)))))

  (setq-local beginning-of-defun-function 'treesit-beginning-of-defun)
  (setq-local end-of-defun-function 'cabal-ng--end-of-defun))

(defun cabal-ng--de-indent-line ()
  "Naive and simple function for de-indenting the current line."
  (interactive "*")
  (let* ((curr-ind (current-indentation))
         (rem (cl-rem curr-ind 2))
         (new-ind (max 0 (if (eq rem 0) (- curr-ind 4) (- curr-ind rem)))))
    (indent-line-to new-ind)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . cabal-ng-mode))

(require 'cabal-ng-goto)
(require 'cabal-ng-consult)

(provide 'cabal-ng-mode)
;;; cabal-ng-mode.el ends here
