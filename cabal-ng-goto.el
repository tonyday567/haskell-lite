;;; cabal-ng-goto.el --- Cabal NG: functions for going places -*- lexical-binding: t -*-

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

(defun cabal-ng--node-section-p (n)
  "Predicate to check if treesit node N is a Cabal section."
  (member (treesit-node-type n)
          '("benchmark" "common" "executable" "flag" "library" "source_repository" "test_suite")))

(defun cabal-ng--node-field-p (n)
  "Predicate to check if treesit node N is a Cabal field."
  (equal (treesit-node-type n) "field"))

(defun cabal-ng--node-properties-p (n)
  (equal "properties" (treesit-node-type n)))

(defun cabal-ng--goto-pos (until-func sibling-func node-pos-func)
  "General function for implementing goto position.

Going to first parent satisfying UNTIL-FUNC, then goint to a
sibling (SIBLING-FUNC), and finally going a
position (NODE-POS-FUNC)."
  (when-let* ((current (treesit-node-at (point)))
              (parent (treesit-parent-until current until-func))
              (sibling (funcall sibling-func parent))
              (new-pos (funcall node-pos-func sibling)))
    (goto-char new-pos)))

;;;###autoload
(defun cabal-ng--end-of-defun (&optional arg)
  "Usable as `end-of-defun-function' in cabal-ng mode.

See `end-of-defun' for usage of ARG.

Unfortunately it's not possible to use `treesit-end-of-defun' due to how
the grammar is defined."
  (interactive)
  (when-let* ((cnt (if (null arg) 1 arg))
              (defun-pos (treesit-navigate-thing (point) cnt 'end 'defun))
              (parent (treesit-node-parent (treesit-node-at defun-pos)))
              (new-pos (treesit-node-end parent)))
    (goto-char new-pos)
    t))

;;;###autoload
(defun cabal-ng-goto-start-of-section ()
  "Go to the beginning of the current section."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-section-p #'identity #'treesit-node-start))

;;;###autoload
(defun cabal-ng-goto-end-of-section ()
  "Go to the end of the current section."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-section-p #'identity #'treesit-node-end))

;;;###autoload
(defalias 'cabal-ng-goto-next-section 'cabal-ng-goto-end-of-section
  "Go to the next section.")

;;;###autoload
(defun cabal-ng-goto-previous-section ()
  "Go to the previous section."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-section-p #'treesit-node-prev-sibling #'treesit-node-start))

;;;###autoload
(defun cabal-ng-goto-start-of-field ()
  "Go to the start of the current field."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-field-p #'identity #'treesit-node-start))

;;;###autoload
(defun cabal-ng-goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-field-p #'identity #'treesit-node-end))

;;;###autoload
(defun cabal-ng-goto-next-field ()
  "Go to the next field."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-field-p #'treesit-node-next-sibling #'treesit-node-start))

;;;###autoload
(defun cabal-ng-goto-previous-field ()
  "Go to the previous field."
  (interactive)
  (cabal-ng--goto-pos #'cabal-ng--node-field-p #'treesit-node-prev-sibling #'treesit-node-start))

(provide 'cabal-ng-goto)
;;; cabal-ng-goto.el ends here
