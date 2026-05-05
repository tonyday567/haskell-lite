;;; cabal-ng-consult.el --- Cabal NG: consult integration -*- lexical-binding: t -*-

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

(require 'consult)
(require 'treesit)

(defun cabal-ng--section-to-string (section)
  "Convert a single SECTION node to a string."
  (let* ((captures (treesit-query-capture section "(_ type: (section_type) @type name: (section_name)? @name)"))
         (type (treesit-node-text (alist-get 'type captures)))
         (name (or (treesit-node-text (alist-get 'name captures)) ""))
         (section-pos (treesit-node-start section))
         (section-marker (move-marker (make-marker) (treesit-node-start section))))
    (propertize (format "%s %s" type name)
                'treesit-node section
                'section-pos section-marker)))

(defun cabal-ng--section-items ()
  "Fetch all sections as a list of strings ."
  (let ((section-nodes (treesit-query-capture (treesit-buffer-root-node 'cabal) "(cabal (sections (_)* @section))" nil nil t)))
    (mapcar #'cabal-ng--section-to-string section-nodes)))

(defun cabal-ng--section-action (item)
  "Go to the section referenced by ITEM."
  (when-let* ((new-pos (get-text-property 0 'section-pos item)))
    (goto-char new-pos)))

(defun cabal-ng--state ()
  "Create a state function for previewing sections."
  (let ((state (consult--jump-state)))
    (lambda (action cand)
      (when cand
        (let ((pos (get-text-property 0 'section-pos cand)))
          (funcall state action pos))))))

(defvar cabal-ng--source-section
  `(:name "Sections"
    :category location
    :narrow ?s
    :action ,#'cabal-ng--section-action
    :items ,#'cabal-ng--section-items
    :state ,#'cabal-ng--state)
  "Definition of source for Cabal sections.")

;;;###autoload
(defun cabal-ng-consult ()
  "Choose a Cabal construct and jump to it."
  (interactive)
  (consult--multi '(cabal-ng--source-section)
                  :sort nil))

(provide 'cabal-ng-consult)

;;; cabal-ng-consult.el ends here
