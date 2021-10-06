;;; org-chef-edit.el --- Cookbook and recipe management with org-mode.  -*- lexical-binding: t; -*-

;; Copyright 2021 Alexander Huntley

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Provides commands to modify the number of servings in a recipe.

;;; Code:

(require 'cl-lib)
(require 'org-chef-utils)

(defun org-chef-edit--do-replace (multiplier)
  "Replaces all numbers (in a variety of formats) in the region,
multiplying them all by MULTIPLIER"
  (save-excursion
    (while (re-search-forward
            "\\([0-9]+\\)?\\([¼½¾⅐⅑⅒⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞]\\)\\|\\([0-9]+\\)/\\([0-9]+\\)\\|\\([0-9]+\\)" (mark) t)
      (cond
       ;; Unicode vulgar/mixed fractions
       ((match-string 2)
        (let* ((char (elt (match-string 2) 0))
               (frac (get-char-code-property char 'numeric-value))
               (int (if (match-string 1) (string-to-number (match-string 1)) 0))
               (original (+ int frac)))
          (replace-match (format "%.3g" (* multiplier original)))))
       ;; ASCII fractions
       ((match-string 3)
        (let ((original (/ (float (string-to-number (match-string 3)))
                           (float (string-to-number (match-string 4))))))
          (replace-match (format "%.3g" (* multiplier original)))))
       ;; simple decimals
       ((match-string 5)
        (replace-match
         (format "%.3g" (* multiplier (string-to-number
                                       (match-string 5))))))))))


(defun org-chef-edit--parse-servings (string)
  "Finds all integers in STRING, and computes their mean"
  (let ((ints (mapcar #'string-to-number (org-chef-regexp-matches "[0-9]+" string))))
    (/ (cl-reduce #'+ ints) (length ints))))


;;;###autoload
(defun org-chef-edit-servings (desired)
  "Changes the serving count of an org-chef recipe entry to DESIRED, also modifying
the quantities inside the ingredients list."
  (interactive "*sChange servings to: ")
  (save-excursion
    (condition-case nil
        (while (not (org-entry-get nil "servings"))
          (outline-up-heading 1))
      (error (error "Could not find recipe with \"servings\" property")))
    (outline-show-subtree)
    (save-excursion
      (let* ((servings (org-chef-edit--parse-servings
                        (org-entry-get nil "servings")))
             (desired (if (numberp desired) desired
                        (org-chef-edit--parse-servings desired)))
             (multiplier (/ (float desired) (float servings))))
        (outline-next-heading)
        (outline-mark-subtree)
        (next-line)
        (org-chef-edit--do-replace multiplier)))
    (org-entry-put nil "servings" (format "%s" desired))))


(provide 'org-chef-edit)
;;; org-chef-edit.el ends here
