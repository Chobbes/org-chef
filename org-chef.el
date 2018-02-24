;;; org-chef.el --- A package for making a cookbook and managing recipes with org-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; Created: 2018
;; Version: 0.1
;; Keywords: convenience, abbrev, outlines
;; Package-Requires: (elquery)

;; Copyright 2018 Calvin Beck

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

;; org-chef is a package for managing recipes in org-mode. One of the
;; main features is that it can automatically extract recipes from
;; websites like allrecipes.com

;;; Code:


(defun chef-remove-empty-strings (lst)
  "Filter out any empty strings in a list of strings."
  (seq-filter (lambda (x) (/= (length x) 0)) lst))


(defun chef-all-recipes-extract-name (ast)
  "Get the name of a recipe from an allrecipes elquery ast"
  (car (mapcar 'elquery-text (elquery-$ "[itemprop=name]" ast))))


(defun chef-all-recipes-extract-ingredients (ast)
  "Get the ingredients for a recipe from an allrecipes elquery ast"
  (reverse (mapcar 'elquery-text (elquery-$ "[itemprop=ingredients]" ast))))

(defun chef-all-recipes-extract-servings (ast)
  "Get the number of servings a recipe makes from an allrecipes elquery ast"
  (car (mapcar (lambda (node) (elquery-prop node "content")) (elquery-$ "[itemprop=recipeYield]" ast))))


(defun chef-all-recipes-extract-prep-time (ast)
  "Get the amount of prep-time for a recipe from an allrecipes elquery ast"
  (mapcar (lambda (node) (mapcar 'elquery-text (elquery-children node))) (elquery-$ "[itemprop=prepTime]" ast)))


(defun chef-all-recipes-extract-cook-time (ast)
  "Get the amount of cook-time for a recipe from an allrecipes elquery ast"
  (mapcar (lambda (node) (mapcar 'elquery-text (elquery-children node))) (elquery-$ "[itemprop=cookTime]" ast)))


(defun chef-all-recipes-extract-ready-in (ast)
  "Get the total amount of time for a recipe from an allrecipes elquery ast"
  (mapcar (lambda (node) (mapcar 'elquery-text (elquery-children node))) (elquery-$ "[itemprop=totalTime]" ast)))


(defun chef-all-recipes-extract-directions (ast)
  "Get the directions for a recipe from an allrecipes elquery ast"
  (reverse (chef-remove-empty-strings (mapcar 'elquery-text (elquery-$ "[class=recipe-directions__list--item]" ast)))))


(defun chef-all-recipes-fetch (url)
  "Given an allrecipes.com URL, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions
- source-url
"
  (with-current-buffer (url-retrieve-synchronously url)
    (let  ((ast (elquery-read-string (buffer-string))))
      `((name . ,(chef-all-recipes-extract-name ast))
        (ingredients . ,(chef-all-recipes-extract-ingredients ast))
        (servings . ,(chef-all-recipes-extract-servings ast))
        (prep-time . ,(chef-all-recipes-extract-prep-time ast))
        (cook-time . ,(chef-all-recipes-extract-cook-time ast))
        (ready-in . ,(chef-all-recipes-extract-ready-in ast))
        (directions . ,(chef-all-recipes-extract-directions ast))
        (source-url . ,url)))))




(defun chef-insert-org-list (lst &optional bullet)
  (mapcar (lambda (x) (progn (insert-string (format "%s" x))
                             (org-cycle)
                             (org-ctrl-c-minus)
                             (if bullet (org-cycle-list-bullet bullet))
                             (org-return)))
          lst))


(defun chef-recipe-insert-org (recipe)
  (org-insert-heading)
  (insert-string (cdr (assoc 'name recipe)))
  (org-return)
  (org-set-property "source-url" (cdr (assoc 'source-url recipe)))
  (org-set-property "servings" (cdr (assoc 'servings recipe)))
  (org-set-property "prep-time" (format "%s" (cdr (assoc 'prep-time recipe))))
  (org-set-property "cook-time" (format "%s" (cdr (assoc 'cook-time recipe))))
  (org-set-property "ready-in" (format "%s" (cdr (assoc 'ready-in recipe))))
  (org-insert-subheading t)
  (insert-string "Ingredients")
  (org-return)
  (org-return)
  (insert-org-list (cdr (assoc 'ingredients recipe)))
  (org-return)
  (org-return)
  (org-insert-heading)
  (insert-string "Directions")
  (org-return)
  (org-return)
  (insert-org-list (cdr (assoc 'directions recipe)) "1."))


(defun chef-recipe-org-string (recipe)
  (with-temp-buffer (org-mode)
                    (chef-recipe-insert-org recipe)
                    (buffer-string)))


(defun chef-insert-recipe ()
  (interactive)
  (chef-recipe-insert-org (chef-all-recipes-fetch (read-string "Recipe URL: "))))


(defun chef-get-recipe-from-url ()
  (chef-recipe-org-string (chef-all-recipes-fetch (read-string "Recipe URL: "))))



(provide 'org-chef)
;;; org-chef.el ends here
