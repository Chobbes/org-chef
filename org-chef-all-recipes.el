;;; org-chef-all-recipes.el --- org-chef allrecipes fetching.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018

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

;; Functions for fetching information from allrecipes.com.

;;; Code:


(defun org-chef-all-recipes-extract-name (ast)
  "Get the name of a recipe from an allrecipes elquery AST."
  (elquery-text (car (elquery-$ "[itemprop=name]" ast))))


(defun org-chef-all-recipes-extract-ingredients (ast)
  "Get the ingredients for a recipe from an allrecipes elquery AST."
  (reverse (mapcar 'elquery-text (elquery-$ "[itemprop=ingredients]" ast))))


(defun org-chef-all-recipes-extract-servings (ast)
  "Get the number of servings for a recipe from an allrecipes elquery AST."
  (car (mapcar (lambda (node) (elquery-prop node "content")) (elquery-$ "[itemprop=recipeYield]" ast))))


(defun org-chef-all-recipes-extract-prep-time (ast)
  "Get the amount of prep-time for a recipe from an allrecipes elquery AST."
  (mapcar (lambda (node) (mapcar 'elquery-text (elquery-children node))) (elquery-$ "[itemprop=prepTime]" ast)))


(defun org-chef-all-recipes-extract-cook-time (ast)
  "Get the amount of cook-time for a recipe from an allrecipes elquery AST."
  (mapcar (lambda (node) (mapcar 'elquery-text (elquery-children node))) (elquery-$ "[itemprop=cookTime]" ast)))


(defun org-chef-all-recipes-extract-ready-in (ast)
  "Get the total amount of time for a recipe from an allrecipes elquery AST."
  (mapcar (lambda (node) (mapcar 'elquery-text (elquery-children node))) (elquery-$ "[itemprop=totalTime]" ast)))


(defun org-chef-all-recipes-extract-directions (ast)
  "Get the directions for a recipe from an allrecipes elquery AST."
  (reverse (org-chef-remove-empty-strings (mapcar 'elquery-text (elquery-$ "[class=recipe-directions__list--item]" ast)))))


(defun org-chef-all-recipes-fetch (url)
  "Given an allrecipes.com URL, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions
- source-url"
  (with-current-buffer (url-retrieve-synchronously url)
    (let  ((ast (elquery-read-string (buffer-string))))
      `((name . ,(org-chef-all-recipes-extract-name ast))
        (ingredients . ,(org-chef-all-recipes-extract-ingredients ast))
        (servings . ,(org-chef-all-recipes-extract-servings ast))
        (prep-time . ,(org-chef-all-recipes-extract-prep-time ast))
        (cook-time . ,(org-chef-all-recipes-extract-cook-time ast))
        (ready-in . ,(org-chef-all-recipes-extract-ready-in ast))
        (directions . ,(org-chef-all-recipes-extract-directions ast))
        (source-url . ,url)))))


(provide 'org-chef-all-recipes)
;;; org-chef-all-recipes.el ends here
