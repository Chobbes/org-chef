
;;; org-chef-recipetin-eats.el --- Functions for fetching recipes from recipetineats.com  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from www.recipetineats.com
(require 'org-chef-utils)
(require 'dom)


(defun org-chef-recipetin-eats-extract-name (dom)
  "Get the name of a recipe from a recipetin-eats DOM."
  (dom-text (car (dom-by-class dom "^wprm-recipe-name"))))


(defun org-chef-recipetin-eats-extract-ingredients (dom)
  "Get the ingredients for a recipe from a RecipeTin Eats DOM."
  (mapcar #'(lambda (n)
              (let ((notes (dom-text (dom-by-class n "wprm-recipe-ingredient-notes")))
                    (unit (dom-text (dom-by-class n "wprm-recipe-ingredient-unit"))))
                (concat (dom-text (dom-by-class n "wprm-recipe-ingredient-amount"))
                        " "
                        unit
                        (if (string= unit "") "" " ")
                        (dom-text (dom-by-class n "wprm-recipe-ingredient-name"))
                        (if (or (zerop (length notes))
                                (= (elt notes 0) ?,))
                            "" ", ")
                        notes)))
          (dom-by-class dom "^wprm-recipe-ingredient$")))


(defun org-chef-recipetin-eats-extract-servings (dom)
  "Get the number of servings for a recipe from a RecipeTin Eats DOM."
  (dom-attr (car (dom-by-class dom "wprm-recipe-container")) 'data-servings))


(defun org-chef-recipetin-eats-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from a RecipeTin Eats DOM."
  (let ((span (nth 1 (dom-children
                      (car (dom-by-class dom "wprm-recipe-prep-time-container"))))))
    (org-chef-join (dom-strings span) "")))


(defun org-chef-recipetin-eats-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from a RecipeTin Eats DOM."
  (let ((span (nth 1 (dom-children
                      (car (dom-by-class dom "wprm-recipe-cook-time-container"))))))
    (org-chef-join (dom-strings span) "")))


(defun org-chef-recipetin-eats-extract-ready-in (dom)
  "Get the total amount of time for a recipe from a RecipeTin Eats DOM."
  (let ((span (nth 1 (dom-children
                      (car (dom-by-class dom "wprm-recipe-total-time-container"))))))
    (org-chef-join (dom-strings span) "")))


(defun org-chef-recipetin-eats-extract-directions (dom)
  "Get the directions for a recipe from a RecipeTin Eats DOM."
  (org-chef-remove-empty-strings (mapcar #'(lambda (n) (string-trim (dom-texts n))) (dom-by-class dom  "^wprm-recipe-instruction$"))))


(defun org-chef-recipetin-eats-fetch (url)
  "Given a recipetineats.com URL, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions
- source-url"
  (let ((dom (org-chef-url-retrieve-dom url)))
    `((name . ,(org-chef-recipetin-eats-extract-name dom))
      (ingredients . ,(org-chef-recipetin-eats-extract-ingredients dom))
      (servings . ,(org-chef-recipetin-eats-extract-servings dom))
      (prep-time . ,(org-chef-recipetin-eats-extract-prep-time dom))
      (cook-time . ,(org-chef-recipetin-eats-extract-cook-time dom))
      (ready-in . ,(org-chef-recipetin-eats-extract-ready-in dom))
      (directions . ,(org-chef-recipetin-eats-extract-directions dom))
      (source-url . ,url))))


(provide 'org-chef-recipetin-eats)
;;; org-chef-recipetin-eats.el ends here
