;;; org-chef-nytimes.el --- Functions for fetching recipes from nytimes.com.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck/Asilata Bapat

;; Author:  Calvin Beck <hobbes@ualberta.ca>/Asilata Bapat <asilata@alum.mit.edu>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018

;; Copyright 2018 Calvin Beck/Asilata Bapat

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

;; Functions for fetching information from nytimes.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-nytimes-extract-name (dom)
  "Get the name of a recipe from an nytimes DOM."
  (string-trim (dom-text (car (dom-by-class dom "recipe-title")))))


(defun org-chef-nytimes-ingredient-to-string (dom)
  "Extract string representation of a single ingredient from an nytimes DOM."
  (replace-regexp-in-string
   " \\([,\.]\\)" "\\1"
   (string-join (org-chef-remove-empty-strings (mapcar 'string-trim (dom-strings dom))) " ")))

(defun org-chef-nytimes-extract-ingredients (dom)
  "Get the ingredients for a recipe from an nytimes DOM."
  (mapcar 'org-chef-nytimes-ingredient-to-string
          (dom-elements dom 'itemprop "recipeIngredient")))


(defun org-chef-nytimes-extract-servings (dom)
  "Get the number of servings for a recipe from an nytimes DOM."
  (replace-regexp-in-string " servings" "" (dom-text (car (dom-elements dom 'itemprop "recipeYield")))))


(defun org-chef-nytimes-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an nytimes DOM."
  "")


(defun org-chef-nytimes-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an nytimes DOM."
  "")


(defun org-chef-nytimes-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an nytimes DOM."
  (dom-texts (dom-by-class (cadr (dom-by-tag (dom-by-class dom "recipe-time-yield") 'li)) "recipe-yield-value")))


(defun org-chef-nytimes-extract-directions (dom)
  "Get the directions for a recipe from an nytimes DOM."
  (mapcar 'dom-texts (dom-by-tag (dom-elements dom 'itemprop "recipeInstructions") 'li)))


(defun org-chef-nytimes-from-dom (dom)
  "Given an nytimes.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let* ((prep-time (org-chef-nytimes-extract-prep-time dom))
         (cook-time (org-chef-nytimes-extract-cook-time dom))
         (total-time (org-chef-nytimes-extract-ready-in dom)))

    `((ingredients . ,(org-chef-nytimes-extract-ingredients dom))
      (name . ,(org-chef-nytimes-extract-name dom))
      (servings . ,(org-chef-nytimes-extract-servings dom))
      (prep-time . ,prep-time)
      (cook-time . ,cook-time)
      (ready-in . ,total-time)
      (directions . ,(org-chef-nytimes-extract-directions dom)))))


(defun org-chef-nytimes-fetch (url)
  "Given an nytimes.com URL, retrieve the recipe information.

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
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (cons `(source-url . ,url) (org-chef-nytimes-from-dom dom)))))


(provide 'org-chef-nytimes)
;;; org-chef-nytimes.el ends here
