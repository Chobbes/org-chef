;;; org-chef-simply-recipes.el --- Functions for fetching recipes from simplyrecipes.com.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from simplyrecipes.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-simply-recipes-extract-name (dom)
  "Get the name of a recipe from an simplyrecipes DOM."
  (dom-text (car (dom-by-class dom "entry-title"))))


(defun org-chef-simply-recipes-extract-ingredients (dom)
  "Get the ingredients for a recipe from an simplyrecipes DOM."
  (mapcar 'dom-text (dom-by-class dom "^ingredient$")))


(defun org-chef-simply-recipes-extract-servings (dom)
  "Get the number of servings for a recipe from an simplyrecipes DOM."
  (let ((servings (dom-text (car (dom-by-class dom "^yield*")))))
    (when servings
      (substring servings 7))))


(defun org-chef-simply-recipes-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an simplyrecipes DOM."
  (dom-text (car (dom-by-class dom "preptime"))))


(defun org-chef-simply-recipes-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an simplyrecipes DOM."
  (let ((cook-time (dom-by-class dom "cooktime"))
        (other-time (dom-by-class dom "othertime")))
    (dom-text (car (if (null cook-time) other-time cook-time)))))


(defun org-chef-simply-recipes-extract-directions (dom)
  "Get the directions for a recipe from an simplyrecipes DOM."
  (mapcar #'dom-text (seq-filter (lambda (x) (not (stringp x)))
                                 (dom-children (car (dom-elements dom 'itemprop "^recipeInstructions$"))))))


(defun org-chef-simply-recipes-from-dom (dom)
  "Given a simplyrecipes.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let* ((prep-time (org-chef-simply-recipes-extract-prep-time dom))
         (cook-time (org-chef-simply-recipes-extract-cook-time dom)))

    `((ingredients . ,(org-chef-simply-recipes-extract-ingredients dom))
      (name . ,(org-chef-simply-recipes-extract-name dom)
            )
      (servings . ,(org-chef-simply-recipes-extract-servings dom))
      (prep-time . ,prep-time)
      (cook-time . ,cook-time)
      (ready-in . nil)
      (directions . ,(org-chef-simply-recipes-extract-directions dom)))))


(defun org-chef-simply-recipes-fetch (url)
  "Given a simplyrecipes.com URL, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions
- source-url"
  (with-current-buffer (org-chef-url-retrieve-synchronously url)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (cons `(source-url . ,url) (org-chef-simply-recipes-from-dom dom)))))


(provide 'org-chef-simply-recipes)
;;; org-chef-simply-recipes.el ends here
