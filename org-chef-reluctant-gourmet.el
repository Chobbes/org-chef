;;; org-chef-reluctant-gourmet.el --- Functions for fetching recipes from reluctantgourmet.com.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from reluctantgourmet.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-reluctant-gourmet-extract-name (dom)
  "Get the name of a recipe from an reluctantgourmet DOM."
  (dom-text (car (dom-by-id dom "zlrecipe-title"))))


(defun org-chef-reluctant-gourmet-extract-ingredients (dom)
  "Get the ingredients for a recipe from an reluctantgourmet DOM."
  (mapcar #'(lambda (n) (string-trim (dom-text n))) (dom-by-class dom "^ingredient$")))


(defun org-chef-reluctant-gourmet-extract-servings (dom)
  "Get the number of servings for a recipe from an reluctantgourmet DOM."
  (dom-texts (car (dom-elements dom 'itemprop "^recipeYield$"))))


(defun org-chef-reluctant-gourmet-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an reluctantgourmet DOM."
  (dom-texts (car (dom-elements dom 'itemprop "^prepTime$"))))


(defun org-chef-reluctant-gourmet-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an reluctantgourmet DOM."
  (dom-texts (car (dom-elements dom 'itemprop "^cookTime$"))))


(defun org-chef-reluctant-gourmet-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an reluctantgourmet DOM."
  (dom-texts (car (dom-elements dom 'itemprop "^totalTime$"))))


(defun org-chef-reluctant-gourmet-extract-directions (dom)
  "Get the directions for a recipe from an reluctantgourmet DOM."
  (mapcar #'dom-text (seq-filter (lambda (x) (not (stringp x)))
                                 (dom-elements dom 'itemprop "^recipeInstructions$"))))


(defun org-chef-reluctant-gourmet-from-dom (dom)
  "Given a reluctantgourmet.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let* ((prep-time (org-chef-reluctant-gourmet-extract-prep-time dom))
         (cook-time (org-chef-reluctant-gourmet-extract-cook-time dom))
         (total-time (org-chef-reluctant-gourmet-extract-ready-in dom)))

    `((ingredients . ,(org-chef-reluctant-gourmet-extract-ingredients dom))
      (name . ,(org-chef-reluctant-gourmet-extract-name dom))
      (servings . ,(org-chef-reluctant-gourmet-extract-servings dom))
      (prep-time . ,prep-time)
      (cook-time . ,cook-time)
      (ready-in . ,total-time)
      (directions . ,(org-chef-reluctant-gourmet-extract-directions dom)))))


(defun org-chef-reluctant-gourmet-fetch (url)
  "Given a reluctantgourmet.com URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-reluctant-gourmet-from-dom dom))))


(provide 'org-chef-reluctant-gourmet)
;;; org-chef-reluctant-gourmet.el ends here
