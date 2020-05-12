
;;; org-chef-bbc-food.el --- Functions for fetching recipes from .de.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from bbc.co.uk/food/
(require 'org-chef-utils)
(require 'dom)

(defun org-chef-bbc-food-extract-name (dom)
  "Get the name of a recipe from a  DOM."
  (dom-text (dom-by-class dom "content-title__text") ))


(defun org-chef-bbc-food-extract-ingredients (dom)
  "Get the ingredients for a recipe from a  DOM."
  (mapcar 'dom-texts (dom-by-tag (dom-by-class dom "recipe-ingredient-list") 'li)))


(defun org-chef-bbc-food-extract-servings (dom)
  "Get the number of servings for a recipe from a  DOM."
  (dom-text (dom-by-class dom "recipe-metadata__serving")))

(defun org-chef-bbc-food-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from a  DOM."
  (dom-text (dom-by-class dom "recipe-metadata__prep-time")))

(defun org-chef-bbc-food-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from a  DOM."
(dom-text (dom-by-class dom "recipe-metadata__cook-time")))

(defun org-chef-bbc-food-extract-ready-in (dom)
  "Get the total amount of time for a recipe from a  DOM."
  (dom-text (dom-by-class dom "recipe-metadata__prep-time")))

(defun org-chef-bbc-food-extract-directions (dom)
  "Get the directions for a recipe from a  DOM."
  (mapcar 'dom-texts (dom-by-tag (dom-by-class dom "recipe-method__list") 'li)))


(defun org-chef-bbc-food-from-dom (dom)
  "Given a .de DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let* ((prep-time (org-chef-bbc-food-extract-prep-time dom))
         (cook-time (org-chef-bbc-food-extract-cook-time dom))
         (total-time (org-chef-bbc-food-extract-ready-in dom)))

    `((ingredients . ,(org-chef-bbc-food-extract-ingredients dom))
      (name . ,(org-chef-bbc-food-extract-name dom))
      (servings . ,(org-chef-bbc-food-extract-servings dom))
      (prep-time . ,prep-time)
      (cook-time . ,cook-time)
      (ready-in . ,total-time)
      (directions . ,(org-chef-bbc-food-extract-directions dom)))))


(defun org-chef-bbc-food-fetch (url)
  "Given a .de URL, retrieve the recipe information.

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
      (cons `(source-url . ,url) (org-chef-bbc-food-from-dom dom)))))


(provide 'org-chef-bbc-food)
;;; org-chef-bbc-food.el ends here
