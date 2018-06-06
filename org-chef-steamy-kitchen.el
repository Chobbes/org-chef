;;; org-chef-steamy-kitchen.el --- org-chef steamykitchen fetching.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from steamykitchen.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-steamy-kitchen-extract-name (dom)
  "Get the name of a recipe from an steamykitchen DOM."
  (dom-text (car (dom-elements dom 'itemprop "^name$"))))


(defun org-chef-steamy-kitchen-extract-ingredients (dom)
  "Get the ingredients for a recipe from an steamykitchen DOM."
  (mapcar 'dom-texts (dom-elements dom 'itemprop "^ingredients$")))


(defun org-chef-steamy-kitchen-extract-servings (dom)
  "Get the number of servings for a recipe from an steamykitchen DOM."
  (dom-text (car (dom-elements dom 'itemprop "^recipeYield$"))))


(defun org-chef-steamy-kitchen-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an steamykitchen DOM."
  (dom-text (dom-parent dom (car (dom-elements dom 'itemprop "^prepTime$")))))


(defun org-chef-steamy-kitchen-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an steamykitchen DOM."
  (dom-text (dom-parent dom (car (dom-elements dom 'itemprop "^cookTime$")))))


(defun org-chef-steamy-kitchen-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an steamykitchen DOM."
  nil)


(defun org-chef-steamy-kitchen-extract-directions (dom)
  "Get the directions for a recipe from an steamykitchen DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts n)))
          (org-chef-dom-children (car (dom-by-class dom "^instructions$")))))


(defun org-chef-steamy-kitchen-fetch (url)
  "Given an steamykitchen.com URL, retrieve the recipe information.

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
    (let  ((dom (libxml-parse-html-region (point-min) (point-max))))

      `((name . ,(org-chef-steamy-kitchen-extract-name dom))
        (ingredients . ,(org-chef-steamy-kitchen-extract-ingredients dom))
        (servings . ,(org-chef-steamy-kitchen-extract-servings dom))
        (prep-time . ,(org-chef-steamy-kitchen-extract-prep-time dom))
        (cook-time . ,(org-chef-steamy-kitchen-extract-cook-time dom))
        (ready-in . ,(org-chef-steamy-kitchen-extract-ready-in dom))
        (directions . ,(org-chef-steamy-kitchen-extract-directions dom))
        (source-url . ,url)))))


(provide 'org-chef-steamy-kitchen)
;;; org-chef-steamy-kitchen.el ends here
