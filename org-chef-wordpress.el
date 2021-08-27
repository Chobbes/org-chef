;;; org-chef-wordpress.el --- org-chef wordpress fetching.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from wordpress sites.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-wordpress-extract-name (dom)
  "Get the name of a recipe from an wordpress DOM."
  (dom-text (car (dom-by-class dom "^wprm-recipe-name"))))


(defun org-chef-wordpress-extract-ingredients (dom)
  "Get the ingredients for a recipe from an wordpress DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts n))) (dom-by-class dom "^wprm-recipe-ingredient$")))


(defun org-chef-wordpress-extract-servings (dom)
  "Get the number of servings for a recipe from an wordpress DOM."
  (dom-texts (car (dom-by-class dom "wprm-recipe-servings$"))))


(defun org-chef-wordpress-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an wordpress DOM."
  (dom-texts (car (dom-by-class dom "^wprm-recipe-time$"))))


(defun org-chef-wordpress-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an wordpress DOM."
  (dom-texts (cadr (dom-by-class dom "^wprm-recipe-time$"))))


(defun org-chef-wordpress-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an wordpress DOM."
  (dom-texts (caddr (dom-by-class dom "^wprm-recipe-time$"))))


(defun org-chef-wordpress-extract-directions (dom)
  "Get the directions for a recipe from an wordpress DOM."
  (org-chef-remove-empty-strings (mapcar #'(lambda (n) (string-trim (dom-texts n))) (dom-by-class dom  "^wprm-recipe-instruction$"))))


(defun org-chef-is-wordpress-p (dom)
  "Check if a DOM corresponds to a wordpress website."
  (dom-by-class dom "^wprm-recipe-container$"))


(defun org-chef-wordpress-fetch (url)
  "Given an wordpress URL, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions
- source-url

If this is not a wordpress site, then return nil."
  (let  ((dom (org-chef-url-retrieve-dom url)))
    (if (org-chef-is-wordpress-p dom)
        `((name . ,(org-chef-wordpress-extract-name dom))
          (ingredients . ,(org-chef-wordpress-extract-ingredients dom))
          (servings . ,(org-chef-wordpress-extract-servings dom))
          (prep-time . ,(org-chef-wordpress-extract-prep-time dom))
          (cook-time . ,(org-chef-wordpress-extract-cook-time dom))
          (ready-in . ,(org-chef-wordpress-extract-ready-in dom))
          (directions . ,(org-chef-wordpress-extract-directions dom))
          (source-url . ,url))
      nil)))


(provide 'org-chef-wordpress)
;;; org-chef-wordpress.el ends here
