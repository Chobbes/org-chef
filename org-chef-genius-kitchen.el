;;; org-chef-genius-kitchen.el --- Functions for fetching recipes from geniuskitchen.com.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from geniuskitchen.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-genius-kitchen-extract-name (dom)
  "Get the name of a recipe from an geniuskitchen DOM."
  (dom-texts (dom-by-class dom "^recipe-title$")))


(defun org-chef-genius-kitchen-extract-ingredients (dom)
  "Get the ingredients for a recipe from an geniuskitchen DOM."
  (mapcar #'dom-texts (dom-by-class dom "^recipe-ingredients__ingredient$")))


(defun org-chef-genius-kitchen-extract-servings (dom)
  "Get the number of servings for a recipe from an geniuskitchen DOM."
  (dom-texts (dom-by-class (car (dom-by-class dom "recipe-facts__yield")) "theme-color")))


(defun org-chef-genius-kitchen-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an geniuskitchen DOM."
  (dom-texts (cddddr (car (dom-by-class dom "recipe-facts__time")))))


(defun org-chef-genius-kitchen-extract-directions (dom)
  "Get the directions for a recipe from an geniuskitchen DOM."
  (let ((directions-list (dom-by-tag (car (dom-by-class dom "recipe-directions__step")) 'li)))
    (org-chef-remove-empty-strings
     (mapcar #'(lambda (n) (string-trim (dom-text n))) directions-list))))


(defun org-chef-genius-kitchen-fetch (url)
  "Given a geniuskitchen.com URL, retrieve the recipe information.

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
    (let  ((dom (libxml-parse-html-region (point-min) (point-max))))
      `((name . ,(org-chef-genius-kitchen-extract-name dom))
        (ingredients . ,(org-chef-genius-kitchen-extract-ingredients dom))
        (servings . ,(org-chef-genius-kitchen-extract-servings dom))
        (prep-time . nil)
        (cook-time . nil)
        (ready-in . ,(org-chef-genius-kitchen-extract-ready-in dom))
        (directions . ,(org-chef-genius-kitchen-extract-directions dom))
        (source-url . ,url)))))


(provide 'org-chef-genius-kitchen)
;;; org-chef-genius-kitchen.el ends here
