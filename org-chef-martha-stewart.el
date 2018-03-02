;;; org-chef-martha-stewart.el --- org-chef marthastewart fetching.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from marthastewart.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-martha-stewart-extract-name (dom)
  "Get the name of a recipe from an marthastewart DOM."
  (string-trim (dom-text (car (dom-by-class dom "^page-title$")))))


(defun org-chef-martha-stewart-extract-ingredients (dom)
  "Get the ingredients for a recipe from an marthastewart DOM."
  (mapcar #'(lambda (n) (string-trim (dom-text n)))
          (dom-by-class dom "^component-text$")))


(defun org-chef-martha-stewart-extract-servings (dom)
  "Get the number of servings for a recipe from an marthastewart DOM."
  (string-trim (dom-text (car (dom-by-class dom "servings-data$")))))


(defun org-chef-martha-stewart-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an marthastewart DOM."
  (string-trim (dom-text (car (dom-by-class dom "prep-time-data$")))))


(defun org-chef-martha-stewart-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an marthastewart DOM."
  nil)


(defun org-chef-martha-stewart-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an marthastewart DOM."
  (string-trim (dom-text (car (dom-by-class dom "total-time-data$")))))


(defun org-chef-martha-stewart-extract-directions (dom)
  "Get the directions for a recipe from an marthastewart DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts (dom-children n))))
          (dom-by-class dom "^directions-item-text$")))


(defun org-chef-martha-stewart-fetch (url)
  "Given an marthastewart.com URL, retrieve the recipe information.

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

      `((name . ,(org-chef-martha-stewart-extract-name dom))
        (ingredients . ,(org-chef-martha-stewart-extract-ingredients dom))
        (servings . ,(org-chef-martha-stewart-extract-servings dom))
        (prep-time . ,(org-chef-martha-stewart-extract-prep-time dom))
        (cook-time . ,(org-chef-martha-stewart-extract-cook-time dom))
        (ready-in . ,(org-chef-martha-stewart-extract-ready-in dom))
        (directions . ,(org-chef-martha-stewart-extract-directions dom))
        (source-url . ,url)))))


(provide 'org-chef-martha-stewart)
;;; org-chef-martha-stewart.el ends here
