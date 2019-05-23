;;; org-chef-cultures-for-health.el --- Functions for fetching recipes from culturesforhealth.com.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from culturesforhealth.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-cultures-for-health-extract-name (dom)
  "Get the name of a recipe from an culturesforhealth DOM."
  (dom-text (car (dom-elements dom 'itemprop "^name$"))))


(defun org-chef-cultures-for-health-extract-ingredients (dom)
  "Get the ingredients for a recipe from an culturesforhealth DOM."
  (mapcar 'dom-texts (dom-elements dom 'itemprop "^ingredients$")))


(defun org-chef-cultures-for-health-extract-directions (dom)
  "Get the directions for a recipe from an culturesforhealth DOM."
  (mapcar #'dom-texts (org-chef-dom-children (cadr (org-chef-dom-children (car (dom-elements dom 'itemprop "^recipeInstructions$")))))))


(defun org-chef-cultures-for-health-from-dom (dom)
  "Given a culturesforhealth.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  `((ingredients . ,(org-chef-cultures-for-health-extract-ingredients dom))
    (name . ,(org-chef-cultures-for-health-extract-name dom)
          )
    (servings . nil)
    (prep-time . nil)
    (cook-time . nil)
    (ready-in . nil)
    (directions . ,(org-chef-cultures-for-health-extract-directions dom))))


(defun org-chef-cultures-for-health-fetch (url)
  "Given a culturesforhealth.com URL, retrieve the recipe information.

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
      (cons `(source-url . ,url) (org-chef-cultures-for-health-from-dom dom)))))


(provide 'org-chef-cultures-for-health)
;;; org-chef-cultures-for-health.el ends here
