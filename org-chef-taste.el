;;; org-chef-taste.el --- org-chef taste.com.au fetching -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Calvin Beck, Peter Hardy

;; Author: Peter Hardy <peter@hardy.dropbear.id.au>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2019

;; Copyright 2019 Peter Hardy

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

;; Functions for fetching information from taste.com.au.

;;; Code:

(require 'org-chef-utils)
(require 'cl-seq)
(require 'dom)

(defun org-chef-taste-extract-cooking-infos (dom info)
  "Get INFO from recipe-cooking-infos in a taste.com.au DOM."
  (let ((i (cl-position info (mapcar #'(lambda (n) (string-trim (dom-text n))) (dom-by-tag (dom-by-class dom "^recipe-cooking-infos$") 'li)) :test 'equal)))
    (if i
        (dom-text (nth i (dom-by-tag (dom-by-class dom "^recipe-cooking-infos$") 'b)))
      "")))

(defun org-chef-taste-extract-name (dom)
  "Get the name of a recipe from a taste.com.au DOM."
  (dom-text (dom-by-tag dom 'h1)))

(defun org-chef-taste-extract-ingredients (dom)
  "Get the ingredients for a recipe from a taste.com.au DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts n)))
          (dom-by-class dom "^ingredient-description$")))

(defun org-chef-taste-extract-servings (dom)
  "Get the servings for a recipe from a taste.com.au DOM."
  (let ((servings (org-chef-taste-extract-cooking-infos dom "Servings")))
    (if (string-equal servings "")
        (org-chef-taste-extract-cooking-infos dom "Makes")
      servings)))

(defun org-chef-taste-extract-prep-time (dom)
  "Get the prep time for a recipe from a taste.com.au DOM."
  (org-chef-taste-extract-cooking-infos dom "Prep"))

(defun org-chef-taste-extract-cook-time (dom)
  "Get the prep time for a recipe from a taste.com.au DOM."
  (org-chef-taste-extract-cooking-infos dom "Cook"))

(defun org-chef-taste-extract-directions (dom)
  "Get the directions for a recipe from a taste.com.au DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts n)))
          (dom-by-class dom "^recipe-method-step-content$")))

(defun org-chef-taste-fetch (url)
  "Given a taste.com.au URL, retrieve the recipe information.

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

      `((name . ,(org-chef-taste-extract-name dom))
        (ingredients . ,(org-chef-taste-extract-ingredients dom))
        (servings . ,(org-chef-taste-extract-servings dom))
        (prep-time . ,(org-chef-taste-extract-prep-time dom))
        (cook-time . ,(org-chef-taste-extract-cook-time dom))
        (ready-in . "")
        (directions . ,(org-chef-taste-extract-directions dom))
        (source-url . ,url)))))

(provide 'org-chef-taste)
;;; org-chef-taste.el ends here
