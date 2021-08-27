;;; org-chef-fine-cooking.el --- Functions for fetching recipes from finecooking.com.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Calvin Beck, Erik Hetzner

;; Author:  Erik Hetzner <egh@e6h.org>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2019

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

;; Functions for fetching information from finecooking.com.

;;; Code:

(require 'org-chef-utils)
(require 'dom)

(defun org-chef-fine-cooking-extract-name (dom)
  "Get the name of a recipe from an finecooking DOM."
  (dom-text (dom-by-class dom "^recipe__title")))

(defun org-chef-fine-cooking-extract-ingredients (dom)
  "Get the ingredients for a recipe from an finecooking DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts n)))
          (org-chef-dom-children (dom-child-by-tag (dom-by-class dom "^recipe__ingredients$") 'ul))))

(defun org-chef-fine-cooking-extract-servings (dom)
  "Get the number of servings for a recipe from an finecooking DOM."
  (dom-text (dom-child-by-tag (dom-by-class dom "^recipe__yield$") 'p)))

(defun org-chef-fine-cooking-extract-directions (dom)
  "Get the directions for a recipe from an finecooking DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts n)))
          (org-chef-dom-children (dom-child-by-tag (dom-by-class dom "^recipe__preparation$") 'ul))))

(defun org-chef-fine-cooking-fetch (url)
  "Given a finecooking.com URL, retrieve the recipe information.

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
    `((name . ,(org-chef-fine-cooking-extract-name dom))
      (ingredients . ,(org-chef-fine-cooking-extract-ingredients dom))
      (servings . ,(org-chef-fine-cooking-extract-servings dom))
      (prep-time . nil)
      (cook-time . nil)
      (ready-in . nil)
      (directions . ,(org-chef-fine-cooking-extract-directions dom))
      (source-url . ,url))))

(provide 'org-chef-fine-cooking)
;;; org-chef-fine-cooking.el ends here
