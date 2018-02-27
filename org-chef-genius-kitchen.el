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
(require 'json)

(defun org-chef-genius-kitchen-extract-name (ast)
  "Get the name of a recipe from an geniuskitchen elquery AST."
  (cadddr (mapcar 'elquery-text (elquery-children (car (elquery-$ ".recipe-header" ast))))))


(defun org-chef-genius-kitchen-extract-ingredients (ast)
  "Get the ingredients for a recipe from an geniuskitchen elquery AST."
  (let ((ingredients (elquery-prop (car (elquery-$ "[name=ingredient]" ast)) "value")))
    (append (json-read-from-string ingredients) nil)))


(defun org-chef-genius-kitchen-extract-servings (ast)
  "Get the number of servings for a recipe from an geniuskitchen elquery AST."
  (elquery-text (car (elquery-$ ".count" (car (elquery-$ ".servings" ast))))))


(defun org-chef-genius-kitchen-extract-ready-in (ast)
  "Get the total amount of time for a recipe from an geniuskitchen elquery AST."
  (elquery-text (car (elquery-$ ".time" (car (elquery-$ ".recipe-facts" ast))))))


(defun org-chef-genius-kitchen-extract-directions (ast)
  "Get the directions for a recipe from an geniuskitchen elquery AST."
  (let ((directions-list (cadddr (elquery-children (car (elquery-$ ".directions-inner" ast))))))
    (org-chef-remove-empty-strings (mapcar 'elquery-text (elquery-children directions-list)))))


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
  (with-current-buffer (url-retrieve-synchronously url)
    (let  ((ast (elquery-read-string (buffer-string))))
      `((name . ,(org-chef-genius-kitchen-extract-name ast))
        (ingredients . ,(org-chef-genius-kitchen-extract-ingredients ast))
        (servings . ,(org-chef-genius-kitchen-extract-servings ast))
        (prep-time . nil)
        (cook-time . nil)
        (ready-in . ,(org-chef-genius-kitchen-extract-ready-in ast))
        (directions . ,(org-chef-genius-kitchen-extract-directions ast))
        (source-url . ,url)))))


(provide 'org-chef-genius-kitchen)
;;; org-chef-genius-kitchen.el ends here
