;;; org-chef-basics-with-babish.el --- Functions for fetching recipes from basicswithbabish.co.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck
;; Copyright (C) 2021 Alexander Huntley

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

;; Functions for fetching information from basicswithbabish.co
;;
;; More manual than most parsers, since the site does not have a
;; standard structure, and each page contains multiple recipes.
;; Hence, we ask the user to choose which recipes are wanted,
;; and we concatenate them together.
;;
;; Reuses functions from org-chef-binging-with-babish
(require 'org-chef-utils)
(require 'org-chef-binging-with-babish)
(require 'dom)


(defun org-chef-basics-with-babish-extract-name (dom)
  "Get the name of a recipe from a  DOM."
  (capitalize (dom-text (dom-by-class dom "BlogItem-title"))))


(defun org-chef-basics-with-babish-extract-ingredients (dom)
  "Get the ingredients for a recipe from a  DOM. Prompts user to select
desired <ul>s."
  (org-chef-babish-choose-from-lists
   (car (dom-by-tag dom 'main)) 'ul
   "Select ingredient lists (comma-separated): "))


(defun org-chef-basics-with-babish-extract-directions (dom)
  "Get the directions for a recipe from a DOM. Prompts user to select
desired <ol>s."
  (org-chef-babish-choose-from-lists
   (car (dom-by-tag dom 'main)) 'ol
   "Select instruction lists (comma-separated): "))


(defun org-chef-basics-with-babish-from-dom (dom)
  "Given a basicswithbabish.co DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  `((ingredients . ,(org-chef-basics-with-babish-extract-ingredients dom))
    (name . ,(org-chef-basics-with-babish-extract-name dom))
    (servings . nil)
    (prep-time . nil)
    (cook-time . nil)
    (ready-in . nil)
    (directions . ,(org-chef-basics-with-babish-extract-directions dom))))


(defun org-chef-basics-with-babish-fetch (url)
  "Given a basicswithbabish.co URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-basics-with-babish-from-dom dom))))


(provide 'org-chef-basics-with-babish)
;;; org-chef-basics-with-babish.el ends here
