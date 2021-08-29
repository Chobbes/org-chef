;;; org-chef-binging-with-babish.el --- Functions for fetching recipes from bingingwithbabish.com.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck
;; Copyright (C) 2021 Alexander Huntley

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; URL: https://github.comm/Chobbes/org-chef
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

;; Functions for fetching information from bingingwithbabish.com
;; More manual than most parsers, since the site does not have a
;; standard structure, and each page contains multiple recipes.
;; Hence, we ask the user to choose which recipes are wanted,
;; and we concatenate them together.
(require 'org-chef-utils)
(require 'dom)


(defun org-chef-babish-list-title (dom list)
  "Gets the title of a <ul> or <ol> by finding the preceding <p> tag"
  (let* ((parent (dom-parent dom list))
         (siblings (dom-children parent))
         (position (position list siblings))
         (previous (nth (1- position) siblings)))
    (unless (member (dom-tag previous) '("ul" "ol" "li"))
      (replace-regexp-in-string ":\\|^[\n ]*\\|[\n ]*$\\|\n" ""
                                (dom-texts previous)))))


(defun org-chef-babish-choose-from-lists (dom tag prompt)
  "Prompts the user to choose between all the lists of a given tag
 type ('ul or 'ol) in the DOM, and then concatenates their items."
  (let* ((ols (dom-by-tag dom tag))
         (alist (mapcar
                 (lambda (ol) (cons (org-chef-babish-list-title dom ol) ol))
                 ols))
         (chosen (completing-read-multiple prompt alist)))
    (mapcan (lambda (ol) (mapcar #'dom-texts (dom-children ol)))
            (mapcar (lambda (s) (cdr (assoc s alist))) chosen))))


(defun org-chef-binging-with-babish-extract-name (dom)
  "Get the name of a recipe from a  DOM."
  (replace-regexp-in-string
   "^[\n\r\t ]*\\|[\n\r\t ]*$" ""
   (dom-text (dom-by-class dom "^page-title$"))))


(defun org-chef-binging-with-babish-extract-ingredients (dom)
  "Get the ingredients for a recipe from a  DOM. Prompts user to select
desired <ul>s."
  (org-chef-babish-choose-from-lists
   (car (dom-by-class dom "entry-content")) 'ul
   "Select ingredient lists (comma-separated): "))


(defun org-chef-binging-with-babish-extract-directions (dom)
  "Get the directions for a recipe from a DOM. Prompts user to select
desired <ol>s."
  (org-chef-babish-choose-from-lists
   (car (dom-by-class dom "entry-content")) 'ol
   "Select instruction lists (comma-separated): "))


(defun org-chef-binging-with-babish-from-dom (dom)
  "Given a bingingwithbabish.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  `((ingredients . ,(org-chef-binging-with-babish-extract-ingredients dom))
    (name . ,(org-chef-binging-with-babish-extract-name dom))
    (servings . nil)
    (prep-time . nil)
    (cook-time . nil)
    (ready-in . nil)
    (directions . ,(org-chef-binging-with-babish-extract-directions dom))))


(defun org-chef-binging-with-babish-fetch (url)
  "Given a bingingwithbabish.com URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-binging-with-babish-from-dom dom))))


(provide 'org-chef-binging-with-babish)
;;; org-chef-binging-with-babish.el ends here
