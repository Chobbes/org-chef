;;; org-chef-chef-koch.el --- Functions for fetching recipes from chefkoch.de.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from chefkoch.de.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-chef-koch-extract-name (dom)
  "Get the name of a recipe from a chefkoch DOM."
  (dom-text (dom-by-tag (dom-by-class dom "recipe-header") 'h1)))


(defun org-chef-chef-koch-ingredient-to-string (dom)
  (let ((amount (replace-regexp-in-string "[ \\t\\n\\r]+" " " (string-trim (dom-texts (car (dom-by-tag dom 'td))))))
        (name (string-trim (dom-texts (cadr (dom-by-tag dom 'td))))))
    (format "%s %s" amount name)))


(defun org-chef-chef-koch-extract-ingredients (dom)
  "Get the ingredients for a recipe from a chefkoch DOM."
  (mapcar #'org-chef-chef-koch-ingredient-to-string (dom-by-tag (dom-by-class dom "ingredients") 'tr)))


(defun org-chef-chef-koch-extract-servings (dom)
  "Get the number of servings for a recipe from a chefkoch DOM."
  (dom-attr (dom-elements dom 'name "^portionen$") 'value))


(defun org-chef-chef-koch-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from a chefkoch DOM."
  (let ((str (replace-regexp-in-string "\n" " " (dom-texts (dom-by-id dom "^preparation-info$")))))
    (string-match "Arbeitszeit:[[:space:]]+ca. \\(?1:[^\.]*\\)\." str)
    (match-string 1 str)))


(defun org-chef-chef-koch-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from a chefkoch DOM."
  (let ((str (replace-regexp-in-string "\n" " " (dom-texts (dom-by-id dom "^preparation-info$")))))
    (string-match "Koch-/Backzeit:[[:space:]]+ca. \\(?1:[^\.]*\\)\." str)
    (match-string 1 str)))


(defun org-chef-chef-koch-extract-ready-in (dom)
  "Get the total amount of time for a recipe from a chefkoch DOM."
  "")


(defun org-chef-chef-koch-extract-directions (dom)
  "Get the directions for a recipe from a chefkoch DOM."
  (with-temp-buffer
    (insert (string-trim (dom-text (car (dom-by-tag (nth 3 (dom-by-tag dom 'article)) 'div)))))
    (delete-trailing-whitespace)
    (split-string (buffer-string) "\n")))


(defun org-chef-chef-koch-from-dom (dom)
  "Given a chefkoch.de DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let* ((prep-time (org-chef-chef-koch-extract-prep-time dom))
         (cook-time (org-chef-chef-koch-extract-cook-time dom))
         (total-time (org-chef-chef-koch-extract-ready-in dom)))

    `((ingredients . ,(org-chef-chef-koch-extract-ingredients dom))
      (name . ,(org-chef-chef-koch-extract-name dom))
      (servings . ,(org-chef-chef-koch-extract-servings dom))
      (prep-time . ,prep-time)
      (cook-time . ,cook-time)
      (ready-in . ,total-time)
      (directions . ,(org-chef-chef-koch-extract-directions dom)))))


(defun org-chef-chef-koch-fetch (url)
  "Given a chefkoch.de URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-chef-koch-from-dom dom))))


(provide 'org-chef-chef-koch)
;;; org-chef-chef-koch.el ends here
