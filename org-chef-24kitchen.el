;;; org-chef-24kitchen.el --- org-chef 24kitchen fetching.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Folkert van der Beek

;; Author:  Folkert van der Beek <folkertvanderbeek@gmail.com>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2019

;; Copyright 2019 Folkert van der Beek

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

;; Functions for fetching information from 24kitchen.nl.

;;; Code:

(require 'org-chef-utils)
(require 'dom)

(defun org-chef-24kitchen-extract-name (dom)
  "Get the name of a recipe from an 24kitchen DOM."
  (dom-text (dom-children (dom-by-class dom "^page-title$"))))

(defun org-chef-24kitchen-extract-ingredients (dom)
  "Get the ingredients for a recipe from an 24kitchen DOM."
  (org-chef-remove-empty-strings (mapcar #'(lambda (node) (string-trim (replace-regexp-in-string "[[:space:]\n]+" " " (dom-texts node)))) (dom-by-class dom "^recipe-component$"))))

(defun org-chef-24kitchen-extract-servings (dom)
  "Get the number of servings for a recipe from an 24kitchen DOM."
  (string-trim (dom-text (dom-by-class dom "^portion-amount$"))))

(defun org-chef-24kitchen-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an 24kitchen DOM."
  (string-trim (dom-text (dom-by-class dom "^cooking-time$"))))

(defun org-chef-24kitchen-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an 24kitchen DOM."
  (string-trim (dom-text (dom-by-class dom "^total-time$"))))

(defun org-chef-24kitchen-extract-directions (dom)
  "Get the directions for a recipe from an 24kitchen DOM."
  (org-chef-remove-empty-strings (mapcar #'(lambda (node) (string-trim (dom-text node))) (dom-children (cdr (dom-by-class dom  "^recipe-body$"))))))

(defun org-chef-24kitchen-fetch (url)
  "Given an 24kitchen.nl URL, retrieve the recipe information.

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

      `((name . ,(org-chef-24kitchen-extract-name dom))
        (ingredients . ,(org-chef-24kitchen-extract-ingredients dom))
        (servings . ,(org-chef-24kitchen-extract-servings dom))
        (prep-time . "")
        (cook-time . ,(org-chef-24kitchen-extract-cook-time dom))
        (ready-in . ,(org-chef-24kitchen-extract-ready-in dom))
        (directions . ,(org-chef-24kitchen-extract-directions dom))
        (source-url . ,url)))))

(provide 'org-chef-24kitchen)
;;; org-chef-24kitchen.el ends here
