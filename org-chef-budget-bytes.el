;;; org-chef-budget-bytes.el --- org-chef budgetbytes fetching.  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from budgetbytes.com.

;;; Code:


(defun org-chef-budget-bytes-extract-name (dom)
  "Get the name of a recipe from an budgetbytes DOM."
  (dom-text (car (dom-by-class dom "^wprm-recipe-name$"))))


(defun org-chef-budget-bytes-ingredient-to-string (dom)
  (let* ((amount (string-trim (dom-text (car (dom-by-class dom "wprm-recipe-ingredient-amount")))))
         (unit (string-trim (dom-text (car (dom-by-class dom "wprm-recipe-ingredient-unit")))))
         (name (string-trim (dom-text (car (dom-by-class dom "wprm-recipe-ingredient-name"))))))
    (format "%s %s %s" amount unit name)))


(defun org-chef-budget-bytes-extract-ingredients (dom)
  "Get the ingredients for a recipe from an budgetbytes DOM."
  (mapcar #'(lambda (n) (org-chef-budget-bytes-ingredient-to-string n))
          (dom-by-class dom "^wprm-recipe-ingredient$")))


(defun org-chef-budget-bytes-extract-servings (dom)
  "Get the number of servings for a recipe from an budgetbytes DOM."
  (dom-text (car (dom-by-class dom "wprm-recipe-servings$"))))


(defun org-chef-budget-bytes-time-to-string (dom)
  (let* ((hours (string-trim (dom-text (car (dom-by-class dom "hours$")))))
         (minutes (string-trim (dom-text (car (dom-by-class dom "minutes$")))))
         (hour-string (if (/= (length hours) 0) (format "%s hours " hours) ""))
         (minute-string (if (/= (length minutes) 0) (format "%s minutes" minutes) "")))
    (string-trim (concat hour-string minute-string))))


(defun org-chef-budget-bytes-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an budgetbytes DOM."
  (org-chef-budget-bytes-time-to-string (car (dom-by-class dom "wprm-recipe-prep-time-container"))))


(defun org-chef-budget-bytes-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an budgetbytes DOM."
  (org-chef-budget-bytes-time-to-string (car (dom-by-class dom "wprm-recipe-cook-time-container"))))


(defun org-chef-budget-bytes-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an budgetbytes DOM."
  (org-chef-budget-bytes-time-to-string (car (dom-by-class dom "wprm-recipe-total-time-container"))))


(defun org-chef-budget-bytes-extract-directions (dom)
  "Get the directions for a recipe from an budgetbytes DOM."
  (mapcar #'(lambda (n) (string-trim (dom-texts (dom-children n))))
          (dom-by-class dom "wprm-recipe-instruction-text")))


(defun org-chef-budget-bytes-fetch (url)
  "Given an budgetbytes.com URL, retrieve the recipe information.

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

      `((name . ,(org-chef-budget-bytes-extract-name dom))
        (ingredients . ,(org-chef-budget-bytes-extract-ingredients dom))
        (servings . ,(org-chef-budget-bytes-extract-servings dom))
        (prep-time . ,(org-chef-budget-bytes-extract-prep-time dom))
        (cook-time . ,(org-chef-budget-bytes-extract-cook-time dom))
        (ready-in . ,(org-chef-budget-bytes-extract-ready-in dom))
        (directions . ,(org-chef-budget-bytes-extract-directions dom))
        (source-url . ,url)))))


(provide 'org-chef-budget-bytes)
;;; org-chef-budget-bytes.el ends here
