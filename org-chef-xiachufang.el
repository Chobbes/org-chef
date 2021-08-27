;;; org-chef-xiachufang.el --- Functions for fetching recipes from xiachufang.com.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck/Sébastien Le Maguer

;; Author:  Calvin Beck <hobbes@ualberta.ca>/Sébastien Le Maguer <seb.lemaguer@gmail.com>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018

;; Copyright 2019 Yiu Fung Cheong

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

;; Functions for fetching information from xiachufang.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)

(defun org-chef-xiachufang-sanitize (str)
  "Sanitize the STR by removing beginning/trailing spaces extracted from a marmiton dom."
  (replace-regexp-in-string "^[ 	\r\n]*\\(.*[^ 	\r\n]\\)[ 	\r\n]*$" "\\1" str))

(defun org-chef-xiachufang-extract-name (dom)
  "Get the name of a recipe from a xiachufang DOM."
  (org-chef-xiachufang-sanitize (dom-text (car (dom-elements dom 'class "^page-title$")))))


(defun org-chef-xiachufang-extract-ingredients (dom)
  "Get the ingredients for a recipe from a xiachufang DOM."
  (mapcar #'(lambda (n)
              (replace-regexp-in-string "\n" " " (org-chef-xiachufang-sanitize (dom-texts n))))
          (dom-elements dom 'itemprop "^recipeIngredient$")))


(defun org-chef-xiachufang-extract-directions (dom)
  "Get the directions for a recipe from a xiachufang DOM."
  (mapcar #'(lambda (n)
              (replace-regexp-in-string "\n" " " (org-chef-xiachufang-sanitize (dom-texts n))))
          (dom-elements dom 'itemprop "^recipeInstructions$")))


(defun org-chef-xiachufang-extract-prep-time (dom)
  "Get the preparation time for a recipe from a xiachufang DOM."
  "")


(defun org-chef-xiachufang-extract-cook-time (dom)
  "Extract the cooking time for a recipe from a xiachufang DOM."
  "")


(defun org-chef-xiachufang-extract-servings (dom)
  "")


(defun org-chef-xiachufang-from-dom (dom)
  "Given a xiachufang.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  `((ingredients . ,(org-chef-xiachufang-extract-ingredients dom))
    (name . ,(org-chef-xiachufang-extract-name dom))
    (servings . ,(org-chef-xiachufang-extract-servings dom))
    (prep-time . ,(org-chef-xiachufang-extract-prep-time dom))
    (cook-time . ,(org-chef-xiachufang-extract-cook-time dom))
    (ready-in . nil)
    (directions . ,(org-chef-xiachufang-extract-directions dom))))


(defun org-chef-xiachufang-fetch (url)
  "Given a xiachufang.com URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-xiachufang-from-dom dom))))


(provide 'org-chef-xiachufang)
;;; org-chef-xiachufang.el ends here
