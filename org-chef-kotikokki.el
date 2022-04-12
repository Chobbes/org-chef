
;;; org-chef-bbc-good-food.el --- Functions for fetching recipes from bbcgoodfood.com  -*- lexical-binding: t; -*-

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

;; Functions for fetching information from www.bbcgoodfood.com
(require 'org-chef-utils)
(require 'org-chef-json-ld)

(defun org-chef-kotikokki-clean-direction (direction)
  "Removes <p> tags from a single direction string"
  (replace-regexp-in-string "</?p>" "" direction))

(defun org-chef-kotikokki-fetch (url)
  "Given a kotikokki.net URL, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- ready-in
- directions
- source-url"
  (let* ((result (org-chef-json-ld-fetch url))
         (directions (assoc 'directions result))
         (cleaned (mapcar #'org-chef-kotikokki-clean-direction (cdr directions))))
    (setf (cdr directions) cleaned)
    result))


(provide 'org-chef-kotikokki)
;;; org-chef-bbc-good-food.el ends here
