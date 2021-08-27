;;; org-chef-saveur.el --- Functions for fetching recipes from saveur.com.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck/Derek Ellis

;; Author:  Calvin Beck <hobbes@ualberta.ca>/Derek Ellis <derek.ellis@gmail.com>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018

;; Copyright 2018 Calvin Beck/Derek Ellis

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

;; Functions for fetching information from saveur.com.

;;; Code:


(require 'org-chef-utils)
(require 'dom)

 (defun org-chef-saveur-chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                        (: (* (any " \t\n")) eos)))
                                ""
                                str))


(defun org-chef-saveur-extract-name (dom)
  "Get the name of a recipe from an saveur DOM."
  (string-trim (dom-text (car (dom-by-class dom "page-title")))))


(defun org-chef-saveur-extract-ingredients (dom)
  "Get the ingredients for a recipe from an saveur DOM."
  (remove ""
	  (mapcar 'org-chef-saveur-chomp
		  (split-string
		   (replace-regexp-in-string  " ⁄ " "/"
					      (string-join (mapcar #'dom-texts (dom-by-class dom "^ingredients$")))) "\n" ))))


(defun org-chef-saveur-extract-servings (dom)
  "Get the number of servings for a recipe from an saveur DOM."
  (replace-regexp-in-string "serves " ""
			    (dom-text (dom-by-tag (dom-by-class dom "yield") 'span))))


(defun org-chef-saveur-extract-prep-time (dom)
  "Get the amount of prep-time for a recipe from an saveur DOM."
  (org-chef-saveur-chomp
   (dom-text (dom-by-class dom "^prep-time$"))))


(defun org-chef-saveur-extract-cook-time (dom)
  "Get the amount of cook-time for a recipe from an saveur DOM."
  (org-chef-saveur-chomp
   (dom-text (dom-by-class dom "^cook-time$"))))


(defun org-chef-saveur-extract-ready-in (dom)
  "Get the total amount of time for a recipe from an saveur DOM."
  (string-trim (dom-texts (dom-by-class (cadr (dom-by-tag (dom-by-class dom "recipe-time-yield") 'li)) "recipe-yield-value"))))


(defun org-chef-saveur-extract-directions (dom)
  "Get the directions for a recipe from an saveur DOM."
  (remove ""
	  (mapcar 'org-chef-saveur-chomp
		  (split-string
		   (replace-regexp-in-string " ⁄ " "/"
					     (dom-texts
					      (dom-by-class dom "^instructions "))) "\n           \n  " ))))

(defun org-chef-saveur-from-dom (dom)
  "Given an saveur.com DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let* ((prep-time (org-chef-saveur-extract-prep-time dom))
         (cook-time (org-chef-saveur-extract-cook-time dom))
         (total-time (org-chef-saveur-extract-ready-in dom)))

    `((ingredients . ,(org-chef-saveur-extract-ingredients dom))
      (name . ,(org-chef-saveur-extract-name dom))
      (servings . ,(org-chef-saveur-extract-servings dom))
      (prep-time . ,prep-time)
      (cook-time . ,cook-time)
      (ready-in . ,total-time)
      (directions . ,(org-chef-saveur-extract-directions dom)))))


(defun org-chef-saveur-fetch (url)
  "Given an saveur.com URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-saveur-from-dom dom))))


(provide 'org-chef-saveur)
;;; org-chef-saveur.el ends here
