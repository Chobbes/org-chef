;;; org-chef-marmiton.el --- Functions for fetching recipes from marmiton.org.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck/Sébastien Le Maguer

;; Author:  Calvin Beck <hobbes@ualberta.ca>/Sébastien Le Maguer <seb.lemaguer@gmail.com>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018

;; Copyright 2018 Calvin Beck/Sébastien Le Maguer

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

;; Functions for fetching information from marmiton.org.

;;; Code:


(require 'org-chef-utils)
(require 'dom)


(defun org-chef-marmiton-sanitize (str)
  "Sanitize the STR by removing beginning/trailing spaces extracted from a marmiton dom."
  (replace-regexp-in-string "\n" " " (replace-regexp-in-string "^[ 	\r\n]*\\(.*[^ 	\r\n]\\)[ 	\r\n]*$" "\\1" str)))

(defun org-chef-marmiton-extract-dom-text (dom)
  "Embedded the operation santize of dom-text from a marmiton DOM."
  (org-chef-marmiton-sanitize (dom-texts dom)))

(defun org-chef-marmiton-extract-name (dom)
  "Get the name of a recipe from a marmiton DOM."
  (dom-text (dom-child-by-tag (dom-elements marmidom 'class "^main-title$") 'h1)))

(defun org-chef-marmiton-extract-ingredients (dom)
  "Get the ingredients for a recipe from a marmiton DOM."
  (mapcar 'org-chef-marmiton-extract-dom-text (dom-elements dom 'class "^card-ingredient-content$")))

(defun org-chef-marmiton-step-container-to-step (dom)
  "Take a step DOM and turn it into a string for the directions."
  (dom-text (dom-child-by-tag dom 'p)))

(defun org-chef-marmiton-extract-directions (dom)
  "Get the directions for a recipe from a marmiton DOM."
  (mapcar #'org-chef-marmiton-step-container-to-step
          (dom-elements marmidom 'class "^recipe-step-list__container$")))

(defun org-chef-marmiton-extract-time-details (dom)
  "Get the time details DOM from a marmiton recipe."
  (dom-elements dom 'class "time__details"))

(defun org-chef-marmiton-prep-time-from-details (dom)
  "Extract prep time from the time details DOM"
  (dom-text (cadddr (dom-children (cadr (dom-children dom))))))

(defun org-chef-marmiton-cook-time-from-details (dom)
  "Extract cook time from the time details DOM"
  (dom-text (cadddr (dom-children (cdr (cddddr (dom-children dom)))))))

(defun org-chef-marmiton-extract-ready-in (dom)
  "Extract the total time for a recipe from a marmiton DOM."
  (org-chef-marmiton-extract-dom-text
   (dom-child-by-tag
    (dom-elements (dom-elements marmidom 'class "recipe-preparation__time")
	          'class "^time__total$")
    'div)))

(defun org-chef-marmiton-extract-servings (dom)
  (dom-text (car (dom-elements dom 'class "^recipe-ingredients__qt-counter__value_container"))))
(dom-elements marmidom 'class "recipe-ingredients__qt-counter__value_container")
(defun org-chef-marmiton-from-dom (dom)
  "Given a marmiton.org DOM, retrieve the recipe information.

This returns an alist with the following keys:

- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions"
  (let
      ((time-details (org-chef-marmiton-extract-time-details dom)))
    `((ingredients . ,(org-chef-marmiton-extract-ingredients dom))
      (name . ,(org-chef-marmiton-extract-name dom))
      (servings . ,(org-chef-marmiton-extract-servings dom))
      (prep-time . ,(org-chef-marmiton-prep-time-from-details time-details))
      (cook-time . ,(org-chef-marmiton-cook-time-from-details time-details))
      (ready-in . ,(org-chef-marmiton-extract-ready-in dom))
      (directions . ,(org-chef-marmiton-extract-directions dom)))))


(defun org-chef-marmiton-fetch (url)
  "Given a marmiton.org URL, retrieve the recipe information.

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
    (cons `(source-url . ,url) (org-chef-marmiton-from-dom dom))))

(provide 'org-chef-marmiton)
;;; org-chef-marmiton.el ends here
