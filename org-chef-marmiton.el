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
  (replace-regexp-in-string "^[ 	\r\n]*\\(.*[^ 	\r\n]\\)[ 	\r\n]*$" "\\1" str))

(defun org-chef-marmiton-extract-dom-text (dom)
  "Embedded the operation santize of dom-text from a marmiton DOM."
  (org-chef-marmiton-sanitize (dom-texts dom)))

(defun org-chef-marmiton-extract-name (dom)
  "Get the name of a recipe from a marmiton DOM."
  (dom-text (car (dom-elements dom 'class "^main-title $"))))

(defun org-chef-marmiton-extract-current-ingredient (dom)
  "Extract element for the current ingredient from a marmiton DOM."
  (concat
   (org-chef-marmiton-extract-dom-text (dom-elements dom 'class "^recipe-ingredient-qt$"))
   " "
   (org-chef-marmiton-extract-dom-text (dom-elements dom 'class "^ingredient$"))))

(defun org-chef-marmiton-extract-ingredients (dom)
  "Get the ingredients for a recipe from a marmiton DOM."
  (mapcar 'org-chef-marmiton-extract-current-ingredient (dom-elements dom 'class "^recipe-ingredients__list__item$")))

(defun org-chef-marmiton-extract-directions (dom)
  "Get the directions for a recipe from a marmiton DOM."

  (mapcar (lambda (x) (org-chef-marmiton-extract-dom-text (cddr x)))
          (dom-elements dom 'class "^recipe-preparation__list__item$")))

(defun org-chef-marmiton-extract-prep-time (dom)
  "Get the preparation time for a recipe from a marmiton DOM."
  (org-chef-marmiton-extract-dom-text
   (dom-elements (dom-elements dom 'class "recipe-infos__timmings__preparation")
		 'class "^recipe-infos__timmings__value$")))

(defun org-chef-marmiton-extract-cook-time (dom)
  "Extract the cooking time for a recipe from a marmiton DOM."
  (org-chef-marmiton-extract-dom-text
   (dom-elements (dom-elements dom 'class "recipe-infos__timmings__cooking")
		 'class "^recipe-infos__timmings__value$")))


(defun org-chef-marmiton-extract-servings (dom)
  (dom-text (car (dom-elements dom 'class "^title-2 recipe-infos__quantity__value$"))))



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
  `((ingredients . ,(org-chef-marmiton-extract-ingredients dom))
    (name . ,(org-chef-marmiton-extract-name dom))
    (servings . ,(org-chef-marmiton-extract-servings dom))
    (prep-time . ,(org-chef-marmiton-extract-prep-time dom))
    (cook-time . ,(org-chef-marmiton-extract-cook-time dom))
    (ready-in . nil)
    (directions . ,(org-chef-marmiton-extract-directions dom))))


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
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (cons `(source-url . ,url) (org-chef-marmiton-from-dom dom)))))

(provide 'org-chef-marmiton)
;;; org-chef-marmiton.el ends here
