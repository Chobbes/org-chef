;;; org-chef.el --- Cookbook and recipe management with org-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018
;; Version: 0.1
;; Keywords: convenience, abbrev, outlines, org, food, recipes, cooking
;; Package-Requires: ((org "0") (emacs "24"))

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

;; org-chef is a package for managing recipes in org-mode.  One of the
;; main features is that it can automatically extract recipes from
;; websites like allrecipes.com

;;; Code:


(require 'org-chef-utils)
(require 'org-chef-all-recipes)
(require 'org-chef-genius-kitchen)
(require 'org-chef-simply-recipes)


(defun org-chef-recipe-insert-org (recipe)
  "Insert a RECIPE as an ‘org-mode’ heading."
  (org-insert-heading)
  (insert (cdr (assoc 'name recipe)))
  (org-return)
  (org-set-property "source-url" (cdr (assoc 'source-url recipe)))
  (org-set-property "servings" (cdr (assoc 'servings recipe)))
  (org-set-property "prep-time" (format "%s" (cdr (assoc 'prep-time recipe))))
  (org-set-property "cook-time" (format "%s" (cdr (assoc 'cook-time recipe))))
  (org-set-property "ready-in" (format "%s" (cdr (assoc 'ready-in recipe))))
  (org-insert-subheading t)
  (insert "Ingredients")
  (org-return)
  (org-return)
  (insert-org-list (cdr (assoc 'ingredients recipe)))
  (org-return)
  (org-return)
  (org-insert-heading)
  (insert "Directions")
  (org-return)
  (org-return)
  (insert-org-list (cdr (assoc 'directions recipe)) "1."))


(defun org-chef-recipe-org-string (recipe)
  "Get an ‘org-mode’ heading string for a RECIPE."
  (with-temp-buffer (org-mode)
                    (org-chef-recipe-insert-org recipe)
                    (buffer-string)))

(defun org-chef-match-url (BASE URL)
  "Match URL against a BASE url."
  (not (null (string-match-p (regexp-quote BASE) URL))))


(defun org-chef-fetch-recipe (URL)
  "Look up a recipe at a URL."
  (cond
   ((org-chef-match-url "allrecipes.com" URL) (org-chef-all-recipes-fetch URL))
   ((org-chef-match-url "geniuskitchen.com" URL) (org-chef-genius-kitchen-fetch URL))
   ((org-chef-match-url "simplyrecipes.com" URL) (org-chef-simply-recipes-fetch URL))))


(defun org-chef-insert-recipe ()
  "Prompt for a recipe URL, and then insert the recipe at point."
  (interactive)
  (let ((URL (read-string "Recipe URL: ")))
    (org-chef-recipe-insert-org (org-chef-fetch-recipe (read-string "Recipe URL: ")))))


(defun org-chef-get-recipe-from-url ()
  "Prompt for a recipe URL, and return the ‘org-mode’ string."
  (let ((URL (read-string "Recipe URL: ")))
    (org-chef-recipe-org-string (org-chef-fetch-recipe URL))))


(provide 'org-chef)
;;; org-chef.el ends here
