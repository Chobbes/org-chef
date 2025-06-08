;;; org-chef.el --- Cookbook and recipe management with org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018
;; Version: 0.1.3
;; Keywords: convenience, abbrev, outlines, org, food, recipes, cooking
;; Package-Requires: ((org "0") (emacs "24.3"))

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
(require 'org-chef-edit)

(require 'org-chef-json-ld)
(require 'org-chef-genius-kitchen)
(require 'org-chef-fine-cooking)
(require 'org-chef-wordpress)
(require 'org-chef-taste)
(require 'org-chef-bbc-good-food)
(require 'org-chef-jamie-oliver)
(require 'org-chef-recipetin-eats)
(require 'org-chef-binging-with-babish)
(require 'org-chef-basics-with-babish)
(require 'org-chef-kotikokki)


(defvar org-chef-fetch-workaround
  (and
   ;; Note: For build sans gnutls, `libgnutls-version' is -1.
   (>= libgnutls-version 30603)
   (version<= emacs-version "26.2")
   't)
  "Inspired by https://github.com/magit/ghub/blob/7d59937d7782d0062216130a4d059b45e8396f82/ghub.el#L452

See https://github.com/magit/ghub/issues/81 and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
for more information.")

(defcustom org-chef-prefer-json-ld t
  "Prefer JSON-LD extractor over custom extractor."
  :type 'boolean)

(defun org-chef-to-unordered-list (list)
  "Convert a LIST of strings into an org-element plain list"
  (if (null list)
      nil
    `(plain-list nil ,(mapcar #'(lambda (x) `(item (:bullet "-" :pre-blank 0) ,x)) list))))

(defun org-chef-to-ordered-list (list)
  "Convert a LIST of strings into an ordered org-element plain list"
    (if (null list)
      nil
      `(plain-list nil ,(mapcar #'(lambda (x) `(item (:bullet "1. " :pre-blank 0) ,x)) list))))

(defun org-chef-recipe-to-org-element (recipe)
  "Convert a RECIPE into an `org-element` AST."
  `(headline (:title ,(cdr (assoc 'name recipe)) :level 1)
             (property-drawer nil
                              ((node-property (:key "source-url" :value ,(cdr (assoc 'source-url recipe))))
                               (node-property (:key "servings"   :value ,(cdr (assoc 'servings recipe))))
                               (node-property (:key "prep-time"  :value ,(format "%s" (cdr (assoc 'prep-time recipe)))))
                               (node-property (:key "cook-time"  :value ,(format "%s" (cdr (assoc 'cook-time recipe)))))
                               (node-property (:key "ready-in"   :value ,(format "%s" (cdr (assoc 'ready-in recipe)))))))
             (headline (:title "Ingredients" :level 2 :pre-blank 1)
                       ,(org-chef-to-unordered-list (cdr (assoc 'ingredients recipe))))
             (headline (:title "Directions" :level 2 :pre-blank 1)
                       ,(org-chef-to-ordered-list (cdr (assoc 'directions recipe))))))

(defun org-chef-recipe-to-org (recipe)
  "Convert a RECIPE into `org-mode` test."
  (org-element-interpret-data (org-chef-recipe-to-org-element recipe)))

(defun org-chef-recipe-insert-org (recipe)
  "Insert a RECIPE as an ‘org-mode’ heading."
  (insert
   (org-chef-recipe-to-org recipe)))

(defun org-chef-recipe-org-string (recipe)
  "Get an ‘org-mode’ heading string for a RECIPE."
  (with-temp-buffer (org-mode)
                    (org-chef-recipe-insert-org recipe)
                    (buffer-string)))

(defun org-chef-match-url (BASE URL)
  "Match URL against a BASE url."
  (not (null (string-match-p (regexp-quote BASE) URL))))

(defun org-chef-fetch-recipe-specific-url (URL)
  "Look up a recipe based on a specific URL."
  (cond
   ((org-chef-match-url "geniuskitchen.com" URL) (org-chef-genius-kitchen-fetch URL))
   ((org-chef-match-url "finecooking.com" URL) (org-chef-fine-cooking-fetch URL))
   ((org-chef-match-url "taste.com.au" URL) (org-chef-taste-fetch URL))
   ((org-chef-match-url "bbcgoodfood.com" URL) (org-chef-bbc-good-food-fetch URL))
   ((org-chef-match-url "jamieoliver.com" URL) (org-chef-jamie-oliver-fetch URL))
   ((org-chef-match-url "recipetineats.com" URL) (org-chef-recipetin-eats-fetch URL))
   ((org-chef-match-url "basicswithbabish.co" URL) (org-chef-basics-with-babish-fetch URL))
   ((org-chef-match-url "andrew-rea-zfrt.squarespace.com" URL) (org-chef-basics-with-babish-fetch URL))
   ((org-chef-match-url "bingingwithbabish.com" URL) (org-chef-binging-with-babish-fetch URL))
   ((org-chef-match-url "andrew-rea-c8g3.squarespace.com" URL) (org-chef-binging-with-babish-fetch URL))
   ((org-chef-match-url "kotikokki.net" URL) (org-chef-kotikokki-fetch URL))
   (t nil)))


(defun org-chef-fetch-recipe (URL)
  "Look up a recipe at a URL."
  (or (and org-chef-prefer-json-ld
           (org-chef-json-ld-fetch URL))
      (org-chef-fetch-recipe-specific-url URL)
      (org-chef-wordpress-fetch URL)
      (and (not org-chef-prefer-json-ld)
           (org-chef-json-ld-fetch URL))))

;;;###autoload
(defun org-chef-insert-recipe (URL)
  "Prompt for a recipe URL, and then insert the recipe at point."
  (interactive "sRecipe URL: ")
  (org-chef-recipe-insert-org (org-chef-fetch-recipe URL)))

;;;###autoload
(defun org-chef-get-recipe-string-from-url (URL)
  "Prompt for a recipe URL, and return the ‘org-mode’ string."
  (org-chef-recipe-org-string (org-chef-fetch-recipe URL)))

;;;###autoload
(defun org-chef-get-recipe-from-url ()
  "Prompt for a recipe URL, and return the ‘org-mode’ string."
  (let ((URL (read-string "Recipe URL: ")))
    (org-chef-get-recipe-string-from-url URL)))

(provide 'org-chef)
;;; org-chef.el ends here
