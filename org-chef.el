;;; org-chef.el --- Cookbook and recipe management with org-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Calvin Beck

;; Author:  Calvin Beck <hobbes@ualberta.ca>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2018
;; Version: 0.1.3
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
(require 'org-chef-24kitchen)
(require 'org-chef-all-recipes)
(require 'org-chef-json-ld)
(require 'org-chef-genius-kitchen)
(require 'org-chef-simply-recipes)
(require 'org-chef-martha-stewart)
(require 'org-chef-cultures-for-health)
(require 'org-chef-marmiton)
(require 'org-chef-serious-eats)
(require 'org-chef-reluctant-gourmet)
(require 'org-chef-chef-koch)
(require 'org-chef-steamy-kitchen)
(require 'org-chef-nytimes)
(require 'org-chef-saveur)
(require 'org-chef-xiachufang)
(require 'org-chef-wordpress)
(require 'org-chef-taste)
(require 'org-chef-bbc-food)
(require 'subr-x)
(require 'url-parse)


(defvar org-chef-fetch-workaround
  (and
   ;; Note: For build sans gnutls, `libgnutls-version' is -1.
   (>= libgnutls-version 30603)
   (version<= emacs-version "26.2")
   't)
  "Inspired by https://github.com/magit/ghub/blob/7d59937d7782d0062216130a4d059b45e8396f82/ghub.el#L452

See https://github.com/magit/ghub/issues/81 and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
for more information.")

(defvar org-chef-fetch-table
  (let ((ht (make-hash-table :test 'equal)))
    (prog1 ht
      (puthash "24kitchen.nl"          'org-chef-24kitchen-fetch           ht)
      (puthash "allrecipes.com"        'org-chef-all-recipes-fetch         ht)
      (puthash "geniuskitchen.com"     'org-chef-genius-kitchen-fetch      ht)
      (puthash "simplyrecipes.com"     'org-chef-simply-recipes-fetch      ht)
      (puthash "marthastewart.com"     'org-chef-martha-stewart-fetch      ht)
      (puthash "culturesforhealth.com" 'org-chef-cultures-for-health-fetch ht)
      (puthash "marmiton.org"          'org-chef-marmiton-fetch            ht)
      (puthash "seriouseats.com"       'org-chef-serious-eats-fetch        ht)
      (puthash "reluctantgourmet.com"  'org-chef-reluctant-gourmet-fetch   ht)
      (puthash "chefkoch.de"           'org-chef-chef-koch-fetch           ht)
      (puthash "steamykitchen.com"     'org-chef-steamy-kitchen-fetch      ht)
      (puthash "nytimes.com"           'org-chef-nytimes-fetch             ht)
      (puthash "saveur.com"            'org-chef-saveur-fetch              ht)
      (puthash "xiachufang.com"        'org-chef-xiachufang-fetch          ht)
      (puthash "finecooking.com"       'org-chef-fine-cooking-fetch        ht)
      (puthash "taste.com.au"          'org-chef-taste-fetch               ht)
      (puthash "bbc.co.uk"             'org-chef-bbc-food-fetch            ht)))
  "Dispatch table associating a host URL with a fetch function.")

(defcustom org-chef-prefer-json-ld nil
  "Prefer JSON-LD extractor over custom extractor. This is for testing the JSON-LD functionality."
  :type 'boolean)


(defun org-chef-supported-sites ()
  "Host URLs of supported recipe sites."
  (hash-table-keys org-chef-fetch-table))


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
  (org-chef-insert-org-list (cdr (assoc 'ingredients recipe)))
  (org-return)
  (org-return)
  (org-insert-heading)
  (insert "Directions")
  (org-return)
  (org-return)
  (org-chef-insert-org-list (cdr (assoc 'directions recipe)) "1."))


(defun org-chef-recipe-org-string (recipe)
  "Get an ‘org-mode’ heading string for a RECIPE."
  (with-temp-buffer (org-mode)
                    (org-chef-recipe-insert-org recipe)
                    (buffer-string)))


(defun org-chef-fetch-recipe-specific-url (URL)
  "Look up a recipe based on a specific URL."
  (when-let* ((host (url-host (url-generic-parse-url URL)))
              (fetch (gethash host org-chef-fetch-table)))
    (funcall fetch URL)))


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


(defun org-chef-get-recipe-from-url ()
  "Prompt for a recipe URL, and return the ‘org-mode’ string."
  (let ((URL (read-string "Recipe URL: ")))
    (org-chef-recipe-org-string (org-chef-fetch-recipe URL))))


(provide 'org-chef)
;;; org-chef.el ends here
