;;; org-chef-json-ld.el --- org-chef json-ld fetching.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Erik Hetzner

;; Author:  Erik Hetzner <egh@e6h.org>
;; URL: https://github.com/Chobbes/org-chef
;; Created: 2020

;; Copyright (C) 2020 Erik Hetzner

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

;; Functions for fetching information from sites supporting json-ld.
;; https://developers.google.com/search/docs/data-types/recipe#structured-data-type-definitions
;;; Code:


(require 'org-chef-utils)
(require 'dom)
(require 'json)
(require 'cl-seq)

(defcustom org-chef-json-ld-debug nil
  "Debug JSON-LD parsing. Surfaces errors in JSON parsing."
  :type 'boolean)

(defun org-chef-json-ld-extract-directions (nodes)
  "Get the directions for a recipe from a list of json NODES."
  (cond ((stringp nodes)
         (list nodes))
        ((sequencep nodes)
         (mapcar (lambda (n)
                   (cond ((stringp n) n)
                         (t (cdr (assq 'text n)))))
                 nodes))
        (t (list "Unknown"))))

(defun org-chef-json-ld-pluralize (count name)
  "Pluralize the NAME based on the value of COUNT."
  (cond ((eq count 0)
         nil)
        ((eq count 1)
         (format "%d %s" count name))
        (t
         (format "%d %ss" count name))))

(defun org-chef-json-ld-parse-duration (duration)
  "Parse a ISO 8601 DURATION and return an English language string, or Unknown if it could not be parsed."
  (if (or (null duration)
          (not (string-match "^P0?D?T\\([0-9]+H\\)?\\([0-9]+M\\)?$" duration)))
    "Unknown"
    (let* ((hours-raw (match-string 1 duration))
           (minutes-raw (match-string 2 duration))
           (hours (and hours-raw (string-to-number (substring hours-raw 0 -1))))
           (minutes (and minutes-raw (string-to-number (substring minutes-raw 0 -1))))
           (hours-string (and hours (org-chef-json-ld-pluralize hours "hour")))
           (minutes-string (and minutes (org-chef-json-ld-pluralize minutes "minute"))))
      (string-join (cl-delete-if #'null (list hours-string minutes-string)) ", "))))

(defun org-chef-json-ld-clean-json (json)
  "Clean JSON string of common errors."
  (let* ((cleaned-1 (replace-regexp-in-string "\n" " " json))
         (cleaned-2 (replace-regexp-in-string "\r" " " cleaned-1))
         (cleaned-3 (replace-regexp-in-string "\t" " " cleaned-2)))
    cleaned-3))

(defun org-chef-json-ld-extract-recipe (json)
  "Find the Recipe LD in JSON and return the hash. Return nil if not found."
  (cond ((and (json-alist-p json)
              (let ((type (cdr (assq '@type json))))
                (and (stringp type)
                     (string= type "Recipe"))))
         json)
        ((and (json-alist-p json)
              (assq '@graph json)
              (sequencep (cdr (assq '@graph json))))
         (cl-some #'org-chef-json-ld-extract-recipe (cdr (assq '@graph json))))
        ((and (not (json-alist-p json))
              (sequencep json))
         (cl-some #'org-chef-json-ld-extract-recipe json))
        (t nil)))

(defun org-chef-json-ld-safe-read-from-string (str)
  "Safely parse STR as json.

Like `json-read-from-string', but catch errors and return nil if
`org-chef-json-ld-debug' is not t."
  (if org-chef-json-ld-debug
      (json-read-from-string str)
    (ignore-errors
      (json-read-from-string str))))
    
(defun org-chef-json-ld-extract-json-ld (dom)
  "Extract a list of the json-ld elements in DOM."
  (let* ((json-lds (dom-elements dom 'type "^application/ld\\+json$"))
         (json-lds-raw (mapcar #'dom-text json-lds))
         (json-lds-cleaned (mapcar #'org-chef-json-ld-clean-json json-lds-raw))
         (json-lds (cl-delete-if #'null (mapcar #'org-chef-json-ld-safe-read-from-string json-lds-cleaned))))
    json-lds))

(defun org-chef-json-ld-fetch (url)
  "Given an URL, retrieve the recipe information.


- name
- ingredients
- servings
- prep-time
- cook-time
- ready-in
- directions
- source-url"
  (with-current-buffer (org-chef-url-retrieve-synchronously url)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (json-lds (org-chef-json-ld-extract-json-ld dom))
           (recipe (org-chef-json-ld-extract-recipe json-lds)))
      (if (null recipe)
          nil
        `((name . ,(cdr (assq 'name recipe)))
          (ingredients . ,(cdr (assq 'recipeIngredient recipe)))
          (servings . ,(format "%s" (cdr (assq 'recipeYield recipe))))
          (ready-in . ,(org-chef-json-ld-parse-duration (cdr (assq 'totalTime recipe))))
          (prep-time . ,(org-chef-json-ld-parse-duration (cdr (assq 'prepTime recipe))))
          (cook-time . ,(org-chef-json-ld-parse-duration (cdr (assq 'cookTime recipe))))
          (directions . ,(org-chef-json-ld-extract-directions (cdr (assq 'recipeInstructions recipe))))
          (source-url . ,url))))))

(provide 'org-chef-json-ld)
