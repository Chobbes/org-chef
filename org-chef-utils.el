;;; org-chef-utils.el --- Utilities for org-chef.    -*- lexical-binding: t; -*-

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

;; Utilitiy functions for org-chef.

;;; Code:

(require 'cl-macs)

(defun org-chef-remove-empty-strings (lst)
  "Filter out any empty strings in a list of strings (LST)."
  (seq-filter (lambda (x) (/= (length x) 0)) lst))


(defun org-chef-dom-children (dom)
  "Get the child nodes of the DOM, remove strings."
  (seq-filter (lambda (x) (not (stringp x)))
                                  (dom-children dom)))


(defun org-chef-insert-org-list (lst &optional bullet)
  "Insert LST as an ‘org-mode’ plain list.

The optional argument BULLET specifies which type of bullet point
should be used."
  (mapcar (lambda (x) (progn (insert (format "%s" x))
                             (org-cycle)
                             (org-ctrl-c-minus)
                             (if bullet (org-cycle-list-bullet bullet))
                             (org-return)))
          lst))


(provide 'org-chef-utils)
;;; org-chef-utils.el ends here
