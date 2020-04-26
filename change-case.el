;;; change-case.el --- Case conversion between camelCase, PascalCase, snake_case and more -*- lexical-binding: t -*-

;; Copyright (C) 2020 sximada

;; Author: sximada <sximada@gmail.com>
;; Maintainer: sximada <sximada@gmail.com>
;; Repository: https://gist.github.com/sximada/819e066481b57f8ea6e5a8ec92fb9c27
;; Version: 3
;; Date: 2020-04-26

;; change-case.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; The change-case.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with change-case.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements the change case functions.

;; Release note:
;;
;; * 3 (2020-04-26)
;; - Added ert tests.
;; - Added Release note.
;;
;; * 2 (2020-04-25)
;; - Added path/case.
;; - Added dotted/case.
;;
;; * 1 (2020-04-25)
;; - First implements. 
;; - Supported case.
;;   - snake_case
;;   - kebab_case
;;   - PascalCase
;;   - camelCase

;;; Code:
(require 'dash)
(require 'ert)
(require 's)


;;; dotted.case
(defvar change-case-dotted-case-separator ".")


(defun change-case-dotted-case-parse (sentence)
  (s-split (regexp-quote change-case-dotted-case-separator)
	   sentence))

(defun change-case-dotted-case-render (word-list)
  (string-join word-list change-case-dotted-case-separator))


;; test 
(ert-deftest change-case-dotted-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-dotted-case-parse "change.case.el"))))

(ert-deftest change-case-dotted-case-renderer-test ()
  (should
   (string-equal "change.case.el"
		 (change-case-dotted-case-render '("change" "case" "el")))))


;;; path/case
(defvar change-case-path-case-separator "/")


(defun change-case-path-case-parse (sentence)
  (s-split change-case-path-case-separator sentence))

(defun change-case-path-case-render (word-list)
  (string-join word-list change-case-path-case-separator))


;; test 
(ert-deftest change-case-path-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-path-case-parse "change/case/el"))))

(ert-deftest change-case-path-case-renderer-test ()
  (should
   (string-equal "change/case/el"
		 (change-case-path-case-render '("change" "case" "el")))))


;;; snake_case
(defvar change-case-snake-case-separator "_")


(defun change-case-snake-case-parse (sentence)
  (s-split change-case-snake-case-separator sentence))


(defun change-case-snake-case-render (word-list)
  (string-join (mapcar 'downcase word-list)
	       change-case-snake-case-separator))


;; test 
(ert-deftest change-case-snake-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-snake-case-parse "change_case_el"))))

(ert-deftest change-case-snake-case-renderer-test ()
  (should
   (string-equal "change_case_el"
		 (change-case-snake-case-render '("change" "case" "el")))))


;;; kebab-case
(defvar change-case-kebab-case-separator "-")


(defun change-case-kebab-case-parse (sentence)
  (s-split change-case-kebab-case-separator sentence))


(defun change-case-kebab-case-render (word-list)
  (string-join (mapcar 'downcase word-list) change-case-kebab-case-separator))


;; test 
(ert-deftest change-case-kebab-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-kebab-case-parse "change-case-el"))))

(ert-deftest change-case-kebab-case-renderer-test ()
  (should
   (string-equal "change-case-el"
		 (change-case-kebab-case-render '("change" "case" "el")))))


;;; PascalCase
(defvar change-case-upper-case-pattern "[A-Z]")


(defun change-case-get-index (sentence pos)
  (if (= pos 0) 0
    (let* ((case-fold-search nil))
      (string-match change-case-upper-case-pattern
		    sentence pos))))


(defun change-case-get-index-list (sentence pos)
  (if-let* ((n (change-case-get-index sentence pos))
	    (n+1 (+ n 1)))
      (cons n (change-case-get-index-list sentence n+1))))


(defun change-case-get-index-pair-list (num-list)
  (cons (-slice num-list 0 2) 
	(if-let* ((after (-slice num-list 1)))
	    (change-case-get-index-pair-list after))))


(defun change-case-pascal-case-parse (sentence)
  (mapcar
   (lambda (pair) (downcase 
		   (substring sentence
			      (car pair)
			      (car (cdr pair)))))
   (change-case-get-index-pair-list (change-case-get-index-list sentence 0))))


(defun change-case-pascal-case-render (word-list)
  (string-join (mapcar 'capitalize word-list))) 


;; test 
(ert-deftest change-case-pascal-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-pascal-case-parse "ChangeCaseEl"))))

(ert-deftest change-case-pascal-case-renderer-test ()
  (should
   (string-equal "ChangeCaseEl"
		 (change-case-pascal-case-render '("change" "case" "el")))))


;;; camelCase
(defun change-case-camel-case-parse (sentence)
  (change-case-pascal-case-parse sentence))


(defun change-case-camel-case-render (word-list)
  (concat 
   (downcase (car word-list))
   (change-case-pascal-case-render (cdr word-list))))


;; test 
(ert-deftest change-case-camel-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-camel-case-parse "changeCaseEl"))))

(ert-deftest change-case-camel-case-renderer-test ()
  (should
   (string-equal "changeCaseEl"
		 (change-case-camel-case-render '("change" "case" "el")))))


;;; Options
(defvar change-case-parser-alist '(change-case-dotted-case-parse
				   change-case-path-case-parse
				   change-case-snake-case-parse
				   change-case-kebab-case-parse
				   change-case-pascal-case-parse
				   change-case-camel-case-parse))


(defvar change-case-renderer-alist '(change-case-dotted-case-render
				     change-case-path-case-render
				     change-case-snake-case-render
				     change-case-kebab-case-render
				     change-case-pascal-case-render
				     change-case-camel-case-render))


;;; U/I
(defvar change-case-parser-prompt "change-case: parser:")
(defvar change-case-renderer-prompt "change-case: renderer:")


(defun change-case-select-parser (prompt)
  (intern
   (ido-completing-read prompt
			(mapcar 'symbol-name change-case-parser-alist))))


(defun change-case-select-renderer (prompt)
  (intern
   (ido-completing-read prompt
			(mapcar 'symbol-name change-case-renderer-alist))))


;;;###autoload
(defun change-case (&optional start end  parser render)
  (interactive (progn
                 (barf-if-buffer-read-only)
		 `(,@(if (use-region-p)
			 (list (region-beginning) (region-end))
		       (list nil nil))
		   ,(change-case-select-parser change-case-parser-prompt)
		   ,(change-case-select-renderer change-case-renderer-prompt))))
  (let ((sentence (funcall render
			   (funcall parser
				    (buffer-substring-no-properties start end)))))
    (delete-region start end)
    (save-excursion
      (goto-char start)
      (insert sentence))))

;;; _
(provide 'change-case)
;;; change-case.el ends here
