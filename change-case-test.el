;;; change-case-test.el

;; Copyright (C) 2020 TakesxiSximada

;; You should have received a copy of the GNU General Public License
;; along with change-case.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Transform a string between camelCase, PascalCase, snake_case, kebab-case, doted.case
;; and others by Emacs Lisp.

;;; Code:
(require 'ert)
(require 'change-case)


;; test
(ert-deftest change-case-dotted-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-dotted-case-parse "change.case.el"))))

(ert-deftest change-case-dotted-case-renderer-test ()
  (should
   (string-equal "change.case.el"
		 (change-case-dotted-case-render '("change" "case" "el")))))

;; test
(ert-deftest change-case-path-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-path-case-parse "change/case/el"))))

(ert-deftest change-case-path-case-renderer-test ()
  (should
   (string-equal "change/case/el"
		 (change-case-path-case-render '("change" "case" "el")))))

;; test
(ert-deftest change-case-snake-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-snake-case-parse "change_case_el"))))

(ert-deftest change-case-snake-case-renderer-test ()
  (should
   (string-equal "change_case_el"
		 (change-case-snake-case-render '("change" "case" "el")))))

;; test
(ert-deftest change-case-kebab-case-parse-test ()
  (should
   (equal '("change" "case" "el")
	  (change-case-kebab-case-parse "change-case-el"))))

(ert-deftest change-case-kebab-case-renderer-test ()
  (should
   (string-equal "change-case-el"
		 (change-case-kebab-case-render '("change" "case" "el")))))

;; test
(ert-deftest change-case-pascal-case-parse-test ()
  (should
   (equal '("Change" "Case" "El")
	  (change-case-pascal-case-parse "ChangeCaseEl"))))

(ert-deftest change-case-pascal-case-renderer-test ()
  (should
   (string-equal "ChangeCaseEl"
		 (change-case-pascal-case-render '("change" "case" "el")))))

;; test
(ert-deftest change-case-camel-case-parse-test ()
  (should
   (equal '("change" "Case" "El")
	  (change-case-camel-case-parse "changeCaseEl"))))

(ert-deftest change-case-camel-case-renderer-test ()
  (should
   (string-equal "changeCaseEl"
		 (change-case-camel-case-render '("change" "case" "el")))))

(provide 'change-case-test)
;;; change-case.el ends here
