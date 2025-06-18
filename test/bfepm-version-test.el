;;; bfepm-version-test.el --- Tests for BFEPM version constraint engine -*- lexical-binding: t -*-

;;; Commentary:

;; Comprehensive tests for the version constraint engine.
;; Tests semantic version comparison, MELPA date versions,
;; and constraint satisfaction (caret, tilde, exact).

;;; Code:

(require 'ert)
(require 'bfepm-version)

;;; Version Comparison Tests

(ert-deftest bfepm-version-compare-semantic-versions-test ()
  "Test semantic version comparison."
  ;; Equal versions
  (should (= (bfepm-version-compare "1.0.0" "1.0.0") 0))
  (should (= (bfepm-version-compare "2.1.3" "2.1.3") 0))
  
  ;; Major version differences
  (should (= (bfepm-version-compare "2.0.0" "1.0.0") 1))
  (should (= (bfepm-version-compare "1.0.0" "2.0.0") -1))
  
  ;; Minor version differences
  (should (= (bfepm-version-compare "1.2.0" "1.1.0") 1))
  (should (= (bfepm-version-compare "1.1.0" "1.2.0") -1))
  
  ;; Patch version differences
  (should (= (bfepm-version-compare "1.0.2" "1.0.1") 1))
  (should (= (bfepm-version-compare "1.0.1" "1.0.2") -1))
  
  ;; Different length versions
  (should (= (bfepm-version-compare "1.0" "1.0.0") -1))
  (should (= (bfepm-version-compare "1.0.0" "1.0") 1))
  (should (= (bfepm-version-compare "1.2.3.4" "1.2.3") 1)))

(ert-deftest bfepm-version-compare-melpa-date-versions-test ()
  "Test MELPA date version comparison."
  ;; Equal date versions
  (should (= (bfepm-version-compare "20250426.1319" "20250426.1319") 0))
  
  ;; Different dates
  (should (= (bfepm-version-compare "20250427.1000" "20250426.1319") 1))
  (should (= (bfepm-version-compare "20250425.1000" "20250426.1319") -1))
  
  ;; Same date, different times
  (should (= (bfepm-version-compare "20250426.1320" "20250426.1319") 1))
  (should (= (bfepm-version-compare "20250426.1318" "20250426.1319") -1))
  
  ;; Year differences
  (should (= (bfepm-version-compare "20260101.0000" "20251231.2359") 1))
  (should (= (bfepm-version-compare "20241231.2359" "20250101.0000") -1)))

(ert-deftest bfepm-version-compare-mixed-versions-test ()
  "Test comparison between semantic and MELPA date versions."
  ;; Should fall back to semantic comparison
  (should (= (bfepm-version-compare "1.0.0" "20250426.1319") -1))
  (should (= (bfepm-version-compare "20250426.1319" "1.0.0") 1)))

;;; Version Normalization Tests

(ert-deftest bfepm-version-normalize-test ()
  "Test version normalization from various formats."
  ;; String versions (no change)
  (should (string= (bfepm-version-normalize "1.2.3") "1.2.3"))
  (should (string= (bfepm-version-normalize "20250426.1319") "20250426.1319"))
  
  ;; Number versions
  (should (string= (bfepm-version-normalize 123) "123"))
  (should (string= (bfepm-version-normalize 1.5) "1.5"))
  
  ;; List versions (MELPA format)
  (should (string= (bfepm-version-normalize '(20250426 1319)) "20250426.1319"))
  (should (string= (bfepm-version-normalize '(1 2 3)) "1.2.3"))
  (should (string= (bfepm-version-normalize '(5)) "5"))
  
  ;; Unknown formats
  (should (string= (bfepm-version-normalize nil) "unknown"))
  (should (string= (bfepm-version-normalize 'symbol) "unknown")))

;;; Constraint Parsing Tests

(ert-deftest bfepm-version-parse-constraint-test ()
  "Test constraint string parsing."
  ;; Latest constraint
  (should (equal (bfepm-version-parse-constraint "latest") '(latest nil)))
  
  ;; Caret constraints
  (should (equal (bfepm-version-parse-constraint "^1.2.3") '(caret "1.2.3")))
  (should (equal (bfepm-version-parse-constraint "^20250426") '(caret "20250426")))
  
  ;; Tilde constraints
  (should (equal (bfepm-version-parse-constraint "~1.2.3") '(tilde "1.2.3")))
  (should (equal (bfepm-version-parse-constraint "~2.0") '(tilde "2.0")))
  
  ;; Exact constraints
  (should (equal (bfepm-version-parse-constraint "1.2.3") '(exact "1.2.3")))
  (should (equal (bfepm-version-parse-constraint "v2.1.0") '(exact "v2.1.0"))))

;;; Constraint Satisfaction Tests

(ert-deftest bfepm-version-satisfies-latest-test ()
  "Test 'latest' constraint satisfaction."
  (should (bfepm-version-satisfies-p "1.0.0" "latest"))
  (should (bfepm-version-satisfies-p "20250426.1319" "latest"))
  (should (bfepm-version-satisfies-p "0.0.1" "latest")))

(ert-deftest bfepm-version-satisfies-exact-test ()
  "Test exact constraint satisfaction."
  (should (bfepm-version-satisfies-p "1.2.3" "1.2.3"))
  (should (bfepm-version-satisfies-p "20250426.1319" "20250426.1319"))
  (should-not (bfepm-version-satisfies-p "1.2.4" "1.2.3"))
  (should-not (bfepm-version-satisfies-p "20250427.1319" "20250426.1319")))

(ert-deftest bfepm-version-satisfies-caret-semantic-test ()
  "Test caret constraint satisfaction for semantic versions."
  ;; Compatible versions (same major, higher minor/patch)
  (should (bfepm-version-satisfies-p "1.2.3" "^1.2.0"))
  (should (bfepm-version-satisfies-p "1.3.0" "^1.2.0"))
  (should (bfepm-version-satisfies-p "1.2.5" "^1.2.3"))
  
  ;; Incompatible versions (different major)
  (should-not (bfepm-version-satisfies-p "2.0.0" "^1.2.0"))
  (should-not (bfepm-version-satisfies-p "0.9.0" "^1.0.0"))
  
  ;; Lower versions
  (should-not (bfepm-version-satisfies-p "1.1.9" "^1.2.0")))

(ert-deftest bfepm-version-satisfies-caret-melpa-test ()
  "Test caret constraint satisfaction for MELPA date versions."
  ;; Same year versions
  (should (bfepm-version-satisfies-p "20250426.1319" "^20250425.1000"))
  (should (bfepm-version-satisfies-p "20250501.0000" "^20250426.1319"))
  
  ;; Different year versions
  (should-not (bfepm-version-satisfies-p "20260101.0000" "^20250426.1319"))
  (should-not (bfepm-version-satisfies-p "20240426.1319" "^20250426.1319")))

(ert-deftest bfepm-version-satisfies-tilde-semantic-test ()
  "Test tilde constraint satisfaction for semantic versions."
  ;; Compatible patch versions
  (should (bfepm-version-satisfies-p "1.2.3" "~1.2.0"))
  (should (bfepm-version-satisfies-p "1.2.5" "~1.2.3"))
  
  ;; Incompatible minor versions
  (should-not (bfepm-version-satisfies-p "1.3.0" "~1.2.0"))
  (should-not (bfepm-version-satisfies-p "1.1.9" "~1.2.0"))
  
  ;; Incompatible major versions
  (should-not (bfepm-version-satisfies-p "2.2.0" "~1.2.0")))

(ert-deftest bfepm-version-satisfies-tilde-melpa-test ()
  "Test tilde constraint satisfaction for MELPA date versions."
  ;; Same date versions
  (should (bfepm-version-satisfies-p "20250426.1319" "~20250426.1000"))
  (should (bfepm-version-satisfies-p "20250426.1500" "~20250426.1319"))
  
  ;; Different date versions
  (should-not (bfepm-version-satisfies-p "20250427.0000" "~20250426.1319"))
  (should-not (bfepm-version-satisfies-p "20250425.2359" "~20250426.1319")))

;;; Best Match Selection Tests

(ert-deftest bfepm-version-find-best-match-single-constraint-test ()
  "Test finding best version match with single constraint."
  (let ((versions '("1.0.0" "1.1.0" "1.2.0" "2.0.0")))
    ;; Latest constraint
    (should (string= (bfepm-version-find-best-match versions '("latest")) "2.0.0"))
    
    ;; Exact constraint
    (should (string= (bfepm-version-find-best-match versions '("1.1.0")) "1.1.0"))
    (should (null (bfepm-version-find-best-match versions '("3.0.0"))))
    
    ;; Caret constraint
    (should (string= (bfepm-version-find-best-match versions '("^1.0.0")) "1.2.0"))
    (should (string= (bfepm-version-find-best-match versions '("^1.1.0")) "1.2.0"))
    
    ;; Tilde constraint
    (should (string= (bfepm-version-find-best-match versions '("~1.1.0")) "1.1.0"))))

(ert-deftest bfepm-version-find-best-match-multiple-constraints-test ()
  "Test finding best version match with multiple constraints."
  (let ((versions '("1.0.0" "1.1.0" "1.2.0" "1.3.0" "2.0.0")))
    ;; Multiple compatible constraints
    (should (string= (bfepm-version-find-best-match versions '("^1.0.0" "~1.2.0")) "1.2.0"))
    
    ;; Conflicting constraints
    (should (null (bfepm-version-find-best-match versions '("^1.0.0" "^2.0.0"))))
    
    ;; Exact + caret constraints
    (should (string= (bfepm-version-find-best-match versions '("1.1.0" "^1.0.0")) "1.1.0"))))

(ert-deftest bfepm-version-find-best-match-melpa-versions-test ()
  "Test finding best version match with MELPA date versions."
  (let ((versions '("20250424.1000" "20250425.1200" "20250426.1319" "20250427.0800")))
    ;; Latest constraint
    (should (string= (bfepm-version-find-best-match versions '("latest")) "20250427.0800"))
    
    ;; Caret constraint (same year)
    (should (string= (bfepm-version-find-best-match versions '("^20250425.0000")) "20250427.0800"))
    
    ;; Tilde constraint (same day)
    (should (string= (bfepm-version-find-best-match versions '("~20250426.0000")) "20250426.1319"))))

;;; Edge Cases and Error Handling Tests

(ert-deftest bfepm-version-edge-cases-test ()
  "Test edge cases and error conditions."
  ;; Empty version strings
  (should (= (bfepm-version-compare "" "") 0))
  
  ;; Invalid MELPA date format (should fall back to semantic)
  (should (= (bfepm-version-compare "2025042.131" "20250426.1319") -1))
  
  ;; Single digit versions
  (should (= (bfepm-version-compare "1" "2") -1))
  (should (= (bfepm-version-compare "9" "10") -1))
  
  ;; Very long version strings
  (should (= (bfepm-version-compare "1.2.3.4.5.6" "1.2.3.4.5.7") -1)))

(ert-deftest bfepm-version-error-handling-test ()
  "Test error handling for invalid inputs."
  ;; Invalid date prefix requirements should signal errors
  (should-error (bfepm-version-satisfies-p "20250426.1319" "^invalid"))
  (should-error (bfepm-version-satisfies-p "20250426.1319" "~abc123")))

;;; Performance Tests

(ert-deftest bfepm-version-performance-test ()
  "Test performance with large version lists."
  (let ((versions (mapcar (lambda (i) (format "1.%d.0" i)) (number-sequence 0 999))))
    ;; Should complete quickly even with 1000 versions
    (should (string= (bfepm-version-find-best-match versions '("^1.500.0")) "1.999.0"))))

(provide 'bfepm-version-test)

;;; bfepm-version-test.el ends here