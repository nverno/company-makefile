(require 'ert)
(require 'company-makefile)

(defmacro company-makefile--should-complete (from to)
  `(with-temp-buffer
     (makefile-gmake-mode)
     (insert ,from)
     (completion-at-point)
     (should (string= (buffer-substring-no-properties (point-min)
                                                      (point-max))
                      ,to))))

(ert-deftest company-makefile--test-complete-var ()
  (company-makefile--should-complete
   "
gcc ?= gcc
$(g"
   "
gcc ?= gcc
$(gcc"))

(ert-deftest company-makefile--test-complete-implicit ()
  (company-makefile--should-complete
   "
$(CXXF"
   "
$(CXXFLAGS"))

(ert-deftest company-makefile--test-complete-keyword ()
  (company-makefile--should-complete
   "
ifnd"
   "
ifndef"))

(ert-deftest company-makefile--test-complete-function-1 ()
  (company-makefile--should-complete
   "
$(abs"
   "
$(abspath"))

(ert-deftest company-makefile--test-complete-function-2 ()
  (company-makefile--should-complete
   "
python ?= \"$(shell cygpath -m $(sh"
   "
python ?= \"$(shell cygpath -m $(shell"))

(ert-deftest company-makefile--test-complete-target ()
  (company-makefile--should-complete
   "
blah:
	./blah
test: b"
   "
blah:
	./blah
test: blah"))


(defun company-makefile--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively ".*")
    (message "Can't run tests without ert.")))

(provide 'company-makefile-tests)
