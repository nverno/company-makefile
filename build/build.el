(require 'json)
(require 'cl-lib)
(require 'company-makefile)

(defun batch-convert ()
  "Batch convert JSON to alist."
  (defvar command-line-args-left)
  (let ((error nil))
    (while command-line-args-left
      (let* ((infile (car command-line-args-left))
             (outfile (concat (file-name-sans-extension infile) ".el")))
        (message "%s -> %s" infile outfile)
        (build-el-data infile outfile))
      (setq command-line-args-left (cdr command-line-args-left)))))

(defun build-el-data (in out)
  (save-data (load-json-data in) (expand-file-name out)))

(defun load-json-data (file)
  (let* ((json-key-type 'string))
    (json-read-file file)))

(defun save-data (dat file)
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string dat))
      (write-region (point-min) (point-max) file))))

;; -------------------------------------------------------------------
;;; Merge info

;; some short blurbs describing automatic variables
(defvar make-autovars '(("@" . "Target name")
                        ("%" . "Target member name, eg. target=foo.a(bar.o), \
then '$%'=bar.o and '$@'=foo.a")
                        ("<" . "Name of first prerequisite")
                        ("?" . "Names of all prerequisites newer than target")
                        ("^" . "Names of all prerequisites (no duplicates)")
                        ("+" . "Like '^', but with duplicates")
                        ("|" . "Names of all order-only prerequisites")
                        ("*" . "Stem of implicit rule, eg. target=dir/a.foo.a, \
pattern=a.%.b, then dir/a")
                        ("@D" . "Directory part of file name of target with \
trailing slash removed.")
                        ("@F" . "Basename of target.")
                        ("*D" . "Directory part of stem.")
                        ("*F" . "Basename part of stem.")
                        ("%D" . "Directory part of target archive member name.")
                        ("%F" . "Basename part of target archive member name.")
                        ("<D" . "Directory part of first prereq.")
                        ("<F" . "Basename part of first prereq.")
                        ("^D" . "List of directory parts of all prereqs.")
                        ("^F" . "List of basenames of all prereqs.")
                        ("?D" . "List of directory parts of all prereqs newer than \
target.")
                        ("?F" . "List of basenames of all prereqs newer than \
target.")))

(cl-defun make-vars-create (name &key (meta "") (default "") annot (index "") type)
  "Create makefile variable."
  (company-makefile-vars--create name meta default annot index type))

(cl-defun merge-and-write-data (&key (default-file "defaults.el")
                                     (impvar-file "impvars.json")
                                     (outfile "company-makefile-data.el"))
  "Merge data scrapped from web with local defaults and write to hash table."
  (let* ((ht (make-hash-table :test 'equal))
         (defaults (with-temp-buffer
                     (insert-file-contents default-file)
                     (car (read-from-string (buffer-string)))))
         (imps (load-json-data impvar-file))
         (mappings '((default "<Implicit>" :implicit)
                     (automatic "<Var>" :var)
                     (makefile "<Dynamic>" :dynamic))))

    ;; Add local defaults to hash table
    (cl-loop for (type . rest) in defaults
       for annot = (cadr (assoc type mappings))
       for type = (caddr (assoc type mappings))
       do (cl-loop for (k . v) in rest
             do (puthash k (make-vars-create k :default v :meta v :annot annot
                                             :type type)
                 ht)))
    
    ;; add scraped descriptions
    (cl-loop for (k . arr) in imps
       do (let ((val (gethash k ht)))
            (if (not val)
                (puthash k (make-vars-create k :meta (aref arr 0)
                                             :index (aref arr 1)
                                             :type :implicit
                                             :annot "<Implicit>")
                         ht)
              ;; otherwise update data with longer descriptions
              (setf (company-makefile-vars-meta val) (aref arr 0))
              (setf (company-makefile-vars-index val) (aref arr 1))
              (setf (company-makefile-vars-type val) :implicit))))

    ;; add auto variable meta descriptions
    (cl-loop for (k . v) in make-autovars
       do (let ((val (gethash k ht)))
            (if (not val)
                (puthash k (make-vars-create k :meta v :type :var :annot "<Var>") ht)
              (setf (company-makefile-vars-meta val) v))))

    ;; add gmake keywords (make-mode)
    (cl-loop for v in makefile-gmake-statements
       do (puthash v (make-vars-create v :annot "<Keyword>" :type :keyword) ht))

    ;; add gmake functions (make-mode)
    (cl-loop for v in (append makefile-gnumake-functions-alist
                              '("abspath" "realpath"))
       do (puthash v (make-vars-create v :annot "<Function>" :type :function) ht))

    ;; store results
    (with-temp-buffer
      (prin1 ht (current-buffer))
      (write-region (point-min) (point-max) outfile))
    ht))

(defun make-vars-add-props (table)
  "Add text properties to keys in hash-TABLE."
  (cl-loop for k being the hash-keys of table using (hash-values v)
     do (add-text-properties
         0 1 (list 'annot (company-makefile-vars-annot v)
                   'meta (company-makefile-vars-meta v)
                   'type (company-makefile-vars-type v)
                   'index (company-makefile-vars-index v)))))
