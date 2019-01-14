(require 'json)
(require 'cl-lib)
(require 'make-mode)

(defconst make-vars-data-file "company-makefile-data.el"
  "File in which to store data.")

(defun batch-create-data ()
  "Gather info from local 'make -p' and web, and dump to file."
  (defvar command-line-args-left)
  (let ((error nil) impvars defaults)
    (while command-line-args-left
      (let ((infile (car command-line-args-left)))
       (pcase (file-name-extension infile)
         (`"json" (setq impvars infile))
         (`"el" (setq defaults infile))
         (_ (message "Unknown file %s" infile))))
      (setq command-line-args-left (cdr command-line-args-left)))
    (if (and impvars defaults)
        (merge-and-write-data :default-file defaults :impvar-file impvars
                              :outfile make-vars-data-file)
      (error "Failed to created data"))))

(defun load-json-data (file)
  "Read JSON data to alist."
  (let* ((json-key-type 'string))
    (json-read-file file)))

(defun make-vars-dump-data (data file)
  "Dump resulting hash table to file."
  (with-temp-buffer
    (prin1 data (current-buffer))
    (write-file file)))

;; -------------------------------------------------------------------
;;; Merge info, propertize keys, dump data

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

;; structure to store variable info - used in build
(cl-defstruct (company-makefile-vars (:constructor nil)
                                     (:constructor company-makefile-vars--create
                                                   (&optional name meta default
                                                              annot index type))
                                     (:copier nil))
  (name nil :read-only t) meta default annot index type)

(cl-defun make-vars-create (name &key meta (default "") annot index type)
  "Create makefile variable."
  (company-makefile-vars--create name meta default annot index type))

(cl-defun merge-and-write-data (&key (default-file "defaults.el")
                                     (impvar-file "impvars.json")
                                     (outfile "company-makefile-data.el"))
  "Merge data scrapped from web with local defaults and dump to file."
  (let* ((ht (make-hash-table :test 'equal))
         (defaults (with-temp-buffer
                     (insert-file-contents default-file)
                     (car (read-from-string (buffer-string)))))
         (imps (load-json-data impvar-file))
         (mappings '((default "<Implicit>" :implicit)
                     (automatic "<AutoVar>" :autovar)
                     (makefile "<Dynamic>" :dynamic)))
         (funcs (append (mapcar 'car makefile-gnumake-functions-alist)
                        '("abspath" "realpath")))
         (keywords makefile-gmake-statements)
         autovars implicits dynamics)

    ;; Add local defaults to hash table
    (cl-loop for (type . rest) in defaults
       for annot = (cadr (assoc type mappings))
       for type = (caddr (assoc type mappings))
       do (cl-loop for (k . v) in rest
             do (puthash k (make-vars-create
                            k :default v
                            :meta (if (eq type :dynamic) "Dynamic implicit variable"
                                    v)
                            :annot annot :type type)
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
              (setf (company-makefile-vars-meta val)
                    (replace-regexp-in-string "\n" " " (aref arr 0)))
              (setf (company-makefile-vars-index val) (aref arr 1))
              (setf (company-makefile-vars-type val) :implicit))))

    ;; add auto variable meta descriptions
    (cl-loop for (k . v) in make-autovars
       do (let ((val (gethash k ht)))
            (if (not val)
                (puthash k (make-vars-create k :meta v :type :autovar
                                             :annot "<AutoVar>")
                         ht)
              (setf (company-makefile-vars-meta val) v))))

    ;; add gmake keywords (make-mode)
    (cl-loop for v in keywords
       do (set-text-properties 0 1 nil v)
         (puthash v (make-vars-create v :annot "<Keyword>" :type :keyword
                                      :meta "Builtin")
                  ht))

    ;; add gmake functions (make-mode)
    (cl-loop for v in funcs
       do (set-text-properties 0 1 nil v)
         (puthash v (make-vars-create v :annot "<Function>" :type :function
                                      :meta "Builtin")
                  ht))

    ;; propertize hash keys
    (make-vars-add-props ht)

    ;; separate types
    (maphash (lambda (k v)
               (pcase (company-makefile-vars-type v)
                 (:function (push k funcs))
                 (:keyword (push k keywords))
                 (:autovar (push k autovars))
                 (:implicit (push k implicits))
                 (:dynamic (push k dynamics))))
             ht)
    
    ;; store results
    (make-vars-dump-data
     (list (cons 'keyword keywords)
           (cons 'function funcs)
           (cons 'autovar autovars)
           (cons 'implicit implicits)
           (cons 'dynamic dynamics))
     outfile)
    ht))

(defun make-vars-add-props (table)
  "Add text properties to hash-TABLE keys."
  (cl-loop for k being the hash-keys of table using (hash-values v)
     do (add-text-properties
         0 1 (list 'annot (company-makefile-vars-annot v)
                   'meta (company-makefile-vars-meta v)
                   'type (company-makefile-vars-type v)
                   'index (company-makefile-vars-index v))
         k)))
