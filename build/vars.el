(require 'json)
(require 'cl-lib)

(defun batch-convert ()
  (defvar command-line-args-left)
  (let ((error nil))
    (while command-line-args-left
      (let* ((infile (car command-line-args-left))
             (outfile (concat (file-name-sans-extension infile) ".dat")))
        (message "%s -> %s" infile outfile)
        (build-el infile outfile))
      (setq command-line-args-left (cdr command-line-args-left)))))

(defun build-el (in out)
  (save-dat (load-dat in) (expand-file-name out)))

(defun load-dat (file)
  (let* ((json-key-type 'string))
    (json-read-file file)))

(defun save-dat (dat file)
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string dat))
      (write-region (point-min) (point-max) file))))

;; (defun format-dat (dat)
;;   (cl-loop for (k . meta) in dat
;;      do (add-text-properties
;;          0 1
;;          (list
;;           'annot "Implicit Variable"
;;           'meta meta
;;           'uri :implicit)
;;          k)
;;      collect k))
