(require 'json)
(require 'cl-lib)

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

(defun merge-data ()
  "Merge data scrapped from web with local defaults."
  (let* ((defaults (with-temp-buffer
                     (insert-file-contents "defaults.el")
                     (car (read-from-string (buffer-string)))))
         (def-imps (assoc 'default defaults))
         (imps (load-json-data "impvars.json")))
    (dolist (def def-imps)
      (when-let ((var (assoc-string def imps)))
        ))))
