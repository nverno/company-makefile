;;; company-makefile.el --- completion backend for gnu makefiles -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-03-14 19:42:21>
;; URL: https://github.com/nverno/company-makefile
;; Package-Requires: 
;; Created: 25 October 2016
;; Keywords: convenience, matching

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; [![Build Status](https://travis-ci.org/nverno/company-makefile.svg?branch=master)](https://travis-ci.org/nverno/company-makefile)

;; Replacement completion at point for `makefile-gmake-mode'. `company' isn't
;; strictly required, but adornment functions are desinged for company-mode.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'make-mode)
(require 'company nil t)
(defvar company-require-match)
(defvar company-backends)
(declare-function company-doc-buffer "company")

(defgroup company-makefile nil
  "Completion backend for gnu makefiles."
  :group 'convenience
  :group 'matching)

(defcustom company-makefile-modes '(makefile-gmake-mode makefile-mode)
  "Modes where `company-makefile' should be active."
  :type '(repeat :inline t (symbol :tag "mode"))
  :group 'company-makefile)

(defcustom company-makefile-dynamic-complete t
  "Offer dynamic completions for macros/targets. Invalidates 
`makefile-need-macro-pickup' and `makefile-need-target-pickup' after ':' and '='
respectively.'"
  :type 'boolean
  :group 'company-makefile)

(eval-and-compile
  (defconst company-makefile-dir
    (file-name-directory
     (cond
      (load-in-progress load-file-name)
      ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
       byte-compile-current-file)
      (:else (buffer-file-name))))))

;; ------------------------------------------------------------
;;* Completion candidates
;;
;; make-mode variables:
;; - makefile-gnumake-functions-alist
;; - makefile-gmake-statements
;; Dynamic:
;; rebuild all with (makefile-pickup-everything t)
;; - makefile-macro-table (makefile-pickup-macros)
;; - makefile-target-table (makefile-pickup-targets)

;; uris for help docs
(defvar company-makefile--uris
  '((:autovar
     . "https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html")
    (:function
     . "https://www.gnu.org/software/make/manual/html_node/Functions.html")
    (:implicit
     . "https://www.gnu.org/software/make/manual/html_node/Implicit-Variables.html")
    (:keyword . nil)
    (:dynamic . nil)))

(defvar company-makefile-data
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "company-makefile-data.el" company-makefile-dir))
    (car (read-from-string (buffer-string))))
  "Info for makefile implicits/builtins.")

;; Dynamic completions
(defmacro company-makefile--dyn-fn (type props)
  "Create dynamic completion function/cache for given TYPE, adding PROPS to \
each variable.
TYPE should be one of [macro|target] to align with `make-mode' variables."
  (declare (indent 1))
  (let ((name (intern (concat "company-makefile--dyn-" type)))
        (need-pickup (intern (concat "makefile-need-" type "-pickup")))
        (pickup (intern (concat "makefile-pickup-" type "s")))
        (table (intern (concat "makefile-" type "-table"))))
    `(progn
       (defvar-local ,name ())           ;cache
       (defun ,name ()
         (when company-makefile-dynamic-complete
           (if (and (not ,need-pickup) ,name)
               ,name
             ;; pickup new vars
             (,pickup)
             (setq ,name
                   (cl-loop for v in (mapcar 'car ,table)
                      do (add-text-properties 0 1 ,props v)
                      collect v))))))))

(company-makefile--dyn-fn "macro" (list 'annot "<Local Var>"))
(company-makefile--dyn-fn "target" (list 'annot "<Target>"))

;; ------------------------------------------------------------
;;* Candidate Adornments

(defun company-makefile--annotation (candidate)
  (or (get-text-property 0 'annot candidate) ""))

(defun company-makefile--meta (candidate)
  (get-text-property 0 'meta candidate))

;; FIXME: todo
(defun company-makefile--doc (_candidate)
  (company-doc-buffer ""))

;; open help docs in browser if uri is available
(defun company-makefile--location (candidate)
  (when-let* ((type (get-text-property 0 'type candidate)))
    (browse-url (or (and (eq type :implicit)
                         (concat (cdr (assq type company-makefile--uris)) "#"
                                 (get-text-property 0 'index candidate)))
                    (cdr (assq type company-makefile--uris))))))

;; ------------------------------------------------------------
;;* Helpers

;; in target line, ie target:|
(defun company-makefile--target-p ()
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at-p "^[^ \t\n]*:")))

;; symbol at point to complete on
(defun company-makefile-batp ()
  (save-excursion
    (let ((start (progn
                   (skip-chars-backward "A-Za-z0-9_." (line-beginning-position))
                   (point)))
          (end (progn
                 (skip-chars-forward "A-Za-z0-9_.")
                 (point))))
      (cons start end))))
(put 'makesym 'bounds-of-thing-at-point 'company-makefile-batp)

;; ------------------------------------------------------------
;;* completion-at-point

(defun company-makefile-capf ()
  (if-let* ((bnds (bounds-of-thing-at-point 'makesym)))
      (cond
       ;; don't try to complete on possible file names, let company-files do it
       ((eq (char-before (car bnds)) ?/) nil)
       ;; point or symbol prefixed by $, company-makefile--autovars
       ((eq (char-before (car bnds)) ?$)
        (list (car bnds) (cdr bnds) (assq 'autovar company-makefile-data)
              :annotation-function #'company-makefile--annotation
              :company-location #'company-makefile--location
              :company-docsig #'company-makefile--meta))
       ;; function / local variable / implicit vars / autovars
       ((and (eq (char-before (car bnds)) ?\()
             (eq (char-before (1- (car bnds))) ?$))
        (list (car bnds) (cdr bnds)
              `(,@(assq 'function company-makefile-data)
                ,@(assq 'implicit company-makefile-data)
                ,@(assq 'autovar company-makefile-data)
                ,@(assq 'dynamic company-makefile-data)
                ,@(company-makefile--dyn-macro))
              :annotation-function #'company-makefile--annotation
              :company-location #'company-makefile--location
              :company-docsig #'company-makefile--meta))
       ;; ${...} - local / auto / implicit vars
       ((and (eq (char-before (car bnds)) ?{)
             (eq (char-before (1- (car bnds))) ?$))
        (list (car bnds) (cdr bnds)
              `(,@(company-makefile--dyn-macro)
                ,@(assq 'implicit company-makefile-data)
                ,@(assq 'dynamic company-makefile-data)
                ,@(assq 'autovar company-makefile-data))))
       ;; targets
       ((company-makefile--target-p)
        (list (car bnds) (cdr bnds)
              (company-makefile--dyn-target)
              :annotation-function #'company-makefile--annotation))
       ;; keywords
       (t
        (list (car bnds) (cdr bnds) (assq 'keyword company-makefile-data)
              :annotation-function #'company-makefile--annotation)))))

;; when dynamically completing, rebind ":" and "=" to 
;; invalidate make-mode dynamic completion tables for macros/targets
(when company-makefile-dynamic-complete
  (with-eval-after-load 'makefile-mode
    (cl-macrolet ((insert-fn (name pickup)
                    (let ((fn-name (intern (concat "insert-" name))))
                      `(progn
                         (defun ,fn-name (arg)
                           (interactive "p")
                           (setq ,pickup t)
                           (self-insert-command arg))))))
      (define-key makefile-mode-map "=" (insert-fn "=" makefile-need-macro-pickup))
      (define-key makefile-mode-map ":"
        (insert-fn ":" makefile-need-target-pickup)))))

;;;###autoload
(defun company-makefile-init ()
  "Update `completion-at-point-functions' and `company' variables when \
`company' is enabled."
  (remove-hook 'completion-at-point-functions 'makefile-completions-at-point 'local)
  (add-hook 'completion-at-point-functions 'company-makefile-capf nil 'local)
  (when (featurep 'company)
    (setq-local company-require-match 'never)
    (make-local-variable 'company-backends)
    (cl-pushnew 'company-capf company-backends)))

;;;###autoload
(add-hook 'makefile-mode-hook 'company-makefile-init)

(provide 'company-makefile)
;;; company-makefile.el ends here

;; Local Variables:
;; lisp-indent-function: common-lisp-indent-function
;; End:
