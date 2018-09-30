;;; company-makefile --- company backend for makefiles -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/company-makefile
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 25 October 2016

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

(defvar company-makefile-modes '(makefile-gmake-mode))

(defvar company-makefile-dynamic-complete t
  "Offer dynamic completions for macros/targets. Invalidates 
`makefile-need-macro-pickup' and `makefile-need-target-pickup' after ':' and '='
respectively.'")

(defvar company-makefile--dir)
(when load-file-name
  (setq company-makefile--dir (file-name-directory load-file-name)))

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
  '((:auto
     . "https://www.gnu.org/software/make/manual/html_node/\
Automatic-Variables.html")
    (:fun
     . "https://www.gnu.org/software/make/manual/html_node/Functions.html")
    (:implicit
     . "https://www.gnu.org/software/make/manual/html_node/\
Implicit-Variables.html")))

;; automatic variables with meta blurb
(defvar company-makefile--autovars
  (eval-when-compile
    (let ((av '(("@" . "Target name")
                ("%" . "Target member name, eg. target=foo.a(bar.o), \
then '$%'=bar.o and '$@'=foo.a")
                ("<" . "Name of first prerequisite")
                ("?" . "Names of all prerequisites newer than target")
                ("^" . "Names of all prerequisites (no duplicates)")
                ("+" . "Like '^', but with duplicates")
                ("|" . "Names of all order-only prerequisites")
                ("*" . "Stem of implicit rule, eg. target=dir/a.foo.a, \
pattern=a.%.b, then dir/a"))))
      (cl-loop for (v . meta) in av
         do (setq v (concat "$" v))
           (add-text-properties
            0 1 (list 'annot "Auto Variable <Gmake>" 'meta meta 'uri :auto) v)
         collect v))))

;; implicit variables
(defvar company-makefile--implicit
  (cl-loop for (k . arr) in
       (with-temp-buffer
         (insert-file-contents
          (expand-file-name "build/impvars.dat"
                            company-makefile--dir))
         (car (read-from-string
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))
     do (add-text-properties
         0 1
         (list
          'annot "Implicit Variable"
          'meta (aref arr 0)
          'index (aref arr 1)
          'uri :implicit)
         k)
     collect k))

;; gmake statements (ifdef, etc)
;; #<marker at makefile-gmake-statemts in make-mode.el>
(defvar company-makefile--keywords
  (eval-when-compile
    (cl-loop for v in makefile-gmake-statements
       do (add-text-properties
           0 1 (list 'annot "Keyword <Gmake>" 'meta "") v)
       collect v)))

;; gmake functions
;; #<marker at makefile-gnumake-functions-alist in make-mode.el>
(defvar company-makefile--functions
  (eval-when-compile
    (cl-loop for v in `(,@makefile-gnumake-functions-alist
                        ("abspath") ("realpath"))
       do (add-text-properties
           0 1 (list 'annot "Function <Gmake>" 'meta "" 'uri :fun) (car v))
       collect (car v))))

;; dynamic completions
(defun company-makefile--dyn-vars ()
  (when company-makefile-dynamic-complete
    (and makefile-need-macro-pickup
         (makefile-pickup-macros))
    (cl-loop for v in (mapcar 'car makefile-macro-table)
       do (add-text-properties 0 1 (list 'annot "Local Variable") v)
       collect v)))

(defun company-makefile--dyn-targets ()
  (when company-makefile-dynamic-complete
    (and makefile-need-target-pickup
         (makefile-pickup-targets))
    (cl-loop for v in (mapcar 'car makefile-target-table)
       do (add-text-properties 0 1 (list 'annot "Target") v)
       collect v)))

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
  (when-let* ((uri (get-text-property 0 'uri candidate)))
    (browse-url (or (and (eq uri :implicit)
                         (concat (cdr (assq uri company-makefile--uris)) "#"
                                 (get-text-property 0 'index candidate)))
                    (cdr (assq uri company-makefile--uris))))))

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
        (list (1- (car bnds)) (cdr bnds) company-makefile--autovars
              :annotation-function 'company-makefile--annotation
              :company-location 'company-makefile--location
              :company-docsig 'company-makefile--meta))
       ;; function / local variable / implicit vars
       ((and (eq (char-before (car bnds)) ?\()
             (eq (char-before (1- (car bnds))) ?$))
        (list (car bnds) (cdr bnds)
              `(,@company-makefile--functions
                ,@(company-makefile--dyn-vars)
                ,@company-makefile--implicit)
              :annotation-function 'company-makefile--annotation
              :company-location 'company-makefile--location
              :company-docsig 'company-makefile--meta))
       ;; local variables ${...}
       ((and (eq (char-before (car bnds)) ?{)
             (eq (char-before (1- (car bnds))) ?$))
        (list (car bnds) (cdr bnds)
              `(,@(company-makefile--dyn-vars))))
       ;; targets
       ((company-makefile--target-p)
        (list (car bnds) (cdr bnds)
              (company-makefile--dyn-targets)
              :annotation-function 'company-makefile--annotation))
       ;; keywords
       (t
        (list (car bnds) (cdr bnds) company-makefile--keywords
              :annotation-function 'company-makefile--annotation)))))

;;;###autoload
(defun company-makefile-init ()
  ;; when dynamically completing, rebind ":" and "=" to 
  ;; invalidate make-mode dynamic completion tables for macros/targets
  (when company-makefile-dynamic-complete
    (eval-after-load 'makefile-mode
      (progn
        (define-key makefile-mode-map "=" (lambda (arg)
                                            (interactive "p")
                                            (setq makefile-need-macro-pickup t)
                                            (self-insert-command arg)))
        (define-key makefile-mode-map ":" (lambda (arg)
                                            (interactive "p")
                                            (setq makefile-need-target-pickup t)
                                            (self-insert-command arg))))))

  ;; replace makefile completion at point function
  (remove-hook 'completion-at-point-functions 'makefile-completions-at-point 'local)
  (add-hook 'completion-at-point-functions 'company-makefile-capf nil 'local)

  ;; setup company-backend
  (when (featurep 'company)
    (setq-local company-require-match 'never)
    (make-local-variable 'company-backends)
    (cl-pushnew 'company-capf company-backends)))

;;;###autoload
(add-hook 'makefile-mode-hook 'company-makefile-init)

(provide 'company-makefile)
;;; company-makefile.el ends here
