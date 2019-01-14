;;; company-makefile.el --- completion backend for gnu makefiles -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/company-makefile
;; Package-Requires: 
;; Created: 25 October 2016
;; Version: 0.0.1
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

(defcustom company-makefile-modes '(makefile-gmake-mode)
  "Modes where `company-makefile' should be active."
  :type '(repeat :inline t (symbol :tag "mode")))

(defcustom company-makefile-dynamic-complete t
  "Offer dynamic completions for macros/targets. Invalidates 
`makefile-need-macro-pickup' and `makefile-need-target-pickup' after ':' and '='
respectively.'"
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

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
  (when load-file-name
    (with-temp-buffer
      (insert-file-contents "company-makefile-data.el")
      (car (read-from-string (buffer-string))))))

;; dynamic completions
(defvar-local company-makefile--dyn-vars ())
(defun company-makefile--dyn-vars ()
  (when company-makefile-dynamic-complete
    (if (and (not makefile-need-macro-pickup)
             company-makefile--dyn-vars)
        company-makefile--dyn-vars
      ;; need to pickup new vars
      (makefile-pickup-macros)
      (setq company-makefile--dyn-vars
            (cl-loop for v in (mapcar 'car makefile-macro-table)
               do (add-text-properties 0 1 (list 'annot "Local Variable") v)
               collect v)))))

(defvar-local company-makefile--dyn-targets ())
(defun company-makefile--dyn-targets ()
  (when company-makefile-dynamic-complete
    (if (and (not makefile-need-target-pickup)
             company-makefile--dyn-targets)
        company-makefile--dyn-targets
      (makefile-pickup-targets)
      (setq company-makefile--dyn-targets
            (cl-loop for v in (mapcar 'car makefile-target-table)
               do (add-text-properties 0 1 (list 'annot "Target") v)
               collect v)))))

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
              :annotation-function 'company-makefile--annotation
              :company-location 'company-makefile--location
              :company-docsig 'company-makefile--meta))
       ;; function / local variable / implicit vars / autovars
       ((and (eq (char-before (car bnds)) ?\()
             (eq (char-before (1- (car bnds))) ?$))
        (list (car bnds) (cdr bnds)
              `(,@(assq 'function company-makefile-data)
                ,@(assq 'implicit company-makefile-data)
                ,@(assq 'autovar company-makefile-data)
                ,@(company-makefile--dyn-vars))
              :annotation-function 'company-makefile--annotation
              :company-location 'company-makefile--location
              :company-docsig 'company-makefile--meta))
       ;; ${...} - local / auto / implicit vars
       ((and (eq (char-before (car bnds)) ?{)
             (eq (char-before (1- (car bnds))) ?$))
        (list (car bnds) (cdr bnds)
              `(,@(company-makefile--dyn-vars)
                ,@(assq 'implicit company-makefile-data)
                ,@(assq 'autovar company-makefile-data))))
       ;; targets
       ((company-makefile--target-p)
        (list (car bnds) (cdr bnds)
              (company-makefile--dyn-targets)
              :annotation-function 'company-makefile--annotation))
       ;; keywords
       (t
        (list (car bnds) (cdr bnds) (assq 'keyword company-makefile-data)
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
