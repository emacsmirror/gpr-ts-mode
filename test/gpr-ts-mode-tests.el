;;; gpr-ts-mode-tests.el --- Tests for Tree-sitter-based GNAT Project mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Troy Brown

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ert-font-lock nil 'noerror) ; Emacs 30+
(require 'ert-x)
(require 'gpr-ts-mode)
(require 'treesit)
(require 'which-func)

(defun default-transform (&optional expect-error)
  "Default transform function for test.

If EXPECT-ERROR is non-nil, then check for an error in the parse tree,
otherwise check that there is no error in the parse tree."
  (gpr-ts-mode)
  (if expect-error
      (should (treesit-search-subtree
               (treesit-buffer-root-node) "ERROR"))
    (should (not (treesit-search-subtree
                  (treesit-buffer-root-node) "ERROR")))))

(defun defun-transform (name)
  "Defun NAME transform function for test."
  (default-transform)
  (should (string-equal (which-function) name)))

(defun filling-transform ()
  "Filling transform function for test."
  (default-transform)
  (fill-paragraph))

(defun indent-transform (&optional setup expect-error)
  "Indentation transform function for test.

SETUP can be used to perform custom initialization.  If EXPECT-ERROR is
non-nil, then check for an error in the parse tree, otherwise check that
there is no error in the parse tree."
  (default-transform expect-error)
  (setq-local indent-tabs-mode nil)
  (when setup
    (funcall setup))
  (goto-char (point-min))
  (cl-flet ((line-length () (- (line-end-position)
                               (line-beginning-position))))
    (while (not (eobp))
      (if (> (line-length) 0)
          (if (= (following-char) ?\s)
              (while (and (> (line-length) 0)
                          (= (following-char) ?\s))
                (delete-char 1))
            (insert-char ?\s)))
      (forward-line 1)
      (beginning-of-line)))
  (indent-region (point-min) (point-max)))

(defun mode-transform (&optional version)
  "Mode transform function for test.

If VERSION is nil, the expected mode is \\='gpr-ts-mode\\='.  If VERSION
is not nil, for an Emacs major version at or above VERSION, the expected
mode is \\='gpr-ts-mode\\=', otherwise the expected mode is
\\='fundamental-mode\\='."
  (let ((inhibit-message t) ; Suppress 'Ignoring unknown mode ...'.
        (expected-mode
         (cond (version
                (if (>= emacs-major-version version)
                    'gpr-ts-mode
                  'fundamental-mode))
               (t 'gpr-ts-mode))))
    (set-auto-mode)
    (should (eq major-mode expected-mode))))

(defun navigation-transform (binding &optional arg)
  "Navigation transform function for test.

Use BINDING to navigate with optional prefix ARG."
  (default-transform)
  (let ((current-prefix-arg arg))
    (call-interactively (key-binding (kbd binding)))))

(defun newline-transform (&optional expect-error)
  "Newline transform function for test.

If EXPECT-ERROR is non-nil, then check for an error in the parse tree,
otherwise check that there is no error in the parse tree."
  (default-transform expect-error)
  (setq-local indent-tabs-mode nil)
  (call-interactively #'newline))

(dolist (file (directory-files (ert-resource-directory)
                               nil
                               directory-files-no-dot-files-regexp))
  (let* ((file-noext (file-name-sans-extension file))
         (file-path (ert-resource-file file))
         (transform (cond ((string-suffix-p "-nl" file-noext) #'newline-transform)
                          ((string-prefix-p "filling" file-noext) #'filling-transform)
                          ((string-prefix-p "indent" file-noext) #'indent-transform)
                          (t #'default-transform))))
    (if (string-prefix-p "font-lock" file-noext)
        (eval `(ert-deftest ,(intern (concat "gpr-ts-mode-test-" file-noext)) ()
                 (skip-unless (featurep 'ert-font-lock))
                 (with-temp-buffer
                   (insert-file-contents ,file-path)
                   (funcall #',transform))
                 ;; Force full fontification
                 (let ((treesit-font-lock-level 4))
                   (ert-font-lock-test-file ,file-path 'gpr-ts-mode))))
      (eval `(ert-deftest ,(intern (concat "gpr-ts-mode-test-" file-noext)) ()
               (ert-test-erts-file ,file-path #',transform))))))

(provide 'gpr-ts-mode-tests)

;;; gpr-ts-mode-tests.el ends here
