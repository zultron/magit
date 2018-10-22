;;; magit-transient.el --- support for transients  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements Magit-specific prefix and suffix classes,
;; and their methods.

;;; Code:

(require 'transient)

;; TODO Actually define the classes and methods here.
;; For now some hacks (taken from magit-popup) instead:

(defun magit--import-file-args (args files)
  (if files
      (cons (concat "-- " (mapconcat #'identity files ",")) args)
    args))

(defun magit--export-file-args (args)
  (let ((files (--first (string-prefix-p "-- " it) args)))
    (when files
      (setq args  (remove files args))
      (setq files (split-string (substring files 3) ",")))
    (list args files)))

;;; _
(provide 'magit-transient)
;;; magit-transient.el ends here

