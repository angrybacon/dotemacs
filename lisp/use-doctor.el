;;; use-doctor.el --- Health and settings check-up   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun me/doctor-error (format &rest arguments)
  "Signal an error with `user-error' using FORMAT and ARGUMENTS."
  (apply #'user-error (concat "[Doctor] " format) arguments))

(let ((current emacs-version)
      (target "30"))
  (when (version< current target)
    (me/doctor-error "Emacs %s is not supported. Install %s" current target)))

(unless (functionp 'json-serialize)
  (me/doctor-error "Native JSON parsing is not enabled"))

(let ((value read-process-output-max))
  (unless (> value 4096)
    (me/doctor-error "`read-process-output-max' is too low: %s" value)))

(let ((value gc-cons-threshold))
  (unless (> value 800000)
    (me/doctor-error "`gc-cons-threshold' is too low: %s" value)))

(unless (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  (me/doctor-error "Native compilation is not enabled"))

(unless (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  (me/doctor-error "Tree-sitter features are not available"))

;;; use-doctor.el ends here
