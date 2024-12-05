;;; capnp-mode.el --- Major mode for editing Cap'n Proto files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Rohit Goswami (HaoZeke)
;;
;; Author: Rohit Goswami (HaoZeke) <rgoswami[at]inventati[dot]org>
;; Maintainer: Rohit Goswami (HaoZeke) <rgoswami[at]inventati[dot]org>
;; Created: October 08, 2024
;; Modified: October 08, 2024
;; Version: 0.0.1
;; Keywords: languages, faces
;; Homepage: https://github.com/HaoZeke/capnp-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; translation of keywords and highlighting from vim [1] and [2] textmate
;; [1] https://github.com/cstrahan/vim-capnp
;; [2] https://github.com/textmate/capnproto.tmbundle/blob/master/Syntaxes/Cap%E2%80%99n%20Proto.tmLanguage

;; Put this in your .emacs file to enable autoloading of capnp-mode
;; and auto-recognition of "*.capnp" files:
;;
;; (autoload 'capnp-mode "capnp-mode.el" "CAPNP mode." t)
;; (setq auto-mode-alist (append auto-mode-alist
;;                               '(("\\.capnp\\'" . capnp-mode))
;;                               ))
;;
;;
;;
;;; Code:

;; Define keywords for Cap'n Proto
(defvar capnp-keywords
  '("struct" "union" "enum" "interface" "const" "annotation" "using" "extends"))

;; Define built-in types for Cap'n Proto
(defvar capnp-builtins
  '("Void" "Bool" "Text" "Data" "List" "Int8" "Int16" "Int32" "Int64"
    "UInt8" "UInt16" "UInt32" "UInt64" "Float32" "Float64" "union" "group"))

;; Define constants (e.g., boolean literals) for Cap'n Proto
(defvar capnp-constants
  '("true" "false" "inf"))

;; Define regex for comments (single line starting with #)
(defvar capnp-comment-regexp "#.*$")

;; Define regex for types (starting with a letter or underscore)
(defvar capnp-type-regexp "\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)\\_>")

;; Define regex for numbers
(defvar capnp-number-regexp "\\_<[0-9]+\\_>")

;; Define regex for floating-point numbers
(defvar capnp-float-regexp "\\_<[0-9]+\\.[0-9]*\\([eE][-+]?[0-9]+\\)?\\_>")

;; Define regex for Cap'n Proto unique IDs (e.g., @0xbd1f89fa17369103)
(defvar capnp-unique-id-regexp "@0x[0-9A-Fa-f]+\\b")

;; Extend keywords to include annotation-related targets
(defvar capnp-annotation-targets
  '("file" "struct" "field" "union" "group" "enum" "enumerant" "interface" "method" "param" "annotation" "const" "*"))

;; Define regex for annotations (e.g., $foo("bar"))
(defvar capnp-annotation-regexp "\\([$]\\w+\\)(\\([^)]+\\))?")

;; Define syntax table to manage comments
(defvar capnp-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; '#' starts a comment
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

;; Define font lock (syntax highlighting) rules
(defvar capnp-font-lock-keywords
  `((,(regexp-opt capnp-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt capnp-builtins 'words) . font-lock-type-face)
    (,(regexp-opt capnp-constants 'words) . font-lock-constant-face)
    (,capnp-type-regexp . font-lock-variable-name-face)
    (,capnp-number-regexp . font-lock-constant-face)
    (,capnp-float-regexp . font-lock-constant-face)
    (,capnp-unique-id-regexp . font-lock-constant-face)
    (,capnp-comment-regexp . font-lock-comment-face)
    (,capnp-annotation-regexp . ((1 font-lock-preprocessor-face) (2 font-lock-string-face)))
    (,(regexp-opt capnp-annotation-targets 'words) . font-lock-builtin-face)))

;; Define simple indentation rules (indent by 2 spaces)
(defun capnp-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((cur-indent
	   (save-excursion
	     (progn
	       (setq need-search t)
	       (while need-search
		 (forward-line -1)
		 (unless (looking-at "^$")
		   (setq need-search nil))))
	     
	     (let ((pos (current-indentation)))
	       (if (looking-at "^.*{.*$")
		   (progn
		     ;;(message "found { at line %d\n" (line-number-at-pos))
		     (setq pos (+ pos 2)))
		 pos)))))
      (when (looking-at "^[ \t]*}")
	;;(message "found } at line %d\n" (line-number-at-pos))
	(setq cur-indent (- cur-indent 2)))
      ;;(message "indent pos %d @ line %d\n"
      ;;       cur-indent (line-number-at-pos))
      (if (> cur-indent 0)
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

;; Define the major mode itself
;;;###autoload
(define-derived-mode capnp-mode prog-mode "Cap'n Proto"
  "Major mode for editing Cap'n Proto schema files."
  :syntax-table capnp-mode-syntax-table
  (setq-local font-lock-defaults '((capnp-font-lock-keywords)))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'capnp-indent-line))

;; Automatically use capnp-mode for .capnp files
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

(provide 'capnp-mode)
;;; capnp-mode.el ends here
