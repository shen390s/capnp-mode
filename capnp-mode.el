;;; capnp-mode.el --- Syntax highlighting for Cap'n'Proto files -*- lexical-binding: t; -*-
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

;; Define the keywords for Cap'n Proto
(defvar capnp-keywords
  '("struct" "union" "enum" "interface" "const" "annotation" "using" "extends"))

(defvar capnp-builtins
  '("Void" "Bool" "Text" "Data" "List" "Int8" "Int16" "Int32" "Int64"
    "UInt8" "UInt16" "UInt32" "UInt64" "Float32" "Float64" "union" "group"))

(defvar capnp-constants
  '("true" "false" "inf"))

;; Define comment syntax
(defvar capnp-comment-regexp "#.*$")

;; Define regex for types
(defvar capnp-type-regexp "\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)\\_>")

;; Define regex for numbers
(defvar capnp-number-regexp "\\_<[0-9]+\\_>")

;; Define regex for floating-point numbers
(defvar capnp-float-regexp "\\_<[0-9]+\\.[0-9]*\\([eE][-+]?[0-9]+\\)?\\_>")

;; Define the syntax table
(defvar capnp-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; '#' starts a comment
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

;; Define the font lock (syntax highlighting) rules
(defvar capnp-font-lock-keywords
  `((,(regexp-opt capnp-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt capnp-builtins 'words) . font-lock-type-face)
    (,(regexp-opt capnp-constants 'words) . font-lock-constant-face)
    (,capnp-type-regexp . font-lock-variable-name-face)
    (,capnp-number-regexp . font-lock-constant-face)
    (,capnp-float-regexp . font-lock-constant-face)
    (,capnp-comment-regexp . font-lock-comment-face)))

;; Define indentation rules (simple for now)
(defun capnp-indent-line ()
  "Indent current line of Cap'n Proto code."
  (interactive)
  (let ((indent-level 2)
        (not-indented t)
        cur-indent)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*\\(}\\|\\)\\)") ;; closing braces
          (setq cur-indent (- (current-indentation) indent-level))
        (if (looking-at "^[ \t]*{") ;; opening braces
            (setq cur-indent (+ (current-indentation) indent-level))))
      (if cur-indent
          (indent-line-to (max cur-indent 0))
        (indent-line-to 0))))
  ;; If the point is before the current indentation, move it to the indentation.
  (if (< (current-column) (current-indentation))
      (move-to-column (current-indentation))))

;; Define the mode itself
(define-derived-mode capnp-mode prog-mode "Cap'n Proto"
  "Major mode for editing Cap'n Proto schema files."
  :syntax-table capnp-mode-syntax-table
  (setq-local font-lock-defaults '((capnp-font-lock-keywords)))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'capnp-indent-line))

;; Automatically use capnp-mode for Cap'n Proto files
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))


(provide 'capnp-mode)
;;; capnp-mode.el ends here
