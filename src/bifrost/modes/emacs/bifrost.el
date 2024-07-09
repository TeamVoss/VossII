;;; bifrost --- A small major mode for the Bifröst language. -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;; For now contains only syntax highlighting.


;;; Code:

;; Custom groups

(defgroup bifrost nil
  "Support for Bifrost."
  :group 'languages)



;; Syntax highlighting

(defconst bifrost-keywords
  (regexp-opt '("define" "action" "subroutine" "by" "name" "provided" "via"
				"for" "do" "while" "if" "then" "else" "return" "protocol"
				"var" "call" "label" "type" "actiontype" "fields" "state")
			  'words))

(defconst bifrost-scissors
  (regexp-opt '("--%<--") 'symbols))

(defconst bifrost-preproc-statements
  (regexp-opt '("#include") 'symbols))

(defconst bifrost-operators
  (regexp-opt '("-->" "^" ";" ":" "->" "=>") t))

(defconst bifrost-typed-decl
  "\\<\\([[:word:]]+\\):\\([[:word:]]+\\)\\>")

(defvar bifrost-highlights
  `((,bifrost-keywords . font-lock-keyword-face)
	(,bifrost-scissors . font-lock-builtin-face)
	(,bifrost-preproc-statements . font-lock-preprocessor-face)
	(,bifrost-operators . font-lock-builtin-face)
	(,bifrost-typed-decl . (2 font-lock-type-face))
	("\\<subroutine \\([[:word:]]+\\) :" . (1 font-lock-function-name-face))
	("\\<var \\([[:word:]]+\\) " . (1 font-lock-type-face))))

(defvar bifrost-mode-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?/ ". 12" st)
	(modify-syntax-entry ?\n ">" st)
	(modify-syntax-entry ?# "_" st)
  st))


;; Mode

(define-derived-mode bifrost-mode prog-mode "Bifrost"
  "Major mode for Bifröst."
  (set (make-local-variable 'font-lock-defaults) '(bifrost-highlights))
  (set-syntax-table bifrost-mode-syntax-table)
  (setq comment-start "//")
  (setq comment-end ""))

(add-to-list 'auto-mode-alist '("\\.bfst\\'" . bifrost-mode))


(provide 'bifrost)
;;; bifrost.el ends here
