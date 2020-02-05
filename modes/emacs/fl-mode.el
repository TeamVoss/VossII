(setq lexical-binding t)

(defun fl-mode ()
  "Initialize fl-mode and (optionally) start an fl prompt."
  (interactive)
  (kill-all-local-variables)

  (fl-setup-syntax-highlighting)
  (use-local-map fl-mode-map)

  (when fl-autostart-repl (fl-start))
  (setq major-mode 'fl-mode)
  (setq mode-name "fl")
  (run-hooks 'fl-mode-hooks)
)

(defun fl-repl-mode ()
  "Syntax highlighting and keyboard shortcuts for the fl repl buffer."
  (interactive)
  (fl-repl-setup t)
)


;;;;; All config vars

(defvar fl-autostart-repl t)
(defvar fl-binary-path nil)
(defvar fl-repl-buffer "fl-output")
(defvar fl-repl-mode t)
(defvar fl-kill-delay 0.5)
(defvar fl-temp-directory "/tmp")
(defvar fl-mostly-headless t)
(defvar fl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-f\C-f" 'fl-send-smart)
    (define-key map "\C-f\C-b" 'fl-send-buffer)
    (define-key map "\C-f\C-e" 'fl-send)
    (define-key map "\C-f\C-s" 'fl-start)
    (define-key map "\C-f\C-q" 'fl-stop)
    (define-key map "\C-f\C-k" 'fl-kill)
    (define-key map "\C-f\C-r" 'fl-restart)
    (define-key map "\C-f\C-c" 'fl-clear)
    (define-key map "\C-f\C-h" 'fl-help)
    (define-key map "\C-f\C-d" 'fl-describe)
    (define-key map "\C-f\C-p" 'fl-preferences)
    (define-key map "\C-f\C-t" 'fl-toggle-window)
    map
  )
  "Keymap for fl-mode"
)
(defvar fl-mode-hooks nil)


;;;;; Internal bookkeeping vars

(defvar fl-process nil)
(defvar fl-file nil)
(defvar fl-window-hidden nil)


;;;;; Syntax highlighting

(defconst fl-keywords
  (regexp-opt
    '("andlettype" "add_open_overload" "assuming" "begin_abstype" "binder"
      "binder_with_accumulator" "clear_fixities" "clet" "cletrec" "end_abstype"
      "export_to_tcl" "forward_declare" "free_binder" "if_then_else_binder"
      "then_binder" "else_binder" "infix" "infix_unary" "infixr" "then" "in"
      "install_print_function" "let" "letrec" "lettype" "list" "new_type_abbrev"
      "non_lazy" "nonfix" "open_overload" "overload" "postfix" "prefix"
      "print_fixities" "ref" "val"
    )
    t
  )
)

(defconst fl-symbols
  (regexp-opt
    '("=>" "->" "|" "=" "#" "/\\" "\\" "::" ";" "." "," "(" ")" "{" "}" "[" "]")
    t
  )
)

(defconst fl-builtins
  (regexp-opt '("load" "Quant_thereis" "Quant_forall" "quit" "fseq"
                "IF" "THEN" "ELSE" "AND" "OR" "NOT" "XOR" "T" "F"))
)

(defconst fl-extra-operators
  (regexp-opt '("+" "-" "*" "/" "<-" "%" "--") t)
)

(defun fl-spaced-regex (regex)
  "Avoid highlighting the given regex when it occurs within a word."
  (concat "\\<" regex "\\>")
)

(defconst fl-mode-keywords
  (list
    ;; Keywords and quoted vars
    '("'\\([^' \t]+\\)'" . font-lock-variable-name-face)
    `(,(fl-spaced-regex fl-keywords) . font-lock-keyword-face)
    `(,fl-symbols . font-lock-keyword-face)
    '("'\\(\\w+\\)" . font-lock-variable-name-face)

    ;; Builtins
    `(,(fl-spaced-regex fl-builtins) . font-lock-builtin-face)
    `(,fl-extra-operators . font-lock-builtin-face)
  )
  "Syntax highlighting expressions for fl-mode."
)

(defvar fl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    table
  )
  "Syntax table for fl mode."
)

(defun fl-setup-syntax-highlighting ()
  (set-syntax-table fl-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(fl-mode-keywords))
  (setq comment-start "//")
  (setq comment-end "")
  )


;;;;; Utils

(defun fl-echo (msg)
  "Write the given text to the fl prompt buffer."
  (with-current-buffer fl-repl-buffer
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (insert (concat msg "\n"))
    (setq buffer-read-only t)
    (when fl-process (set-marker (process-mark fl-process) (point-max)))
  )
  (let ((window (get-buffer-window fl-repl-buffer)))
    (if window
      (with-selected-window window
        (goto-char (point-max))
      )
    )
  )
)

(defun remove-prompt-colon (start end oldlen)
  "Remove ':' prompt from the current buffer unless configured not to."
  (save-excursion
    (goto-char start)
    (setq buffer-read-only nil)
    (while (search-forward-regexp "^: " end t)
      (replace-match "")
    )
    (while (search-forward-regexp "^> " end t)
      (replace-match ": ")
    )
    (setq buffer-read-only t)
  )
)

(defun with-fl-path (expr)
  "Evaluate the given expression with the fl binary path (if defined) in PATH."
  (if fl-binary-path
    (progn
      (let ((old-path (getenv "PATH")))
        (setenv "PATH" (concat old-path ":" fl-binary-path))
        (setq exec-path (append exec-path `(,fl-binary-path)))
        (eval expr)
        (setenv "PATH" old-path)
        (setq exec-path (nbutlast exec-path 1))
      )
    )
    (eval expr)
  )
)


;;;;; fl process control

(defun fl-process-sentinel (process event)
  (delete-process process)
  (setq fl-process nil)
  (setq fl-file nil)
  (fl-echo "<fl process terminated>")
)

(defun fl-repl-setup (force)
  (when (or force fl-repl-mode) (fl-mode))
  (setq buffer-read-only t)
  (add-hook 'after-change-functions 'remove-prompt-colon t t)
)

(defun fl-start ()
  "Start an fl prompt. Does nothing if one is already running."
  (interactive)
  (when (not fl-process)
    (setq fl-file (concat fl-temp-directory "/" (make-temp-name "fl-")))
    (with-fl-path
      '(setq fl-process
        (start-process
          "fl-process"
          fl-repl-buffer
          "fl"
          "--read_input_from_file"
          fl-file
          "--use_stdout"
          (if fl-mostly-headless "--hide-window" "")
        )
      )
    )
    (with-current-buffer fl-repl-buffer (fl-repl-setup nil))
    (set-process-sentinel fl-process 'fl-process-sentinel)
    (setq fl-window-hidden fl-mostly-headless)
  )
)

(defun fl-stop ()
  "Stops the currently running fl prompt gracefully, if any."
  (interactive)
  (fl-send "quit;")
)

(defun fl-kill ()
  "Attempts to stop the fl process, then kills it forcefully is unsuccessful."
  (interactive)
  (fl-stop)
  (sit-for fl-kill-delay)
  (when fl-process (delete-process fl-process))
)

(defun fl-restart ()
  "Restart the currently running fl prompt. If no prompt is running, start one."
  (interactive)
  (fl-kill)
  (fl-start)
)

(defun with-fl (f)
  (if fl-process (eval f) (message "No fl session running"))
)


;;;;; Sending text to fl

(defun fl-send-buffer ()
  "Send the contents of the entire buffer to fl."
  (interactive)
  (fl-send (buffer-string))
  (message (concat "Buffer " (buffer-name) " sent to fl"))
)

(defun fl-send-region ()
  "Send the contents of the region to fl."
  (interactive)
  (fl-send (buffer-substring-no-properties (mark) (point)))
  (message "Region sent to fl")
)

(defun fl-send (s)
  "Sends the given line to fl."
  (interactive "sExpression to send: ")
  (with-fl '(progn
    (fl-echo (concat
      "> "
      (replace-regexp-in-string "\n" "\n>  " (string-trim-right s))
    ))
    (append-to-file s nil fl-file)
    (message "Line sent to fl")
  ))
)

(defun fl-send-line ()
  "Sends the contents of the current line to fl."
  (interactive)
  (fl-send (thing-at-point 'line t))
  (message "Line sent to fl")
)

(defun fl-help ()
  "Opens the help window."
  (interactive)
  (fl-send "open_help ();")
  (message "Opening fl help window")
)

(defun fl-toggle-window ()
  "Toggles the visibility of the fl interpreter window."
  (interactive)
  (if fl-window-hidden
      (fl-show-window)
      (fl-hide-window)
  )
)

(defun fl-hide-window ()
  "Hides the fl interpreter window."
  (interactive)
  (setq fl-window-hidden t)
  (fl-send "tcl_eval [\"wm withdraw .\"];")
)

(defun fl-show-window ()
  "Shows the fl interpreter window."
  (interactive)
  (setq fl-window-hidden nil)
  (fl-send "tcl_eval [\"wm deiconify .\"];")
)

(defun fl-preferences ()
  "Opens the preferences window."
  (interactive)
  (fl-send "open_preferences ();")
  (message "Opening fl preferences")
)

(defun fl-describe ()
  "Ask fl for help about the symbol at point."
  (interactive)
  (fl-send (concat "help \"" (thing-at-point 'symbol t) "\";"))
  (message "Asked fl for help")
)

(defun fl-send-smart ()
  "Sends the current selection to fl, if any, otherwise sends the current line."
  (interactive)
  (if (use-region-p) (fl-send-region) (fl-send-line))
)

(defun fl-clear ()
  "Clears the fl output buffer."
  (interactive)
  (with-current-buffer fl-repl-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
  )
  (message "Cleared the fl buffer")
)


;;;;; Finishing up

(add-to-list 'auto-mode-alist '("\\.fl\\'" . fl-mode))

(provide 'fl-mode)
