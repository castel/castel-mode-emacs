;;
;;

(defvar castel-keywords
  '("and"
    "else"
    "function"
    "if"
    "or"
    "return"
    "var")
  "Castel keywords.")

(defvar castel-keywords-regexp
  (regexp-opt castel-keywords 'words)
  "Castel keywords regular expression.")

;;
;;

(defun castel-indent-line ()
  "Indent current line."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent (castel-previous-indent))
            (cur-indent (current-indentation)))
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char castel-tab-width))

        (when (> (- (current-indentation) prev-indent) castel-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun castel-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (and (looking-at "^[ \t]*$") (not (bobp))) (forward-line -1))
        (current-indentation)))))

(defun castel-newline-and-indent ()
  "Insert a newline and indent it to the same level as the previous line."
  (interactive)

  (let ((prev-indent (current-indentation)) (indent-next nil))
    (delete-horizontal-space t)
    (newline)
    (insert-tab (/ prev-indent castel-tab-width))

    (when (castel-line-wants-indent)
      (insert-tab))))

(defun castel-line-wants-indent ()
  "Return t if the current line should be indented relative to the previous line."
  (interactive)

  (save-excursion
    (let ((indenter-at-bol) (indenter-at-eol))
      (forward-line -1)
      (end-of-line)

      (char-equal (char-before) ?:))))

;;
;;

(defvar castel-syntax-table
  nil
  "Syntax table for `castel-mode'.")

(setq castel-syntax-table
  (let ((synTable (make-syntax-table)))

    (modify-syntax-entry ?# "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)

  synTable)
)

;;
;;

(defvar castel-mode-map (make-keymap)
  "Keymap for CoffeeScript major mode.")

(define-derived-mode castel-mode fundamental-mode
  "castel mode"
  "Major mode for editing Castel source files"
  :syntax-table castel-syntax-table

  (define-key castel-mode-map "\C-m" 'castel-newline-and-indent)

  (setq castel-font-lock-keywords `(
    (, castel-keywords-regexp . font-lock-keyword-face)
  ))

  (setq font-lock-defaults '(castel-font-lock-keywords))

  (setq indent-tabs-mode t)
  (setq indent-line-function 'castel-indent-line)
)

(provide 'castel-mode)
