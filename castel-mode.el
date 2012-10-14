;;
;;

(defvar castel-keywords
  '("and"
    "class"
    "constructor"
    "exists"
    "export"
    "list"
    "dict"
    "else"
    "function"
    "if"
    "import"
    "new"
    "not"
    "operator"
    "or"
    "private"
    "protected"
    "public"
    "return"
    "until"
    "var"
    "while")
  "Castel keywords.")

(defvar castel-predefined-variables
  '("self"
    "null"
    "undefined")
  "Castel predefined variables.")

(defvar castel-keywords-regexp
  (regexp-opt castel-keywords 'words)
  "Castel keywords regular expression.")

(defvar castel-predefined-variables-regexp
  (regexp-opt castel-predefined-variables 'words)
  "Castel predefined variables regular expression.")

(defvar castel-descriptors-regexp
  "@.*"
  "Castel descriptors regexp.")

(defvar castel-identifiers-regexp
  "[a-zA-Z][a-zA-Z0-9_]*"
  "Castel identifiers regular expression.")

(defvar castel-white-spaces-regexp
  "\s"
  "Castel white spaces regular expression.")

(defvar castel-class-names-regexp
  (concat "class"castel-white-spaces-regexp"+\\("castel-identifiers-regexp"\\)")
  "Castel class names regular expression.")

(defvar castel-function-regexp
  (concat "\\(?:constructor\\|\\(?:function\\|operator\\)\\(?:"castel-white-spaces-regexp"+\\("castel-identifiers-regexp"\\)\\)?\\)\\(?:"castel-white-spaces-regexp"*\\(\\(..."castel-white-spaces-regexp"*\\)?"castel-identifiers-regexp"\\)"castel-white-spaces-regexp"*[,:]\\)+")
  "Castel function parameters regular expression.")

(defvar castel-types-regexp
  (concat "\\(?:\\(?:public\\|protected\\|private\\)"castel-white-spaces-regexp"+\\)?\\("castel-identifiers-regexp"\\)"castel-white-spaces-regexp"+"castel-identifiers-regexp)
  "Castel types regular expression.")

(defvar castel-variables-declarations-regexp
  (concat "\\(?:var\\|public\\|protected\\|private\\)"castel-white-spaces-regexp"+\\(?:"castel-identifiers-regexp""castel-white-spaces-regexp"+\\)*\\("castel-identifiers-regexp"\\)")
  "Castel variables declarations regular expression.")

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
          (forward-char tab-width))

        (when (> (- (current-indentation) prev-indent) tab-width)
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
    (insert-tab (/ prev-indent tab-width))

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
  "Keymap for Castel major mode.")

(define-derived-mode castel-mode fundamental-mode
  "castel mode"
  "Major mode for editing Castel source files"
  :syntax-table castel-syntax-table

  (define-key castel-mode-map "\C-m" 'castel-newline-and-indent)

  (setq castel-font-lock-keywords `(
    (, castel-descriptors-regexp . font-lock-warning-face)
    (, castel-keywords-regexp . font-lock-keyword-face)
    (, castel-predefined-variables-regexp . font-lock-constant-face)
    (, castel-class-names-regexp 1 font-lock-function-name-face)
    (, castel-function-regexp 1 font-lock-function-name-face)
    (, castel-function-regexp 2 font-lock-variable-name-face)
    (, castel-types-regexp 1 font-lock-type-face)
    (, castel-variables-declarations-regexp 1 font-lock-variable-name-face)
   ))

  (setq font-lock-defaults '(castel-font-lock-keywords))

  (setq indent-tabs-mode t)
  (setq indent-line-function 'castel-indent-line)
  (setq tab-width 2)
)

(provide 'castel-mode)
