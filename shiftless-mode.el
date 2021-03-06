(require 'rx)
(require 'cl-lib)
(require 'files)
(require 'hideshow)

(defgroup shiftless ()
  "Group for `shiftless-mode' customization.")

(defcustom shiftless:indent-level 2
  "The number of spaces to indent the line after a left bracket followed by a new line.
key = [
<indent-level number of spaces>next-key"
  :type 'integer
  :group 'shiftless)

(defvar shiftless:syntax-table
  (let ((st (make-syntax-table)))
    ;; ()
    (modify-syntax-entry 40 "_" st)
    (modify-syntax-entry 41 "_" st)
    ;; {}
    (modify-syntax-entry 123 "_" st)
    (modify-syntax-entry 124 "_" st)
    ;; "
    (modify-syntax-entry 34 "_" st)
    ;; '
    (modify-syntax-entry 39 "\"" st)
    ;; ; comment
    (modify-syntax-entry 59 "<" st)
    (modify-syntax-entry 10 "> " st)
    ;; .
    (modify-syntax-entry 46 "'" st)
    ;; -
    (modify-syntax-entry 45 "." st)
    st)
  "Syntax table for `shiftless-mode'")

(defvar shiftless:font-lock-keywords
  (rx-let ((edge (or " " "\n" "\t" "\r" "[" "]"))
           (assoc (seq (one-or-more (or whitespace "\n"))
                       "="
                       (one-or-more (or whitespace "\n")))))
    (rx-let ((property-list (seq "["
                                 (group-n 1
                                          (+? (not edge))
                                          (*? (+? edge)
                                              (+? (not edge))))
                                 "]")))
      (list
       ;; discouraged characters
       (cons (rx (or "_" upper-case))
             'font-lock-warning-face)
       ;; equal signs that are not symbols missing spaces around them
       (cons (rx (or (seq (one-or-more (not whitespace) (group-n 1 "=")))
                     (seq (group-n 1 "=") (one-or-more (not whitespace)))))
             '(1 font-lock-warning-face))
       ;; t and nil
       (cons (rx word-boundary
                 (regex (eval-when-compile
                          (regexp-opt '("t" "nil")
                                      "\\(?1:")))
                 word-boundary)
             '(1 font-lock-builtin-face))
       (cons (rx "[]")
             'font-lock-builtin-face)
       ;; numbers
       (cons (rx edge
                 (group (? "-") (one-or-more digit)
                        (? "." (one-or-more digit)))
                 edge)
             '(1 font-lock-constant-face))
       ;; properties
       (cons (rx (or edge line-start)
                 (group-n 1 (one-or-more (not edge)))
                 assoc)
             '(1 font-lock-variable-name-face))
       ;; sequenced properties
       (cons (rx property-list assoc)
             '(1 font-lock-variable-name-face t))
       ;; previous property dot
       (cons (rx "." property-list)
             '(0 font-lock-function-name-face prepend))
       ;; escape codes
       (cons (rx (group-n 1 "\\" (or "\\" "'")))
             '(1 font-lock-builtin-face t)))))
  "Default font-lock keywords for shiftless.")

(defvar shiftless-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-l") 'shiftless:insert-list)
    (define-key m (kbd "C-c C-a") 'shiftless:insert-association)
    (define-key m (kbd "C-c C-m") 'shiftless:insert-tag)
    (define-key m (kbd "C-c C-v") 'shiftless:magic-insert-value)
    m)
  "Keymap for `shiftless-mode'.")

(defcustom shiftless-mode-hook (list)
  "List of functions to be run after entering `shiftless-mode'."
  :type 'list
  :group 'shiftless)

(defun shiftless:calculate-indent-line ()
  "Calculate what column a line should be indented to in `shiftless-mode'.
Does not move point."
  (save-excursion
    (beginning-of-line)
    (while (and (/= 91 (or (char-before) 0)) ;[
                (not (bobp)))
      (backward-char)
      (condition-case nil
          (backward-sexp)
        (t (skip-chars-backward " \t\r\n"))))
    (if (and (not (bobp))
             (looking-at (rx (zero-or-more whitespace)
                             line-end)))
        (+ (current-indentation)
           shiftless:indent-level)
      (- (point) (progn (beginning-of-line)
                        (point))))))

(defun shiftless:indent-line ()
  "Indent a line in `shiftless-mode'."
  (let ((point (point)))
    (indent-line-to (max 0 (shiftless:calculate-indent-line)))
    (when (< (point) point)
      (goto-char point))))

(defun shiftless::escape-string (string)
  (replace-regexp-in-string
   (rx "'") "\\\\'"
   (replace-regexp-in-string
    (rx "\\") "\\\\\\\\" string)))

(defun shiftless:magic-insert-value (value)
  (cond
   ((equal "a" value)
    (shiftless:insert-association 1))
   ((equal "l" value)
    (shiftless:insert-list 1))
   ((equal "m" value)
    (shiftless:insert-tag))
   ((or (and (seq-contains-p value 32)
             (not (= 46 (elt value 0))) ;.
             (not (= 91 (elt value 1)))) ;[
        (seq-contains-p value 39)        ;'
        (not (equal value (downcase value))))
    (insert "'" (shiftless::escape-string value) "'"))
   (:else
    (insert value))))

(defun shiftless:insert-list (top-level)
  (interactive "p")
  (when (/= 4 top-level)
    (insert "[]")
    (forward-char -1))
  (cl-loop
   for first-time = t then nil
   for value = (read-from-minibuffer "Value: ")
   when (string-empty-p value) return nil
   if (not first-time)
   do (insert (if (= 16 top-level) " " "\n"))
   do (shiftless:indent-line)
   do (shiftless:magic-insert-value value))
  (when (= 32 (char-before))            ;space
    (delete-char -1))
  (when (/= 4 top-level)
      (forward-char)))

(defun shiftless:insert-association (top-level)
  (interactive "p")
  (when (/= 4 top-level)
    (insert "[]")
    (forward-char -1))
  (cl-loop
   for first-time = t then nil
   for key = (read-from-minibuffer "Key: ")
   when (string-empty-p key) return nil
   do (insert (if (= 16 top-level)
                  (if first-time "" " ")
                "\n"))
   do (shiftless:indent-line)
   do (if (seq-contains-p key 32)       ;space
          (insert "[" key "]")
        (insert key))
   do (insert " = ")
   do (shiftless:magic-insert-value
       (read-from-minibuffer "Value: ")))
  (when (/= 4 top-level)
      (forward-char)))

(defun shiftless:insert-tag ()
  (interactive)
  (insert "[]")
  (forward-char -1)
  (let ((tag-name (read-from-minibuffer "Tag Name: ")))
    (unless (string-empty-p tag-name)
      (insert tag-name " ")
      (shiftless:insert-association 16)
      (when (looking-back (rx "[]"))
        (delete-char -3))
      (insert "\n")
      (shiftless:indent-line)
      (shiftless:insert-list 4)
      (while (member (char-before) '(32 ?\r ?\n ?\t))
        (delete-char -1))
      (forward-char))))

;;;###autoload
(define-derived-mode shiftless-mode
  prog-mode
  "Shiftless"
  "Major mode for editing shiftless configuration files."
  :group 'shiftless
  :syntax-table shiftless:syntax-table
  (setq-local font-lock-defaults '(shiftless:font-lock-keywords))
  (setq-local comment-start "; ")
  (setq-local indent-line-function 'shiftless:indent-line)
  (run-hooks shiftless-mode-hook))

(add-to-list 'auto-mode-alist '("\\.slc\\'" . shiftless-mode))
(add-to-list 'hs-special-modes-alist '(shiftless-mode "\\[" "\\]" ";" nil nil))

(provide 'shiftless-mode)
;;; shiftless.el ends here
