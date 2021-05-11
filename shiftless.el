(require 'rx)
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
  (rx-let ((edge (or " " "\n" "\t" "\r" "[" "]")))
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
       ;; t and nil
       (cons (rx edge
                 (regex (eval-when-compile
                          (regexp-opt '("t" "nil" "[]")
                                      "\\(?1:")))
                 edge)
             '(1 font-lock-builtin-face))
       ;; numbers
       (cons (rx edge
                 (group (? "-") (one-or-more digit)
                        (? "." (one-or-more digit)))
                 edge)
             '(1 font-lock-constant-face))
       ;; properties
       (cons (rx (or edge string-start)
                 (group-n 1 (one-or-more (not edge)))
                 (one-or-more whitespace) "=" (one-or-more whitespace))
             '(1 font-lock-variable-name-face))
       ;; sequenced properties
       (cons (rx property-list
                 (one-or-more whitespace) "=" (one-or-more whitespace))
             '(1 font-lock-variable-name-face t))
       ;; previous property dot
       (cons (rx "." property-list)
             '(0 font-lock-function-name-face prepend))
       ;; escape codes
       (cons (rx (group-n 1 "\\" (or "\\" "'")))
             '(1 font-lock-builtin-face t)))))
  "Default font-lock keywords for shiftless.")

(defcustom shiftless-mode-map
  (let ((m (make-sparse-keymap)))
    m)
  "Keymap for `shiftless-mode'."
  :type 'keymap
  :group 'shiftless)

(defcustom shiftless-mode-hook (list)
  "List of functions to be run after entering `shiftless-mode'."
  :type 'list
  :group 'shiftless)

(defun shiftless:calculate-indent-line ()
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        0
      (while (not (looking-back (rx "[")))
        (while (/= 91 (or (char-before) 0)) ;[
          (backward-char)
          (condition-case nil
              (backward-sexp)
            (t (skip-chars-backward " \t\r\n")))))
      (if (looking-at (rx (zero-or-more whitespace) line-end))
          (+ (current-indentation)
             shiftless:indent-level)
        (- (point) (progn (beginning-of-line)
                          (point)))))))

(defun shiftless:indent-line ()
  (indent-line-to (max 0 (shiftless:calculate-indent-line))))

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

(provide 'shiftless)
;;; shiftless.el ends here
