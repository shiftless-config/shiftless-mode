(require 'dash)
(require 'cl-lib)

(require 'shiftless-load)

(defcustom shiftless:print-indent 2
  "The number of spaces to put as indentation when outputing shiftless format."
  :group 'shiftless)

(defvar shiftless::*print-current-indent* 0)

(defcustom shiftless:print-newline nil
  "The string to use as newline when outputing shiftless format.
When `nil', do not output newlines."
  :group 'shiftless)

(defun shiftless::output-indent ()
  (cl-loop repeat shiftless::*print-current-indent*
           do (insert " ")))

(defun shiftless::alistp (obj)
  (and (listp obj)
       (-every-p
        (lambda (item)
          (and (consp item)
               (or (symbolp (car item))
                   (and (stringp item)
                        (symbolp (shiftless::make-atom (car item)))))))
        obj)))

(defun shiftless::remove-extra-whitespace ()
  (while (member (or (char-before) 0)
                 '(32 ?\t ?\r ?\n))
    (delete-char -1)))

(defun shiftless::output-key-value (key value)
  (if (and (shiftless::alistp value)
           (= 1 (length value)))
      (let ((cell (car value)))
        (shiftless::output-key-value
         (append
          (if (listp key)
              key
            (list key))
          (if (listp (car cell))
              (car cell)
            (list (car cell))))
         (cdr cell)))
    (shiftless::output key)
    (insert " = ")
    (shiftless::output value)))

(defun shiftless::output-alist (alist)
  (insert "[]") (forward-char -1)
  (let ((shiftless::*print-current-indent*
         shiftless::*print-current-indent*))
    (if (and shiftless:print-newline
             (/= 1 (length alist)))
        (progn
          (insert shiftless:print-newline)
          (cl-incf shiftless::*print-current-indent*
                   shiftless:print-indent)
          (shiftless::output-indent))
      (when (and shiftless:print-newline
                 (not (consp (cdr (car (cdr alist))))))
        (cl-incf shiftless::*print-current-indent*)))
    (cl-loop
     for (key . value) in alist
     do (shiftless::output-key-value key value)
     do (if (not shiftless:print-newline)
            (insert " ")
          (insert shiftless:print-newline)
         (shiftless::output-indent)))
    (shiftless::remove-extra-whitespace)
    (forward-char)))

(cl-defmethod shiftless::output ((struct null))
  (insert "nil"))

(cl-defmethod shiftless::output ((struct cons))
  (if (shiftless::alistp struct)
      (shiftless::output-alist struct)
    (insert "[]") (forward-char -1)
    (let ((shiftless::*print-current-indent*
           shiftless::*print-current-indent*)
          (newline (-some 'consp struct)))
      (if (and shiftless:print-newline
               newline
               (< 1 (length struct)))
          (progn
            (insert shiftless:print-newline)
            (cl-incf shiftless::*print-current-indent*
                     shiftless:print-indent)
            (shiftless::output-indent))
        (when shiftless:print-newline
          (cl-incf shiftless::*print-current-indent*)))
      (cl-loop
       for item in struct
       do (shiftless::output item)
       do (if (or (not shiftless:print-newline)
                  (not newline))
              (insert " ")
            (insert shiftless:print-newline)
            (shiftless::output-indent)))
      (shiftless::remove-extra-whitespace)
      (forward-char))))

(cl-defmethod shiftless::output ((struct string))
  (insert struct))

(cl-defmethod shiftless::output (struct)
  (insert (prin1-to-string struct)))

(defun shiftless:stringify (data &optional newline indent)
  (let ((shiftless:print-newline (or newline shiftless:print-newline))
        (shiftless:print-indent (or indent shiftless:print-indent 2)))
    (with-temp-buffer
    (shiftless::output data)
    (buffer-substring-no-properties (point-min) (point-max)))))

(defun shiftless:dump (data file &optional newline indent)
  (let ((shiftless:print-newline (or newline shiftless:print-newline "\n"))
        (shiftless:print-indent (or indent shiftless:print-indent 2)))
    (with-temp-file file
      (shiftless::output data))))

(provide 'shiftless-stringify)
;;; shiftless-stringify.el ends here

