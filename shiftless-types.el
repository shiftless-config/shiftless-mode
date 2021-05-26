(require 'cl-lib)
(require 'dash)

(defun shiftless::atom-predicate (obj)
  (or (symbolp obj)
      (numberp obj)
      (stringp obj)))

;;; symbol
(cl-defmethod shiftless::atom-string ((atom symbol))
  (symbol-name atom))

(cl-defmethod shiftless::atom-symbol ((atom symbol))
  atom)

(cl-defmethod shiftless::value ((atom symbol))
  atom)

(cl-defmethod shiftless::atom-boolean ((atom symbol))
  atom)

(cl-defmethod shiftless::atom-integer ((atom symbol))
  (error "symbol cannot be converted to integer"))

(cl-defmethod shiftless::atom-float ((atom symbol))
  (error "symbol cannot be converted to float"))

;;; number
(cl-defmethod shiftless::atom-symbol ((atom number))
  (error "number cannot be converted to symbol"))

(cl-defmethod shiftless::value ((atom number))
  atom)

(cl-defmethod shiftless::atom-boolean ((atom number))
  (declare (ignore atom))
  t)

(cl-defmethod shiftless::atom-integer ((atom number))
  (round atom))

(cl-defmethod shiftless::atom-float ((atom number))
  (float atom))

(cl-defmethod shiftless::atom-string ((atom number))
  (prin1-to-string atom))

;;; string
(defun shiftless::stringp (string)
  (and (stringp string)
       (< 0 (length string))
       (= ?' (elt string 0))))

(cl-defmethod shiftless::value ((atom string))
  (if (shiftless::stringp atom)
      (shiftless::make-atom atom)
    atom))

(cl-defmethod shiftless::atom-symbol ((atom string))
  (if (shiftless::stringp atom)
      (shiftless::atom-symbol (shiftless::make-atom atom))
    (intern (downcase atom))))

(cl-defmethod shiftless::atom-boolean ((atom string))
  (if (shiftless::stringp atom)
      t
    (shiftless::atom-boolean (shiftless::make-atom atom))))

(cl-defmethod shiftless::atom-integer ((atom string))
  (if (shiftless::stringp atom)
      (shiftless::atom-integer (shiftless::make-atom atom))
    (round (string-to-number atom))))

(cl-defmethod shiftless::atom-float ((atom string))
  (if (shiftless::stringp atom)
      (shiftless::atom-float (shiftless::make-atom atom))
    (float (string-to-number atom))))

(cl-defmethod shiftless::atom-string ((atom string))
  (if (shiftless::stringp atom)
      (shiftless::make-atom atom)
    atom))

(provide 'shiftless-types)
;;; shiftless-types.el ends here
