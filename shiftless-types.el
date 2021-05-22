(require 'eieio)
(require 'cl-lib)
(require 'dash)

(defclass shiftless::atom ()
  ((original-string :type string
                    :initarg :original
                    :initform ""
                    :reader shiftless::atom-string)))

(defun shiftless::atom-predicate (obj)
  (or (symbolp obj)
      (numberp obj)
      (stringp obj)))

(defclass shiftless::symbol (shiftless::atom)
  ((name :type symbol
         :initarg :name
         :accessor shiftless::atom-symbol)))

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

(defclass shiftless::number (shiftless::atom)
  ((value :type (or integer float)
          :initarg :value
          :accessor shiftless::value)))

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
(cl-defmethod shiftless::value ((string string))
  (if (eq :explicit shiftless::*schema*)
      (let ((shiftless::*schema* :implicit))
        (shiftless::make-atom atom))
      string))

(cl-defmethod shiftless::atom-symbol ((atom string))
  (if (eq :explicit shiftless::*schema*)
      (let ((shiftless::*schema* :implicit))
        (shiftless::atom-symbol (shiftless::make-atom atom)))
    (intern (downcase atom))))

(cl-defmethod shiftless::atom-boolean ((atom string))
  (if (eq :explicit shiftless::*schema*)
      (let ((shiftless::*schema* :implicit))
        (shiftless::atom-boolean (shiftless::make-atom atom)))
    t))

(cl-defmethod shiftless::atom-integer ((atom string))
  (if (eq :explicit shiftless::*schema*)
      (let ((shiftless::*schema* :implicit))
        (shiftless::atom-integer (shiftless::make-atom atom)))
    (round (string-to-number atom))))

(cl-defmethod shiftless::atom-float ((atom string))
  (if (eq :explicit shiftless::*schema*)
      (let ((shiftless::*schema* :implicit))
        (shiftless::atom-float (shiftless::make-atom atom)))
    (float (string-to-number atom))))

(cl-defmethod shiftless::atom-string ((atom string))
  (if (eq :explicit shiftless::*schema*)
      (if (= 39 (elt atom 0))                       ;'
          (let ((shiftless::*schema* :implicit))
            (shiftless::make-atom atom))
        atom)
    atom))

(provide 'shiftless-types)
;;; shiftless-types.el ends here
