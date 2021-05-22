(require 'dash)
(require 'shiftless-types)


(defun shiftless::access-spot (data accessors)
  (cond
   ((or (null accessors)
        (null data))
    data)
   ((and (integerp (car accessors))
         (listp data))
    (shiftless::access-spot
     (elt data (car accessors))
     (cdr accessors)))
   ((and (symbolp (car accessors))
         (listp data))
    (shiftless::access-spot
     (cdr (cl-assoc (car accessors) data
                    :key 'shiftless::atom-symbol))
     (cdr accessors)))
   (:else
    (error "Don't know how to access a %s with a %s"
           (type-of data)
           (type-of (car accessors))))))

(defun shiftless:access (data &rest accessors)
  (let ((spot (shiftless::access-spot data accessors)))
    (if (shiftless::atom-predicate spot)
        (shiftless::value spot)
      spot)))

(defun shiftless:access-as (type data &rest accessors)
  (let* ((shiftless::*schema* :explicit)
         (spot (shiftless::access-spot data accessors)))
    (if (shiftless::atom-predicate spot)
        (funcall
         (cond
          ((eq 'symbol type) 'shiftless::atom-symbol)
          ((eq 'boolean type) 'shiftless::atom-boolean)
          ((eq 'integer type) 'shiftless::atom-integer)
          ((eq 'float type) 'shiftless::atom-float)
          ((eq 'string type) 'shiftless::atom-string)
          (:else (error "%S is not a supported type" type)))
         spot)
      spot)))


(provide 'shiftless-access)
;;; shiftless-access.el ends here
