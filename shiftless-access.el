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


(defun shiftless::function-from-type (type)
  (cond
   ((eq 'symbol type) 'shiftless::atom-symbol)
   ((eq 'boolean type) 'shiftless::atom-boolean)
   ((eq 'integer type) 'shiftless::atom-integer)
   ((eq 'float type) 'shiftless::atom-float)
   ((eq 'string type) 'shiftless::atom-string)
   (:else 'identity)))


(defun shiftless:presentp (data &rest accessors)
  (let ((accessors (reverse (cdr (reverse accessors))))
        (key (car (last accessors))))
    (let ((struct (apply 'shiftless:access data accessors)))
      (cond
       ((null struct) nil)
       ((shiftless::alistp struct)
        (cl-member key struct
                   :key 'car))
       ((consp struct)
        (< key (length struct)))))))

(defun shiftless:access-value (data &rest accessors)
  (let ((spot (shiftless::access-spot data accessors)))
    (when spot
      (if (consp spot)
          spot
        (shiftless::value (shiftless::make-atom spot))))))

(defun shiftless:access (data &rest accessors)
  (let ((spot (shiftless::access-spot data accessors)))
    (when spot
      (if (consp spot)
          spot
        (shiftless::value spot)))))

(defun shiftless:access-as (type data &rest accessors)
  (let ((spot (or (shiftless::access-spot data accessors) "")))
    (if (shiftless::atom-predicate spot)
        (funcall (shiftless::function-from-type type)
                 spot)
      spot)))


(provide 'shiftless-access)
;;; shiftless-access.el ends here
