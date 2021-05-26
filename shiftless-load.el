(require 'cl-lib)
(require 'dash)
(require 'rx)

(require 'shiftless-types)
(require 'shiftless-access)

(defvar shiftless::read-table
  '(;; [
    (91 . shiftless::read-sequence)
    ;; '
    (39 . shiftless::read-string)))

(defun shiftless::split-references (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((start (point)))
      (append
       (cl-loop
        while (char-after)
        for spot = (point)
        for ref = (shiftless::read-reference)
        if ref collect (buffer-substring-no-properties
                                     start spot)
        and collect ref
        and do (setf start (point))
        else do (forward-char))
       (list (string-trim (buffer-substring-no-properties
                           start (point-max))
                          ""))))))

(defun shiftless::delay-reference (string)
  (if (string-match (rx ".[" (one-or-more anything) "]")
                    string)
      (let ((parts (shiftless::split-references string)))
        (vector
         `(lambda (data)
            (shiftless::read-from-string
             (apply 'concat
                    (mapcar
                     (lambda (part)
                       (if (consp part)
                           (apply 'shiftless:access-as 'string data part)
                         part))
                     ',parts))))))
    string))

(defun shiftless::make-atom (string)
  (cond
   ((not (stringp string)) string)
   ((equal "" string) nil)
   ;; string
   ;; '
   ((shiftless::stringp string)
    (let ((unquoted (substring string 1 (- (length string) 1))))
      (->>
       unquoted
       (replace-regexp-in-string
        (rx "\\'") "'")
       (replace-regexp-in-string
        (rx "\\\\") "\\"))))
   ;; number
   ((string-match (rx string-start
                      (zero-or-one "-")
                      (one-or-more digit)
                      (zero-or-one "."
                                   (one-or-more digit))
                      string-end)
                  string)
    (string-to-number string))
   ;; symbol
   (:else
    (intern (downcase string)))))

(defun shiftless::association-p (obj)
  (and obj (listp obj)
       (= 0 (mod (length obj) 3))
       (->>
        obj
        (-map-indexed
         (lambda (index item)
           (cons (mod index 3) item)))
        (-every-p
         (lambda (pair)
           (let ((place (car pair))
                 (item (cdr pair)))
             (cond
              ;; key
              ((= 0 place)
               (or (consp item)
                   (symbolp (shiftless::make-atom item))))
              ;; equal sign
              ((= 1 place)
               (equal item "="))
              (:else t))))))))

(defun shiftless::add-to-association (key value alist)
  (cond
   ((null key) value)
   ((consp key)
    (let ((key (shiftless::atom-symbol (car key)))
          (keys (cdr key)))
      (let ((existing-value (cdr (assoc key alist))))
        (unless (listp existing-value)
          (error "Sequenced key cannt mix with non-assocation value (%S)" key))
        (append (cl-remove key alist :key 'car)
                (list (cons key
                            (shiftless::add-to-association
                             keys value existing-value)))))))
   ((cl-member key alist :key 'car)
    (error "duplicate keys not allowed (%S)" key))
   ;; not member
   (:else 
    (append alist (list (cons (shiftless::atom-symbol key) value))))))

(defun shiftless::association-from-sequence (obj)
  (cl-loop 
   with data = (list)
   for (key = value) on obj by 'cdddr
   do (setf data (shiftless::add-to-association
                  key value data))
   finally return data))

(defun shiftless::reference-p ()
  (save-excursion
    (and (= 46 (or (char-after) 0)) ;; .
         (progn (forward-char) t)
         (= 91 (or (char-after) 0))))) ;; [

(defun shiftless::read-reference ()
  (when (shiftless::reference-p)
    (forward-char 2)
    (prog1 (mapcar 'shiftless::make-atom ;automatically imply types for reference parts
                   (cl-loop
                    do (shiftless::skip-whitespace-and-comments)
                    until (eq 93 (or (char-after) 0)) ;]
                    for sym = (shiftless::read-symbol-or-number)
                    collect sym))
      (forward-char))))

(defun shiftless::read-string ()
  (let ((start (point)))
    (forward-char)
    (while (not (and (= 39 (char-after)) ;'
                     ;; \
                     (not (= 92 (char-before)))))
      (forward-char))
    (forward-char)
    (shiftless::delay-reference
     (buffer-substring-no-properties start (point)))))
  
(defun shiftless::skip-whitespace-and-comments ()
  (cl-loop for start = (point)
           do (when (= 59 (or (char-after) 0)) ; ;
                (while (not (= ?\n (or (char-after) ?\n)))
                  (forward-char)))
           do (skip-chars-forward " \t\r\n")
           while (/= start (point))))

(defun shiftless::read-symbol-or-number ()
  (let ((start (point)))
    (while (and (char-after)
                (not (member (char-after) '(32 ;spacep
                                            91 ;[
                                            93 ;]
                                            39 ;'
                                            ?\n
                                            ?\t
                                            ?\r))))
      (shiftless::read-reference)
      (forward-char))
    (shiftless::delay-reference
     (buffer-substring-no-properties start (point)))))

(defun shiftless::read-sequence ()
  (forward-char)
  (let ((sexp (cl-loop
               do (shiftless::skip-whitespace-and-comments)
               ;; ]
               while (/= 93 (or (char-after) 0))
               collect (shiftless::read))))
    (forward-char)
    (if (shiftless::association-p sexp)
        (shiftless::association-from-sequence sexp)
      (when (cl-member "=" sexp :test 'equal)
        (warn "The list %S contains the = symbol but is not an association"
              sexp))
      sexp)))

(defun shiftless::read ()
  (shiftless::skip-whitespace-and-comments)
  (funcall (or (cdr (assoc (char-after) shiftless::read-table
                           'equal))
               'shiftless::read-symbol-or-number)))

(defun shiftless::read-from-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (shiftless::read)))

(defun shiftless::resolve-references (data structure)
  (cond
   ;; reference
   ((vectorp structure)
    (funcall (elt structure 0) data))
   ;; list and association
   ((proper-list-p structure)
    (mapcar
     (lambda (struct)
       (shiftless::resolve-references data struct))
     structure))
   ;; association cell
   ((consp structure)
    (cons (car structure)
          (shiftless::resolve-references data (cdr structure))))
   (:else
    structure)))

(defun shiftless::apply-implicit-schema (data)
  (cond
   ((proper-list-p data)
    (mapcar 'shiftless::apply-implicit-schema data))
   ((consp data)
    (cons (car data)
          (shiftless::apply-implicit-schema (cdr data))))
   ((stringp data)
    (shiftless::make-atom data))
   (:else
    data)))

(defun shiftless::apply-schema (data schema
                                     &optional accessors)
  (cond
   ((shiftless::alistp data)
    (mapcar
     (lambda (pair)
       (let ((key (car pair))
             (value (cdr pair)))
         (cons key
               (shiftless::apply-schema value schema
                                        (append accessors
                                                (list key))))))
     data))
   ((proper-list-p data)
    (mapcar
     (lambda (item)
       (shiftless::apply-schema item schema
                                (append accessors
                                        (list 0))))
     data))
   (:else
    (funcall (shiftless::function-from-type
              (apply 'shiftless:access schema accessors))
             data))))

(defun shiftless:load (string &optional schema)
  "Return a lisp data structure encoded in the file STRING or directly in STRING."
  (with-temp-buffer
    (if (not (file-exists-p string))
        ;; load string
        (insert string)
      ;; load file
      (insert "[]")
      (forward-char -1)
      (insert-file-contents string))
    (goto-char (point-min))
    (let* ((struct (shiftless::read))
           (struct (shiftless::resolve-references
                    struct struct)))
      (unless (looking-at (rx string-end))
        (warn "You have some extra characters at the end: %S"
              (buffer-substring-no-properties
               (point) (point-max))))
      (cond
       ((eq :implicit schema)
        (shiftless::apply-implicit-schema struct))
       (schema
        (shiftless::apply-schema struct
                                 (shiftless:load schema :implicit)))
       (:else
        struct)))))

(provide 'shiftless-load)
;;; shiftless-load.el ends here
