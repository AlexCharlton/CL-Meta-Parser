    (in-package :meta-parser)

    (defparameter *defs* nil)

    (defun make-def (name expression args)
      (setf *defs* (remove-duplicates *defs*))
      (prog1
          `(defun ,name ,args
             (let ,*defs*
               ,expression))
        (setf *defs* nil)))

    (defun make-choice (sequence predicate semantic)
      (let ((seq `(lambda () (sequential (list ,@sequence))))
            (pred (if predicate
                      `(lambda () ,predicate)
                      `(lambda () t))))
        (if semantic
            `(list ,seq ,pred (lambda () ,semantic))
            `(list ,seq ,pred))))

    (defun make-rule (prefix primary suffix binding)
      (let ((prim primary))
        (if suffix (setf prim `(,suffix #'(lambda () ,prim))))
        (if prefix (setf prim `(,prefix #'(lambda () ,prim))))
        (if binding (setf prim `(setf ,binding ,prim)))
        `(lambda () ,prim)))
(PROGN
 (DEFUN META-GRAMMAR ()
   (LET (DS D)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE))))
                 (LAMBDA () (SETF D (DEFINITION)))
                 (LAMBDA ()
                   (SETF DS
                           (ZERO-OR-MORE
                            #'(LAMBDA ()
                                (CHOOSE
                                 (LIST
                                  (LIST
                                   (LAMBDA ()
                                     (SEQUENTIAL
                                      (LIST (LAMBDA () (literal ","))
                                            (LAMBDA ()
                                              (ZERO-OR-MORE
                                               #'(LAMBDA () (SPACE))))
                                            (LAMBDA () (DEFINITION)))))
                                   (LAMBDA () T))))))))
                 (LAMBDA () (END-OF-FILE)))))
        (LAMBDA () T) (LAMBDA () `(PROGN ,D ,@DS)))))))
 (DEFUN DEFINITION ()
   (LET (E ARGS I)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (SETF I (IDENTIFIER)))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE))))
                 (LAMBDA ()
                   (SETF ARGS (OPTIONAL #'(LAMBDA () (ARGLIST)))))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE))))
                 (LAMBDA () (literal "="))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE))))
                 (LAMBDA () (SETF E (EXPRESSION)))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE)))))))
        (LAMBDA () T) (LAMBDA () (MAKE-DEF I E ARGS)))))))
 (DEFUN IDENTIFIER ()
   (LET (LS L)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (SETF L (ALPHA)))
                 (LAMBDA ()
                   (SETF LS
                           (ZERO-OR-MORE
                            #'(LAMBDA ()
                                (CHOOSE
                                 (LIST
                                  (LIST
                                   (LAMBDA ()
                                     (SEQUENTIAL
                                      (LIST (LAMBDA () (ALPHA)))))
                                   (LAMBDA () T))
                                  (LIST
                                   (LAMBDA ()
                                     (SEQUENTIAL
                                      (LIST
                                       (LAMBDA () (literal "-")))))
                                   (LAMBDA () T) (LAMBDA () #\-)))))))))))
        (LAMBDA () T)
        (LAMBDA () (READ-FROM-STRING (COERCE `(,L ,@LS) 'STRING))))))))
 (DEFUN ARGLIST ()
   (LET (ARGS I)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal "("))
                 (LAMBDA ()
                   (SETF ARGS
                           (ZERO-OR-MORE
                            #'(LAMBDA ()
                                (CHOOSE
                                 (LIST
                                  (LIST
                                   (LAMBDA ()
                                     (SEQUENTIAL
                                      (LIST (LAMBDA () (SETF I (IDENTIFIER)))
                                            (LAMBDA ()
                                              (ZERO-OR-MORE
                                               #'(LAMBDA () (SPACE)))))))
                                   (LAMBDA () T) (LAMBDA () I))))))))
                 (LAMBDA () (literal ")")))))
        (LAMBDA () T) (LAMBDA () ARGS))))))
 (DEFUN EXPRESSION ()
   (LET (CS C)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (SETF C (CHOICE)))
                 (LAMBDA ()
                   (SETF CS
                           (ZERO-OR-MORE
                            #'(LAMBDA ()
                                (CHOOSE
                                 (LIST
                                  (LIST
                                   (LAMBDA ()
                                     (SEQUENTIAL
                                      (LIST (LAMBDA () (literal "|"))
                                            (LAMBDA ()
                                              (ZERO-OR-MORE
                                               #'(LAMBDA () (SPACE))))
                                            (LAMBDA () (CHOICE)))))
                                   (LAMBDA () T)))))))))))
        (LAMBDA () T) (LAMBDA () `(CHOOSE (LIST ,C ,@CS))))))))
 (DEFUN CHOICE ()
   (LET (S P RS)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST
            (LAMBDA () (SETF RS (ZERO-OR-MORE #'(LAMBDA () (RULE)))))
            (LAMBDA () (SETF P (OPTIONAL #'(LAMBDA () (PREDICATE)))))
            (LAMBDA () (SETF S (OPTIONAL #'(LAMBDA () (SEMANTIC))))))))
        (LAMBDA () T) (LAMBDA () (MAKE-CHOICE RS P S)))))))
 (DEFUN RULE ()
   (LET (B SUF P PRE)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST
            (LAMBDA () (SETF PRE (OPTIONAL #'(LAMBDA () (PREFIX)))))
            (LAMBDA () (SETF P (PRIMARY)))
            (LAMBDA () (SETF SUF (OPTIONAL #'(LAMBDA () (SUFFIX)))))
            (LAMBDA () (SETF B (OPTIONAL #'(LAMBDA () (BINDING)))))
            (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE)))))))
        (LAMBDA () T) (LAMBDA () (MAKE-RULE PRE P SUF B)))))))
 (DEFUN PREDICATE ()
   (LET (F)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal "?")) (LAMBDA () (SETF F (FORM)))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE)))))))
        (LAMBDA () T) (LAMBDA () F))))))
 (DEFUN SEMANTIC ()
   (LET (F)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal "->"))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE))))
                 (LAMBDA () (SETF F (FORM)))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE)))))))
        (LAMBDA () T) (LAMBDA () F))))))
 (DEFUN BINDING ()
   (LET (I)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal ":"))
                 (LAMBDA () (SETF I (IDENTIFIER))))))
        (LAMBDA () T) (LAMBDA () (PROGN (PUSH I *DEFS*) I)))))))
 (DEFUN PREFIX ()
   (LET ()
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (literal "!")))))
        (LAMBDA () T) (LAMBDA () 'NEGATION))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (literal "&")))))
        (LAMBDA () T) (LAMBDA () 'LOOKAHEAD))))))
 (DEFUN SUFFIX ()
   (LET ()
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (literal "*")))))
        (LAMBDA () T) (LAMBDA () 'ZERO-OR-MORE))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (literal "+")))))
        (LAMBDA () T) (LAMBDA () 'ONE-OR-MORE))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (literal "?")))))
        (LAMBDA () T) (LAMBDA () 'OPTIONAL))))))
 (DEFUN PRIMARY ()
   (LET (E I)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (SETF I (IDENTIFIER))))))
        (LAMBDA () T) (LAMBDA () `(,I)))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal "["))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE))))
                 (LAMBDA () (SETF E (EXPRESSION)))
                 (LAMBDA () (literal "]"))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE)))))))
        (LAMBDA () T) (LAMBDA () E))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST
            (LAMBDA () (LOOKAHEAD #'(LAMBDA () (literal "("))))
            (LAMBDA () (FORM)))))
        (LAMBDA () T))
       (LIST (LAMBDA () (SEQUENTIAL (LIST (LAMBDA () (LIT)))))
             (LAMBDA () T))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal "."))
                 (LAMBDA () (ZERO-OR-MORE #'(LAMBDA () (SPACE)))))))
        (LAMBDA () T) (LAMBDA () '(ANYTHING)))))))
 (DEFUN LIT ()
   (LET (XS)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (literal "'"))
                 (LAMBDA ()
                   (SETF XS
                           (ZERO-OR-MORE
                            #'(LAMBDA ()
                                (CHOOSE
                                 (LIST
                                  (LIST
                                   (LAMBDA ()
                                     (SEQUENTIAL
                                      (LIST
                                       (LAMBDA ()
                                         (NEGATION
                                          #'(LAMBDA () (literal "'"))))
                                       (LAMBDA () (ECHAR)))))
                                   (LAMBDA () T))))))))
                 (LAMBDA () (literal "'")))))
        (LAMBDA () T) (LAMBDA () `(literal ,(COERCE XS 'STRING))))))))
 (DEFUN ECHAR ()
   (LET (C)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST
            (LAMBDA () (OPTIONAL #'(LAMBDA () (literal "\\"))))
            (LAMBDA () (SETF C (CHAR))))))
        (LAMBDA () T) (LAMBDA () C))))))
 (DEFUN END-OF-FILE ()
   (LET ()
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST
            (LAMBDA () (NEGATION #'(LAMBDA () (ANYTHING)))))))
        (LAMBDA () T))))))
 (DEFUN CHAR ()
   (LET (A)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (SETF A (ANYTHING))))))
        (LAMBDA () (CHARACTERP A)))))))
 (DEFUN DIGIT ()
   (LET (A)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (SETF A (ANYTHING))))))
        (LAMBDA () (DIGIT-CHAR-P A)) (LAMBDA () (DIGIT-CHAR-P A)))))))
 (DEFUN ALPHA ()
   (LET (A)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (SETF A (ANYTHING))))))
        (LAMBDA () (ALPHA-CHAR-P A)))))))
 (DEFUN SPACE ()
   (LET (A)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (literal " ")))))
        (LAMBDA () T))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (SETF A (ANYTHING))))))
        (LAMBDA () (EQL A #\Tab)))
       (LIST
        (LAMBDA ()
          (SEQUENTIAL (LIST (LAMBDA () (SETF A (ANYTHING))))))
        (LAMBDA () (EQL A #\Newline)))))))
 (DEFUN ONE-OR-MORE (R)
   (LET (XS X)
     (CHOOSE
      (LIST
       (LIST
        (LAMBDA ()
          (SEQUENTIAL
           (LIST (LAMBDA () (SETF X (FUNCALL R)))
                 (LAMBDA ()
                   (SETF XS
                           (ZERO-OR-MORE
                            #'(LAMBDA () (FUNCALL R))))))))
        (LAMBDA () T) (LAMBDA () (CONS X XS))))))))
