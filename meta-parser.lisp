; # Parser
; This is a parser based on the OMeta parsing language, which is itself based on parsing expression grammars.

(in-package #:meta-parser)


; ## Parsers, PEGs, OMeta, and this parser

; Parsers are programs that analyze a text in order to determine its structure with respect to a grammar. Grammars can be thought of as a nested set of units of meaning. For instance, a paragraph is made up of sentences, which are made up of sentence fragments, which are made up of words, which themselves are made up of smaller elements.

; Ever since Chomsky introduced (in the 1950s) his formalized way of describing grammars -- context-free grammars -- they have been the primary tool computers have used to analyze text. In 2004, Brian Ford introduced Parsing Expression Grammars (PEGs), a formalism designed to remove some of the ambiguities that arise when using context-free grammars. In removing these ambiguities, he created a way of describing grammars that is more suitable for the very structured sort of grammars that are used in programs.

; ### PEGs

; A PEG is made up of a set of definitions. These definitions define a unit of meaning in a particular grammar. In this program, we will refer to these definitions as *rules*. The rules in a PEG take the following form:

;   Identifier <- Expression

; Where the `Identifier is the name of the rule and `Expression is the expression that describes it. We can describe this form that rules in PEGs take in a PEG rule itself:

;   Definition <- Identifier '<-' Expression

; Where the single quotes surround an literal string of text. The expressions in a PEG are made up of a set of rules that are modified through some special operators. The `/ operator chooses between two expressions. If it finds that the first does not match, it moves on to the second. Thus, the rule:

;   zeroOrOne <- '0' / '1'

; Will match the input "0" or "1".

;The `* operator allows zero or more repetitions of a rule and is greedy -- that is, it will apply itself as many times as it can. Hence:

;   Binary <- zeroOrOne*

; Will match an empty string as well as "01011100" or "0". `+ behaves similarly to `*, but will only match one or more instances of a rule. Therefore, if `Binary were defined with a `+, it would not match the empty string, but it would match "01011100" and "0".

; `? denotes an optional rule:

;   squirrel <- 'squirrel' 's'?

; This would match the word "squirrel" whether or not it was pluralized.

; The `& (and) and `! (not) operators do not use up any input. `& succeeds if the rule it attached to succeeds, and fails if it does not, while `! returns the opposite result. Therefore:

;   apples <- 'apple' &'s'

; Will succeed with the string "apples", whereas:

;   apple <- 'apple' !'s'

; Would fail. Both `apples and `apple would leave the "s" in "apples" in the input.

; The `. operator will match anything, failing only if it reaches the end of the input.

; Finally, parenthesis can be used to group expressions together. From the PEG grammar, expressions are defined as:

;  Expression <- Sequence ('/' Sequence)*

; In other words, an expression is made up of a sequence and then zero or more sequences separated with a choice operator.


; ### OMeta

; OMeta extends PEGs in order to add a number of features that are designed to make creating parsers easy. There are two features of OMeta that I consider to be key: the addition of semantic actions, and the addition of higher-order rules.

;#### Semantic actions

; PEGs may describe the form of a grammar, but they do not include information about the meaning behind the rules. For instance if you were to parse the string "32" with a rule called `Number, the result of the parse would be some kind of datatype that describes the fact that a number was matched and the string that is associated with is "32". Something like, `<Number string="32">. This doesn't describe what it means to be a number, however. What we really want is to know the value behind the string "32", eg. the literal 32.

; OMeta uses the notion of semantic actions in order fill this gap in the representation of grammars. Rules in OMeta take the form:

;   Identifier = Expression -> SemanticAction

; In order for semantic actions to be of any use, they have to refer to values that were matched in the expression. That is why OMeta expressions use the `: operator to indicate binding of a variable `Rule:x binds the result of the application of `Rule to the variable x, which can then be used in the `SemanticAction statement. A semantic action is a piece of code describing what to return for that particular rule. The `Number rule could be expressed as:

;   Number = Digit+:ds  -> (parse-integer (coerce ds 'string))

; Any choice can have a semantic action assigned to it. If no semantic action is present.

; <Should this be carried on?>

; ### References
; 1 - http://en.wikipedia.org/wiki/Parser
; 2 - http://en.wikipedia.org/wiki/Context-free_grammar
; 3 - Parsing Expression Grammars: A Recognition-Based Syntactic Foundation, Brian Ford
; 4 - Experimenting with Programming Languages, Alessandro Warth



; ## Parser core

; A grammar is made up of a set of rules. The entire grammar can be encapsulated by the top-level rule -- that is, the rule that describes the largest grammatical unit.

; In order to parse a given input with a grammar, we therefore need two things: an input and the top-level rule of that grammar. To avoid having to propogate the input through every rule, we will make a variable with which to hold it:

(defvar *input* nil)

; The primary function of the parser, `parse, can now be written. Strings will be converted to streams so that they can take advantage of the features that streams offer.

(defun parse (rule input-string)
  (setf *furthest-error* 0)
  (with-open-stream (s (make-string-input-stream input-string))
    (setf *input* s)
    (funcall rule)))

; Each rule is divided into a set of choices, which are tried in succession. Each choice is made up of three parts: a sequence of rules that are to be applied to the input, an optional predicate, and an optional result. Each rule in the sequence can be bound to a variable in order for the predicate/result to refer to the result of that rule application. A rule could therefore look like the following:

;   rule = r1 r2:x ?(is-this-a-thing-p x) -> (do-something-to x)
;        : r3

; Where the predicate is the form preceded by the "?", the result is preceded by "->", the choices are separated by "OR", and variable binding is done through "!".

; When a rule is applied to the input and it does not succeed, we need to signal that an error has occurred in the parsing. It is typically going to be the case that we will care about the furthest point at which the parser reaches before an error occurs. It's possible for an `parse-error to be caught, the input position reset, and have other rules tried and fail before all the possibilities in a grammar are exhausted. For this reason, we will store the location of the furthest position at which an error occurs. This will be the position that gets reported if the grammar fails to match.

(defparameter *furthest-error* 0)

(define-condition parse-error (error)
  ((pos :initform (setf *furthest-error*
                        (max (1- (file-position *input*))
                             *furthest-error*)) :reader pos))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~a:~%~%~a~%"
                     *furthest-error*
                     (parse-error-print *furthest-error*)))))

; Since we'd like informative `parse-errors, we should print out where the error happens, as well as the surrounding lines of the input. The function `last-line will help us seek the position of the last line in a stream, and `read-line can do the rest.

(defun last-line (stream)
  (loop
     for position = (max (- (file-position stream) 2)
                         0)
     for c = (progn
               (file-position stream position)
               (read-char stream))
     until (or (eql c #\Newline)
               (= position 0))
     finally (progn (file-position stream (if (= position 0)
                                              0
                                              (1+ position)))
                    (return (file-position stream)))))

(defun parse-error-print (pos)
  (file-position *input* pos)
  (let* ((error-start (last-line *input*))
         (prev-line (if (/= error-start 0)
                       (progn
                         (last-line *input*)
                         (read-line *input*))))
         (error-first (coerce (loop repeat (- pos error-start)
                                 collect (read-char *input*))
                              'string))
         (error-rest (read-line *input*))
         (next-line (read-line *input* nil)))
    (format nil "~@[>  ~a~%~]>  ~a<!ERROR!>~a~@[~%>  ~a~]"
            prev-line error-first error-rest next-line)))

; With `parse-error defined we can now implement the `choice function. Its job is to take a list of choices and try each one in succession. If a choice succeeds, the result is returned immediately. If the choice signals a `parse-error, the input position is reset to where it was before the choice was applied before the next choice is tried. If none of the choices succeed, a `parse-error is thrown.

(defun choose (choices)
  (let ((initial-position (file-position *input*)))
    (loop for choice in choices do
         (handler-case
             (return (apply-choice choice))
           (parse-error () (file-position *input* initial-position)))
         finally (error 'parse-error))))


; Choices will be represented as a list containing two or three anonymous functions. The first is the sequence of rules to be applied. The second is the predicate -- if this form evaluates to false then a `parse-error should be thrown. If no predicate is specified then the second function should be `(lambda () t). If there is a third function, it is the form from which the return value is derived. If no third function is supplied then the value returned by the first function should be returned.

; TODO Choices could be a single function with `make-choice combinining forms.

(defun apply-choice (choice)
  (let ((rules-result (funcall (first choice)))
        (pred (funcall (second choice)))
        (ret  (third choice)))
    (if (not pred) (error 'parse-error))
    (if ret
        (funcall ret)
        rules-result)))

; The `sequential function tries a list of rules -- contained in anonymous functions as they may be more than just a rule -- in sequence. It returns the last value returned by a rule application.

(defun sequential (rules)
  (loop for rule in rules
     for x = (funcall rule)
     finally (return x)))

; Given these definitions, the rule used as an example above can now be expressed as:

; (LET (X)
;   (CHOICE
;    (LIST
;     (LIST
;      (LAMBDA ()
;        (SEQUENTIAL (LIST (LAMBDA () (R1)) (LAMBDA () (SETF X (R2))))))
;      (LAMBDA () (IS-THIS-A-THING? X)) (LAMBDA () (DO-SOMETHING-TO X)))
;     (LIST (LAMBDA () (SEQUENTIAL (LIST (LAMBDA () (R3))))) (LAMBDA () T)))))


; `zero-or-more (*) greedily matches zero ore more applications of a rule, returning the results of the matches in list form.

(defun zero-or-more (rule)
  (loop for position = (file-position *input*)
     collect (handler-case
                 (funcall rule)
               (parse-error () (progn
                                 (file-position *input* position)
                                 (return ret))))
     into ret))

; `optional (?) will succeed whether or not a rule matches.
(defun optional (rule)
  (let ((position (file-position *input*)))
    (handler-case (funcall rule)
      (parse-error () (progn
                        (file-position *input* position)
                        nil)))))

; `negation (!) succeeds if a rule fails, and fails if it succeeds, never consuming any input.

(defun negation (rule)
  (let ((position (file-position *input*))
        result)
    (handler-case (progn
                    (funcall rule)
                    (setf result t))
      (parse-error () nil))
    (file-position *input* position)
    (if result
        (error 'parse-error)
        t)))

; `lookahead (&) succeeds if a rule matches, but does not consume any input. This can be achieved by applying `negation to a rule twice.

(defun lookahead (rule)
  (negation #'(lambda () (negation rule))))

; `literal attempts to match a string exactly.

(defun literal (string)
  (loop for c in (coerce string 'list)
     as x = (anything)
     when (not (eql x c)) do (error 'parse-error))
  string)



; ## Rules

; `anything is the first actual rule that we will define. It is the basis of all other rules. It removes one item from the input stream, throwing a `parse-error if the end of the input has been reached.

(defun anything ()
  (handler-case (read-char *input*)
    (cl::end-of-file () (error 'parse-error))))

(defun form ()
  (handler-case (read-preserving-whitespace *input*)
    (cl::end-of-file () (error 'parse-error))))


; ## Meta grammer
(defparameter *grammar-prelude*
"(in-package :meta-parser)

(defparameter *defs* nil)

; The functions `make-def, `make-choice, and `make-rule are could be replaced by meta code, but the resulting grammars would be longer and would require more backtracking. 

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
    `(lambda () ,prim)))")

(defparameter *grammar*
   "meta-grammar  = space* definition:d [',' space* definition]*:ds end-of-file 
                    -> `(progn ,d ,@ds),
    definition    = identifier:i space* argList?:args space* '=' space* expression:e space*
                        -> (make-def i e args),
    identifier    = alpha:l [alpha | '-' -> #\\- ]*:ls
                        -> (read-from-string (coerce `(,l ,@ls) 'string)),
    argList       = '(' [identifier:i space* -> i ]*:args ')' -> args,
    expression    = choice:c ['|' space* choice]*:cs
                        -> `(choose (list ,c ,@cs)),
    choice        = rule*:rs predicate?:p semantic?:s
                        -> (make-choice rs p s),
    rule          = prefix?:pre primary:p suffix?:suf binding?:b space*
                        -> (make-rule pre p suf b),
    predicate     = '?' form:f space* -> f,
    semantic      = '->' space* form:f  space* -> f,
    binding       = ':' identifier:i -> (progn (push i *defs*) i),
    prefix        = '!' -> 'negation
                  | '&' -> 'lookahead,
    suffix        = '*' -> 'zero-or-more
                  | '+' -> 'one-or-more
                  | '?' -> 'optional,
    primary       = identifier:i -> `(,i)
                  | '[' space* expression:e ']' space* -> e
                  | &'(' form
                  | lit
                  | '.' space*  -> '(anything),
    lit           = '\\'' [!'\\'' eChar]*:xs '\\'' 
                        -> `(literal ,(coerce xs 'string)),
    eChar         = '\\\\'? char:c -> c,

    end-of-file   = !.,
    char          = .:a ?(characterp a),
    digit         = .:a ?(digit-char-p a) -> (digit-char-p a),
    alpha         = .:a ?(alpha-char-p a),
    space         = ' '
                  | .:a ?(eql a #\\Tab)
                  | .:a ?(eql a #\\Newline),
    one-or-more (r)
                  = (funcall r):x (funcall r)*:xs -> (cons x xs)")

(defun bootstrap ()
  (with-open-file (stream "grammar1.lisp"
                          :direction :output
                          :if-exists :supersede)
    (format stream "~a~%~s"
            *grammar-prelude*
            (parse 'meta-grammar *grammar*))))

; A helpful macro
(defmacro meta (s)
  (parse 'meta-grammar s))
