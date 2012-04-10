;;;; package.lisp

(defpackage #:meta-parser
  (:use #:cl)
  (:shadow :char :end-of-file :space :parse-error)
  (:export :parse
           :anything
           :form
           :meta-grammar
           :char
           :digit
           :alpha
           :one-or-more
           :end-of-file
           :space))

