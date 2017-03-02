#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core Context-Free LL(1) Grammar (desugared)
;;


;; Program ::= (PRINT) Expression
;;           | Statement Program

;; Statement ::= Assign
;; Assign ::= (IDENTIFIER i) (EQUAL) Expression (SEMICOLON)
;; skipping separate assign datatype

;; Expression ::= CompExpression ExprSuffix
;; ExprSuffix ::= ε
;;              | (QUESTION) Expression (COLON) Expression  

;; CompExpression ::= AddExpression CompSuffix
;; CompSuffix ::= ε
;;              | (SMALLER) AddExpression

;; AddExpression ::= MultExpression AddList
;; AddList ::= ε
;;            | AddOp MultExpression AddList
;; AddOP ::= (PLUS)
;;         | (MINUS)

;; MultExpression ::= NotExpression MultList
;; MultList ::= ε
;;           | (MULT) NotExpression MultList

;; NotExpression ::= (NOT) NotExpression
;;                 | PrimaryExpression

;; PrimaryExpression ::= (INTEGER n)
;;                     | (IDENTIFIER i)
;;                     | (LPAREN) Expression (RPAREN)
;;
