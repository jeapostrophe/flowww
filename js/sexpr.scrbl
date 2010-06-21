#lang scribble/doc
@(require scribble/manual)

@title{JavaScript S-expression Syntax}

@schemegrammar*[
 #:literals (regexp null object this field-ref field
             new prefix postfix ? function begin
             default var case catch block if do while
             for continue break return with switch
             label throw try finally
             ; prefix
             delete void typeof ++ -- + - ~ !
             ; postfix
             ++ --
             ; infix
             * / % + - << >> >>> < > <= >= instanceof in == != === !== & ^ \| && \|\|
             ; assigment
             = *= /= %= += -= <<= >>= >>>= &= ^= \|=)
 [SourceElement (function symbol (symbol ...) SourceElement ...)
                SubStatement]
 [SubStatement (function symbol (symbol ...) SourceElement ...)
               (var VariableInitializer ...)
               Statement]
 [BlockStatement (block SubStatement ...)]
 [VariableInitializer symbol
                      (symbol Expression)]
 [Statement BlockStatement
            ()
            (if Expression SubStatement SubStatement)
            (if Expression SubStatement)
            (do SubStatement Expression)
            (while Expression SubStatement)
            (for (var VariableInitializer ...) Expression Expression SubStatement)
            (for Expression Expression Expression SubStatement)
            (for-in ((var VariableInitializer) Expression) SubStatement)
            (for-in (Expression Expression) SubStatement)
            (continue symbol)
            (continue)
            (break symbol)
            (break)
            (return Expression)
            (return)
            (with Expression SubStatement)
            (switch Expression CaseClause)
            (label symbol SubStatement)
            (throw Exprssion)
            (try BlockStatement CatchClause ...)
            (try BlockStatement CatchClause ... (finally BlockStatement))
            Expression] 
 [CaseClause (default SubStatement ...)
             (case Expression SubStatement ...)]
 [CatchClause (catch symbol BlockStatement)]
 [Expression null
             Expression-w/o-null]
 [Expression-w/o-null string
             (regexp string boolean boolean)
             number
             boolean
             (array ArrayElement ...)
             (object (Property Expression) ...)
             this
             symbol
             (field-ref Expression Expression)
             (field Expression symbol)
             (new Expression Expression ...)
             (prefix prefix-op Expression)
             (postfix Expression postfix-op)
             (infix-op Expression Expression)
             (? Expression Expression Expression)
             (assignment-op Expression Expression)
             (function (symbol symbol ...) SourceElement ...)
             (function (symbol ...) SourceElement ...)
             (begin Expression ...)
             (Expression Expression ...)]
 [Property symbol
           string
           number]
 [ArrayElement Expression-w/o-null]
 [prefix-op delete void typeof ++ -- + - ~ !]
 [postfix-op ++ --]
 [infix-op * / % + - << >> >>> < > <= >= instanceof in == != === !== & ^ \| && \|\|]
 [assignment-op = *= /= %= += -= <<= >>= >>>= &= ^= \|=]
 ]