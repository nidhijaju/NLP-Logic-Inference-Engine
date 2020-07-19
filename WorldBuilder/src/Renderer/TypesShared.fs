module TypesShared

type Token = | N of string
             | Q of string
             | QR of string
             | A of string
             | C of string
             | R of string
             | V of string
             | I of string
             | O of string
             | U of string
             | Not of string
             | NULL of string


//AST from parser module
type Ast = | NOUN of string
           | OBJ of string
           | RAD of string
           | ADJ of string
           | NOT of Ast
           | VERB of string*Ast*Ast
           | QUANT of string*Ast
           | QUANTR of string*string*Ast
           | COND of string*Ast*Ast
           | IMPL of string*Ast
           | UNION of string*Ast*Ast

//WFF = Well Formed Formulas (way of defining logic statements)

/// Interface with Evaluator ///
type Object = Object of string

type Predicate = Predicate of string*bool

type Radical = Radical of string

type WFF = | PREDICATEEXP of Predicate
           | PREDICATER of Predicate*(Radical list)
           | OBJECTEXP of Object
           | AND of WFF*WFF
           | OR of WFF*WFF

type GroundTruth = GroundTruth of Predicate*(Object list)