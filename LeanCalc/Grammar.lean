structure VarId where
  name : Char
  deriving Repr, BEq, DecidableEq

structure INum where
  val : Nat
  deriving Repr, BEq, DecidableEq

inductive Val where
  | inum  : INum  -> Val
  | varId : VarId -> Val
  deriving Repr, BEq, DecidableEq

inductive Expr where
  | minus  : Val -> Expr -> Expr
  | plus   : Val -> Expr -> Expr
  | lambda : Expr
  deriving Repr, BEq, DecidableEq

inductive Stmt where
  | assign : VarId -> Val -> Expr -> Stmt
  | printId  : VarId -> Stmt
  deriving Repr, BEq, DecidableEq

inductive Stmts where
  | cons   : Stmt -> Stmts -> Stmts
  | lambda : Stmts
  deriving Repr, BEq, DecidableEq

inductive Dcl where
  | iNumDcl : VarId -> Dcl
  deriving Repr, BEq, DecidableEq

inductive Dcls where
  | cons   : Dcl -> Dcls -> Dcls
  | lambda : Dcls
  deriving Repr, BEq, DecidableEq

structure Prog where
  dcls  : Dcls
  stmts : Stmts
  deriving Repr

def hellocfg := IO.println "hello from cfg"
