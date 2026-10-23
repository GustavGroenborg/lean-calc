-- Implemented by Google Gemini
import LeanCalc.Grammar

def spaces (n : Nat) : String :=
  "".pushn ' ' n

def ppVarId (v : VarId) : String :=
  s!"'{v.name}'"

def ppINum (n : INum) : String :=
  s!"{n.val}"

def ppVal : Val -> String
  | Val.inum n  => s!"Val.inum {ppINum n}"
  | Val.varId v => s!"Val.varId {ppVarId v}"

def ppExpr (indent : Nat) : Expr -> List String
  | Expr.plus v e =>
    (s!"{spaces indent}Expr.plus: {ppVal v}") :: ppExpr indent e
  | Expr.minus v e =>
    (s!"{spaces indent}Expr.minus: {ppVal v}") :: ppExpr indent e
  | Expr.lambda => []

def ppStmt (indent : Nat) : Stmt -> List String
  | Stmt.assign v val expr =>
    let header := s!"{spaces indent}Stmt.assign {ppVarId v}"
    let valLine := s!"{spaces (indent + 2)}{ppVal val}"
    let exprLines := match ppExpr (indent + 4) expr with
      | [] => []
      | lines => (s!"{spaces (indent + 2)}Expr:") :: lines
    header :: valLine :: exprLines
  | Stmt.printId v =>
    [s!"{spaces indent}Stmt.printId {ppVarId v}"]

def ppStmts (indent : Nat) : Stmts -> List String
  | Stmts.cons s rest => ppStmt indent s ++ ppStmts indent rest
  | Stmts.lambda => []

def ppDcl (indent : Nat) : Dcl -> String
  | Dcl.iNumDcl v => s!"{spaces indent}Dcl.iNumDcl {ppVarId v}"

def ppDcls (indent : Nat) : Dcls -> List String
  | Dcls.cons d rest => ppDcl indent d :: ppDcls indent rest
  | Dcls.lambda => []

def ppTree (prog : Prog) : String :=
  let dclLines := match ppDcls 4 prog.dcls with
    | [] => [s!"  Declarations: (none)"]
    | lines => "  Declarations:" :: lines
  let stmtLines := match ppStmts 4 prog.stmts with
    | [] => [s!"  Statements: (none)"]
    | lines => "  Statements:" :: lines
  String.intercalate "\n" ("Prog" :: dclLines ++ stmtLines)

def prettyPrint (prog : Prog) : String :=
  (ppTree prog).append "\n ---"

instance : ToString Prog where
  toString := prettyPrint
