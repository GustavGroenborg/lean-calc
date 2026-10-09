import LeanCalc.Grammar
import LeanCalc.Analyser

def evalVal : Val -> StateT SymbolTable (Except String) INum
  | Val.inum num  => return num
  | Val.varId var => do
    let symbolTable <- get
    match retrieveSymbol var symbolTable with 
    | some { val := some num, .. } => return num
    | some { id, val := none } => throw s!"Symbol with id '{id.name}' has no associated value in symbol table."
    | none => throw s!"No symbol with id '{var.name}' in symbol table."

def evalExpr (val : Val) (expr : Expr) : StateT SymbolTable (Except String) INum := do
  let lhs <- evalVal val
  match expr with
  | Expr.minus val' expr' => do
    let rhs <- evalExpr val' expr'
    return subtract lhs rhs
  | Expr.plus val' expr' => do
    let rhs <- evalExpr val' expr'
    return add lhs rhs
  | Expr.lambda => return lhs

def evalStmt : Stmt -> StateT SymbolTable (Except String) (Option String)
  | Stmt.assign varId val expr => do
    let rhs <- evalExpr val expr
    let symbolTable <- get
    set <| setSymbol (Symbol.mk varId rhs) symbolTable
    return none
  | Stmt.printId varId => do
    let symbolTable <- get
    match retrieveSymbol varId symbolTable with
    | some { val := some num, .. } => return s!" {varId.name} := {num.val}"
    | some { val := none, .. } => throw s!"No value associated with '{varId.name}' in symbol table"
    | none => throw s!"No entrance in symbol table for '{varId.name}'"

def evalStmts : Stmts -> StateT SymbolTable (Except String) (List String)
  | Stmts.cons stmt stmts => do
    let str? <- evalStmt stmt
    let strs <- evalStmts stmts
    match str? with
    | some str => return str :: strs
    | none     => return strs
  | Stmts.lambda => return []

def evaluate (symbols : List Symbol) (stmts : Stmts) : Except String (List String) := do
  sorry
