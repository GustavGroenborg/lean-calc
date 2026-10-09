import LeanCalc.Grammar

structure Symbol where
  id  : VarId
  val : Option INum
  deriving Repr, BEq, DecidableEq

abbrev SymbolTable := List Symbol

def retrieveSymbol (id : VarId) (symbols : SymbolTable) : Option Symbol :=
  symbols.find? (fun s => s.id.name == id.name)

def enterSymbol (symbol : Symbol) (symbols : SymbolTable) : Option (SymbolTable) :=
  match retrieveSymbol symbol.id symbols with
  | some _ => none
  | none   => some <| symbol :: symbols

def setSymbol (symbol : Symbol) (symbols : SymbolTable) : SymbolTable :=
  match retrieveSymbol symbol.id symbols with
  | some _ => symbols.map (fun s => if s.id == symbol.id then symbol else s)
  | none   => symbol :: symbols

def initSymbols : Dcls -> SymbolTable
  | Dcls.cons dcl dcls' =>
    -- match statement is overkill, but allows for easily adding more types later on
    match dcl with
    | Dcl.iNumDcl id => (Symbol.mk id none) :: initSymbols dcls'
  | Dcls.lambda => []
