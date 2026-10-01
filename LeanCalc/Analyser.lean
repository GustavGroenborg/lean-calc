import LeanCalc.Grammar

structure Symbol where
  id  : VarId
  val : Option INum
  deriving Repr, BEq, DecidableEq

def retrieveSymbol (id : VarId) (symbols : List Symbol) : Option Symbol :=
  symbols.find? (fun s => s.id.name == id.name)

def enterSymbol (symbol : Symbol) (symbols : List Symbol) : Option (List Symbol) :=
  match retrieveSymbol symbol.id symbols with
  | some _ => none
  | none   => some <| symbol :: symbols

def setSymbol (symbol' : Symbol) (symbols : List Symbol) : Except String (List Symbol) :=
  match retrieveSymbol symbol'.id symbols with
  | some _ => Except.ok <| symbols.map (fun s => if s.id == symbol'.id then symbol' else s)
  | none   => Except.error s!"Could not find symbol with id '{symbol'.id.name}' in symbol table."

def initSymbols : Dcls -> List Symbol
  | Dcls.cons dcl dcls' =>
    -- match statement is overkill, but allows for easily adding more types later on
    match dcl with
    | Dcl.iNumDcl id => (Symbol.mk id none) :: initSymbols dcls'
  | Dcls.lambda => []
