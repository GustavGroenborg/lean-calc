import LeanCalc.Grammar
import LeanCalc.Lexer

def syntaxError (t : Token) : Except String α :=
  Except.error s!"Syntax error: unexpected token '{repr t}'"

def varId' (t : Token) : Except String VarId :=
  match t with
    | Token.id c => Except.ok <| VarId.mk c
    | _          => syntaxError t

def iNum (t : Token) : Except String INum :=
  match t with
    | Token.inum n => Except.ok <| INum.mk n
    | _            => syntaxError t

def val (t : Token) : Except String Val :=
  match t with
    | Token.id _   => do let id <- varId' t
                         Except.ok <| Val.varId id
    | Token.inum _ => do let num <- iNum t
                         Except.ok <| Val.inum num
    | _ => syntaxError t

-- def expr (t : Token) : Except String Expr :=
--   match t with
--     | Token.plus _ =>
--     | _ => syntaxError t
