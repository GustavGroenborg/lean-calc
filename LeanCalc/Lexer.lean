inductive Token where
  | intdcl : Token
  | print  : Token
  | id     : Char -> Token
  | assign : Token
  | plus   : Token
  | minus  : Token
  | inum   : Nat  -> Token
  | space  : Token
  deriving BEq, Repr

instance : ToString Token where
  toString
  | Token.intdcl => "Token.intdcl"
  | Token.print  => "Token.print"
  | Token.id c   => s!"Token.id '{c}'"
  | Token.assign => "Token.assing"
  | Token.plus   => "Token.plus"
  | Token.minus  => "Token.minus"
  | Token.inum n => s!"Token.inum '{n}'"
  | Token.space  => "Token.space"

def isVarChar (char : Char) : Bool := 
  'a' <= char && char <= 'h'
  || 'j' <= char && char <= 'o'
  || 'r' <= char && char <= 'z'

def lexId? (str : String) : Except String Token :=
  if str.length == 1 then Except.ok <| Token.id str.front
  else Except.error s!"Lexical error: ID token '{str}' too long. Max length 1."

def lexNum? (str : String) : Except String Token := 
  if str.isNat then Except.ok <| Token.inum str.toNat!
  else Except.error s!"Lexical error: INum token '{str}' is not a natural number."

def lex (str : String) : Except String Token :=
    if str.isNat then Except.ok <| Token.inum str.toNat!
    else match str with
      | "p" => Except.ok Token.print
      | "i" => Except.ok Token.intdcl
      | "=" => Except.ok Token.assign
      | "+" => Except.ok Token.plus
      | "-" => Except.ok Token.minus
      | " " => Except.ok Token.space
      | _   =>
        if isVarChar str.front then lexId? str
        else lexNum? str

def lexInput (input : List String) : Except String (List Token) :=
  match input with
    | []         => Except.ok []
    | head::tail => do
      let tok  <- lex head
      let toks <- lexInput tail
      Except.ok <| tok::toks
