inductive Token where
  | intdcl : Token
  | print  : Token
  | id     : Char -> Token
  | assign : Token
  | plus   : Token
  | minus  : Token
  | inum   : Nat  -> Token
  | space  : Token
  deriving Repr

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
        if str.length == 1 then Except.ok <| Token.id str.front
        else Except.error s!"Lexical error: Unrecognised token '{str}'"

def lexInput (input : List String) : Except String (List Token) :=
  match input with
    | []         => Except.ok []
    | head::tail => do
      let tok  <- lex head
      let toks <- lexInput tail
      Except.ok <| tok::toks
