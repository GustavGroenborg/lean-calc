import LeanCalc.Grammar
import LeanCalc.Lexer

-- TODO: Delete this
abbrev Parser α := StateT (List Token) (Except String) α

-- TODO: Consider deleting
def parseToken : Parser Token := do
  let tokens <- get
  match tokens with
  | t :: ts =>
    set ts
    return t
  | [] => throw "Syntax error: Unexpected EOL"

-- TODO: Consider deleting
def peekToken? : Parser (Option Token) := do
  let tokens <- get
  match tokens with
  | t :: _ => return some t
  | _      => return none

def varId : List Token -> Except String (VarId × List Token)
  | Token.id c :: tokens => Except.ok (VarId.mk c, tokens)
  | token :: _ => Except.error s!"Expected identifier but received '{repr token}'"
  | [] => Except.error "Unexpected EOL."

theorem varId_le_length (tokens : List Token) (variableId : VarId) (tokens' : List Token)
  (hypothesis : varId tokens = Except.ok (variableId, tokens')) : tokens'.length < tokens.length := by
  dsimp [varId] at hypothesis
  split at hypothesis
  -- case 0: Token.id :: tokens
  · simp_all
  -- case 1 : other token :: tokens
  · contradiction
  -- case 2 : empty input
  · contradiction

-- TODO: Consider deleting the function
def iNum : List Token -> Except String (INum × List Token)
  | Token.inum n :: tokens => Except.ok (INum.mk n, tokens)
  | token :: _ => Except.error s!"Unexpected token: '{repr token}'"
  | [] => Except.error "Unexpected EOL."

def val (tokens : List Token) : Except String (Val × List Token) :=
  match tokens with
  | Token.id c :: tokens'   => Except.ok (Val.varId (VarId.mk c), tokens')
  | Token.inum n :: tokens' => Except.ok (Val.inum (INum.mk n), tokens')
  | token :: _ => throw s!"Expected id or integer but received '{repr token}'"
  | [] => throw s!"Unexpected EOL"

-- Proof made with the help of Google Gemini
theorem val_le_length (tokens : List Token) (value : Val) (tokens' : List Token)
  (hypothesis : val tokens = Except.ok (value, tokens')) : tokens'.length < tokens.length := by
  dsimp [val] at hypothesis
  split at hypothesis
  -- case 0: Token.id c :: tokens
  · simp_all
  -- case 1: Token.inum n :: tokens
  · simp_all
  -- case 2: other token :: tokens
  · contradiction
  -- case 3: empty list
  · contradiction

def expr : List Token -> Except String (Expr × List Token)
  | Token.plus :: tokens =>
    match hypothesis : val tokens with
    | Except.ok (value, tokens') =>
      have : tokens'.length < tokens.length := val_le_length _ _ _ hypothesis
      match expr tokens' with
      | Except.ok (expression, tokens'') => Except.ok (Expr.plus value expression, tokens'')
      | Except.error e => Except.error e
    | Except.error e => Except.error e
  | Token.minus :: tokens =>
    match hypothesis : val tokens with
    | Except.ok (value, tokens') =>
      have : tokens'.length < tokens.length := val_le_length _ _ _ hypothesis
      match expr tokens' with
      | Except.ok (expression, tokens'') => Except.ok (Expr.minus value expression, tokens'')
      | Except.error e => Except.error e
    | Except.error e => Except.error e
  | tokens => Except.ok (Expr.lambda, tokens)
termination_by tokens => tokens.length

-- This proof was made with the help of Google Gemini
theorem expr_le_length (tokens : List Token) (expression : Expr) (tokens' : List Token)
  (hypothesis : expr tokens = Except.ok (expression, tokens')) : tokens'.length <= tokens.length := by
  unfold expr at hypothesis
  split at hypothesis
  -- case 1 : Token.plus :: Token.val :: tokens
  · repeat split at hypothesis
    -- happy path
    · have h1 := val_le_length _ _ _ (by assumption)
      have h2 := expr_le_length _ _ _ (by assumption)
      simp_all
      omega
    -- expr fails
    · simp_all
    -- val fails
    · simp_all
  -- case 2 : Token.minus :: Token.val :: tokens
  · repeat split at hypothesis
    -- happy path
    · have h1 := val_le_length _ _ _ (by assumption)
      have h2 := expr_le_length _ _ _ (by assumption)
      simp_all
      omega
    -- expr fails
    · simp_all
    -- val fails
    · simp_all
  -- case 3 : otherwise
  · simp_all
termination_by tokens.length
decreasing_by
  all_goals
    simp_all
    omega

def stmt : List Token -> Except String (Stmt × List Token)
  | Token.assign :: tokens => 
    match varId tokens with
    | Except.ok (variableId, tokens') =>
      match val tokens' with
      | Except.ok (value, tokens'') =>
        match expr tokens'' with
        | Except.ok (expression, tokens''') =>
          Except.ok (Stmt.assign variableId value expression, tokens''')
        | Except.error e => Except.error e
      | Except.error e => Except.error e
    | Except.error e => Except.error e
  | Token.print :: tokens =>
    match varId tokens with
    | Except.ok (variableId, tokens') => Except.ok (Stmt.printId variableId, tokens')
    | Except.error e => Except.error e
  | token :: _ => Except.error s!"Syntax error: Expected assignment or print statement but receiced '{token}'"
  | [] => Except.error "Syntax error: Unexpected EOL"

-- This proof I actually made myself.
theorem stmt_le_length (tokens : List Token) (statement : Stmt) (tokens' : List Token)
  (hypothesis : stmt tokens = Except.ok (statement, tokens')) : tokens'.length < tokens.length := by
  dsimp [stmt] at hypothesis
  split at hypothesis
  -- case 1: Token.assign :: tokens
  · repeat split at hypothesis
    -- happy path
    · have h1 := varId_le_length _ _ _ (by assumption)
      have h2 := val_le_length _ _ _ (by assumption)
      have h3 := expr_le_length _ _ _ (by assumption)
      simp_all
      omega
    -- varId fails
    · simp_all
    -- val fails
    · simp_all
    -- expr fails
    · simp_all
  -- case 2: Token.print :: tokens
  · split at hypothesis
    -- happy path
    · have h1 := varId_le_length _ _ _ (by assumption)
      simp_all
      omega
    -- varId fails
    · simp_all
  -- case 3: other token :: tokens
  · contradiction
  -- case 4: empty list
  · contradiction

def stmts : List Token -> Except String (Stmts × List Token)
  | tokens@(Token.assign :: _) | tokens@(Token.print :: _) => 
    match hypothesis : stmt tokens with
    | Except.ok (statement, tokens') =>
      have : tokens'.length < tokens.length := stmt_le_length _ _ _ hypothesis
      match stmts tokens' with
      | Except.ok (statements, tokens'') => Except.ok (Stmts.cons statement statements, tokens'')
      | Except.error e => Except.error e
    | Except.error e => Except.error e
  | tokens => Except.ok (Stmts.lambda, tokens)
termination_by tokens => tokens.length

def dcl : List Token -> Except String (Dcl × List Token)
  | Token.intdcl :: tokens =>
    match varId tokens with
    | Except.ok (id, tokens') => Except.ok ( Dcl.iNumDcl id, tokens')
    | Except.error e => Except.error e
  | token :: _ => Except.error s!"Expected '{Token.intdcl}' but received '{token}'"
  | [] => Except.error "Unexpected EOL!"

theorem dcl_le_length (tokens : List Token) (declaration : Dcl) (tokens' : List Token)
  (hypothesis : dcl tokens = Except.ok (declaration, tokens')) : tokens'.length < tokens.length := by
  dsimp [dcl] at hypothesis
  split at hypothesis
  -- case 1 : Token.intdcl
  · split at hypothesis
    -- Happy path
    · have h1 := varId_le_length _ _ _ (by assumption)
      simp_all
      omega
    -- varId fails
    · simp_all
  -- case 2 : other token
  · contradiction
  -- case 3 : empty list
  · contradiction    

def dcls : List Token -> Except String (Dcls × List Token)
  | tokens@(Token.intdcl :: _) => 
    match hypothesis : dcl tokens with
    | Except.ok (declaration, tokens') =>
      have : tokens'.length < tokens.length := dcl_le_length _ _ _ hypothesis
      match dcls tokens' with
      | Except.ok (declarations, tokens'') => Except.ok (Dcls.cons declaration declarations, tokens'')
      | Except.error e => Except.error e
    | Except.error e => Except.error e
  | tokens => Except.ok (Dcls.lambda, tokens)
termination_by tokens => tokens.length

def parseInput (tokens : List Token) : Except String Prog := do
  let (declarations, tokens') <- dcls tokens
  let (statements, _) <- stmts tokens'
  return Prog.mk declarations statements
  
