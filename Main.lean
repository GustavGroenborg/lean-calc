import LeanCalc.Lexer
import LeanCalc.Parser
import LeanCalc.Analyser
import LeanCalc.Evaluator
import LeanCalc.PrettyPrinter

def processInput (input : String) : IO (Except String (List String)) := do
  match lexInput input.splitOn with
  | Except.error err => return Except.error err
  | Except.ok tokens =>
    match parseInput tokens with
    | Except.error err => return Except.error err
    | Except.ok prog =>
      IO.println (prettyPrint prog)
      return evaluate prog

partial def main : IO Unit := do
  let stdin <- IO.getStdin
  repeat do
    let input  <- stdin.getLine
    if input.isEmpty then break
    let input' := input.toSlice.trimAscii.toString
    let input'' <- processInput input'
    match input'' with
    | Except.ok output => output.forM IO.println
    | Except.error err => IO.println err
