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

partial def repl : IO Unit := do
  let stdin <- IO.getStdin
  repeat do
    let input <- stdin.getLine
    if input.isEmpty then
      break
    let trimmedInput := input.toSlice.trimAscii.toString
    if trimmedInput.isEmpty then
      continue
    match (<- processInput trimmedInput) with
    | Except.ok outputs => outputs.forM IO.println
    | Except.error err  => IO.println err

