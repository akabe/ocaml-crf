{
open Crf_graph_parser
}

rule token = parse
  [' ' '\t' '\n' '\r'] { token lexbuf }
| '('                  { LPAREN }
| ')'                  { RPAREN }
| eof                  { raise End_of_file }
| ['0'-'9']+           { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '\"' ([^ '\\' '\"'] | '\\' _)* '\"'
  { let s = Lexing.lexeme lexbuf in
    STRING (String.sub s 1 (String.length s - 2)
            |> Scanf.unescaped) }