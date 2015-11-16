%{
open Crf_graph_interm
%}

%token <int> INT
%token <string> STRING
%token LPAREN RPAREN
%start main
%type <(int, string) Crf_graph_interm.t> main
%%

main: node { $1 }

node:
  INT                                { Ref $1 }
| LPAREN INT STRING node_list RPAREN { Node ($2, $3, List.rev $4) }

node_list:
                 { [] }
| node_list node { $2 :: $1 }