%%

%name PlcParser

%pos int

%term SEMIC | COLON | DCOLON | COMMA | EXMARK
		| LPAREN | RPAREN | LBRACE | RBRACE
		| LBRACKET | RBRACKET
		| MINUS | PLUS | MULTI | DIV
		| EQ | NOTEQ | LESS | LESSEQ
		| AND | IF | THEN | ELSE
		| NIL | BOOL | INT
		| TRUE | FALSE
		| HD | TL | ISE | MATCH | WITH | PRINT
		| VAR | FUN | REC
		| FN | LEQARROW | REQARROW | END | DARROW 
		| EOF
		| NAME of string | CINT of int

%nonterm Prog | Decl | Expr | AtomExpr | AppExpr | Const | Comps | MatchExpr | CondExpr | Args | Params | TypedVar | Type | AtomType | Types

%right SEMIC DARROW DCOLON 
%left ELSE AND EQ NOTEQ LESS LEQARROW PLUS MINUS MULTI DIV LBRACKET
%nonassoc IF NOT HD TL ISE PRINT NAT 

%eop EOF

%noshift EOF

%start Prog

%%
