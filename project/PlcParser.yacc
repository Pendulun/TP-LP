%%

%name PlcParser

%pos int

(*TUPLA*)
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

%right SEMIC DARROW 
%nonassoc IF
%left ELSE
%left AND
%left EQ NOTEQ
%left LESS LEQARROW
%right DCOLON
%left PLUS MINUS
%left MULTI DIV
%nonassoc NOT HD TL ISE PRINT NAME
%left LBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

(*%prec*)
(*fun_app*)

Prog : Expr (Expr)
	| Decl SEMIC Prog ()

Decl : VAR NAME EQ Expr ()
	| FUN NAME Args EQ Expr () (*Let ("f", Anon (IntT, "x", Var "x"), Call ("f", ConI 1))*)
	| FUN REC NAME Args COLON Type EQ Expr ()

Expr : AtomExpr (AtomExpr)
	| AppExpr (AppExpr)
	| IF Expr THEN Expr ELSE Expr ()
	| MATCH Expr WITH MatchExpr ()
	| EXMARK Expr ()
	| MINUS Expr ()
	| HD Expr ()
	| TL Expr ()
	| ISE Expr ()
	| PRINT Expr ()
	| Expr AND Expr ()
	| Expr PLUS Expr ()
	| Expr MINUS Expr ()
	| Expr MULTI Expr ()
	| Expr DIV Expr ()
	| Expr EQ Expr ()
	| Expr NOTEQ Expr ()
	| Expr LESS Expr ()
	| Expr LESSEQ Expr ()
	| Expr DCOLON Expr ()
	| Expr SEMIC Expr ()
	| Expr LBRACKET CINT RBRACKET ()

AtomExpr : Const (Const)
	| NAME ()
	| LBRACE Prog RBRACE
	| LPAREN Expr RPAREN
	| LPAREN Comps RPAREN
	| FN Args REQARROW Expr END

AppExpr : AtomExpr AtomExpr ()
	| AppExpr AtomExpr ()

Const : TRUE ()
	| FALSE ()
	| CINT ()
	| LPAREN RPAREN ()
	| LPAREN Type LBRACKET RBRACKET RPAREN ()

Comps : Expr COMMA Expr ()
	| Expr COMMA Comps ()

MatchExpr : END ()
	| (*???*)

CondExpr : Expr (Expr)
	| (*???*)

Args : LPAREN RPAREN ()
	| LPAREN Params RPAREN ()

Params : TypedVar (TypedVar)
	| TypedVar COMMA Params ()

TypedVar : Type NAME ()
	
Type : AtomType (AtomType)
	| LPAREN Types RPAREN ()
	| LBRACKET Type RBRACKET ()
	| Type DARROW Type ()

AtomType : NIL ()
	| BOOL ()
	| INT ()
	| LPAREN Type RPAREN ()

Types : Type COMMA Type ()
	| Type COMMA Types ()





