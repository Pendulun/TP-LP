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
		| NILV | NILT | BOOLT | INTT
		| TRUE | FALSE
		| HD | TL | ISE | MATCH | WITH | PRINT
		| VAR | FUN | REC
		| FN | REQARROW | END | DRARROW 
		| EOF
		| PIPE | UNDER
		| NAME of string | CINT of int

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr | AppExpr of expr | Const of expr | Comps of expr list | MatchExpr of expr option * expr | CondExpr of expr option | Args of (plcType * string) list | Params of (plcType * string) list | TypedVar of (plcType * string) | Type of plcType | AtomType of plcType | Types of plcType list

%right SEMIC DRARROW 
%nonassoc IF
%left ELSE
%left AND
%left EQ NOTEQ
%left LESS LESSEQ
%right DCOLON
%left PLUS MINUS
%left MULTI DIV
%nonassoc EXMARK HD TL ISE PRINT NAME
%left LBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

(*%prec*)	
(*fun_app*)

Prog : Expr (Expr)
	| Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
		(*Let ("f", Anon (IntT, "x", Var "x"), Call ("f", ConI 1))*)
	| FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog)) 
	| FUN REC NAME Args COLON Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomExpr (AtomExpr)
	| AppExpr (AppExpr)
	| IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
	| MATCH Expr WITH MatchExpr END (Match(Expr, MatchExpr::[]))
	| EXMARK Expr (Prim1("!", Expr))
	| MINUS Expr (Prim1("-", Expr))
	| HD Expr (Prim1("hd", Expr))
	| TL Expr (Prim1("tl", Expr))
	| ISE Expr (Prim1("ise", Expr))
	| PRINT Expr (Prim1("print", Expr))
	| Expr AND Expr (Prim2("&&", Expr1, Expr2))
	| Expr PLUS Expr (Prim2("+", Expr1, Expr2))
	| Expr MINUS Expr (Prim2("-", Expr1, Expr2))
	| Expr MULTI Expr (Prim2("*", Expr1, Expr2))
	| Expr DIV Expr (Prim2("/", Expr1, Expr2))
	| Expr EQ Expr (Prim2("=", Expr1, Expr2))
	| Expr NOTEQ Expr (Prim2("!=", Expr1, Expr2))
	| Expr LESS Expr (Prim2("<", Expr1, Expr2))
	| Expr LESSEQ Expr (Prim2("<=", Expr1, Expr2))
	| Expr DCOLON Expr (Prim2("::", Expr1, Expr2))
	| Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
	| Expr LBRACKET CINT RBRACKET (Item(CINT, Expr))

AtomExpr : Const (Const)
	| NAME (Var(NAME))
	| LBRACE Prog RBRACE (Prog)
	| LPAREN Expr RPAREN (Expr)
	| LPAREN Comps RPAREN (List(Comps))
	| FN Args REQARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
	| AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : TRUE (ConB(true))
	| FALSE (ConB(false))
	| CINT (ConI(CINT))
	| NILV (List([]))
	| LPAREN Type LBRACKET RBRACKET RPAREN (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
	| Expr COMMA Comps (Expr::Comps)

MatchExpr : END ()
	| PIPE CondExpr DRARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr::[])

CondExpr : Expr (SOME (Expr))
	| UNDER (NONE)

Args : LPAREN RPAREN ([])
	| LPAREN Params RPAREN (Params)

Params : TypedVar (TypedVar)
	| TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type NAME ((Type, NAME))
	
Type : AtomType (AtomType)
	| LPAREN Types RPAREN (ListT(Types))
	| LBRACKET Type RBRACKET (SeqT(Type))
	| Type DRARROW Type (FunT(Type1,Type2))

AtomType : NILT (ListT([]))
	| BOOLT (BoolT)
	| INTT (IntT)
	| LPAREN Type RPAREN (Type)

Types : Type COMMA Type (Type1::Type2::[])
	| Type COMMA Types (Type::Types)