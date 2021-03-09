(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(*Necessário para transformar String em Int no CINT*)
fun strToInt s = 
	case Int.fromString s of
		SOME i => i
		| NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(*Verifica se o identifier se refere a uma palavra reservada ou não*)
fun keyword (s,lpos,rpos) = 
	case s of
	"if"  => (IF(lpos,rpos))
	| "then" => (THEN(lpos,rpos))
	| "else" => (ELSE(lpos,rpos))
	| "Nil" => (NILT(lpos,rpos))
	| "Bool" => (BOOLT(lpos,rpos))
	| "Int" => (INTT(lpos,rpos))
	| "true" => (TRUE(lpos,rpos))
	| "false" => (FALSE(lpos,rpos))
	| "hd" => (HD(lpos,rpos))
	| "tl" => (TL(lpos,rpos))
	| "ise" => (ISE(lpos,rpos))
	| "match" => (MATCH(lpos,rpos))
	| "with" => (WITH(lpos,rpos))
	| "print" => (PRINT(lpos,rpos))
	| "var" => (VAR(lpos,rpos))
	| "fun" => (FUN(lpos,rpos))
	| "rec" => (REC(lpos,rpos))
	| "fn" => (FN(lpos,rpos))
	| "end" => (END(lpos,rpos))
	| _ => (NAME(s,lpos,rpos))

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()

%%

%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(strToInt(yytext), yypos, yypos)); (*CINT*)
{identifier} => (keyword(yytext, yypos, yypos)); (*NAME??*) (*IMPLEMENTAR KEYWORD!*)

";" => (SEMIC(yypos, yypos));
":" => (COLON(yypos, yypos));
"::" => (DCOLON(yypos, yypos));	(*CASARÁ A MAIOR REGRA*)
"," => (COMMA(yypos, yypos));
"!" => (EXMARK(yypos, yypos));

"(" => (LPAREN(yypos, yypos));
")" => (RPAREN(yypos, yypos));
"{" => (LBRACE(yypos, yypos)); 
"}" => (RBRACE(yypos, yypos)); 
"[" => (LBRACKET(yypos, yypos)); 
"]" => (RBRACKET(yypos, yypos)); 
"-" => (MINUS(yypos, yypos)); 
"+" => (PLUS(yypos, yypos)); 
"*" => (MULTI(yypos, yypos)); 
"/" => (DIV(yypos, yypos)); 

"=" => (EQ(yypos, yypos)); 
"!=" => (NOTEQ(yypos, yypos)); 
"<" => (LESS(yypos, yypos)); 
"<=" => (LESSEQ(yypos, yypos)); 

"&&" => (AND(yypos, yypos)); (*CONJUNÇÃO BOOLEANA*) 

"()" => (NILV(yypos, yypos));

"=>" => (REQARROW(yypos, yypos));
"->" => (DRARROW(yypos, yypos));

"|" => (PIPE(yypos,yypos))
"_" => (UNDER(yypos,yypos))

(*NÃO SEI SE ALGUMA REGRA É ESCRITA PARA O EOF*)