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
	| "_" => (UNDER(lpos,rpos))
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
%s COMMENT;

alpha=[A-Za-z];
digit=[0-9]+;
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (CINT(strToInt(yytext), yypos, yypos)); 
<INITIAL>{identifier} => (keyword(yytext, yypos, yypos));

<INITIAL>";" => (SEMIC(yypos, yypos));
<INITIAL>":" => (COLON(yypos, yypos));
<INITIAL>"::" => (DCOLON(yypos, yypos));
<INITIAL>"," => (COMMA(yypos, yypos));
<INITIAL>"!" => (EXMARK(yypos, yypos));

<INITIAL>"(" => (LPAREN(yypos, yypos));
<INITIAL>")" => (RPAREN(yypos, yypos));
<INITIAL>"{" => (LBRACE(yypos, yypos)); 
<INITIAL>"}" => (RBRACE(yypos, yypos)); 
<INITIAL>"[" => (LBRACKET(yypos, yypos)); 
<INITIAL>"]" => (RBRACKET(yypos, yypos)); 
<INITIAL>"-" => (MINUS(yypos, yypos)); 
<INITIAL>"+" => (PLUS(yypos, yypos)); 
<INITIAL>"*" => (MULTI(yypos, yypos)); 
<INITIAL>"/" => (DIV(yypos, yypos)); 

<INITIAL>"=" => (EQ(yypos, yypos)); 
<INITIAL>"!=" => (NOTEQ(yypos, yypos)); 
<INITIAL>"<" => (LESS(yypos, yypos)); 
<INITIAL>"<=" => (LESSEQ(yypos, yypos)); 

<INITIAL>"&&" => (AND(yypos, yypos));

<INITIAL>"=>" => (REQARROW(yypos, yypos));
<INITIAL>"->" => (DRARROW(yypos, yypos));

<INITIAL>"|" => (PIPE(yypos,yypos));

<INITIAL>"(*" => (YYBEGIN COMMENT;lex());

<INITIAL>. => (error("\n***Lexer error: bad character***\n"); raise Fail("Lexer error: bad character "^ yytext));

<COMMENT>"*)" => (YYBEGIN INITIAL; lex());
<COMMENT>. => (lex());

