(* Plc interpreter main file *)
CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "Environ.sml";
use "Absyn.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";
use "testParserCases.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run (e:expr) =
	(let
		(*No caso de passar um env não vazio, AMBAS as listas, typeEnv e valueEnv devem ser modificadas.*)
		val typeEnv = []; (*Exemplo para env não vazio: ("x",IntT)::[];*)
		val valueEnv = []; (*Exemplo para env não vazio: ("x", IntV(5))::[];*)
	in
		let 
			val eTypeChecking = teval(e,typeEnv);
			val eInterpreting = eval(e,valueEnv);
		in
			(val2string eInterpreting) ^ " : " ^ (type2string eTypeChecking)
		end
	end) handle 
		(*Checker*)
		EmptySeq => "***TypeCheckerError: Operacao hd ou tl em sequencia de tipo Nil.***"
		| UnknownType => "***TypeCheckerError: Ocorreu erro de tipagem.***"
		| NotEqTypes => "***TypeCheckerError: Comparacao entre tipos diferentes.***"
		| WrongRetType => "***TypeCheckerError: Tipo de retorno de funcao nao condiz com seu corpo.***"
		| DiffBrTypes => "***TypeCheckerError: Tipos dos branches de If divergem.***"
		| IfCondNotBool => "***TypeCheckerError: Condicao de If nao e do tipo booleano.***"
		| NoMatchResults => "***TypeCheckerError: Lista de expressoes de Match e vazia.***"
		| MatchResTypeDiff => "***TypeCheckerError: Tipo de caso de Match difere dos demais.***"
		| MatchCondTypesDiff => "***TypeCheckerError: Tipo das opcoes de Match difere do tipo da expressao passada para Match.***"
		| CallTypeMisM => "***TypeCheckerError: Tipo de parametro passado para funcao diferente do esperado.***"
		| NotFunc => "***TypeCheckerError: Tentativa de chamar algo que nao e uma funcao.***"
		| ListOutOfRange => "***TypeCheckerError: Tentativa de acessar uma posicao fora dos limites da lista.***"
		| OpNonList => "***TypeCheckerError: Tentativa de acessar um elemento em uma expressao que nao e uma lista.***"
		(*Environ*)
		| SymbolNotFound => "***EnvironmentError: Uma variavel ou funcao nao foi encontrada no ambiente.***"
		(*Interp*)
		| Impossible => "***InterpreterError: Ocorreu erro durante a interpretacao.***"
		| HDEmptySeq => "***InterpreterError: Tentativa de operacao hd em sequencia vazia.***"
		| TLEmptySeq => "***InterpreterError: Tentativa de operacao tl em sequencia vazia.***"
		| ValueNotFoundInMatch => "***InterpreterError: Nenhuma opcao de Match casou com a expressao passada para Match.***"
		| NotAFunc => "***InterpreterError: Tentativa de chamar algo que nao e uma funcao.***";


(*Usando testParserCases.sml. (OBS.: Comentar o use Absyn.sml)*)
(*List.app (fn ((_,e)) => print(run(e) ^" \n") ) cases;*)

(*Casos Corretos Fórum*)
(*val expr = If(Prim2("=", ConI 11, ConI 12), ConI 1, ConI 0);
val expr = Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4));
val expr = Letrec("f1",IntT,"x",IntT,Prim2 ("+",Var "x",ConI 1),Call (Var "f1",ConI 12));*)

(*Casos Incorretos Fórum*)
(*val expr = Let("b", Prim2("=", ConI 1, ConI 2),If(Var "b", Var "b", ConI 6));
val expr = Let("f",Anon (BoolT,"x",If (Var "x",ConI 11,ConI 22)),Call (Var "f",ConI 0));
val expr = Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true));*)

(*Alguns outros casos*)
(*val expr = fromString "fun rec f(Int a): Int = if a < 1 then 0 else f (a-1); f(5)";
val expr = fromString "fun rec f(Int a): Int = if a < 1 then 0 else a+f (a-1); f(5)";

val expr = fromString "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(1)";
val expr = Letrec("f1",IntT,"x",IntT,Prim2("+",Var "x",ConI 1),Call(Var "f1",ConI 12));*)

(*ConI*)
(*val expr = fromString "5";*)

(*ConB*)
(*val expr = fromString "false";*)

(*Var e Let*)
(*val expr = fromString "var x = 5; x";*)
(*val expr = fromString "var x = 5; y";*)

(*If*)
(*val expr = fromString "if true then 1 else 0";*)
(*val expr = fromString "if 5 then 1 else 0";*)
(*val expr = fromString "if true then 1 else false";*)
(*val expr = fromString "var x = 5; if true then x else 0";*)

(*Nil*)
(*val expr = fromString "()";*)

(*List*)
(*val expr = fromString "(1)";*)
(*val expr = fromString "(1, true)";*)
(*val expr = fromString "var x = 5; (true, x, 2)";*)
(*val expr = fromString "(5, if true then true else false, 7)";*)

(*Eseq*)
(*val expr = fromString "([Int] [])";*)
(*val expr = fromString "([[Bool]] [])";*)
(*val expr = fromString "(Int [])";*)

(*Prim1*)
(*val expr = fromString "!(if true then false else true)";*)
(*val expr = fromString "var x = 3; !3";*)
(*val expr = fromString "var true = 5; !true";*)
(*val expr = fromString "var b = true; !b";*)

(*val expr = fromString "-true";*)
(*val expr = fromString "var x = 23; -x";*)

(*val expr = fromString "hd(3::2::1::([Int] []))";*)
(*val expr = fromString "hd((1, 2, 3))";*)
(*val expr = fromString "hd(([Int] []))";*)
(*val expr = fromString "hd(()::()::([Nil] []))";*)

(*val expr = fromString "tl(3::2::1::([Int] []))";*)
(*val expr = fromString "tl((1, 2, 3))";*)
(*val expr = fromString "tl(([Int] []))";*)
(*val expr = fromString "tl(()::()::([Nil] []))";*)

(*val expr = fromString "ise(3::2::1::([Int] []))";*)
(*val expr = fromString "ise((1, 2, 3))";*)
(*val expr = fromString "ise(([Int] []))";*)
(*val expr = fromString "ise(()::()::([Nil] []))";*)

(*val expr = fromString "print(4)";*)
(*val expr = fromString "print(if true then 3 else 0)";*)
(*val expr = fromString "print(3::2::1::([Int] []))";*)

(*Prim2*)
(*val expr = fromString "5=2";*)
(*val expr = fromString "5=true";*)
(*val expr = fromString "fun a(Int x) = x; fun b(Int x) = x; a = b";*)
(*val expr = fromString "var a = 3; var b = 3; a=b";*)
(*val expr = fromString "var a = 3; fun a(Int x) = -3; a";*)
(*val expr = fromString "fun a(Int x) = -3; var a = 3; a";*)

(*val expr = fromString "5!=2";*)
(*val expr = fromString "5!=true";*)

(*val expr = fromString "3<4";*)
(*val expr = fromString "4<=4";*)
(*val expr = fromString "true<false";*)
(*val expr = fromString "4<false";*)

(*val expr = fromString "5+true";*)
(*val expr = fromString "true+true";*)
(*val expr = fromString "5+3";*)
(*val expr = fromString "5-3";*)
(*val expr = fromString "5*3";*)
(*val expr = fromString "10/5";*)
(*val expr = fromString "10/(if true then 5 else 2)";*)

(*val expr = fromString "true && true";*)
(*val expr = fromString "true && 5";*)
(*val expr = fromString "(if true then 5 else 3) && 5";*)

(*val expr = fromString "(1::2::3::([Int] []))::(4::5::6::([Int] []))::([[Int]] [])";*)
(*val expr = fromString "fun f(Int x) = x; fun g(Int x) = -x; f::g::([Int -> Int] [])";*)
(*val expr = fromString "1::true::([[Int]] [])";*)
(*val expr = fromString "fun f(Int x) = x; fun g(Int x) = -x; var seqF = f::g::([Int -> Int] []); (hd(seqF))(10)";*)
(*val expr = fromString "fun f(Int x) = x; fun g(Int x) = -x; var seqF = f::g::([Int -> Int] []); (hd(tl(seqF)))(10)";*)

(*Item*)
(*val expr = fromString "(1,2,3)[2]";*)
(*val expr = fromString "(1,2,3)[4]";*)
(*val expr = fromString "true[4]";*)
(*val expr = fromString "fun f(Int x) = x; fun g(Int x) = -x; var listaF = (f,g); (listaF[2])";*)
(*val expr = fromString "fun f(Int x) = x; fun g(Int x) = -x; var listaF = (f,g); (listaF[2])(3)";*)
(*val expr = fromString "()[1]";*)
(*val expr = fromString "(3)[1]";*)

(*Match*)
(*val expr = fromString "match 5 with | 5->true | _->false end";*)
(*val expr = fromString "match 5 with | 5->true | _->false; 7";*)
(*val expr = fromString "match 5 with | 5->true | _->false end; 7";*)
(*val expr = fromString "match 5 with end";*)
(*val expr = fromString "match 5 with | 5->true end";*)
(*val expr = fromString "match 5 with | 3->true end";*)
(*val expr = fromString "match 5 with | 5->true | 3->1 end";*)
(*val expr = fromString "match true with | 5->true | 3->1 end";*)
(*val expr = fromString "match 5 with | 5->(fn(Int x) => x*2 end) | 3->(fn(Int x) => x*3 end) end";*)

(*Letrec*)
(*val expr = fromString "fun rec f(Int x) : Bool = true; f(1)";*)
(*val expr = fromString "fun rec f(Int x) : Int = if 3 < x then f(x-1) else x ; f(1)";*)
(*val expr = fromString "fun rec f(Int x) : Int = if 3 < x then f(x-1) else x ; f(5)";*)
(*val expr = fromString "fun rec f(Int x) : Int = f(x-1) ; f(5)";*)
(*val expr = fromString "fun rec f(Int x) : Bool = x ; f(5)";*)




(*Para executar o Interpretador*)
run(expr);

(*val expr = fromString "";*)