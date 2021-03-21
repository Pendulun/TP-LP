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
		val typeEnv = []; (*("x",IntT)::[];*)
		val valueEnv = []; (*("x", IntV(5))::[];*)
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

(*val expr = fromString "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(1)";*)

(*val expr = fromString "print 5; print 6"; *)
(*val expr = Letrec("f1",IntT,"x",IntT,Prim2("+",Var "x",ConI 1),Call(Var "f1",ConI 12));*)
(*Casos do monitor*)
(*
val expr = If(Prim2("=", ConI 11, ConI 12), ConI 1, ConI 0);
val expr = Let("b", Prim2("=", ConI 1, ConI 2), If(Var "b", ConI 3, ConI 4));

val expr = Letrec("f1",IntT,"x",IntT,Prim2 ("+",Var "x",ConI 1),Call (Var "f1",ConI 12));
val expr = Let("b", Prim2("=", ConI 1, ConI 2),If(Var "b", Var "b", ConI 6));
val expr = Let("f",Anon (BoolT,"x",If (Var "x",ConI 11,ConI 22)),Call (Var "f",ConI 0));
val expr = Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true));
val expr = fromString "print x";
run (expr);
*)

(*Comentar o use Absyn.sml de testParserCases.sml*)
val expr1 = fromString "fun f (Int x) = {fun g(Int y) = x+y; g}; f(3)(4)";
val expr2 = fromString "fun f (Int x) = fn (Int y) => x+y end; f(3)(4)";
val expr3 = fromString "fun inc (Int x) = x + 1; fun add (Int x, Int y) = x + y; fun cadd (Int x) = fn (Int y) => x + y end; var y = add(3, inc(4)); var x = cadd(3)(7-y); var z = x * 3; fun rec fac (Int n) : Int = match n with | 0 -> 1 | 1 -> 1 | _ -> n * fac(n - 1) end; print x; print y; x :: y :: z :: fac(z) :: ([Int] [])";
val expr4 = fromString "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";

run (expr1);
run (expr2);
run (expr3);
run (expr4);

(*List.app (fn ((_,e)) => print(run(e) ^" \n") ) cases;*)