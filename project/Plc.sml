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

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run (e:expr) =
	(let
		val typeEnv = [];
		val valueEnv = [];

		val eTypeChecking = teval(e,typeEnv);
		val eInterpreting = eval(e,valueEnv);
	in
		(val2string eInterpreting) ^ " : " ^ (type2string eTypeChecking)
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
		| CallTypeMisM => "***TypeCheckerError: Tipo de parametro passado para funcao nao suportado.***"
		| NotFunc => "***TypeCheckerError: Tentativa de chamar algo que nao e uma funcao.***"
		| ListOutOfRange => "***TypeCheckerError: Tentativa de acessar uma posicao fora dos limites da lista.***"
		| OpNonList => "***TypeCheckerError: Tentativa de acessar um elemento em uma expressao que nao e uma lista.***"
		(*Environ*)
		| SymbolNotFound => "***EnvironmentError: Uma variavel ou funcao nao foi declarada.***"
		(*Interp*)
		| Impossible => "***InterpreterError: Ocorreu erro durante a interpretacao.***"
		| HDEmptySeq => "***InterpreterError: Tentativa de operacao hd em sequencia vazia.***"
		| TLEmptySeq => "***InterpreterError: Tentativa de operacao tl em sequencia vazia.***"
		| ValueNotFoundInMatch => "***InterpreterError: Nenhuma opcao de Match casou com a expressao passada para Match.***"
		| NotAFunc => "***InterpreterError: Tentativa de chamar algo que nao e uma funcao.***"

(*val expr = fromString "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(1)";*)
val expr = fromString "hd(([Int] []))"; 
run (expr);