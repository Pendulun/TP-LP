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
		EmptySeq => "***Erro: hd ou tl em sequencia de tipo Nil.***"
		| UnknownType => "***Erro: Ocorreu erro de tipagem.***"
		| NotEqTypes => "***Erro: Comparacao entre tipos diferentes.***"
		| WrongRetType => "***Erro: Tipo de retorno de funcao nao condiz com seu corpo.***"
		| DiffBrTypes => "***Erro: Tipos dos branches de If divergem.***"
		| IfCondNotBool => "***Erro: Condicao de If nao e do tipo booleano.***"
		| NoMatchResults => "***Erro: Lista de expressoes de Match e vazia.***"
		| MatchResTypeDiff => "***Erro: Tipo de caso de Match difere dos demais.***"
		| MatchCondTypesDiff => "***Erro: Tipo das opcoes de Match difere do tipo da expressao passada para Match.***"
		| CallTypeMisM => "***Tipo de parametro passado para funcao nao suportado.***"
		| NotFunc => "***Erro: Tentativa de chamar algo que nao e uma funcao.***"
		| ListOutOfRange => "***Erro: Tentativa de acessar uma posicao fora dos limites da lista.***"
		| OpNonList => "***Erro: Tentativa de acessar um elemento em uma expressao que nao e uma lista.***"
		(*Environ*)
		| SymbolNotFound => "***Erro: Uma variavel ou funcao nao foi declarada.***"
		| 

(*val expr = fromString "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(1)";*)
val expr = fromString "hd(([Int] []))"; 
run (expr);