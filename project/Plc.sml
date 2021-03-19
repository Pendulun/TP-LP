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

fun run (e:expr)= (*SEM ENV MESMO?*)
	let
		val typeEnv = [];
		val valueEnv = [];

		val eTypeChecking = teval(e,typeEnv);
		val eInterpreting = eval(e,valueEnv);
	in
		(val2string eInterpreting) ^ " : " ^ (type2string eTypeChecking) 
	end
	

val expr = fromString "(3, true, 5, false)";
run (expr);