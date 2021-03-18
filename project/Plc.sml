(* Plc interpreter main file *)
CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "Environ.sml";
use "PlcChecker.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run (e:expr, env:((string * plcType) list))=
	let
		val eTypeChecking = teval(e,env);
		val eInterpreting = eval(e,env);
	in
		(val2string eInterpreting) ^ " : " ^ (type2string eTypeChecking) 
	end
	

val expr = fromString "match n with end";
run (expr, []);