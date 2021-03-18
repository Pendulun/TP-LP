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
	teval(e,env);

val expr = fromString "fun rec f (Int x) : Int = if x <= 0 then 0 else 1; f(5)";
run (expr, []);