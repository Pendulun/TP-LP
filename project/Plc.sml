(* Plc interpreter main file *)
use "PlcChecker.sml";

fun run (e:expr, env:((string * plcType) list))=
	teval(e,env);

val expr = Prim2("&&", ConB true, ConB false);

run (expr, []);