(* Plc interpreter main file *)
use "PlcChecker.sml";

fun run (e:expr, env:((string * plcType) list))=
	teval(e,env);

val expr = Prim1("tl", ESeq(SeqT(IntT)));

run (expr, []);