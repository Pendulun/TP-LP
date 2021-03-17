(* PlcChecker *)
use "Environ.sml";
use "Absyn.sml";

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

(*
fun checkListTypeEqType (h::[]):(plcType list) = 
	(checkEqType h)
	| checkListTypeEqType(h::t):(plcType list) =
	if (checkEqType h) then (checkListTypeEqType t) else false;

*)
	 
fun checkEqType (t:plcType):bool = 
	case t of BoolT => true
		| IntT => true
		| ListT([]) => true
		| SeqT (t2) => (checkEqType (t2))
		| ListT(l) => (List.all (checkEqType) l)
		| FunT(_) => false;

fun checkIsSeqType (t:plcType):bool = case t 
	of SeqT(_) => true
	| _ => false; 


(*fun checkBasicEqType (t:plcType = 
	case t of BoolT => true
		| IntT => true
		| ListT([]) => true
		| _ => false;*)


fun teval (e:expr,env:((string * plcType) list)) = 
	case e of 
		If(exp1,v1,v2) => 
			if (teval (exp1,env)) = BoolT 
			then 
				(let
					val tv1 = (teval (v1,env))
					val tv2 = (teval (v2,env))
				in
					if  tv1 = tv2
					then 
						tv1
					else
						raise NotEqTypes
				end)
			else
				raise IfCondNotBool
	| Prim2(ope,expr1,expr2) =>
		(case ope of "=" => (let
			val tv1 = (teval (expr1,env))
			val tv2 = (teval (expr2,env))
		in
			if tv1 = tv2
			then 
				if checkEqType tv1
				then BoolT
				else
					raise UnknownType (*TROCAR DEPOIS*)
			else
				raise NotEqTypes
		end))
	| ESeq(t) => if (checkIsSeqType (t)) then t else raise WrongRetType (*TROCAR DEPOIS*)
	| ConI(_) => IntT
	| ConB(_) => BoolT;


val progEst = If(Prim2("=", ESeq(SeqT(BoolT)), ESeq(SeqT(BoolT))), ConB true, ConB false);

teval (progEst,[]);




