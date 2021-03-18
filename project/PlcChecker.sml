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
		(
			if ope = "=" orelse ope = "!=" then 
				(let
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
			end) 
			else if ope = "<" orelse ope = "<=" then
				(let
					val tv1 = (teval (expr1,env))
					val tv2 = (teval (expr2,env))
				in
					if tv1 = tv2 
					then 
						if tv1 = IntT
							then BoolT
						else
							raise UnknownType 
					else
						raise NotEqTypes
				end) 
			else if ope = "+" orelse ope = "-" orelse ope = "*" orelse ope = "/" then
				(let
					val tv1 = (teval (expr1,env))
					val tv2 = (teval (expr2,env))
				in
					if tv1 = tv2 
					then 
						if tv1 = IntT
							then IntT
						else
							raise UnknownType 
					else
						raise NotEqTypes
				end) else if ope = "&&" then
					(let
							val tv1 = (teval (expr1,env))
							val tv2 = (teval (expr2,env))
						in
							if tv1 = tv2
							then 
								if tv1 = BoolT
								then BoolT
								else
									raise UnknownType 
							else
								raise NotEqTypes
					end)
				else if ope = "::" then
					(let
							val tv1 = (teval (expr1,env))
							val tv2 = (teval (expr2,env))
						in
							if tv2 =  SeqT(tv1)
							then 
								SeqT(tv1)
							else
								raise UnknownType
					end)
				else raise NotFunc
		)
	| ESeq(t) => if (checkIsSeqType (t)) then t else raise WrongRetType (*TROCAR DEPOIS*)
	| ConI(_) => IntT
	| ConB(_) => BoolT
	| Let(variavel, value, prog) => 
			let
				val tvar = teval(value,env)
			in
				teval(prog,(variavel,tvar)::env)
			end
	| Var(x) => lookup env x
	| List([]) => ListT([])
	| List(lista) => if (List.length lista) > 1 then
		 ListT((List.map (fn (x) => teval (x,env)) lista))
		else
			raise UnknownType
	| Prim1(ope,expr) => 
			if ope = "!" then
				if (teval (expr,env)) = BoolT
				then
					BoolT
				else
					raise WrongRetType
			else if ope = "-" then
				if (teval (expr,env)) = IntT
				then
					IntT
				else
					raise WrongRetType
			else if ope = "hd" then
				let
					val texpr = (teval (expr,env))
				in
					case texpr of 
						SeqT(IntT) => IntT
					|	SeqT(BoolT) => BoolT
					|	SeqT(FunT(a,b)) => FunT(a,b)
					|	SeqT(ListT(l)) => ListT(l)
					|	SeqT(SeqT(a)) => SeqT(a)
					| _ => raise UnknownType
				end
			else if ope = "tl" then
				let
					val texpr = (teval (expr,env))
				in
					case texpr of 
						SeqT(_) => texpr
					| _ => raise UnknownType
				end
			else if ope = "ise" then
				let
					val texpr = (teval (expr,env))
				in
					case texpr of 
						SeqT(_) => BoolT
					| _ => raise UnknownType
				end
			else if ope = "print" then
				let
					val texpr = (teval (expr,env))
				in
					ListT([])
				end
			else raise NotFunc