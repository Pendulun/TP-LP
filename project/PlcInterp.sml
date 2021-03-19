(* PlcInterp *)

(*use "Environ.sml";
use "Absyn.sml";*) 

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(*fun checkEqType (t:plcType):bool = 
	case t of BoolT => true
		| IntT => true
		| ListT([]) => true
		| SeqT (t2) => (checkEqType (t2))
		| ListT(l) => (List.all (checkEqType) l)
		| FunT(_) => false;

fun checkIsSeqType (t:plcType):bool = case t 
	of SeqT(_) => true
	| _ => false; *)

fun eval (e:expr,env:((string * plcVal) list)) = 
	case e of 
		ConI(n) => IntV(n)
		| ConB(b) => BoolV(b)
		| Var(x) => lookup env x
		| If(exp1,v1,v2) => 
			let
				val eExp1 = eval(exp1,env)
			in
				case eExp1 of 
					BoolV(b) => 
						if (b) 
						then eval(v1,env)
						else eval(v2,env)
					| _ => raise Impossible
			end
			
		| Let(variavel, value, prog) => 
			let
				val evar = eval(value,env)
			in
				eval(prog,(variavel,evar)::env)
			end
		| List([]) => ListV([])
		| List(lista) => if (List.length lista) > 1 then
		 ListV((List.map (fn (x) => eval (x,env)) lista))
		else
			raise Impossible
	(*| Prim2(ope,expr1,expr2) =>
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
							raise UnknownType
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
				else if ope = ";" then
					(let
							val tv1 = (teval (expr1,env))
							val tv2 = (teval (expr2,env))
						in
							tv2
					end)
				else raise NotFunc
		)
	| ESeq(t) => if (checkIsSeqType (t)) then t else raise WrongRetType
	| Anon(tipos, lista, expr) => 
	let
		val texpr = teval(expr, (lista,tipos)::env)
	in
		FunT(tipos, texpr)
	end
	| Call(f,params) => 
	let
		val fType = teval(f,env)
		val tparams = teval(params, env)
	in
		case fType of 
		FunT(tipos,texpr) => 
			if tparams = tipos
			then
				texpr
			else 
				raise CallTypeMisM
		| _ =>	raise NotFunc
	end
	| Match(expComp, listaOp) => 
		let
			val texprComp = teval(expComp,env)
		in
			if List.null listaOp 
			then
				raise NoMatchResults
			else
				if List.all (fn((opcao,retorno)) => 
					case opcao of 
						SOME(tipo) => (teval (tipo,env)) = texprComp
						| NONE => true
					)  listaOp
				then
					(let
						val tPrimRetorno = teval((#2 (hd(listaOp))),env)
					in
						if List.all (fn((opcao,retorno)) => (teval(retorno,env)) = tPrimRetorno) listaOp
						then 
							tPrimRetorno
						else
							raise MatchResTypeDiff
					end)
					
				else
					raise MatchCondTypesDiff
		end
	
	| Letrec(nome, tipos, lista, tRetorno, corpo, prog) => 
		let
			val tcorpo = teval(corpo,(nome,FunT(tipos,tRetorno))::(lista, tipos)::env)
			val tprog = teval(prog, (nome,FunT(tipos,tRetorno))::env)
		in
			if tcorpo = tRetorno 
			then
				tprog
			else
				raise WrongRetType
		end
	
	| Item(n,expr) =>
	let
		val texpr = teval (expr,env)
	in
		case texpr of ListT([]) => raise ListOutOfRange 
			| ListT(h::t) => if n > ((List.length (h::t))) orelse n < 1
				then raise ListOutOfRange
				else List.nth ((h::t),n-1)
			| _ => raise OpNonList
	end
	
	
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
			else raise NotFunc*)