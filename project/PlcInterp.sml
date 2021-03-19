(* PlcInterp *)

(*use "Environ.sml";
use "Absyn.sml";*) 

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun getIntV(n:plcVal) = 
	case n of IntV(a) => a
		| _ => raise Impossible 

fun getBoolV(v:plcVal) = 
	case v of BoolV(a) => a
		| _ => raise Impossible 


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
		| ESeq(t) => SeqV([])
		| Prim1(ope,expr) => 
			if ope = "!" then
				let
					val eExp = eval(expr,env)
				in
					case eExp of 
						BoolV(true) => BoolV(false)
						| BoolV (false) => BoolV(true)
						| _ => raise Impossible
				end
			else if ope = "-" then
				let
					val eExp = eval(expr,env)
				in
					case eExp of 
						IntV(n) => IntV(~n)
						| _ => raise Impossible
				end
			else if ope = "hd" then
				let
					val eExpr = (eval (expr,env))
				in
					case eExpr of 
						SeqV([]) => raise HDEmptySeq
						| SeqV(lista) => hd(lista)
						| _ => raise Impossible
				end
			else if ope = "tl" then
				let
					val eExpr = (eval (expr,env))
				in
					case eExpr of 
						SeqV([]) => raise TLEmptySeq
						| SeqV(lista) => SeqV(tl(lista))
						| _ => raise Impossible
				end
			else if ope = "ise" then
				let
					val eExpr = (eval (expr,env))
				in
					case eExpr of 
						SeqV(lista) => BoolV(List.null lista)
					| _ => raise Impossible
				end
			else if ope = "print" then
				let
					val eExpr = (eval (expr,env))
				in
					print(val2string(eExpr)^"\n");
					ListV([])
				end
			else raise Impossible
		| Prim2(ope,expr1,expr2) =>
			(
				if ope = "=" orelse ope = "!=" then 
					(let
						val eExp1 = (eval (expr1,env))
						val eExp2 = (eval (expr2,env))
					in
						if ope = "=" then BoolV(eExp1 = eExp2) else BoolV(not (eExp1 = eExp2))
					end) 
				else if ope = "<" orelse ope = "<=" then
					(let
						val eExp1 = (eval (expr1,env))
						val eExp2 = (eval (expr2,env))
					in
						if ope = "<" then BoolV(getIntV(eExp1) < getIntV(eExp2)) else BoolV(getIntV(eExp1) <= getIntV(eExp2))
					end) 
				else if ope = "+" orelse ope = "-" orelse ope = "*" orelse ope = "/" then
					(let
						val eExp1 = (eval (expr1,env))
						val eExp2 = (eval (expr2,env))
					in
						if ope = "+" then IntV(getIntV(eExp1)+getIntV(eExp2))
						else if ope = "-" then IntV(getIntV(eExp1)-getIntV(eExp2))
						else if ope = "*" then IntV(getIntV(eExp1)*getIntV(eExp2))
						else if ope = "/" then IntV(getIntV(eExp1) div getIntV(eExp2))
						else raise Impossible
					end)
				else if ope = "&&" then
						(let
							val eExp1 = (eval (expr1,env))
							val eExp2 = (eval (expr2,env))
						in
							BoolV(getBoolV(eExp1) andalso getBoolV(eExp2))
						end)
				else if ope = "::" then
					(let
						val eExp1 = (eval (expr1,env))
						val eExp2 = (eval (expr2,env))
					in
						case eExp1 of SeqV(a) => (
							case eExp2 of SeqV(b) => SeqV(a@b)
								| _ => SeqV(eExp2::a)
								)
						| _ => (
								case eExp2 of SeqV([]) => SeqV(eExp1::[]) 
								| SeqV(b) => SeqV(eExp1::b))
					end)
				else if ope = ";" then
					(let
						val eExp1 = (eval (expr1,env))
						val eExp2 = (eval (expr2,env))
					in
						eExp2
					end)
					else raise NotFunc
			)
		| Item(n,expr) =>
			let
				val eExpr = eval (expr,env)
			in
				case eExpr of
					ListV(lista) => List.nth (lista, n-1)
					| _ => raise Impossible
			end
		
	(*| Anon(tipos, lista, expr) => 
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
	
	
	*)