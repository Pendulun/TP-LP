(* PlcInterp *)

(*Comentei esses imports por que já os importo em Plc.sml*)
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
	(case e of 
		ConI(n) => IntV(n)
		| ConB(b) => BoolV(b)
		| Var(x) => 
			(let
				val valor = (lookup env x) 
			in
				valor
			end) 
		| If(exp1,v1,v2) => 
			let
				val eExp1 = eval(exp1,env)
			in
				if (getBoolV(eExp1)) 
				then eval(v1,env)
				else eval(v2,env)
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
					BoolV(not (getBoolV(eExp)))
				end
			else if ope = "-" then
				let
					val eExp = eval(expr,env)
				in
					IntV(~(getIntV(eExp)))
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
		| Match(expComp, listaOp) => 
			let
				val eExprComp = eval(expComp,env)
				val opcoesValidas = List.filter (fn((opcao,retorno)) => 
						case opcao of 
							SOME(tipo) => (eval (tipo,env)) = eExprComp
							| NONE => true
						)  listaOp
			in
				if(List.null opcoesValidas) 
				then
					raise ValueNotFoundInMatch
				else
					eval(#2((List.nth (opcoesValidas, 0))),env)
			end
		| Letrec(nome, tipos, lista, tRetorno, corpo, prog) => 
				eval(prog,(nome,Clos(nome,lista,corpo,env))::env)
		| Anon(tipos, lista, corpo) => 
			Clos("",lista, corpo, env)
		| Call(f,params) =>
				(let
					val vf = eval(f,env);
					(* A avaliação dos parâmetros reais*)
			        val paramsEv = eval(params,env);
				in
					case vf of 
						(*Função anônima*)
						(Clos("", lista, corpo, envP)) =>
							(let
			                    (* O ambiente em que o corpo da função será avaliado, sem a própria função*)
			                    val envNovoCorpo = (lista, paramsEv)::envP
			                in
			                    eval(corpo,envNovoCorpo)
			                end)
			            (*Função recursiva*)
		                | (Clos(f, lista, corpo, envP)) =>
							(let
			                    (* O ambiente em que o corpo da função será avaliado, com a própria função*)
			                    val envNovoCorpo = (lista, paramsEv)::(f,vf)::envP
			                in
			                    eval(corpo,envNovoCorpo)
			                end)
			            | _ => raise NotAFunc
				end))