# <AE> ::= number
#           | (+ <AE> <AE> <AE>*)   # at least two parameters, possibly infinity.  note extra star!
#           | (- <AE> <AE>)
#           | (* <AE> <AE>)
#           | (/ <AE> <AE>)
#           | (mod <AE> <AE>)
#           | (collatz <AE>)
#           | (- <AE>)
#           | id
#           | (if0 <AE> <AE> <AE>)
#           | (with ( (id <AE>)* ) <AE>)
#           | (lambda (id*) <AE>)
#           | (and <AE> <AE> <AE>*)  # new operation 'and'
#           | (<AE> <AE>*)

module TransInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, analyze, interp

#
# ==================================================
#

abstract type AE
end

# <AE> ::= <number>
struct NumNode <: AE
    n::Real
end

struct ManyNums <: AE
	nodes::Array{ NumNode }
end

struct PlusNode <: AE
	numbers::Array{ AE }
end

# <AE> ::= (x <AE> <AE>)
struct BinopNode <: AE
	op::Function
    lhs::AE
    rhs::AE
end

# <AE> ::= (x <AE>)
struct UnaryNode <: AE
	op::Function
	num::AE
end

# <AE> ::= (if0 <AE> <AE> <AE>)
struct If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

struct AndNode <: AE
	args::Array{ AE }
end

# <AE> ::= <id>
struct VarRefNode <: AE
    sym::Symbol
end

struct WithExpr <: AE
	ref_node::VarRefNode
	binding_expr::AE
end

# <AE> ::= (with <id> <AE> <AE>)
struct WithNode <: AE
    expr::Array{ WithExpr }
    body::AE
end

# <AE> ::= (lambda <id> <AE>)
struct FuncDefNode <: AE
    formal::Array{ Symbol }
    body::AE
end

# <AE> ::= (<AE> <AE>)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_expr::Array{AE}
end

#
# ==================================================
#

function Dict(op::Symbol)
	if op == :+
		return +
	elseif op == :-
		return -
	elseif op == :*
		return *
	elseif op == :/
		return /
	elseif op == :mod
		return mod
	elseif op == :collatz
		return collatz
	# elseif op == :and
	# 	return and
	else
		return nothing
	end
end

function Dict(op::Any)
	return nothing
end

#
# ==================================================
#

abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
    formal::Array{ Symbol }
    body::AE
    env::Environment
end

#
# ==================================================
#

struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
    sym::Symbol
    val::RetVal
    parent::Environment
end

#
# ================================================== Parse
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
	if ((Dict( expr ) != nothing) || (expr == :if0) || (expr == :with) || (expr == :lambda))
		throw( LispError( "Don't use keywords for ID's!" ) )
	else
		return VarRefNode( expr )
	end
end

function parse( expr::Array{Any} )
	 # display(expr)
	if length(expr) == 0
		throw(LispError("No arguments given! Incorrect syntax!"))
	end

	if (((typeof(expr[1]) == Int64)) && (length( expr ) == 1))
		return parse( expr[1] )
	end

	if (typeof(expr[1]) == Int64)
		list = Array{ NumNode,1 }()
		for i = 1:length( expr )
			push!(list, parse(expr[i]))
		end

		return ManyNums( list )
	end

	operator = Dict( expr[1] )

	if operator != nothing
		if operator == +
			if length(expr) == 3
				return BinopNode(operator, parse(expr[2]), parse(expr[3]))
			elseif length(expr) > 3
				numbers = Array{ AE, 1}()
				for i=2:length( expr )
					push!(numbers, parse(expr[i]))
				end
				return PlusNode(numbers)
			else
				throw(LispError("Invalid number of arguments for $operator"))
			end

		elseif length( expr ) == 3
			if operator != collatz
				return BinopNode(operator, parse( expr[2] ), parse( expr[3] ) )
			else
				throw( LispError( "Too many arguements for collatz!" ) )
			end
		elseif length( expr ) == 2
			if ((operator == -) || (operator == collatz))
				return UnaryNode( operator, parse( expr[2] ) )
			else
				throw( LispError( "Too few arguements for $operator" ) )
			end
		else
			throw( LispError( "Invalid number of arguments for $operator"))
		end
	elseif expr[1] == :if0
		if length(expr) == 4
			return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )
		else
			throw( LispError("Invalid number of arguments for if0!"))
		end
	elseif expr[1] == :with
		if length(expr) == 3
			return WithNode( withHelper( expr[2] ), parse( expr[3] ) )
		else
			throw( LispError("Invalid number of arguments for with!"))
		end
	elseif expr[1] == :lambda
		if length(expr) == 3
			return FuncDefNode( lambdaHelper( expr[2] ), parse( expr[3] ) )
		else
			throw( LispError("Invalid number of arguments for lambda!"))
		end
	elseif expr[1] == :and
		if length(expr) >= 3
			args = Array{ AE, 1}()
			for i=2:length( expr )
				push!(args, parse(expr[i]))
			end
			return AndNode(args)
		else
			throw(LispError("Invalid number of arguments for $operator"))
		end
	else
		return FuncAppNode( parse( expr[1] ), lambdaParser( expr ) )
	end
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

function withHelper(expr::Any)
	throw( LispError("Invalid with type $expr") )
end

function withHelper( expr::Array{Any})
	result = Array{WithExpr, 1}()
	for i = 1:length(expr)
		if typeof(expr[i]) != Array{Any, 1}
			throw(LispError("Invalid with syntax!"))
		end

		if length( expr[i] ) == 2
			repeatVariables(result, expr[i][1] )
			push!(result, WithExpr( parse( expr[i][1] ) , parse( expr[i][2] ) ) )
		else
			throw(LispError("Not part of the grammar for with!"))
		end
	end
	return result
end

function lambdaHelper(expr::Any)
	throw( LispError("Invalid lambda type $expr") )
end

function lambdaHelper( expr::Array{Any})
	result = Array{Symbol, 1}()
	for i = 1:length( expr )
		if typeof(expr[i]) == Symbol
			repeatVariables(result, expr[i] )
  			push!(result, parse( expr[i] ).sym )
		else
			throw(LispError("Not part of the grammar for lambda!"))
		end
	end
	return result
end

function lambdaParser(expr::Any)
	throw( LispError("Invalid lambda type $expr") )
end

function lambdaParser( expr::Array{Any} )
	array = Array{AE, 1}()
	for i = 2:length( expr )
		push!(array, parse( expr[i] ) )
	end
	return array
end

function repeatVariables( expr::Array{WithExpr}, sym::Symbol)
	if length(expr) == 0
		return
	end

	for i = 1:length( expr )
		if expr[i].ref_node.sym == sym
			throw( LispError("Don't repeat values when defining!"))
		end
	end
end

function repeatVariables( expr::Array{Symbol}, sym::Symbol)
	if length(expr) == 0
		return
	end

	for i = 1:length( expr )
		if expr[i] == sym
			throw( LispError("Don't repeat values when defining!"))
		end
	end
end

#
# ====================================================================== analyze
#

function analyze( ast::NumNode )
    return ast
end

function analyze( ast::VarRefNode )
    return ast
end

function analyze( ast::PlusNode )
    return BinopNode(+, analyze(ast.numbers[1]), plusHelper(ast.numbers, 2) )
end

function plusHelper( ast::Array{AE}, index::Int64)
	if length(ast) - index == 0
		return analyze(ast[index])
	else
		index += 1
		return BinopNode(+, analyze( ast[index - 1] ), plusHelper(ast, index))
	end
end

function analyze( ast::AndNode )
	return If0Node(analyze(ast.args[1]), NumNode(0), andHelper(ast.args, 1))
end

function andHelper( ast::Array{AE}, index::Int64)
	if length(ast) - index == 0
		return If0Node(analyze(ast[index]), NumNode(0), NumNode(1))
	else
		index += 1
		return If0Node(analyze( ast[index - 1] ), NumNode(0), andHelper(ast, index))
	end
end

function analyze( ast::UnaryNode )
    anum = analyze( ast.num )
    if typeof(anum) == NumNode && anum.n > 0
        return NumNode( ast.op(anum.n) )
    end

    return UnaryNode( ast.op, anum )
end

function analyze(ast::BinopNode)
	    alhs = analyze( ast.lhs )
	    arhs = analyze( ast.rhs )

		if typeof(alhs) == NumNode && typeof(arhs) == NumNode
			if ((ast.op == /) && (arhs.n == 0))
				throw(LispError("Can't divide by 0!"))
			end
	        return NumNode( ast.op(alhs.n, arhs.n) )
		end

		return BinopNode(ast.op, alhs, arhs)
end

function analyze( ast::WithNode )
	withSymbols = Array{Symbol, 1}()
	withArgs = Array{AE, 1}()

	for i = 1:length( ast.expr )
		push!(withSymbols, ast.expr[i].ref_node.sym)
		push!(withArgs, analyze(ast.expr[i].binding_expr))
	end
	# display(ast.body)
    # transform from a with expression to application of a function
     fdn = FuncDefNode( withSymbols, analyze( ast.body ) )
     return FuncAppNode( fdn, withArgs )
end

function analyze( ast::If0Node )
    acond = analyze( ast.cond )

    if typeof( acond ) == NumNode
        if acond.n == 0
            return analyze( ast.zerobranch )
        else
            return analyze( ast.nzerobranch )
        end
    end

    azb = analyze( ast.zerobranch )
    anzb = analyze( ast.nzerobranch )
    return If0Node( acond, azb, anzb )
end

function analyze( ast::FuncDefNode )
    return FuncDefNode( ast.formal, analyze( ast.body ) )
end

function analyze( ast::FuncAppNode )
	args = Array{AE, 1}()
	for i = 1:length( ast.arg_expr )
		push!(args, analyze(ast.arg_expr[i]))
	end

    return FuncAppNode( analyze( ast.fun_expr), args )
end


#
# =================== collatz helper
#

function collatz( n::Real )
  return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end


#
# ============================================================================= Calc
#


function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc( ast::ManyNums, env::Environment )
	throw( LispError( "Cannot calculate a list of numbers!") )
end

function calc( ast::BinopNode, env::Environment )
	if ((typeof(calc(ast.lhs, env)) != NumVal) || (typeof(calc(ast.rhs, env)) != NumVal))
		throw( LispError("Invalid arguments for calculating a binary operator!"))
	end

	if ast.op == /
		if calc( ast.rhs, env ).n == 0
			throw( LispError("Undefined: can't divide by 0!") )
		else
			return NumVal( ast.op( calc( ast.lhs, env ).n, calc( ast.rhs, env ).n ) )
		end
	else
		return NumVal( ast.op( calc( ast.lhs, env ).n, calc( ast.rhs, env ).n ) )
	end
end

function calc( ast::UnaryNode, env::Environment)
	if typeof(calc(ast.num)) != NumVal
		throw( LispError("Invalid arguments for calculating a unary operator!"))
	end

	if ast.op == collatz
		if calc( ast.num, env ).n < 0
			throw( LispError("Tried to collatz with a negative number!") )
		else
			return NumVal( collatz( calc( ast.num, env ).n ) )
		end
	else
		return NumVal( ast.op( calc( ast.num, env ).n ) )
	end
end

function calc( ast::If0Node, env::Environment )
	# display(ast)
	if typeof(calc(ast.cond, env)) != NumVal
		throw( LispError("Invalid arguments for calculating a If0 operator!"))
	end

    cond = calc( ast.cond, env )
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

# function calc( ast::WithNode, env::Environment )
# 	throw(LispError("WithNode calc! Should never get here!"))
# 	ext_env = env
# 	for i = 1:length(ast.expr)
# 		binding = ast.expr[i]
# 		binding_val = calc( binding.binding_expr, env )
# 	    ext_env = ExtendedEnv( binding.ref_node.sym, binding_val, ext_env )
# 	end
#     return calc( ast.body, ext_env )
# end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
    if ast.sym == env.sym
		return env.val
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formal, ast.body, env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )

	if typeof(closure_val) != ClosureVal
		throw( LispError("Unexpected type when calculating a function!"))
	end

	ext_env = closure_val.env

	if ((length(ast.arg_expr)) != (length(closure_val.formal)))
	    throw(LispError("Mismatch in arguments when calculating function!"))
	  end

	for i = 1:length(ast.arg_expr)
	    actual_parameter = calc( ast.arg_expr[i], env )
	    ext_env = ExtendedEnv(  closure_val.formal[i],
								actual_parameter,
								ext_env)
	  end
    return calc( closure_val.body, ext_env )
end

function calc( ast::ClosureVal)

end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end

function calc( ast::ManyNums )
	throw( LispError("Can't calculate a string of numbers!"))
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    revised_ast = analyze(ast)
    return calc( revised_ast, EmptyEnv() )
end

function runTests()
	display("--------------- Binop and Unary Op tests ------------------------")
	assert("(+ 1 3)", TransInt.NumVal(4), "1. Basic addition")
	assert("(- 1 3)", TransInt.NumVal(-2), "2. Basic subtraction")
	assert("(* 2 3)", TransInt.NumVal(6), "3. Basic mulitiplication")
	assert("(/ 20 5)", TransInt.NumVal(4.0), "4. Basic division")
	assert("(mod 17 3)", TransInt.NumVal(2), "5. Basic mod")
	assert("(collatz 13)", TransInt.NumVal(9), "6. Basic collatz")

	expectLispError("(/ 1 0)", "7. Division errors")
	expectLispError("(collatz -20)", "8. Collatz negative")

	assert("(+ 1 (+ 1 3))", TransInt.NumVal(5), "9. Nested Addition")
	assert("(/ 20 (+ 2 3))", TransInt.NumVal(4.0), "10. Nested Division")
	assert("(mod (+ 10 7) 3)", TransInt.NumVal(2), "11. Nested mod")
	assert("(collatz (+ 10 3))", TransInt.NumVal(9), "12. Nested Collatz")
	assert("(- (+ 1 1))", TransInt.NumVal(-2), "13. Nested Negative")

	expectLispError("(collatz (- 1 3))", "14. Collatz nested negative")

	display("------------------ If0 tests ------------------")

	assert("(if0 1 3 2)", NumVal(2), "1. Basic if0")
	assert("(if0 0 3 2)", NumVal(3), "2. Basic if0")
	expectLispError("(if0 0 x 1)", "3. Basic error checking if0")
	expectLispError("(if0 0 if0 1)","4. Id checking")
	expectLispError("(if0 0 with 1)","5. Id checking")
	assert("(if0 (with ((x 0)) ((lambda () x))) 1 -1)", NumVal(1), "6. Nested with")
	expectLispError("(if0 (with ((x 0)) (lambda () x)) 1 -1)",  "7. Nested with (closure)")
	expectLispError("(if0 0 3 2 2 3 1)", "8. Parameter checking")
	expectLispError("(if0 (- 1 2) 1 x)", "9. Type checking")
	expectLispError("(if0 (lambda (x y) (* x y)) 1 2)", "10. Lambda if0 type check")
	assert("(if0 ((lambda (x y) (* x y)) 0 3) 1 2)", NumVal(1), "11. Lambda if0 type check")



	display("----------------- Ids tests ----------------")

	expectLispError("(with (+ 1) 3)", "1. With Id checking")
	expectLispError("(with (mod 1) 3)", "2. With Id checking")
	expectLispError("(with (lambda 1) 3)", "3. With Id checking")
	expectLispError("(with (if0 1) 3)", "4. With Id checking")
	expectLispError("(f)", "5. Simple Id checking")
	expectLispError("f", "6. Simple Id checking")
	expectLispError("(f f f)", "7. Multiple Id checking")
	assert("(with ((x 1)) (+ 1 x))", NumVal(2), "8. Positive Id check")



	display("----------------- With tests ------------------")

	assert("(with ((x 1)) 2)", NumVal(2), "1. Basic with")
	assert("(with ((x 5)) (+ 50 x))", NumVal(55), "2. With with expressions")
	assert("(with ((x 3) (y 4)) (* y x))", NumVal(12), "3. Mulitple parameters")
	assert("(with () 1)", NumVal(1), "4. Nothing")
	expectLispError("(with a a)", "5. Type checking")
	expectLispError("(with 1 2)", "6. Type checking")
	expectLispError("(with (x) 1)", "7. More syntax checks")
	expectLispError("(with (()) 1)", "8. More syntax checks")
	assert("(with ((x 5) (y 6)) (+ x y))", NumVal(11), "9. With->Lambda check")


	display("---------------- Lambda tests -------------------")

	assert("(lambda () 1)", TransInt.ClosureVal(Symbol[], TransInt.NumNode(1), TransInt.EmptyEnv()), "1. Basic lambda")
	assert("((lambda () 1))", NumVal(1), "2. Basic lambda application")
	assert("(with ((x 1)) (with ((f (lambda () x))) (with ((x 2)) (f))))", NumVal(1), "3. Super complicated with/lambda test")
	assert("(with ((x 1)) (with ((f (lambda (y) y))) (with ((x 2)) (f 4))))", NumVal(4), "4. Super complicated with/lambda test")
	assert("(with ((x 1)) (with ((f (lambda () x))) (with ((x 2)) f)))", TransInt.ClosureVal(Symbol[], TransInt.VarRefNode(:x), TransInt.ExtendedEnv(:x, TransInt.NumVal(1), TransInt.EmptyEnv())), "5. Super complicated with/lambda test")
	assert("(with ((x 1) (y 5)) (with ((f (lambda () (* x y)))) (with () (f))))", NumVal(5), "6. Double parameters and withs")
	assert("((lambda (x y) (* x y)) 2 3)", NumVal(6), "7. Simple double parameters")
	expectLispError("((lambda (x y) (* x y)) 3)", "8. Arity check")
	assert("(+ ((lambda (x y) (* x y)) 2 3) 6)", NumVal(12), "9. Binop check double parameters")
	expectLispError("(+ ((lambda (x y) (* x y)) 6)", "10. Lambda binop type check")
	expectLispError("(collatz ((lambda (x y) (* x y)) 2 -3))", "11. Lambda collatz calc check")
	expectLispError("(/ ((lambda (x y) (* x y)) 2 0))", "12. Lambda division calc check")
	assert("(+ ((lambda (x y) (* x y)) 2 -6) 6)", NumVal(-6), "13. Binop check double parameters negative")
	assert("(+ ((lambda (x y) (* x y)) 2 (- 6)) 6)", NumVal(-6), "14. Binop check double parameters negative 2")
	expectLispError("((lambda (x 1) (* x 1)) 2)", "15. Simple double parameters type checking")
	assert("(lambda () a)", TransInt.ClosureVal(Symbol[], TransInt.VarRefNode(:a), TransInt.EmptyEnv()), "16. Basic lambda")

	display("---------------- And tests -------------------")
	assert("(and 1 2)", NumVal(1), "1. Basic and")
	assert("(and 1 2 3)", NumVal(1), "2. Basic and, multiple arguments")
	assert("(and 1 2 3 4 5 6 7 8)", NumVal(1), "3. Basic and, tons of arguments")
	assert("(and 1 0)", NumVal(0), "4. Basic and 0")
	expectLispError("(and (lambda () 1) 2)", "5. Closureval checking with and")
	assert("(and 1 2 3 0 5)", NumVal(0), "6. Basic and 0, multiple parameters")
	assert("(and (+ 1 2 3 4) (+ (- 1 2) (+ 1 0) (- 1 2) (+ 1 0)) 5)", NumVal(0), "7. Basic and 0, binops")


	expectLispError("()", "Null array")

end

function assert( ast::AbstractString, result::AE, message::String)
	 a = interp(ast)
	if isequal(a, result)
		display("Passed! $message")
	else
		display("Failed! $message *******************************")
	end
end

function assert( ast::AbstractString, result::RetVal, message::String)
	a = interp(ast)

	if typeof(a) == ClosureVal
		if repr(a) == repr(result)
			display("Passed! $message")
		else
			display("Failed! $message *******************************")
		end
	elseif a == result
		display("Passed! $message")
	else
		display("Failed! $message *******************************")
	end
end

function expectLispError( ast::AbstractString, message::String )
	try interp(ast)
		display("Failed! No error thrown! $message ============================")
	catch e
		if isa(e, LispError)
			display("Passed! $message")
		else
			throw(e)
		end
	end
end


# evaluate a series of tests in a file
function interpf( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( interp( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end







end #module
