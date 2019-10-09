# <AE> ::= number
#           | (+ <AE> <AE>)
#           | (- <AE> <AE>)
#           | (* <AE> <AE>)
#           | (/ <AE> <AE>)
#           | (mod <AE> <AE>)
#           | (collatz <AE>)
#           | (- <AE>)
#           | id
#           | (if0 <AE> <AE> <AE>)
#
#           # Major change: function definitions, calls &
#           # with statements now take a variable number of arguments!
#           # Note the extra parens.
#
#           | (with ( (id <AE>)* ) <AE>)
#           | (lambda (id*) <AE>)
#           | (<AE> <AE>*)

module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp

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
	if (((typeof(expr[1]) == Symbol)||(typeof(expr[1]) == Int64)) && (length( expr ) == 1))
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
		if length( expr ) == 3
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
	else
		return FuncAppNode( parse( expr[1] ), lambdaParser( expr ) )
	end
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

function withHelper( expr::Array{Any})
	display(expr)
	result = Array{WithExpr, 1}()
	for i = 1:length(expr)
		if length( expr[i] ) == 2
			repeatVariables(result, expr[i][1] )
			push!(result, WithExpr( parse( expr[i][1] ) , parse( expr[i][2] ) ) )
		else
			throw(LispError("Not part of the grammar for with!"))
		end
	end
	return result
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
# ================================================== Calc
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
    cond = calc( ast.cond, env )
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

function calc( ast::WithNode, env::Environment )
	ext_env = env
	for i = 1:length(ast.expr)
		binding = ast.expr[i]
		binding_val = calc( binding.binding_expr, env )
	    ext_env = ExtendedEnv( binding.ref_node.sym, binding_val, ext_env )
	end
    return calc( ast.body, ext_env )
end

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
    return ClosureVal( ast.formal, ast.body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )
	ext_env = closure_val.env

	if ((length(ast.arg_expr)) != (length(closure_val.formal)))
	    throw(LispError("Mismatch in arguments when calculating closure val!"))
	  end

	for i = 1:length(ast.arg_expr)
	    actual_parameter = calc( ast.arg_expr[i], env )
	    ext_env = ExtendedEnv(  closure_val.formal[i],
								actual_parameter,
								ext_env)
	  end
    return calc( closure_val.body, ext_env )
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
    return calc( ast, EmptyEnv() )
end

function runTests()
	assert("(+ 1 3)", ExtInt.NumVal(4), "1. Basic addition")
	assert("(- 1 3)", ExtInt.NumVal(-2), "2. Basic subtraction")
	assert("(* 2 3)", ExtInt.NumVal(6), "3. Basic mulitiplication")
	assert("(/ 20 5)", ExtInt.NumVal(4.0), "4. Basic division")
	assert("(mod 17 3)", ExtInt.NumVal(2), "5. Basic mod")
	assert("(collatz 13)", ExtInt.NumVal(9), "6. Basic collatz")

	expectLispError("(/ 1 0)", "7. Division errors")
	expectLispError("(collatz -20)", "8. Collatz negative")

	assert("(+ 1 (+ 1 3))", ExtInt.NumVal(5), "9. Nested Addition")
	assert("(/ 20 (+ 2 3))", ExtInt.NumVal(4.0), "10. Nested Division")
	assert("(mod (+ 10 7) 3)", ExtInt.NumVal(2), "11. Nested mod")
	assert("(collatz (+ 10 3))", ExtInt.NumVal(9), "12. Nested Collatz")
	assert("(- (+ 1 1))", ExtInt.NumVal(-2), "13. Nested Negative")


	expectLispError("(collatz (- 1 3))", "14. Collatz nested negative")




	#assert("(with ((x 1)) (with (f (lambda () x)) (with ((x 2)) (f))))", "", "Super complicated with")
end

function assert( ast::AbstractString, result::AE, message::String)
	a = interp(ast)
	# display(a)
	if a == result
		display("Passed! $message")
	else
		display("Failed! $message ============================")
	end
end

function assert( ast::AbstractString, result::RetVal, message::String)
	a = interp(ast)
	# display(a)

	if a == result
		display("Passed! $message")
	else
		display("Failed! $message ============================")
	end
end

function expectLispError( ast::AbstractString, message::String )
	try interp(ast)
		display("Failed! No error thrown! $message ============================")
	catch LispError
		display("Passed! $message")
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
