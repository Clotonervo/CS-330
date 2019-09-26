#
# Class Interpreter 0
# Base interpreter with numbers, plus, and minus
#

module RudInt

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

# <AE> ::= (x <AE> <AE>)
struct BinopNode <: AE
	op::Function
    lhs::AE
    rhs::AE
end

#
# ==================================================
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Array{Any} )

    if expr[1] == :+
        return BinopNode( +, parse( expr[2] ), parse( expr[3] ) )
	elseif expr[1] == :*
		return BinopNode( *, parse( expr[2] ), parse( expr[3] ))
    end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function calc( ast::NumNode )
    return ast.n
end

function calc( ast::BinopNode )
	if ast.op == +
		return calc( ast.lhs ) + calc( ast.rhs )
	elseif ast.op == *
		return calc( ast.lhs ) * calc( ast.rhs )
	end
end


#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end

end #module
