module CI0_inclass

push!(LOAD_PATH,pwd())
using Lexer
using Error

export interp


struct PlusNode <: AE
    lhs::AE
    rhs::AE
end

struct MinusNode <: AE
    lhs::AE
    rhs::AE
end

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Array{Any} )
    if length( expr ) != 3
        throw ( LispError( "Must have 3 items in list" ) )
    end

    if expr[1] == :+
        return PlusNode( parse(expr[2]), parse(expr[3]) )
    elseif expr[1] == :-
        return MinusNode( parse(expr[2]), parse(expr[3]) )
    else
        throw ( LispError("Bad Syntax"))
    end

end

function interp (cs::AbstractString)
    tokens = Lexer.lex( cs )
    ast = parse( tokens )
    result = calc( ast )
    return result
end

function calc ( n:: NumNode)
    return n.the_number
end

function calc ( ast::PlusNode )
    return calc(ast.lhs) + calc(ast.rhs)
end


end
