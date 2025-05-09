module Argus

export SyntaxPatternNode, Pattern, SyntaxClass

using JuliaSyntax: JuliaSyntax, head, kind, @K_str,
    children, is_leaf, leaf_string, is_error, source_location

include("syntax-pattern-node.jl")
include("pattern.jl")
include("syntax-class.jl")

function __init__()
    _register_kinds()

    eval(Meta.parseall("""
       using JuliaSyntax: JuliaSyntax, @K_str
       using JuliaSyntax: Tokenize
       using JuliaSyntax.Tokenize: Lexer, emit, accept, peekchar3
       using JuliaSyntax.Tokenize: iswhitespace, isvalid, is_invisible_char, is_identifier_char
       using JuliaSyntax.Tokenize: lex_whitespace, lex_star, lex_circumflex, lex_dollar, lex_xor,
           lex_comment, lex_equal, lex_exclaim, lex_greater, lex_less, lex_colon, lex_bar,
           lex_amper, lex_prime, lex_division, lex_quote, lex_percent, lex_forwardslash,
           lex_backslash, lex_dot, lex_plus, lex_minus, lex_backtick, lex_identifier,
           lex_digit

       function lex_tilda(l::Lexer)
           if peekchar3(l) == ('v', 'a', 'r')
               accept(l, 'v')
               accept(l, 'a')
               accept(l, 'r')
               return emit(l, K"~var")
           else
               return emit(l, K"~")
           end
       end

       function _next_token(l::Lexer, c)
           if c == EOF_CHAR
               return emit(l, K"EndMarker")
           elseif iswhitespace(c)
               return lex_whitespace(l, c)
           elseif c == '['
               return emit(l, K"[")
           elseif c == ']'
               return emit(l, K"]")
           elseif c == '{'
               return emit(l, K"{")
           elseif c == ';'
               return emit(l, K";")
           elseif c == '}'
               return emit(l, K"}")
           elseif c == '('
               return emit(l, K"(")
           elseif c == ')'
               return emit(l, K")")
           elseif c == ','
               return emit(l, K",")
           elseif c == '*'
               return lex_star(l);
           elseif c == '^'
               return lex_circumflex(l);
           elseif c == '@'
               return emit(l, K"@")
           elseif c == '?'
               return emit(l, K"?")
           elseif c == '\$'
               return lex_dollar(l);
           elseif c == '⊻'
               return lex_xor(l);
           elseif c == '~'
               return lex_tilda(l)
           elseif c == '#'
               return lex_comment(l)
           elseif c == '='
               return lex_equal(l)
           elseif c == '!'
               return lex_exclaim(l)
           elseif c == '>'
               return lex_greater(l)
           elseif c == '<'
               return lex_less(l)
           elseif c == ':'
               return lex_colon(l)
           elseif c == '|'
               return lex_bar(l)
           elseif c == '&'
               return lex_amper(l)
           elseif c == '\''
               return lex_prime(l)
           elseif c == '÷'
               return lex_division(l)
           elseif c == '"'
               return lex_quote(l);
           elseif c == '%'
               return lex_percent(l);
           elseif c == '/'
               return lex_forwardslash(l);
           elseif c == Char(0x5c)
               return lex_backslash(l);
           elseif c == '.'
               return lex_dot(l);
           elseif c == '+'
               return lex_plus(l);
           elseif c == '-'
               return lex_minus(l);
           elseif c == '−' # \\minus '−' treated as hyphen '-'
               return emit(l, accept(l, '=') ? K"op=" : K"-")
           elseif c == '`'
               return lex_backtick(l);
           elseif is_identifier_start_char(c)
               return lex_identifier(l, c)
           elseif isdigit(c)
               return lex_digit(l, K"Integer")
           elseif (k = get(_unicode_ops, c, K"None")) != K"None"
               return emit(l, k)
           else
               emit(l,
                   !isvalid(c)           ? K"ErrorInvalidUTF8"   :
                   is_invisible_char(c)  ? K"ErrorInvisibleChar" :
                   is_identifier_char(c) ? K"ErrorIdentifierStart" :
                   K"ErrorUnknownCharacter")
           end
       end
       """))
end

end  # Argus
