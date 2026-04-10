# Comment pattern
# ===============

"""
    CommentPattern

Trivia pattern used for matching comments.
"""
struct CommentPattern
    comment::Union{String, Regex}
end

"""
    @comment(expr)

Create a [`CommentPattern`](@ref) from the given `String` or `Regex`.

# Examples

```
julia> @comment "This is a comment"
CommentPattern:
# This is a comment

julia> @comment \"\"\"
           This is a
           multiline comment\"\"\"
CommentPattern:
#= This is a
multiline comment =#

julia> @comment r"TODO[\\S\\s]*"
CommentPattern:
# r"TODO[\\S\\s]*"
```
"""
macro comment(expr)
    err_msg_general =
        """
        invalid `@comment` syntax
        Comment patterns should be created in one of the following ways:
         -- `@comment <string>`
         -- `@comment <regex>`
         """
    isa(expr, String) || is_regex(expr) ||
        throw(SyntaxError(err_msg_general, __source__.file, __source__.line))

    return :( CommentPattern($expr) )
end

# Comment matching
# ----------------

"""
    comment_match_all(c::CommentPattern, src::JS.SyntaxNode)

Match all comments in `src` with the string or regex from the given `CommentPattern`.

# Examples

```
julia> source = \"\"\"
       # This is a comment
       # continuing here
       bla()

       #= TODO: This is
       a
       multiline comment =#

       function f(x)
           # comment
           x + 1  # TODO: Another comment
           return x
       end
       \"\"\";

julia> c = @comment r"TODO[\\S\\s]*";

julia> match_result = comment_match_all(c, parseall(SyntaxNode, source))
MatchResults with 2 matches and 0 failures:
Matches:
  BindingSet()
  BindingSet()

julia> match_result.matches
2-element Vector{BindingSet}:
 BindingSet @ :5:1 with 0 entries
 BindingSet @ :11:12 with 0 entries
```
"""
function comment_match_all(c::CommentPattern, src::JS.SyntaxNode)
    match_results = MatchResults()
    comments_with_ranges = comments(src)
    for (comment, range) in comments_with_ranges
        match_result = _comment_match(c.comment, strip_comment(comment))
        if is_successful(match_result)
            (match_result::BindingSet)._comment_location =
                JS.source_location(JS.sourcefile(src), range[1])
            push!(match_results.matches, match_result)
        end
    end

    return match_results
end
_comment_match(s::String, comment::AbstractString) =
    s == comment ? BindingSet() : MatchFail()
function _comment_match(r::Regex, comment::AbstractString)
    m = match(r, comment)
    isnothing(m) && return MatchFail()
    m.match == comment && return BindingSet()
    return MatchFail()
end

# Utils
# -----

is_regex(ex) = @isexpr(ex, :macrocall) && ex.args[1] == Symbol("@r_str")

strip_comment(comment::AbstractString) = startswith(comment, "#=") ?
    strip(comment, ['#', '=', ' ']) :
    lstrip(comment, ['#', ' '])

# Display
# -------

function Base.show(io::IO, ::MIME"text/plain", c::CommentPattern)
    println(io, "CommentPattern:")
    comment = c.comment
    if isa(comment, String) && contains(comment, '\n')
        print(io, "#= ", comment, " =#")
    else
        print(io, "# ", comment)
    end
end
