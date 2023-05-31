
# This is very helpful for debugging the behavior of macros
macro print_expand(x)
    quote
        let
            a = @macroexpand $(esc(x))
            # println("\nexpanded:")
            # println(a)
            b = MacroTools.prewalk(MacroTools.rmlines, a)
            # println("\nrmlines:")
            # println(b)
            c = MacroTools.prewalk(MacroTools.unblock, b)
            # println("\nunblock:")
            # println(c)
            d = MacroTools.alias_gensyms(c)
            # println("alias_gensyms:")
            println(d)
        end
    end
end

const get_current_exceptions = (VERSION >= v"1.7") ? current_exceptions : Base.catch_stack
macro where_thrown()
    quote
        stack = $get_current_exceptions()
        l = last(stack)
        trace = stacktrace(l[2])
        trace[1]
    end
end

struct True; end

struct Foo
    x
    y
end

abstract type RBTree end

struct Leaf <: RBTree
end

struct Red <: RBTree
    value
    left::RBTree
    right::RBTree
end

struct Black <: RBTree
    value
    left::RBTree
    right::RBTree
end

struct Address
    street::AbstractString
    city::AbstractString
    zip::AbstractString
end

struct Person
    firstname::AbstractString
    lastname::AbstractString
    address::Address
end

abstract type Term end

struct Var <: Term
    name::AbstractString
end

struct Fun <: Term
    arg::AbstractString
    body::Term
end

struct App <: Term
    f::Term
    v::Term
end

struct T207a
    x; y; z
    T207a(x, y) = new(x, y, x)
end
Rematch2.fieldnames(::Type{T207a}) = (:x, :y)

struct T207b
    x; y; z
    T207b(x, y; z = x) = new(x, y, z)
end

struct T207c
    x; y; z
end
T207c(x, y) = T207c(x, y, x)
