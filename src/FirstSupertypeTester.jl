struct SupertypeTesterKey
    type::Type
    start_index::Int
end

function brute_next_supertype(@nospecialize(t::Type), types::Vector{Type}, start_index::Int = 1)::Int
    n::Int = length(types)
    for i::Int in start_index:n
        t <: types[i] && return i
    end
    return 0::Int
end

struct FirstSupertypeTester
    types::Vector{Type}
    lock::ReentrantLock
    cache::Dict{SupertypeTesterKey, Int}
    function FirstSupertypeTester(types::Vector{Type})
        types = Type[types...] # defensive copy into Vector{Type}
        cache = Dict{SupertypeTesterKey, Int}()
        result = new(types, ReentrantLock(), cache)
        n::Int = length(types)
        for i in 1:n
            t::Type = types[i]
            if isconcretetype(t)
                index = 1
                while index <= n
                    r = brute_next_supertype(t, types, index)
                    result.cache[SupertypeTesterKey(t, index)] = r
                    if r == 0
                        break
                    else
                        index = (r > index) ? r : index + 1
                    end
                end
            end
        end
        result
    end
end

function next_supertype_slow(stt::FirstSupertypeTester, key::SupertypeTesterKey)::Int
    l = stt.lock
    lock(l)
    try
        return get!(stt.cache, key) do
            brute_next_supertype(key.type, stt.types, key.start_index)
        end::Int
    finally
        unlock(l)
    end
end

function next_supertype(@nospecialize(t::Type), stt::FirstSupertypeTester, start_index::Int = 1)::Int
    key::SupertypeTesterKey = SupertypeTesterKey(t, start_index)
    age0::UInt = stt.cache.age
    r::Int = get(stt.cache, key, -1) # avoid locking or allocating a lambda in normal cases
    if stt.cache.age != age0 || r == -1
        return next_supertype_slow(stt, key)
    end
    r::Int
end
