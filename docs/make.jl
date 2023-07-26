using Rematch2
using Documenter

DocMeta.setdocmeta!(Rematch2, :DocTestSetup, :(using Rematch2); recursive=true)

makedocs(;
    modules=[Rematch2],
    authors="Neal Gafter <neal.gafter@relational.ai> and contributors",
    repo="https://github.com/JuliaServices/Rematch2.jl/blob/{commit}{path}#{line}",
    sitename="Rematch2.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://JuliaServices.github.io/Rematch2.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/JuliaServices/Rematch2.jl.git",
    devbranch="main",
    push_preview = true,
)
