using Documenter, Argus

makedocs(
    # modules = [Argus],
    format=Documenter.HTML(
        repolink="https://github.com/iuliadmtru/Argus.jl"
    ),
    pages=[
        "Home" => "index.md"
        "Installation" => "installation.md"
        "Tutorial" => [
            "Basics" => "tutorial-basics.md"
            "Patterns" => "tutorial-patterns.md"
            "Comment Patterns" => "tutorial-comment-patterns.md"
            "Ellipses" => "tutorial-ellipses.md"
            "Syntax Classes" => "tutorial-syntax-classes.md"
            "Pattern Forms" => "tutorial-pattern-forms.md"
            "Templates" => "tutorial-templates.md"
            "Rules" => "tutorial-rules.md"
        ]
        "Further Reading" => [
            "Matching Utils" => "further-reading-matching-utils.md"
            "Replacing Macros With Function Calls" => "further-reading-replacing-macros-with-function-calls.md"
        ]
    ],
    sitename="Argus.jl"
)

deploydocs(
    repo = "github.com/iuliadmtru/Argus.jl.git",
)

