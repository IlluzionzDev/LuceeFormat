# ColdFusion Formatter

This is a ColdFusion formatter (based of the Lucee dialect) for formatting and linting cfc code. This mainly works
with .cfc (Cold Fusion Component) files. This was not designed to work with .cfm files as that is deemed legacy.

The project is coded in Rust for maximum performance.

## Under the Hood

The lexer/parser is a hand-written recursive descent parser. This makes for a very fast parser as it doesn't bother with
complex rules, and just runs through tokens in microseconds.

# Dev TODO

- [X] Preserve comments/whitespace in AST
- [X] Properly handle breaking lines while formatting
-   [X] Determine inline-ness of comments when wrapping
- [ ] When printing struct keys, if is string and has spaces, preserve quotes
- [ ] Better error handling
- [ ] Robust tests
- [ ] (Future) Add linting capabilities

## Breaking TODOS (Need to be done before production code formatter)

- Line breaks with '// comments' as "inline" comments should preserve line breaks
- ^ Need a way for a Doc node to "force" it's group to break
- Handle empty struct `[:]`
- Ternary can contain assignment expressions
- finally to try catch statement
- Sometimes lucee functions (where space between arg name and value) join function name to first arg

## Backlog TODOS (Niceties but not critical)

- Arena allocator for AST nodes