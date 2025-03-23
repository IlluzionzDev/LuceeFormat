# ColdFusion Formatter

This is a ColdFusion formatter (based of the Lucee dialect) for formatting and linting cfc code. This mainly works
with .cfc (Cold Fusion Component) files, but can format code within <cfscript> blocks in .cfm files. This was not
designed to work with .cfm files as that is deemed legacy.

The project is coded in Rust for maximum performance.

## Under the Hood

The lexer/parser is a hand-written recursive descent parser. This makes for a very fast parser as it doesn't bother with
complex rules, and just runs through tokens in microseconds.

# Dev TODO

- [ ] Preserve comments/whitespace in AST
- [ ] Properly handle breaking lines while formatting
- [ ] Parse AST into a pretty printer
- [ ] Better error handling
- [ ] Support parsing/formatting .cfm files with <cfscript> blocks
- [ ] Robust tests
- [ ] (Future) Add linting capabilities