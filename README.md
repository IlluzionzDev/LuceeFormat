# ColdFusion Formatter
This is a ColdFusion formatter (based of the Lucee dialect) for formatting and linting cfc code. This mainly works
with .cfc (Cold Fusion Component) files, but can format code within <cfscript> blocks in .cfm files. This was not designed
to work with .cfm files as that is deemed legacy.

The project is coded in Rust for maximum performance.

# Dev TODO
- [ ] Preserve comments/whitespace in AST
- [ ] Parse AST into a pretty printer
- [ ] Support parsing/formatting .cfm files with <cfscript> blocks