# LuceeFormat

This is a ColdFusion formatter (based of the Lucee dialect) for formatting and linting cfc code. This mainly works
with .cfc (Cold Fusion Component) files.

This project is still in early beta, not recommended to be used in production yet.

The project is coded in Rust for maximum performance.

## Under the Hood

The lexer/parser is a hand-written recursive descent parser. This makes for a very fast parser as it doesn't bother with
complex rules, and just runs through tokens in microseconds.

# Dev TODO

- [X] Better error handling - improve, show main block that requires token
- [ ] Lexer to report errors, mostly string parsing errors

- [ ] Handle CFML, as well as CFScript in CFM files :/
- [ ] Robust tests
- [ ] (Future) Add linting capabilities

# Debugging TODOs

- [ ] AST Validator: Check the the newly formatted code parses the same AST.

## Breaking TODOS (Need to be done before production code formatter)

- Still seems to be issue with MemberExpression chaining and indented when not broken, and breaking when shouldn't. #1
  Priority
- Allow escaping ' with another ' inside string. Lexer should detect '' sequence
- Passing named args to function doesn't require commas ??
- Lucee function, allow name only args (without value)
- Static access can also just access identifiers
- Handle 'does not contain' statement
- Indents seem to sometimes indent before line break
- Indent in case body, first statement isn't indented
- Instantiation of package components (e.g one.two.three()) need to be on same line. Should check after ObjectCreation
  node

## Backlog TODOS (Niceties but not critical)

- Arena allocator for AST nodes / Zero Copy AST