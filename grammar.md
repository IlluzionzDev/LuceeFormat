<program> ::= <statement_list>

<statement_list> ::= <statement> <statement_list> | <statement>

<statement> ::= <variable_declaration>
              | <variable_assignment>
              | <function_definition>
              | <component_definition>
              | <control_structure>
              | <cfml_tag>
              | <expression_statement>
              | <return_statement>

<expression_statement> ::= <expression> ";"?

<expression> ::= <ternary>

// TODO ?: operator
<ternary> ::= <equality> "?" <ternary> ":" <ternary> | <equality>

<equality> ::= <comparison> (( "==" | "!=" || "EQ" || "NEQ" ) <comparison>)* | <comparison>

<comparison> ::= <term> (<comparison_operator> <term>)* | <term>

// & for string concatenation grouped for here ease
<term> ::= <factor> (( "+" | "-" | "+=" | "-=" | "&" | "&=" ) <factor>)* | <factor>

// TODO: ^ operator
<factor> ::= <unary> (( "*" | "/" | "*=" | "/=" ) <unary>)* | <unary>

<unary> ::= ( "!" | "-" ) <unary> | <dot_access>

<dot_access> ::= <index_access> ("." <index_access>)* | index_access

<index_access> ::= <primary> ("[" <expression> "]")* | <primary>

<primary> :== <literal> | <identifier>
               | <function_call>
               | <object_creation>
               | <array_expression>
               | <struct_expression>
               | <lambda_expression>
               | "(" <expression> ")"

<literal> ::= <number>
            | <string>
            | <boolean>
            | "null"

<function_call> ::= <identifier> "(" <argument_list> ")"

<argument_list> ::= <expression> | <expression> "," <argument_list>

<comparison_operator> ::= "<" | ">" | "<=" | ">=" | "&&" | "||" | "LT" | "GT" | "AND" | "OR" | "XOR" | "CONTAINS"

<identifier> ::= /[a-zA-Z_][a-zA-Z0-9_]*/

<variable_declaration> ::= "var" <identifier> "=" <expression> ";"?

// TODO: Short hand assignments, +=, -=, /=, *=, ++, --, &=
<variable_assignment> ::= <expression> "=" <expression> ";"?

<return_statement> ::= "return" <expression> ";"?

<object_creation> ::= "new" <expression>

<function_definition> ::= <access_modifier>? <identifier> "function" <identifier> "(" <parameter_list> ")" "{" <statement_list> "}"

// Special lucee functions like
// transaction {
// }
// or
// lock name="myLock" type="exclusive" timeout="10" {
// }
<lucee_function> ::= <identifier> <loop_attribute_list>? "{" <statement_list> "}"

<component_definition> ::= "component" <loop_attribute_list>? "{" <statement_list> "}"

<access_modifier> ::= "public" | "private" | "protected"

<parameter_list> ::= <parameter> | <parameter> "," <parameter_list> | ε

<parameter> ::= "required"? (<identifier> | <identifier> <identifier> | <identifier> <identifier> "=" <expression>)

<array_expression> ::= "[" <expression> ("," <expression>)* "]"

<struct_expression> ::= "{" <key_value_pair> ("," <key_value_pair>)* "}"

<key_value_pair> ::= <literal> (":" || "=") <expression>

<lambda_expression> ::= ("(" <identifier> ("," <identifier>)* ")" | <identifier>) "=>" (<statement_list> | "{" <statement_list> "}" )

<control_structure> ::= <if_statement>
| <loop_statement>
| <switch_statement>
| <try_catch_statement>

<if_statement> ::= "if" "(" <expression> ")" "{" <statement_list> "}" <else_clause>

<else_clause> ::= "else" "{" <statement_list> "}" | ε

<loop_statement> ::= "for" "(" (<expression> ";" <expression> ";" <expression> | <identifier> "in" <expression>) ")" "{" <statement_list> "}"
| "while" "(" <expression> ")" "{" <statement_list> "}"
| "do" "{" <statement_list> "}" "while" "(" <expression> ")" ";"

<switch_statement> ::= "switch" "(" <expression> ")" "{" <case_list> "}"

<case_list> ::= <case_statement> | <case_statement> <case_list>

<case_statement> ::= "case" <expression> ":" <statement_list> | "default" ":" <statement_list>

<try_catch_statement> ::= "try" "{" <statement_list> "}" "catch" "(" <identifier> ")" "{" <statement_list> "}"

<loop_attribute_list> ::= <identifier> "=" <expression> | <identifier> "=" <expression> " " <loop_attribute_list>

# Special case cfml tags
<cfml_tag> ::= "<cfset" <variable_declaration> ">"
| "<cfif" <expression> ">" <statement_list> "<cfelse>" <statement_list> "<cfif>"
| <general_cfml_tag>

# TODO: Special cfml tags such as cfoutput and cfquery, treat inside tags
# as raw text

<general_cfml_tag> ::= "<cf" <identifier> (<expression> | <loop_attribute_list>) "/"? ">" <statement_list> ("</cf" <identifier> ">")?

# Handle comments for formatting / linting
<comment> ::= "/*" <comment_body> "*/" | "//" <comment_body> | "<!--" <comment_body> "-->"
<comment_body> ::= (<character>)*
