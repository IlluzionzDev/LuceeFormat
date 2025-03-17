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

<expression> ::= <literal>
               | <identifier>
               | <function_call>
               | <object_creation>
               | <array_expression>
               | <struct_expression>
               | <lambda_expression>
               | "(" <expression> ")"
               | <unary_expression>
               | <binary_expression>
               | <ternary_expression>
               | <expression> "." <expression>
               | <expression> "[" <expression> "]" <expression>?

<literal> ::= <number>
            | <string>
            | <boolean>
            | "null"

<identifier> ::= /[a-zA-Z_][a-zA-Z0-9_]*/

<variable_declaration> ::= "var" <identifier> "=" <expression> ";"?

<variable_assignment> ::= <expression> "=" <expression> ";"?

<return_statement> ::= "return" <expression> ";"?

<object_creation> ::= "new" <expression>

<ternary_expression> ::= <expression> "?" <expression> ":" <expression>

<function_definition> ::= <access_modifier>? <identifier> "function" <identifier> "(" <parameter_list> ")" "{" <statement_list> "}"

<component_definition> ::= "component" <loop_attribute_list>? "{" <statement_list> "}"

<access_modifier> ::= "public" | "private" | "protected"

<parameter_list> ::= <parameter> | <parameter> "," <parameter_list> | ε

<parameter> ::= "required"? (<identifier> | <identifier> <identifier> | <identifier> <identifier> "=" <expression>)

<function_call> ::= <identifier> "(" <argument_list> ")"

<argument_list> ::= <expression> | <expression> "," <argument_list> | ε

<binary_expression> ::= <expression> <binary_operator> <expression>

<binary_operator> ::= "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||" | "EQ" | "NEQ" | "LT" | "GT" | "AND" | "OR"

<unary_expression> ::= <unary_operator> <expression>

<unary_operator> ::= "!" | "-"

<array_expression> ::= "[" <expression> ("," <expression>)* "]"

<struct_expression> ::= "{" <key_value_pair> ("," <key_value_pair>)* "}"

<key_value_pair> ::= <literal> ":" <expression>

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

<general_cfml_tag> ::= "<cf" <identifier> (<expression> | <loop_attribute_list>) "/"? ">" <statement_list> ("</cf" <identifier> ">")?

# Handle comments for formatting / linting
<comment> ::= "/*" <comment_body> "*/" | "//" <comment_body> | "<!--" <comment_body> "-->"
<comment_body> ::= (<character>)*
