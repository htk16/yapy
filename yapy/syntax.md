# Syntax

Program ::= Statements
Statements ::= Statement { Statement }
Statement ::= Import
            | FunctionDefinition
            | Expression
            
Import ::= 'import' ModuleName
ModuleName ::= Identifier { '.' Identifier }
FunctionDefinition ::= 'fun' Identifier '(' Arguments ')' -> Type ':' Expression
Arguments ::= [ Argument { ',' Argument } ]
Argument ::= Identifier ':' Type

Expression ::= Block
             | If
             | For
             | FunctionCall
             | BinaryOperation
             | UnaryOperation
             | VariableBinding
             | List
             | Set
             | Dict
             | Function
             | Variable
             | String
             | Int
             | Float
             | Boolean
             | None

Block ::= '{' Statements '}'
If ::= 'if' Expression 'then' Expression [ 'else' Expression ]
For ::= 'for' '(' Variable 'in' Expression ')' Expression
FunctionCall ::= Expression '(' [ Expression { ',' Expression } ] ')'
BinaryOperation ::= Expression BinaryOperator Expression
UnaryOperation ::= UnaryOperator Expression
VariableBinding ::= 'val' Variable '=' Expression
List ::= '[' [ Expression { ',' Expression } ] ']'
Set ::= '{' [ Expression { ',' Expression } ] '}'
Dict ::= '{' [ KeyValue { ',' KeyValue } ] '}'
Function ::= 'fn' '(' Arguments ')' -> Type ':' Expression
Variable ::= Identifier
String ::= r"\".*\""
Int ::= r"-?[1-9]*[0-9]"
Float ::= r"-?[1-9]*[0-9](\.[0-9]+)"
Boolean ::= 'True' | 'False'
None ::= 'None'

Identifier ::= r"[_a-zA-Z][_a-zA-Z0-9]*"

 Type ::=


