#lang pl
;Q4) Design a BNF grammar

#|
<program> ::= <statement> 

<statement> ::= <variable> "=" <expression>
               
<expression> ::= <value> | <concatenation>
            
<concatenation> ::= <value> "++" <value>

<variable> ::= <identifier>

<value> ::= <string> | <variable>

<string>   ::= '"' <characters> '"'


<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<digit> ::= "0" | "1" | ... | "9"

<pecial> ::= "+" | "-" | "%" | ....

<characters>   ::= <character> | <characters>

<character> :: <letter> | <digit> | <special>

<identifier> ::= <letter> { <letter> | <digit> }



"x = "hello""
<program> -> <statement>

<statement> -> <variable> "=" <expression>

<variable> -> <identifier>
<expression> -> <value>

<identifier> -> <letter>
<value> -> <string>

<letter> -> "x"
<string> -> " <characters> "
" <characters> " => "hello"
|#
