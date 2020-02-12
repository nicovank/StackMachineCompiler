# StackMachineCompiler

This project provides a compiler to compile from a simple language to the
[StackMachine](https://danielschlegel.org/wp/teaching/csc344-spring-2020/assignment-1/) language. I wrote this program
to make it easier to generate test cases for students.

## Grammar

```
Program    :- Statement*

Statement  :- 'IF' '(' Expression ')' Statement ('ELSE' Statement)?
Statement  :- 'WHILE' '(' Expression ')' Statement
Statement  :- '{' Statement* '}'
Statement  :- Register '=' Expression
Statement  :- 'OUT' '(' Expression ')'

Expression :- Factor (('==' | '!=' | '<' | '>') Expression)?
Factor     :- Term (('+' | '-') Factor)?
Term       :- Atom (('*' | '/' | '%') Term)?
Atom       :- Quark (('&' | '|') Atom)?
Quark      :- '(' Expression ')'
Quark      :- Register
Quark      :- Literal
Quark      :- 'IN'

Register   :- '$' [0-9A-F][0-9A-F]
Literal    :- '0' | ([1-9][0-9]*)
```

## Example

Consider the following program, which runs the Extended Euclidean Algorithm on two given numbers.

```
$AA = IN
$BB = IN

$11 = 0     $01 = 1
$22 = $BB   $02 = $AA

WHILE ($22 != 0) {
    $FF = $02 / $22

    $00 = $22
    $22 = $02 - $FF * $22
    $02 = $00

    $00 = $11
    $11 = $01 - $FF * $11
    $01 = $00
}

IF ($BB != 0) $20 = ($02 - $01 * $AA) / $BB
ELSE          $20 = 0

OUT($02)
OUT($01)
OUT($20)
```

```
70

IN
STOR 170
IN
STOR 187
LIT 0
STOR 17
LIT 1
STOR 1
LOAD 187
STOR 34
LOAD 170
STOR 2
LIT 0
LOAD 34
IFEQ 18
LIT 1
JUMP 19
LIT 0
LIT 0
IFEQ 46
LOAD 34
LOAD 2
DIV
STOR 255
LOAD 34
STOR 0
LOAD 34
LOAD 255
MUL
LOAD 2
SUB
STOR 34
LOAD 0
STOR 2
LOAD 17
STOR 0
LOAD 17
LOAD 255
MUL
LOAD 1
SUB
STOR 17
LOAD 0
STOR 1
JUMP 13
LIT 0
LOAD 187
IFEQ 51
LIT 1
JUMP 52
LIT 0
LIT 0
IFEQ 63
LOAD 187
LOAD 170
LOAD 1
MUL
LOAD 2
SUB
DIV
STOR 32
JUMP 65
LIT 0
STOR 32
LOAD 2
OUT
LOAD 1
OUT
LOAD 32
OUT
```
