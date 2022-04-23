# Bytes

A program for calculating simple integer expressions.
Supports mathematical integer operators, binary and logical operators, and simple variables.

For a full list of operators, type `.ops` (or read them here).

``` markdown
Assignment:         =
Plus:               +
Subtract:           -
Multiply:           *
(Integer) Divide:   /
Modulo:             %
Exponent:           #
Open Bracket:       (
Close Bracket:      )
Bitwise And:        &
Bitwise Or:         |
Bitwise Xor:        ^
Bitwise Not:        ~
Bit Shift Right:    >>
Bit Shift Left:     <<
Logical And:        &&
Logical Or:         ||
Logical Not:        !
Greater:            >
Greater Eq:         >=
Lesser:             <
Lesser Eq:          <=
Equal:              ==
Not Equal:          !=
Ternary Condition:  ()?
Ternary Options:    :
Function (log2):    $func(...)
```

The backing integer type can be printed with `.num_type`.
Note: the backing type is unsigned, but division works with two's complement.
This means that division behaves like signed arithmetic i.e. `-10 / -5 = 2`

Variables can be bound to single word variables using alphanumeric characters and `_`.
The value of the last expression is bound to the variable `_` automatically.
To print the current variable state, type `.vars`

Supports arbitrary depth evaluation for expressions, like:

``` markdown
(bytes) result = (15 << 4)? (2 _15 - (log2(12)) / 3) % 2 : (12 << 4) < (2_ 4)#5
1
(bytes) _ << 4
16
```

To exit, give EOF (^D) or type `.exit`
