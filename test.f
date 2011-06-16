: x VARIABLE
: y VARIABLE
: CR "\n" . ;

INPUT x !
8 y !

5 x @ CMP
@gt5 JGE

x @ y @ + . CR
@end GOTO

.gt5
x @ y @ * . CR

.end
NOP
