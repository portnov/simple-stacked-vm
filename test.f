: n VARIABLE
: i VARIABLE
: CR "\n" . ;

INPUT n !
0 i !

5 x @ CMP
@gt5 JGE

x @ y @ + . CR
@end GOTO

.gt5
x @ y @ - . CR

.end
