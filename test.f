: CR "\n" . ;
: INC DUP @ 1 + SWAP ! ;
: ODD 2 REM ;
: ? @ . ;

: n VARIABLE
: i VARIABLE

"Enter number:\n" .
INPUT n !

0 i !
.loop
"Hello." . CR
i @ ODD
@even JZ
i ? " is odd." . CR
@endloop GOTO
.even
i ? " is even." . CR
.endloop
i INC
i @ n @ CMP
@loop JLT

"End" . CR

