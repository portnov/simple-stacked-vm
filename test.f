: CR "\n" . ;
: INC DUP @ 1 + SWAP ! ;
: ODD 2 REM ;
: ? @ . ;

: PRINT_ODD
  DUP @ ODD
  @even JZ
  ? " is odd." .
  @endprint GOTO
  .even
  ? " is even." .
  .endprint
  CR
  ;

: n VARIABLE
: i VARIABLE

"Enter number:\n" .
INPUT n !

0 i !
.loop
"Hello." . CR
i PRINT_ODD
i INC
i @ n @ CMP
@loop JLT

"End" . CR

