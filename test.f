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
: arr VARIABLE

5 arr ARRAY
17 arr 3 [!]
"test" arr 4 [!]
arr ? CR
arr 3 [@] . CR

"Enter number: " .
INPUT n !

0 i !
.loop
"Hello." . CR
i PRINT_ODD
i INC
i @ n @ CMP
@loop JLT

"End" . CR

