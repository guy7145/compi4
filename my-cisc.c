
/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include "arch/debug_macros.h.c"

#include <stdio.h>
#include <stdlib.h>
#include "arch/cisc.h"

int main() {
START_MACHINE;
JUMP(CONTINUE);
#include "arch/char.lib"
#include "arch/io.lib"
#include "arch/math.lib"
#include "arch/string.lib"
#include "arch/system.lib"
#include "arch/scheme.lib"
CONTINUE:
#define consts_addr 1000
#define fvars_addr consts_addr + 108
#define sym_tbl_addr fvars_addr + 23
/* encode-const-table */
MOV(INDD(consts_addr , 0) , T_VOID);
MOV(INDD(consts_addr , 1) , T_NIL);
MOV(INDD(consts_addr , 3) , IMM(1));

MOV(INDD(consts_addr , 2) , T_BOOL);

MOV(INDD(consts_addr , 5) , IMM(0));

MOV(INDD(consts_addr , 4) , T_BOOL);

MOV(INDD(consts_addr , 7) , IMM(3));

MOV(INDD(consts_addr , 6) , T_INTEGER);

MOV(INDD(consts_addr , 9) , IMM(1));

MOV(INDD(consts_addr , 8) , T_INTEGER);

MOV(INDD(consts_addr , 11) , IMM(0));

MOV(INDD(consts_addr , 10) , T_INTEGER);

MOV(INDD(consts_addr , 13) , IMM(2));

MOV(INDD(consts_addr , 12) , T_INTEGER);

MOV(INDD(consts_addr , 16) , IMM(consts_addr + 1));

MOV(INDD(consts_addr , 15) , IMM(consts_addr + 6));

MOV(INDD(consts_addr , 14) , T_PAIR);

MOV(INDD(consts_addr , 19) , IMM(consts_addr + 14));

MOV(INDD(consts_addr , 18) , IMM(consts_addr + 12));

MOV(INDD(consts_addr , 17) , T_PAIR);

MOV(INDD(consts_addr , 22) , IMM(consts_addr + 17));

MOV(INDD(consts_addr , 21) , IMM(consts_addr + 8));

MOV(INDD(consts_addr , 20) , T_PAIR);

MOV(INDD(consts_addr , 25) , IMM(consts_addr + 1));

MOV(INDD(consts_addr , 24) , IMM(consts_addr + 12));

MOV(INDD(consts_addr , 23) , T_PAIR);

MOV(INDD(consts_addr , 28) , IMM(consts_addr + 23));

MOV(INDD(consts_addr , 27) , IMM(consts_addr + 8));

MOV(INDD(consts_addr , 26) , T_PAIR);

MOV(INDD(consts_addr , 31) , 'a');

MOV(INDD(consts_addr , 32) , 's');

MOV(INDD(consts_addr , 33) , 'd');

MOV(INDD(consts_addr , 34) , 'n');

MOV(INDD(consts_addr , 35) , 'l');

MOV(INDD(consts_addr , 36) , 's');

MOV(INDD(consts_addr , 37) , 'd');

MOV(INDD(consts_addr , 38) , 'k');

MOV(INDD(consts_addr , 39) , 'f');

MOV(INDD(consts_addr , 40) , 'j');


MOV(INDD(consts_addr , 30) , IMM(10));

MOV(INDD(consts_addr , 29) , T_STRING);

MOV(INDD(consts_addr , 43) , '1');

MOV(INDD(consts_addr , 44) , '2');

MOV(INDD(consts_addr , 45) , '4');

MOV(INDD(consts_addr , 46) , 'd');


MOV(INDD(consts_addr , 42) , IMM(4));

MOV(INDD(consts_addr , 41) , T_STRING);


MOV(INDD(consts_addr , 48) , IMM(0));

MOV(INDD(consts_addr , 47) , T_STRING);

MOV(INDD(consts_addr , 50) , 'a');

MOV(INDD(consts_addr , 49) , T_CHAR);

MOV(INDD(consts_addr , 52) , 'b');

MOV(INDD(consts_addr , 51) , T_CHAR);

MOV(INDD(consts_addr , 54) , 'c');

MOV(INDD(consts_addr , 53) , T_CHAR);

MOV(INDD(consts_addr , 56) , '9');

MOV(INDD(consts_addr , 55) , T_CHAR);

MOV(INDD(consts_addr , 58) , '0');

MOV(INDD(consts_addr , 57) , T_CHAR);

MOV(INDD(consts_addr , 60) , IMM(123));

MOV(INDD(consts_addr , 59) , T_INTEGER);

MOV(INDD(consts_addr , 62) , IMM(99));

MOV(INDD(consts_addr , 61) , T_INTEGER);

MOV(INDD(consts_addr , 64) , IMM(98));

MOV(INDD(consts_addr , 63) , T_INTEGER);

MOV(INDD(consts_addr , 67) , 'a');

MOV(INDD(consts_addr , 68) , 'b');

MOV(INDD(consts_addr , 69) , 'c');

MOV(INDD(consts_addr , 70) , ' ');

MOV(INDD(consts_addr , 71) , 'd');

MOV(INDD(consts_addr , 72) , ' ');

MOV(INDD(consts_addr , 73) , 'f');

MOV(INDD(consts_addr , 74) , ' ');

MOV(INDD(consts_addr , 75) , ' ');

MOV(INDD(consts_addr , 76) , 'e');

MOV(INDD(consts_addr , 77) , 'd');

MOV(INDD(consts_addr , 78) , 'f');


MOV(INDD(consts_addr , 66) , IMM(12));

MOV(INDD(consts_addr , 65) , T_STRING);

MOV(INDD(consts_addr , 81) , 'a');

MOV(INDD(consts_addr , 82) , 'b');

MOV(INDD(consts_addr , 83) , 'c');

MOV(INDD(consts_addr , 84) , 'd');

MOV(INDD(consts_addr , 85) , 'f');

MOV(INDD(consts_addr , 86) , 'e');

MOV(INDD(consts_addr , 87) , 'd');

MOV(INDD(consts_addr , 88) , 'f');


MOV(INDD(consts_addr , 80) , IMM(8));

MOV(INDD(consts_addr , 79) , T_STRING);

MOV(INDD(consts_addr , 90) , IMM(7));

MOV(INDD(consts_addr , 89) , T_INTEGER);

MOV(INDD(consts_addr , 93) , 'a');

MOV(INDD(consts_addr , 94) , 'b');

MOV(INDD(consts_addr , 95) , 'c');

MOV(INDD(consts_addr , 96) , 'd');

MOV(INDD(consts_addr , 97) , 'e');


MOV(INDD(consts_addr , 92) , IMM(5));

MOV(INDD(consts_addr , 91) , T_STRING);

MOV(INDD(consts_addr , 99) , 't');

MOV(INDD(consts_addr , 98) , T_CHAR);

MOV(INDD(consts_addr , 101) , '5');

MOV(INDD(consts_addr , 100) , T_CHAR);

MOV(INDD(consts_addr , 104) , 'a');

MOV(INDD(consts_addr , 105) , 'b');

MOV(INDD(consts_addr , 106) , 'c');

MOV(INDD(consts_addr , 107) , 'd');


MOV(INDD(consts_addr , 103) , IMM(4));

MOV(INDD(consts_addr , 102) , T_STRING);

/* encode-symbol-table */
#define SOB_VOID IMM(consts_addr + 0)
#define SOB_NIL IMM(consts_addr + 1)
#define SOB_TRUE IMM(consts_addr + 2)
#define SOB_FALSE IMM(consts_addr + 4)
#define DUMMY_ENVIROMENT 0
/* library-function: boolean? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_21));
MOV(INDD(fvars_addr , 0) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_21);
LIB_FUNC_21:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_BOOL);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_0);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_0);

L_LIBARY_PREDICATE_TRUE_0:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_0:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_21:
MOV(R0 , SOB_VOID);
/* library-function: char? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_22));
MOV(INDD(fvars_addr , 1) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_22);
LIB_FUNC_22:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_CHAR);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_8);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_8);

L_LIBARY_PREDICATE_TRUE_8:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_8:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_22:
MOV(R0 , SOB_VOID);
/* library-function: integer? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_19));
MOV(INDD(fvars_addr , 2) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_19);
LIB_FUNC_19:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_INTEGER);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_9);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_9);

L_LIBARY_PREDICATE_TRUE_9:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_9:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_19:
MOV(R0 , SOB_VOID);
/* library-function: null? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_20));
MOV(INDD(fvars_addr , 3) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_20);
LIB_FUNC_20:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_NIL);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_6);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_6);

L_LIBARY_PREDICATE_TRUE_6:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_6:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_20:
MOV(R0 , SOB_VOID);
/* library-function: pair? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_17));
MOV(INDD(fvars_addr , 4) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_17);
LIB_FUNC_17:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_PAIR);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_7);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_7);

L_LIBARY_PREDICATE_TRUE_7:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_7:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_17:
MOV(R0 , SOB_VOID);
/* library-function: procedure? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_18));
MOV(INDD(fvars_addr , 5) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_18);
LIB_FUNC_18:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_CLOSURE);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_4);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_4);

L_LIBARY_PREDICATE_TRUE_4:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_4:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_18:
MOV(R0 , SOB_VOID);
/* library-function: string? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_15));
MOV(INDD(fvars_addr , 6) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_15);
LIB_FUNC_15:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_STRING);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_5);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_5);

L_LIBARY_PREDICATE_TRUE_5:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_5:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_15:
MOV(R0 , SOB_VOID);
/* library-function: symbol? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_16));
MOV(INDD(fvars_addr , 7) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_16);
LIB_FUNC_16:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_SYMBOL);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_2);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_2);

L_LIBARY_PREDICATE_TRUE_2:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_2:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_16:
MOV(R0 , SOB_VOID);
/* library-function: vector? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_13));
MOV(INDD(fvars_addr , 8) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_13);
LIB_FUNC_13:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_VECTOR);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_3);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_3);

L_LIBARY_PREDICATE_TRUE_3:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_3:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_13:
MOV(R0 , SOB_VOID);
/* library-function: rational? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_14));
MOV(INDD(fvars_addr , 9) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_14);
LIB_FUNC_14:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

CMP(IND(R0) , T_RATIONAL);

JUMP_EQ(L_LIBARY_PREDICATE_TRUE_1);

MOV(R0 , SOB_FALSE);

JUMP(L_LIBARY_PREDICATE_EXIT_1);

L_LIBARY_PREDICATE_TRUE_1:

MOV(R0 , SOB_TRUE);

L_LIBARY_PREDICATE_EXIT_1:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_14:
MOV(R0 , SOB_VOID);
/* library-function: car */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_11));
MOV(INDD(fvars_addr , 10) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_11);
LIB_FUNC_11:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 1));

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_11:
MOV(R0 , SOB_VOID);
/* library-function: cdr */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_12));
MOV(INDD(fvars_addr , 11) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_12);
LIB_FUNC_12:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 2));

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_12:
MOV(R0 , SOB_VOID);
/* library-function: cons */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_9));
MOV(INDD(fvars_addr , 12) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_9);
LIB_FUNC_9:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 1));

PUSH(R0);

MOV(R0 , FPARG(2 + 0));

PUSH(R0);

CALL(MAKE_SOB_PAIR);

DROP(2);

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_9:
MOV(R0 , SOB_VOID);
/* library-function: set-car! */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_10));
MOV(INDD(fvars_addr , 13) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_10);
LIB_FUNC_10:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(INDD(R0 , 1) , FPARG(2 + 1));

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_10:
MOV(R0 , SOB_VOID);
/* library-function: set-cdr! */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_7));
MOV(INDD(fvars_addr , 14) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_7);
LIB_FUNC_7:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(INDD(R0 , 2) , FPARG(2 + 1));

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_7:
MOV(R0 , SOB_VOID);
/* library-function: string-length */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_8));
MOV(INDD(fvars_addr , 15) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_8);
LIB_FUNC_8:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 1));

PUSH(R0);

CALL(MAKE_SOB_INTEGER);

DROP(1);

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_8:
MOV(R0 , SOB_VOID);
/* library-function: vector-length */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_5));
MOV(INDD(fvars_addr , 16) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_5);
LIB_FUNC_5:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 1));

PUSH(R0);

CALL(MAKE_SOB_INTEGER);

DROP(1);

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_5:
MOV(R0 , SOB_VOID);
/* library-function: char->integer */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_6));
MOV(INDD(fvars_addr , 17) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_6);
LIB_FUNC_6:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 1));

PUSH(R0);

CALL(MAKE_SOB_INTEGER);

DROP(1);

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_6:
MOV(R0 , SOB_VOID);
/* library-function: integer->char */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_3));
MOV(INDD(fvars_addr , 18) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_3);
LIB_FUNC_3:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 1));

PUSH(R0);

CALL(MAKE_SOB_CHAR);

DROP(1);

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_3:
MOV(R0 , SOB_VOID);
/* library-function: string-ref */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_4));
MOV(INDD(fvars_addr , 19) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_4);
LIB_FUNC_4:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R1 , FPARG(2 + 1));

MOV(R1 , INDD(R1 , 1));

MOV(R0 , INDD(R0 , 2 + R1));

PUSH(R0);

CALL(MAKE_SOB_CHAR);

DROP(1);

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_4:
MOV(R0 , SOB_VOID);
/* library-function: vector-ref */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_1));
MOV(INDD(fvars_addr , 20) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_1);
LIB_FUNC_1:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R1 , FPARG(2 + 1));

MOV(R1 , INDD(R1 , 1));

MOV(R0 , INDD(R0 , 2 + R1));

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_1:
MOV(R0 , SOB_VOID);
/* library-function: zero? */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_2));
MOV(INDD(fvars_addr , 21) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_2);
LIB_FUNC_2:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R0 , INDD(R0 , 1));

CMP(R0 , IMM(0));

JUMP_EQ(LIB_ZERO_EQUAL);

MOV(R0 , SOB_FALSE);

JUMP(LIB_ZERO_EXIT);

LIB_ZERO_EQUAL:

MOV(R0 , SOB_TRUE);

LIB_ZERO_EXIT:

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_2:
MOV(R0 , SOB_VOID);
/* library-function: string-set! */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , DUMMY_ENVIROMENT);
MOV(INDD(R0 , 2) , LABEL(LIB_FUNC_0));
MOV(INDD(fvars_addr , 22) , R0);
JUMP(L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_0);
LIB_FUNC_0:
PUSH(FP);
MOV(FP , SP);

MOV(R0 , FPARG(2 + 0));

MOV(R1 , FPARG(2 + 1));

MOV(R2 , FPARG(2 + 2));

MOV(R1 , INDD(R1 , 1));

MOV(R2 , INDD(R2 , 1));

MOV(INDD(R0 , 2 + R1) , R2);

MOV(R0 , SOB_VOID);

SHOW("", R0)

MOV(SP , FP);
POP(FP);
RETURN

L_GENERATE_LIBRARY_FUNCTIONS_SKIP_LABEL_0:
MOV(R0 , SOB_VOID);

#define SOB_VOID IMM(consts_addr + 0)
#define SOB_NIL IMM(consts_addr + 1)
#define SOB_TRUE IMM(consts_addr + 2)
#define SOB_FALSE IMM(consts_addr + 4)

/* applic (fvar boolean?) ((const #f)) */
/* const #f */
MOV(R0 , IMM(consts_addr + 4));
PUSH(R0);

PUSH(1);
/* fvar boolean? */
MOV(R0 , IND(fvars_addr + 0));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_45);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_45:

/* applic (fvar boolean?) ((const #t)) */
/* const #t */
MOV(R0 , IMM(consts_addr + 2));
PUSH(R0);

PUSH(1);
/* fvar boolean? */
MOV(R0 , IND(fvars_addr + 0));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_46);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_46:

/* applic (fvar boolean?) ((const 3)) */
/* const 3 */
MOV(R0 , IMM(consts_addr + 6));
PUSH(R0);

PUSH(1);
/* fvar boolean? */
MOV(R0 , IND(fvars_addr + 0));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_43);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_43:

/* applic (fvar integer?) ((const 1)) */
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(1);
/* fvar integer? */
MOV(R0 , IND(fvars_addr + 2));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_44);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_44:

/* applic (fvar integer?) ((const 0)) */
/* const 0 */
MOV(R0 , IMM(consts_addr + 10));
PUSH(R0);

PUSH(1);
/* fvar integer? */
MOV(R0 , IND(fvars_addr + 2));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_41);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_41:

/* applic (fvar pair?) ((const (1 2 3))) */
/* const (1 2 3) */
MOV(R0 , IMM(consts_addr + 20));
PUSH(R0);

PUSH(1);
/* fvar pair? */
MOV(R0 , IND(fvars_addr + 4));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_42);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_42:

/* applic (fvar pair?) ((const ())) */
/* const () */
MOV(R0 , IMM(consts_addr + 1));
PUSH(R0);

PUSH(1);
/* fvar pair? */
MOV(R0 , IND(fvars_addr + 4));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_39);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_39:

/* applic (fvar pair?) ((const 1)) */
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(1);
/* fvar pair? */
MOV(R0 , IND(fvars_addr + 4));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_40);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_40:

/* applic (fvar car) ((const (1 2))) */
/* const (1 2) */
MOV(R0 , IMM(consts_addr + 26));
PUSH(R0);

PUSH(1);
/* fvar car */
MOV(R0 , IND(fvars_addr + 10));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_37);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_37:

/* applic (fvar cdr) ((const (1 2))) */
/* const (1 2) */
MOV(R0 , IMM(consts_addr + 26));
PUSH(R0);

PUSH(1);
/* fvar cdr */
MOV(R0 , IND(fvars_addr + 11));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_38);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_38:

/* applic (fvar car) ((const (2))) */
/* const (2) */
MOV(R0 , IMM(consts_addr + 23));
PUSH(R0);

PUSH(1);
/* fvar car */
MOV(R0 , IND(fvars_addr + 10));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_35);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_35:

/* applic (fvar cdr) ((const (2))) */
/* const (2) */
MOV(R0 , IMM(consts_addr + 23));
PUSH(R0);

PUSH(1);
/* fvar cdr */
MOV(R0 , IND(fvars_addr + 11));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_36);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_36:

/* applic (fvar cons) ((const 1) (const 2)) */
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(2);
/* fvar cons */
MOV(R0 , IND(fvars_addr + 12));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_33);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_33:

/* applic (fvar car) ((applic (fvar cons) ((const 1) (const 2)))) */
/* applic (fvar cons) ((const 1) (const 2)) */
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(2);
/* fvar cons */
MOV(R0 , IND(fvars_addr + 12));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

PUSH(R0);

PUSH(1);
/* fvar car */
MOV(R0 , IND(fvars_addr + 10));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_34);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_34:

/* applic (fvar cdr) ((applic (fvar cons) ((const 1) (const 2)))) */
/* applic (fvar cons) ((const 1) (const 2)) */
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(2);
/* fvar cons */
MOV(R0 , IND(fvars_addr + 12));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

PUSH(R0);

PUSH(1);
/* fvar cdr */
MOV(R0 , IND(fvars_addr + 11));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_31);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_31:

/* applic (lambda-simple (x) (seq ((applic (fvar set-cdr!) ((pvar x 0) (const 3))) (pvar x 0)))) ((applic (fvar cons) ((const 1) (const 2)))) */
/* applic (fvar cons) ((const 1) (const 2)) */
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(2);
/* fvar cons */
MOV(R0 , IND(fvars_addr + 12));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

PUSH(R0);

PUSH(1);
/* lambda code-gen, body: (seq ((applic (fvar set-cdr!) ((pvar x 0) (const 3))) (pvar x 0))) */
PUSH(1);
CALL(MALLOC);
DROP(1);

MOV(R2 , R0);

MOV(R1 , FPARG(0));
/* copy old enviroment to new enviroment: */
MOV(R7 , 0);
loop_head_27:
CMP(R7 , 0);
JUMP_GE(loop_exit_27);
MOV(INDD(R2 , R7 + 1) , INDD(R1 , R7));
INCR(R7);
JUMP(loop_head_27);
loop_exit_27:

/* allocate space for args in the enviroment: */
MOV(R3 , FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);

MOV(INDD(R2 , 0) , R0);

/* copy arguments to new enviroment: */
MOV(R0 , INDD(R2 , 0));
MOV(R7 , 0);
loop_head_26:
CMP(R7 , R3);
JUMP_GE(loop_exit_26);
MOV(INDD(R0 , R7) , FPARG(R7 + 1));
INCR(R7);
JUMP(loop_head_26);
loop_exit_26:

/* create lambda_sob object: */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , R2);
MOV(INDD(R0 , 2) , LABEL(closure_body_10));
JUMP(closure_exit_10);
closure_body_10:
PUSH(FP);
MOV(FP , SP);
/* stack-fix: */

/* end of stack-fix */
/* applic (fvar set-cdr!) ((pvar x 0) (const 3)) */
/* const 3 */
MOV(R0 , IMM(consts_addr + 6));
PUSH(R0);
/* pvar x 0 */
MOV(R0 , FPARG(2 + 0));

PUSH(R0);

PUSH(2);
/* fvar set-cdr! */
MOV(R0 , IND(fvars_addr + 14));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

/* pvar x 0 */
MOV(R0 , FPARG(2 + 0));


MOV(SP , FP);
POP(FP);
RETURN
closure_exit_10:
/* end of lambda. */

PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_32);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_32:

/* applic (fvar string-length) ((const "asdnlsdkfj")) */
/* const "asdnlsdkfj" */
MOV(R0 , IMM(consts_addr + 29));
PUSH(R0);

PUSH(1);
/* fvar string-length */
MOV(R0 , IND(fvars_addr + 15));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_29);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_29:

/* applic (fvar string-length) ((const "124d")) */
/* const "124d" */
MOV(R0 , IMM(consts_addr + 41));
PUSH(R0);

PUSH(1);
/* fvar string-length */
MOV(R0 , IND(fvars_addr + 15));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_30);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_30:

/* applic (fvar string-length) ((const "")) */
/* const "" */
MOV(R0 , IMM(consts_addr + 47));
PUSH(R0);

PUSH(1);
/* fvar string-length */
MOV(R0 , IND(fvars_addr + 15));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_27);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_27:

/* applic (fvar char->integer) ((const #\a)) */
/* const #\a */
MOV(R0 , IMM(consts_addr + 49));
PUSH(R0);

PUSH(1);
/* fvar char->integer */
MOV(R0 , IND(fvars_addr + 17));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_28);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_28:

/* applic (fvar char->integer) ((const #\b)) */
/* const #\b */
MOV(R0 , IMM(consts_addr + 51));
PUSH(R0);

PUSH(1);
/* fvar char->integer */
MOV(R0 , IND(fvars_addr + 17));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_25);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_25:

/* applic (fvar char->integer) ((const #\c)) */
/* const #\c */
MOV(R0 , IMM(consts_addr + 53));
PUSH(R0);

PUSH(1);
/* fvar char->integer */
MOV(R0 , IND(fvars_addr + 17));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_26);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_26:

/* applic (fvar char->integer) ((const #\9)) */
/* const #\9 */
MOV(R0 , IMM(consts_addr + 55));
PUSH(R0);

PUSH(1);
/* fvar char->integer */
MOV(R0 , IND(fvars_addr + 17));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_23);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_23:

/* applic (fvar char->integer) ((const #\0)) */
/* const #\0 */
MOV(R0 , IMM(consts_addr + 57));
PUSH(R0);

PUSH(1);
/* fvar char->integer */
MOV(R0 , IND(fvars_addr + 17));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_24);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_24:

/* applic (fvar integer->char) ((const 123)) */
/* const 123 */
MOV(R0 , IMM(consts_addr + 59));
PUSH(R0);

PUSH(1);
/* fvar integer->char */
MOV(R0 , IND(fvars_addr + 18));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_21);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_21:

/* applic (fvar integer->char) ((const 0)) */
/* const 0 */
MOV(R0 , IMM(consts_addr + 10));
PUSH(R0);

PUSH(1);
/* fvar integer->char */
MOV(R0 , IND(fvars_addr + 18));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_22);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_22:

/* applic (fvar integer->char) ((const 99)) */
/* const 99 */
MOV(R0 , IMM(consts_addr + 61));
PUSH(R0);

PUSH(1);
/* fvar integer->char */
MOV(R0 , IND(fvars_addr + 18));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_19);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_19:

/* applic (fvar integer->char) ((const 98)) */
/* const 98 */
MOV(R0 , IMM(consts_addr + 63));
PUSH(R0);

PUSH(1);
/* fvar integer->char */
MOV(R0 , IND(fvars_addr + 18));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_20);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_20:

/* applic (fvar zero?) ((const 0)) */
/* const 0 */
MOV(R0 , IMM(consts_addr + 10));
PUSH(R0);

PUSH(1);
/* fvar zero? */
MOV(R0 , IND(fvars_addr + 21));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_17);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_17:

/* applic (fvar zero?) ((const 1)) */
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);

PUSH(1);
/* fvar zero? */
MOV(R0 , IND(fvars_addr + 21));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_18);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_18:

/* applic (fvar zero?) ((const 2)) */
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);

PUSH(1);
/* fvar zero? */
MOV(R0 , IND(fvars_addr + 21));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_15);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_15:

/* applic (fvar string-ref) ((const "abc d f  edf") (const 0)) */
/* const 0 */
MOV(R0 , IMM(consts_addr + 10));
PUSH(R0);
/* const "abc d f  edf" */
MOV(R0 , IMM(consts_addr + 65));
PUSH(R0);

PUSH(2);
/* fvar string-ref */
MOV(R0 , IND(fvars_addr + 19));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_16);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_16:

/* applic (fvar string-ref) ((const "abc d f  edf") (const 1)) */
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);
/* const "abc d f  edf" */
MOV(R0 , IMM(consts_addr + 65));
PUSH(R0);

PUSH(2);
/* fvar string-ref */
MOV(R0 , IND(fvars_addr + 19));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_13);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_13:

/* applic (fvar string-ref) ((const "abc d f  edf") (const 2)) */
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);
/* const "abc d f  edf" */
MOV(R0 , IMM(consts_addr + 65));
PUSH(R0);

PUSH(2);
/* fvar string-ref */
MOV(R0 , IND(fvars_addr + 19));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_14);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_14:

/* applic (fvar string-ref) ((const "abcdfedf") (const 7)) */
/* const 7 */
MOV(R0 , IMM(consts_addr + 89));
PUSH(R0);
/* const "abcdfedf" */
MOV(R0 , IMM(consts_addr + 79));
PUSH(R0);

PUSH(2);
/* fvar string-ref */
MOV(R0 , IND(fvars_addr + 19));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_11);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_11:

/* applic (lambda-simple (x) (seq ((applic (fvar string-set!) ((pvar x 0) (const 1) (const #\a))) (pvar x 0)))) ((const "abcde")) */
/* const "abcde" */
MOV(R0 , IMM(consts_addr + 91));
PUSH(R0);

PUSH(1);
/* lambda code-gen, body: (seq ((applic (fvar string-set!) ((pvar x 0) (const 1) (const #\a))) (pvar x 0))) */
PUSH(1);
CALL(MALLOC);
DROP(1);

MOV(R2 , R0);

MOV(R1 , FPARG(0));
/* copy old enviroment to new enviroment: */
MOV(R7 , 0);
loop_head_25:
CMP(R7 , 0);
JUMP_GE(loop_exit_25);
MOV(INDD(R2 , R7 + 1) , INDD(R1 , R7));
INCR(R7);
JUMP(loop_head_25);
loop_exit_25:

/* allocate space for args in the enviroment: */
MOV(R3 , FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);

MOV(INDD(R2 , 0) , R0);

/* copy arguments to new enviroment: */
MOV(R0 , INDD(R2 , 0));
MOV(R7 , 0);
loop_head_24:
CMP(R7 , R3);
JUMP_GE(loop_exit_24);
MOV(INDD(R0 , R7) , FPARG(R7 + 1));
INCR(R7);
JUMP(loop_head_24);
loop_exit_24:

/* create lambda_sob object: */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , R2);
MOV(INDD(R0 , 2) , LABEL(closure_body_9));
JUMP(closure_exit_9);
closure_body_9:
PUSH(FP);
MOV(FP , SP);
/* stack-fix: */

/* end of stack-fix */
/* applic (fvar string-set!) ((pvar x 0) (const 1) (const #\a)) */
/* const #\a */
MOV(R0 , IMM(consts_addr + 49));
PUSH(R0);
/* const 1 */
MOV(R0 , IMM(consts_addr + 8));
PUSH(R0);
/* pvar x 0 */
MOV(R0 , FPARG(2 + 0));

PUSH(R0);

PUSH(3);
/* fvar string-set! */
MOV(R0 , IND(fvars_addr + 22));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

/* pvar x 0 */
MOV(R0 , FPARG(2 + 0));


MOV(SP , FP);
POP(FP);
RETURN
closure_exit_9:
/* end of lambda. */

PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_12);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_12:

/* applic (lambda-simple (x) (seq ((applic (fvar string-set!) ((pvar x 0) (const 2) (const #\t))) (pvar x 0)))) ((const "abcde")) */
/* const "abcde" */
MOV(R0 , IMM(consts_addr + 91));
PUSH(R0);

PUSH(1);
/* lambda code-gen, body: (seq ((applic (fvar string-set!) ((pvar x 0) (const 2) (const #\t))) (pvar x 0))) */
PUSH(1);
CALL(MALLOC);
DROP(1);

MOV(R2 , R0);

MOV(R1 , FPARG(0));
/* copy old enviroment to new enviroment: */
MOV(R7 , 0);
loop_head_21:
CMP(R7 , 0);
JUMP_GE(loop_exit_21);
MOV(INDD(R2 , R7 + 1) , INDD(R1 , R7));
INCR(R7);
JUMP(loop_head_21);
loop_exit_21:

/* allocate space for args in the enviroment: */
MOV(R3 , FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);

MOV(INDD(R2 , 0) , R0);

/* copy arguments to new enviroment: */
MOV(R0 , INDD(R2 , 0));
MOV(R7 , 0);
loop_head_20:
CMP(R7 , R3);
JUMP_GE(loop_exit_20);
MOV(INDD(R0 , R7) , FPARG(R7 + 1));
INCR(R7);
JUMP(loop_head_20);
loop_exit_20:

/* create lambda_sob object: */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , R2);
MOV(INDD(R0 , 2) , LABEL(closure_body_7));
JUMP(closure_exit_7);
closure_body_7:
PUSH(FP);
MOV(FP , SP);
/* stack-fix: */

/* end of stack-fix */
/* applic (fvar string-set!) ((pvar x 0) (const 2) (const #\t)) */
/* const #\t */
MOV(R0 , IMM(consts_addr + 98));
PUSH(R0);
/* const 2 */
MOV(R0 , IMM(consts_addr + 12));
PUSH(R0);
/* pvar x 0 */
MOV(R0 , FPARG(2 + 0));

PUSH(R0);

PUSH(3);
/* fvar string-set! */
MOV(R0 , IND(fvars_addr + 22));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

/* pvar x 0 */
MOV(R0 , FPARG(2 + 0));


MOV(SP , FP);
POP(FP);
RETURN
closure_exit_7:
/* end of lambda. */

PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_9);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_9:

/* applic (lambda-simple (a) (seq ((applic (fvar string-set!) ((pvar a 0) (const 3) (const #\5))) (pvar a 0)))) ((const "abcde")) */
/* const "abcde" */
MOV(R0 , IMM(consts_addr + 91));
PUSH(R0);

PUSH(1);
/* lambda code-gen, body: (seq ((applic (fvar string-set!) ((pvar a 0) (const 3) (const #\5))) (pvar a 0))) */
PUSH(1);
CALL(MALLOC);
DROP(1);

MOV(R2 , R0);

MOV(R1 , FPARG(0));
/* copy old enviroment to new enviroment: */
MOV(R7 , 0);
loop_head_23:
CMP(R7 , 0);
JUMP_GE(loop_exit_23);
MOV(INDD(R2 , R7 + 1) , INDD(R1 , R7));
INCR(R7);
JUMP(loop_head_23);
loop_exit_23:

/* allocate space for args in the enviroment: */
MOV(R3 , FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);

MOV(INDD(R2 , 0) , R0);

/* copy arguments to new enviroment: */
MOV(R0 , INDD(R2 , 0));
MOV(R7 , 0);
loop_head_22:
CMP(R7 , R3);
JUMP_GE(loop_exit_22);
MOV(INDD(R0 , R7) , FPARG(R7 + 1));
INCR(R7);
JUMP(loop_head_22);
loop_exit_22:

/* create lambda_sob object: */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , R2);
MOV(INDD(R0 , 2) , LABEL(closure_body_8));
JUMP(closure_exit_8);
closure_body_8:
PUSH(FP);
MOV(FP , SP);
/* stack-fix: */

/* end of stack-fix */
/* applic (fvar string-set!) ((pvar a 0) (const 3) (const #\5)) */
/* const #\5 */
MOV(R0 , IMM(consts_addr + 100));
PUSH(R0);
/* const 3 */
MOV(R0 , IMM(consts_addr + 6));
PUSH(R0);
/* pvar a 0 */
MOV(R0 , FPARG(2 + 0));

PUSH(R0);

PUSH(3);
/* fvar string-set! */
MOV(R0 , IND(fvars_addr + 22));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);

/* pvar a 0 */
MOV(R0 , FPARG(2 + 0));


MOV(SP , FP);
POP(FP);
RETURN
closure_exit_8:
/* end of lambda. */

PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_10);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_10:

/* applic (lambda-simple (x) (tc-applic (fvar string-set!) ((pvar x 0) (const 2) (const #\t)))) ((const "abcde")) */
/* const "abcde" */
MOV(R0 , IMM(consts_addr + 91));
PUSH(R0);

PUSH(1);
/* lambda code-gen, body: (tc-applic (fvar string-set!) ((pvar x 0) (const 2) (const #\t))) */
PUSH(1);
CALL(MALLOC);
DROP(1);

MOV(R2 , R0);

MOV(R1 , FPARG(0));
/* copy old enviroment to new enviroment: */
MOV(R7 , 0);
loop_head_19:
CMP(R7 , 0);
JUMP_GE(loop_exit_19);
MOV(INDD(R2 , R7 + 1) , INDD(R1 , R7));
INCR(R7);
JUMP(loop_head_19);
loop_exit_19:

/* allocate space for args in the enviroment: */
MOV(R3 , FPARG(1));
PUSH(R3);
CALL(MALLOC);
DROP(1);

MOV(INDD(R2 , 0) , R0);

/* copy arguments to new enviroment: */
MOV(R0 , INDD(R2 , 0));
MOV(R7 , 0);
loop_head_18:
CMP(R7 , R3);
JUMP_GE(loop_exit_18);
MOV(INDD(R0 , R7) , FPARG(R7 + 1));
INCR(R7);
JUMP(loop_head_18);
loop_exit_18:

/* create lambda_sob object: */
PUSH(3);
CALL(MALLOC);
DROP(1);

MOV(R0 , R0);

MOV(INDD(R0 , 0) , T_CLOSURE);
MOV(INDD(R0 , 1) , R2);
MOV(INDD(R0 , 2) , LABEL(closure_body_6));
JUMP(closure_exit_6);
closure_body_6:
PUSH(FP);
MOV(FP , SP);
/* stack-fix: */

/* end of stack-fix */

MOV(SP , FP);
POP(FP);
RETURN
closure_exit_6:
/* end of lambda. */

PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_7);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_7:

/* applic (fvar string-set!) ((const "abcd") (const 0) (const #\c)) */
/* const #\c */
MOV(R0 , IMM(consts_addr + 53));
PUSH(R0);
/* const 0 */
MOV(R0 , IMM(consts_addr + 10));
PUSH(R0);
/* const "abcd" */
MOV(R0 , IMM(consts_addr + 102));
PUSH(R0);

PUSH(3);
/* fvar string-set! */
MOV(R0 , IND(fvars_addr + 22));
PUSH(INDD(R0 , 1));
CALLA(INDD(R0 , 2));
SHOW("R0 =", R0)
DROP(1);
POP(R1);
DROP(R1);
CMP(R0 , SOB_VOID);
JUMP_EQ(skip_print_8);
PUSH(R0);
CALL(WRITE_SOB);
DROP(1);
CALL(NEWLINE);
skip_print_8:

STOP_MACHINE;
return 0; 
}
