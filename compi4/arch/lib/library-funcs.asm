APPLY:
	MOV(R1, STARG(0));
	MOV(R2, STARG(1));

	APPLY_LOOP:
	MOV(R2, IND(R2));
	CMP(R2, T_NIL);
	JUMP_EQ(APPLY_LOOP_END);
	PUSH(INDD(R2, 1));
	MOV(R2, INDD(R2, 2));
	JUMP(APPLY_LOOP);
	
	APPLY_LOOP_END:
	CALL(R1);
	RETURN;

OP_LESS_THAN:
	MOV(R0, SOB_TRUE);
	MOV(R1, INDD(STARG(0), 1));
	MOV(R2, INDD(STARG(1), 1));
	CMP(R1, R2);
	JUMP_LT(OP_LESS_THAN_TRUE);
	MOV(R0, SOB_FALSE);
	OP_LESS_THAN_TRUE: RETURN;

OP_EQUALS:
	MOV(R0, SOB_TRUE);
	MOV(R1, INDD(STARG(0), 1));
	MOV(R2, INDD(STARG(1), 1));
	CMP(R1, R2);
	JUMP_EQ(OP_EQUALS_TRUE);
	MOV(R0, SOB_FALSE);
	OP_EQUALS_TRUE: RETURN;

OP_GREATER_THAN:
	MOV(R0, SOB_TRUE);
	MOV(R1, INDD(STARG(0), 1));
	MOV(R2, INDD(STARG(1), 1));
	CMP(R1, R2);
	JUMP_GT(OP_GREATER_THAN_TRUE);
	MOV(R0, SOB_FALSE);
	OP_GREATER_THAN_TRUE: RETURN;

OP_BIN_PLUS:
	MOV(R0, INDD(STARG(0), 1));
	MOV(R1, INDD(STARG(1), 1));
	ADD(R0, R1);
	RETURN;

OP_BIN_DIV:
	MOV(R0, INDD(STARG(0), 1));
	MOV(R1, INDD(STARG(1), 1));
	DIV(R0, R1);
	RETURN;

OP_BIN_MUL:
	MOV(R0, INDD(STARG(0), 1));
	MOV(R1, INDD(STARG(1), 1));
	MUL(R0, R1);
	RETURN;

OP_BIN_SUB:
	MOV(R0, INDD(STARG(0), 1));
	MOV(R1, INDD(STARG(1), 1));
	SUB(R0, R1);
	RETURN;

PRED_BOOLEAN:
	PUSH(STARG(0));
	CALL(IS_SOB_BOOL);
	RETURN;

CAR:
	MOV(R0, INDD(STARG(0), 1));
	RETURN;

CDR:
	MOV(R0, INDD(STARG(0), 2));
	RETURN;

CHAR_TO_INT:
	MOV(IND(STARG(0)), T_INTEGER);
	RETURN;

PRED_CHAR:
	PUSH(STARG(0));
	CALL(IS_SOB_CHAR);
	RETURN;

CONS:
	PUSH(STARG(0));
	PUSH(STARG(1));
	CALL(MAKE_SOB_PAIR);
	RETURN;

DENOMINATOR:
	MOV(R0, INDD(STARG(0), 2));
	PUSH(R0);
	CALL(SOB_MAKE_INTEGER);
	RETURN;

PRED_EQ:
	MOV(R0, SOB_TRUE);
	MOV(R1, IND(STARG(0)));
	MOV(R2, IND(STARG(1)));
	CMP(R1, R2);
	JUMP_EQ(PRED_EQ_NOT_EQUAL);
	MOV(R1, INDD(STARG(0), 1));
	MOV(R2, INDD(STARG(1), 1));
	CMP(R1, R2);
	JUMP_EQ(PRED_EQ_NOT_EQUAL);
	RETURN;
	PRED_EQ_NOT_EQUAL:
	MOV(R0, SOB_FALSE);
	RETURN;

PRED_INT:
	PUSH(STARG(0));
	CALL(IS_SOB_INTEGER);
	RETURN;

INT_TO_CHAR:
	MOV(IND(STARG(0)), T_CHAR);
	RETURN;

MAKE_STRING:
	MOV(R0, STARG(-1));
	MOV(R1, 0);
	
	MAKE_STRING_LOOP:
	CMP(R0, R1);
	JUMP_GE(MAKE_STRING_LOOP_END);
	PUSH(STARG(R1));
	INCR(R1);
	JUMP(MAKE_STRING_LOOP);

	MAKE_STRING_LOOP_END:
	CALL(MAKE_SOB_STRING);
	RETURN;

MAKE_VECTOR:
	MOV(R0, STARG(-1));
	MOV(R1, 0);
	
	MAKE_STRING_LOOP:
	CMP(R0, R1);
	JUMP_GE(MAKE_STRING_LOOP_END);
	PUSH(STARG(R1));
	INCR(R1);
	JUMP(MAKE_STRING_LOOP);
	
	MAKE_STRING_LOOP_END:
	CALL(MAKE_SOB_VECTOR);
	RETURN;

PRED_NULL:
	PUSH(STARG(0));
	CALL(IS_SOB_NIL);
	RETURN;

NUMERATOR:
	MOV(R0, INDD(STARG(0), 1));
	PUSH(R0);
	CALL(SOB_MAKE_INTEGER);
	RETURN;

PRED_PAIR:
	PUSH(STARG(0));
	CALL(IS_SOB_PAIR);
	RETURN;

PRED_PROC:
	PUSH(STARG(0));
	CALL(IS_SOB_CLOSURE);
	RETURN;

PRED_RATIONAL:
	PUSH(STARG(0));
	CALL(IS_SOB_RATIONAL);
	RETURN;

REMAINDER:
	MOV(R0, INDD(STARG(0), 1));
	MOV(R1, INDD(STARG(1), 1));
	REM(R0, R1);
	RETURN;

SET_CAR:
	MOV(INDD(STARG(0), 1), STARG(1));
	RETURN;

SET_CDR:
	MOV(INDD(STARG(0), 2), STARG(1));
	RETURN;

STRING_LENGTH:
	MOV(R0, INDD(STARG(0), 1));
	RETURN;

STRING_REF:
	MOV(R1, INDD(STARG(1), 1));
	ADD(R1, IMM(2)); 			// skip type & length
	PUSH(INDD(STARG(0), R1));
	CALL(MAKE_SOB_CHAR);
	RETURN;

STRING_SET:
	MOV(R1, INDD(STARG(1), 1));
	ADD(R1, IMM(2)); 			// skip type & length
	MOV(INDD(STARG(0), R1));
	RETURN;

STRING_TO_SYMBOL: ////////////////////////////////////////////////////////////////


PRED_STRING:
	PUSH(STARG(0));
	CALL(IS_SOB_STRING);
	RETURN;

PRED_SYMBOL:
	PUSH(STARG(0));
	CALL(IS_SOB_SYMBOL);
	RETURN;

SYMBOL_TO_STRING: ////////////////////////////////////////////////////////////////


VECTOR: goto MAKE_VECTOR;


VECTOR_LENGTH:
	MOV(R0, INDD(STARG(0), 1));
	RETURN;

VECTOR_REF:
	MOV(R1, INDD(STARG(1), 1));
	ADD(R1, IMM(2)); 			// skip type & length
	MOV(R0, INDD(STARG(0), R1));
	RETURN;

VECTOR_SET:
	MOV(R1, INDD(STARG(1), 1));
	ADD(R1, IMM(2)); 			// skip type & length
	MOV(INDD(STARG(0), IND(STARG(2))));
	RETURN;

PRED_VECTOR:
	PUSH(STARG(0));
	CALL(IS_SOB_VECTOR);
	RETURN;

PRED_ZERO:
	PUSH(STARG(0));
	CALL(IS_ZERO);
	RETURN;





