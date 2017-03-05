
MAKE_SOB_RATIONAL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_RATIONAL);
  
  MOV(R1, FPARG(0));
  MOV(R2, FPARG(1));

  //gcd
  MOV(R3, R1);
  MOV(R4, R2);
  CMP(R1, R2);
  JUMP_GE(GCD_LOOP);
  MOV(R3, R2);
  MOV(R4, R1);
  GCD_LOOP:
  CMP(R4, IMM(0));
  JUMP_EQ(GCD_END);
  MOV(R5, R4);
  MOV(R4, R3);
  REM(R4, R5);
  MOV(R3, R5);
  JUMP(GCD_LOOP);

  GCD_END:
  DIV(R1, R3);
  DIV(R2, R3);
  CMP(R2, 0);
  JUMP_GE(DENOM_POSITIVE);
  MUL(R1, IMM(-1));
  MUL(R2, IMM(-1));
  DENOM_POSITIVE:

  MOV(INDD(R0, 1), R1);
  MOV(INDD(R0, 2), R2);

  POP(FP);
  RETURN;
