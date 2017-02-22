/* 
 *
 * play with cisc and see what happens  
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "compi4/arch/cisc.h"

int main()
{
	START_MACHINE;

	JUMP(CONTINUE);

#include "compi4/arch/char.lib"
#include "compi4/arch/io.lib"
#include "compi4/arch/math.lib"
#include "compi4/arch/string.lib"
#include "compi4/arch/system.lib"
#include "compi4/arch/scheme.lib"

	CONTINUE:
	PUSH(IMM(15663));
	CALL(MAKE_SOB_INTEGER);
	PUSH(R0);
	CALL(NUMBER_TO_STRING);
	PUSH(R0);
	CALL(WRITELN);
	DROP(1);
	STOP_MACHINE;

	return 0;
}
