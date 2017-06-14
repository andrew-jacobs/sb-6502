
#include <stdlib.h>

extern char	END_UDATA;

void *heap_start 	= (void *) &END_UDATA;
void *heap_end 		= (void *) 0xe000;

void _exit ()
{
	;
}
