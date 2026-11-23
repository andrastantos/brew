/* Baseline: startup and exit only.  Subtract its counts from the other
   tests to get the kernel's own.  */
#include "common.h"

int
main (void)
{
  OPT_RESULT (opt_seed, 0x2545f491u);
}
