/* newlib qsort with a comparison callback: indirect calls, a small
   leaf function called very often, library code.  */
#include <stdlib.h>
#include "common.h"

#define N 1000

static uint32_t v[N];

static int
cmp (const void *pa, const void *pb)
{
  uint32_t a = *(const uint32_t *) pa, b = *(const uint32_t *) pb;
  return (a > b) - (a < b);
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N; i++)
    v[i] = opt_rand (&s);
  qsort (v, N, sizeof v[0], cmp);
  for (int i = 0; i < N; i++)
    h = opt_mix (h, v[i]);
  OPT_RESULT (h, 0xcd654116u);
}
