/* Sieve of Eratosthenes over a byte array, then counting: strided
   byte stores and a sparse conditional.  */
#include "common.h"

#define N 20000

static uint8_t composite[N];

static OPT_NOINLINE uint32_t
sieve (uint32_t n)
{
  for (uint32_t i = 2; i * i < n; i++)
    if (!composite[i])
      for (uint32_t j = i * i; j < n; j += i)
        composite[j] = 1;
  uint32_t count = 0, h = 0;
  for (uint32_t i = 2; i < n; i++)
    if (!composite[i])
      {
        count++;
        h += i;
      }
  return opt_mix (count, h);
}

int
main (void)
{
  OPT_RESULT (sieve (N - (opt_seed & 1)), 0x1d7decf3u);
}
