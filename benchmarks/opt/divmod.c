/* Division and remainder: by constants (multiply-high candidates), by
   powers of two (signed), and by variables (library calls).  */
#include "common.h"

#define N 1500

static uint32_t u[N];
static int32_t sv[N];

static OPT_NOINLINE uint32_t
kernel (void)
{
  uint32_t h = 0;
  for (int i = 0; i < N; i++)
    {
      uint32_t x = u[i];
      int32_t y = sv[i];
      h = opt_mix (h, x / 10);
      h = opt_mix (h, x % 10);
      h = opt_mix (h, x / 1000);
      h = opt_mix (h, (uint32_t) (y / 8));
      h = opt_mix (h, (uint32_t) (y % 16));
      h = opt_mix (h, (uint32_t) (y / 7));
      uint32_t d = (x & 0xff) + 1;
      h = opt_mix (h, x / d);
      h = opt_mix (h, (uint32_t) (y % (int32_t) d));
    }
  return h;
}

int
main (void)
{
  uint32_t s = opt_seed;
  for (int i = 0; i < N; i++)
    {
      u[i] = opt_rand (&s);
      sv[i] = (int32_t) opt_rand (&s);
    }
  OPT_RESULT (kernel (), 0x8c639078u);
}
