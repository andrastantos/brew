/* Single- and double-precision arithmetic, compares and conversions:
   all of it libgcc soft-float calls on Brew.  Only +, -, *, / and
   conversions, so the results are bit-exact on any IEEE host.  */
#include <string.h>
#include "common.h"

#define N 300

static float fa[N];
static double da[N];

static uint32_t
fbits (float f)
{
  uint32_t u;
  memcpy (&u, &f, sizeof u);
  return u;
}

static uint32_t
dbits (double d)
{
  uint64_t u;
  memcpy (&u, &d, sizeof u);
  return (uint32_t) u ^ (uint32_t) (u >> 32);
}

static OPT_NOINLINE uint32_t
kernel (void)
{
  float fs = 0.0f, fp = 1.0f;
  double ds = 0.0, dmax = -1e300;
  int32_t isum = 0;
  for (int i = 0; i < N; i++)
    {
      fs += fa[i] * 0.5f;
      fp = fp * 0.999f + fa[i] / 1024.0f;
      ds += da[i] * da[i] - 3.0;
      if (da[i] > dmax)
        dmax = da[i];
      isum += (int32_t) (fa[i] * 16.0f);
      ds += (double) isum / 7.0;
    }
  return fbits (fs) ^ fbits (fp) ^ dbits (ds) ^ dbits (dmax) ^ (uint32_t) isum;
}

int
main (void)
{
  uint32_t s = opt_seed;
  for (int i = 0; i < N; i++)
    {
      fa[i] = (float) (int32_t) (opt_rand (&s) & 0xffff) / 256.0f - 100.0f;
      da[i] = (double) (opt_rand (&s) & 0xfffff) / 1000.0;
    }
  OPT_RESULT (kernel (), 0xdfc185cfu);
}
