/* Selects that need no branch: min, max, abs, clamp, saturating add
   and sign, over arrays (if-conversion opportunities).  */
#include "common.h"

#define N 2000

static int32_t a[N], b[N];
static int16_t sat[N];

static OPT_NOINLINE uint32_t
kernel (void)
{
  int32_t mn = INT32_MAX, mx = INT32_MIN;
  uint32_t h = 0;
  for (int i = 0; i < N; i++)
    {
      int32_t x = a[i], y = b[i];
      mn = x < mn ? x : mn;
      mx = x > mx ? x : mx;
      int32_t ax = x < 0 ? -x : x;
      int32_t cl = x < -1000 ? -1000 : x > 1000 ? 1000 : x;
      int32_t sg = (x > 0) - (x < 0);
      int32_t sum = (int16_t) x + (int16_t) y;
      sat[i] = sum > 32767 ? 32767 : sum < -32768 ? -32768 : sum;
      uint32_t ux = (uint32_t) x, uy = (uint32_t) y;
      uint32_t umin = ux < uy ? ux : uy;
      h = opt_mix (h, (uint32_t) (ax + cl + sg) ^ umin);
    }
  return opt_mix (opt_mix (h, (uint32_t) mn), (uint32_t) mx);
}

int
main (void)
{
  uint32_t s = opt_seed, h;
  for (int i = 0; i < N; i++)
    {
      int32_t r = (int32_t) opt_rand (&s);
      a[i] = r >> (1 + (opt_rand (&s) & 30));
      b[i] = (int32_t) opt_rand (&s);
    }
  h = kernel ();
  for (int i = 0; i < N; i++)
    h = opt_mix (h, (uint32_t) sat[i]);
  OPT_RESULT (h, 0x5169f0a7u);
}
