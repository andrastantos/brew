/* 64-bit integer arithmetic on a 32-bit core: add/sub carries,
   multiplies, variable shifts and comparisons.  */
#include "common.h"

#define N 1500

static uint64_t x[N];

static OPT_NOINLINE uint64_t
kernel (void)
{
  uint64_t acc = 0x0123456789abcdefull, mx = 0;
  int64_t sacc = 0;
  for (int i = 0; i < N; i++)
    {
      uint64_t v = x[i];
      acc += v;
      acc ^= acc >> 29;
      acc *= 0x9e3779b97f4a7c15ull;
      sacc -= (int64_t) v >> (32 + (i & 31));
      acc += v << (i % 41);
      if (v > mx)
        mx = v;
      if ((int64_t) v < sacc)
        sacc += 12345;
    }
  return acc ^ mx ^ (uint64_t) sacc;
}

int
main (void)
{
  uint32_t s = opt_seed;
  for (int i = 0; i < N; i++)
    {
      uint64_t hi = opt_rand (&s);
      x[i] = (hi << 32) | opt_rand (&s);
    }
  uint64_t r = kernel ();
  OPT_RESULT ((uint32_t) r ^ (uint32_t) (r >> 32), 0xd2ae8a78u);
}
