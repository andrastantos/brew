/* Binary search in a sorted table: an unpredictable two-way branch per
   step (a candidate for branchless selection), plus a lookup loop.  */
#include "common.h"

#define N 1024
#define Q 4000

static uint32_t tab[N];

static OPT_NOINLINE int
find (const uint32_t *t, int n, uint32_t key)
{
  int lo = 0, hi = n;
  while (lo < hi)
    {
      int mid = (lo + hi) >> 1;
      if (t[mid] < key)
        lo = mid + 1;
      else
        hi = mid;
    }
  return lo;
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0, x = 0;
  for (int i = 0; i < N; i++)
    {
      x += 1 + (opt_rand (&s) & 0xfff);
      tab[i] = x;
    }
  for (int q = 0; q < Q; q++)
    h = opt_mix (h, (uint32_t) find (tab, N, opt_rand (&s) % (x + 1)));
  OPT_RESULT (h, 0x1e63f23fu);
}
