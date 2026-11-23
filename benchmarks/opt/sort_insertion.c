/* Insertion sort of random keys: an inner loop with a two-condition
   exit (bounds and comparison) whose outcome is data-dependent.  */
#include "common.h"

#define N 400

static int32_t v[N];

static OPT_NOINLINE void
isort (int32_t *a, int n)
{
  for (int i = 1; i < n; i++)
    {
      int32_t x = a[i];
      int j = i - 1;
      while (j >= 0 && a[j] > x)
        {
          a[j + 1] = a[j];
          j--;
        }
      a[j + 1] = x;
    }
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N; i++)
    v[i] = (int32_t) opt_rand (&s);
  isort (v, N);
  for (int i = 0; i < N; i++)
    h = opt_mix (h, (uint32_t) v[i]);
  OPT_RESULT (h, 0x4e8ce3ebu);
}
