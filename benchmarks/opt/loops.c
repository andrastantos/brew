/* Loop shapes: counted loops with small and unknown trip counts,
   search loops with an early exit, nested loops with a break, and a
   down-counting loop.  Mostly about where the branches go: a loop
   that tests at the bottom takes one branch per iteration, one that
   tests at the top and jumps back takes two.  */
#include "common.h"

#define N 2000

static int32_t a[N];
static uint8_t small[N];

static OPT_NOINLINE int
find_first (const int32_t *v, int n, int32_t key)
{
  for (int i = 0; i < n; i++)
    if (v[i] == key)
      return i;
  return -1;
}

static OPT_NOINLINE uint32_t
short_trips (const uint8_t *trip, int n)
{
  uint32_t h = 0;
  for (int i = 0; i < n; i++)
    for (uint32_t k = 0; k < trip[i]; k++)
      h += k ^ i;
  return h;
}

static OPT_NOINLINE int
count_until_negative (const int32_t *v, int n)
{
  int c = 0;
  while (c < n && v[c] >= 0)
    c++;
  return c;
}

static OPT_NOINLINE uint32_t
nested_break (const int32_t *v, int n)
{
  uint32_t h = 0;
  for (int i = 0; i < n; i += 50)
    for (int j = i; j < n; j++)
      {
        if ((v[j] & 0xff) == 0x42)
          break;
        h += (uint32_t) v[j];
      }
  return h;
}

static OPT_NOINLINE uint32_t
countdown (const int32_t *v, int n)
{
  uint32_t h = 0;
  do
    h = h * 3 + (uint32_t) v[--n];
  while (n);
  return h;
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N; i++)
    {
      a[i] = (int32_t) (opt_rand (&s) & 0x7fffffff);
      small[i] = opt_rand (&s) % 5;
    }
  a[N - 7] = -1;
  for (int q = 0; q < 20; q++)
    h = opt_mix (h, (uint32_t) find_first (a, N, a[(q * 97) % N]));
  h = opt_mix (h, short_trips (small, N));
  h = opt_mix (h, (uint32_t) count_until_negative (a, N));
  h = opt_mix (h, nested_break (a, N));
  h = opt_mix (h, countdown (a, N));
  OPT_RESULT (h, 0xad3dbc69u);
}
