/* Integer matrix multiply: triple loop nest, multiply-accumulate,
   two-dimensional indexing, strided loads.  */
#include "common.h"

#define N 24

static int32_t a[N][N], b[N][N], c[N][N];

static OPT_NOINLINE void
matmul (void)
{
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      {
        int32_t sum = 0;
        for (int k = 0; k < N; k++)
          sum += a[i][k] * b[k][j];
        c[i][j] = sum;
      }
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      {
        a[i][j] = (int32_t) (opt_rand (&s) % 201) - 100;
        b[i][j] = (int32_t) (opt_rand (&s) % 201) - 100;
      }
  matmul ();
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      h = opt_mix (h, (uint32_t) c[i][j]);
  OPT_RESULT (h, 0x4981046au);
}
