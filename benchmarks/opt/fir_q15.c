/* Fixed-point FIR filter on 16-bit samples: halfword loads with sign
   extension, multiply-accumulate, rounding shift and saturation.  */
#include "common.h"

#define N 1200
#define TAPS 16

static int16_t in[N + TAPS], out[N];
static const int16_t coef[TAPS] = {
  -120, 340, -812, 1530, -2600, 4400, -8100, 26000,
  26000, -8100, 4400, -2600, 1530, -812, 340, -120
};

static OPT_NOINLINE void
fir (const int16_t *x, int16_t *y, int n)
{
  for (int i = 0; i < n; i++)
    {
      int32_t acc = 1 << 14;
      for (int k = 0; k < TAPS; k++)
        acc += x[i + k] * coef[k];
      acc >>= 15;
      y[i] = acc > 32767 ? 32767 : acc < -32768 ? -32768 : acc;
    }
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N + TAPS; i++)
    in[i] = (int16_t) opt_rand (&s) >> 1;  /* keeps the sum within int32_t */
  fir (in, out, N);
  for (int i = 0; i < N; i++)
    h = opt_mix (h, (uint16_t) out[i]);
  OPT_RESULT (h, 0x05b1476au);
}
