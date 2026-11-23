/* Call overhead: recursive Fibonacci, small leaf functions with
   several arguments, a function using many call-saved registers,
   and calls through a function pointer table.  */
#include "common.h"

static OPT_NOINLINE uint32_t
fib (uint32_t n)
{
  return n < 2 ? n : fib (n - 1) + fib (n - 2);
}

static OPT_NOINLINE uint32_t
leaf4 (uint32_t a, uint32_t b, uint32_t c, uint32_t d)
{
  return (a + b) ^ (c - d);
}

static OPT_NOINLINE uint32_t
leaf6 (uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e, uint32_t f)
{
  return a * b + c * d + e * f;
}

/* Keeps many values live across calls, so it saves call-saved registers.  */
static OPT_NOINLINE uint32_t
busy (uint32_t x)
{
  uint32_t a = x + 1, b = x * 3, c = x ^ 0x55, d = x >> 3, e = x << 2, f = ~x;
  uint32_t r = leaf4 (a, b, c, d);
  r += leaf4 (e, f, a, b);
  return r + a + b + c + d + e + f;
}

static uint32_t op_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t op_sub (uint32_t a, uint32_t b) { return a - b; }
static uint32_t op_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t op_mul (uint32_t a, uint32_t b) { return a * b; }

static uint32_t (*const ops[4]) (uint32_t, uint32_t) = { op_add, op_sub, op_xor, op_mul };

int
main (void)
{
  uint32_t s = opt_seed, h = fib (17);
  for (int i = 0; i < 500; i++)
    {
      uint32_t x = opt_rand (&s);
      h = opt_mix (h, leaf4 (x, h, i, 7));
      h = opt_mix (h, leaf6 (x, h, i, 7, x >> 3, 11));
      h = opt_mix (h, busy (x));
      h = opt_mix (h, ops[x & 3] (h, x));
    }
  OPT_RESULT (h, 0x2690df82u);
}
