/* Hand-written string loops: strlen, strcmp, strchr and a copy, over
   generated words.  Byte loads, zero tests, loop rotation.  */
#include "common.h"

#define NW 300
#define WL 24

static char words[NW][WL];
static char out[NW * WL];

static OPT_NOINLINE size_t
my_strlen (const char *s)
{
  const char *p = s;
  while (*p)
    p++;
  return p - s;
}

static OPT_NOINLINE int
my_strcmp (const char *a, const char *b)
{
  while (*a && *a == *b)
    a++, b++;
  return (uint8_t) *a - (uint8_t) *b;
}

static OPT_NOINLINE const char *
my_strchr (const char *s, char c)
{
  for (; *s; s++)
    if (*s == c)
      return s;
  return 0;
}

static OPT_NOINLINE char *
my_stpcpy (char *d, const char *s)
{
  while ((*d = *s++))
    d++;
  return d;
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int w = 0; w < NW; w++)
    {
      int len = 1 + opt_rand (&s) % (WL - 1);
      for (int i = 0; i < len; i++)
        words[w][i] = 'a' + opt_rand (&s) % 6;
      words[w][len] = 0;
    }
  char *d = out;
  for (int w = 0; w < NW; w++)
    {
      h = opt_mix (h, (uint32_t) my_strlen (words[w]));
      h = opt_mix (h, (uint32_t) my_strcmp (words[w], words[(w * 7 + 3) % NW]));
      const char *p = my_strchr (words[w], 'f');
      h = opt_mix (h, p ? (uint32_t) (p - words[w]) : 0xffu);
      d = my_stpcpy (d, words[w]);
    }
  h = opt_mix (h, (uint32_t) my_strlen (out));
  OPT_RESULT (h, 0xd0464b98u);
}
