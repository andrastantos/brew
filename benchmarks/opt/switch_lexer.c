/* A tokenizer: a switch over character classes inside a state machine
   (jump table versus compare tree, dense and sparse cases).  */
#include "common.h"

#define N 6000

static char text[N + 1];

enum { T_NUM, T_ID, T_OP, T_LP, T_RP, T_SEMI, T_STR, T_OTHER, T_COUNT };

static OPT_NOINLINE uint32_t
lex (const char *p, uint32_t counts[T_COUNT])
{
  uint32_t h = 0;
  while (*p)
    {
      int tok;
      switch (*p)
        {
        case ' ': case '\t': case '\n':
          p++;
          continue;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          {
            uint32_t v = 0;
            while (*p >= '0' && *p <= '9')
              v = v * 10 + (*p++ - '0');
            h = opt_mix (h, v);
            tok = T_NUM;
            break;
          }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
        case 'g': case 'h': case 'x': case 'y': case 'z': case '_':
          while ((*p >= 'a' && *p <= 'z') || *p == '_' || (*p >= '0' && *p <= '9'))
            p++;
          tok = T_ID;
          break;
        case '+': case '-': case '*': case '/': case '=': case '<': case '>':
          if (p[1] == '=')
            p++;
          p++;
          tok = T_OP;
          break;
        case '(': p++; tok = T_LP; break;
        case ')': p++; tok = T_RP; break;
        case ';': p++; tok = T_SEMI; break;
        case '"':
          p++;
          while (*p && *p != '"')
            p++;
          if (*p)
            p++;
          tok = T_STR;
          break;
        default:
          p++;
          tok = T_OTHER;
          break;
        }
      counts[tok]++;
    }
  return h;
}

int
main (void)
{
  static const char alphabet[] = "  \n0123456789abcxyz_+-*/=<>();\"?";
  uint32_t s = opt_seed, counts[T_COUNT] = { 0 };
  for (int i = 0; i < N; i++)
    text[i] = alphabet[opt_rand (&s) % (sizeof alphabet - 1)];
  uint32_t h = lex (text, counts);
  for (int t = 0; t < T_COUNT; t++)
    h = opt_mix (h, counts[t]);
  OPT_RESULT (h, 0xd9f95579u);
}
