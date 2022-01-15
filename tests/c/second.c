long long foo (long long a, long long *w)
{
  return __builtin_add_overflow (a, a, w);
  //return __builtin_sadd_overflow(a, a, &w);
}
