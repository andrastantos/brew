#include <stdio.h>
#include <stdarg.h>

static void put_digit(int a, FILE *f)
{
  putc(a > 9 ? 'a'+a-10 : '0'+a, f);
}

static void put_hex(int a, FILE *f)
{
  put_digit((a >> 28) & 0xf, f);
  put_digit((a >> 24) & 0xf, f);
  put_digit((a >> 20) & 0xf, f);
  put_digit((a >> 16) & 0xf, f);
  put_digit((a >> 12) & 0xf, f);
  put_digit((a >>  8) & 0xf, f);
  put_digit((a >>  4) & 0xf, f);
  put_digit((a >>  0) & 0xf, f);
}

static void dbg(const char *prefix, int a)
{
  fputs(prefix, stdout);
  //puts(prefix);
  put_hex(a, stdout);
}

/*
static void one_arg(int a) {
  fputs(__FUNCTION__, stdout);
  dbg(" a: ", a);
  putc('\n', stdout);
}

static void two_arg(int a, int b) {
  fputs(__FUNCTION__, stdout);
  dbg(" a: ", a);
  dbg(" b: ", b);
  putc('\n', stdout);
}

static void three_arg(int a, int b, int c) {
  fputs(__FUNCTION__, stdout);
  dbg(" a: ", a);
  dbg(" b: ", b);
  dbg(" c: ", c);
  putc('\n', stdout);
}

static void four_arg(int a, int b, int c, int d) {
  fputs(__FUNCTION__, stdout);
  dbg(" a: ", a);
  dbg(" b: ", b);
  dbg(" c: ", c);
  dbg(" d: ", d);
  putc('\n', stdout);
}

static void five_arg(int a, int b, int c, int d, int e) {
  fputs(__FUNCTION__, stdout);
  dbg(" a: ", a);
  dbg(" b: ", b);
  dbg(" c: ", c);
  dbg(" d: ", d);
  dbg(" e: ", e);
  putc('\n', stdout);
}

static void six_arg(int a, int b, int c, int d, int e, int f) {
  fputs(__FUNCTION__, stdout);
  dbg(" a: ", a);
  dbg(" b: ", b);
  dbg(" c: ", c);
  dbg(" d: ", d);
  dbg(" e: ", e);
  dbg(" f: ", f);
  putc('\n', stdout);
}
*/
static int var_arg(int count, ...)
{
  fputs(__FUNCTION__, stdout);
  dbg(" count: ", count);
  va_list args;
  va_start(args, count);
  int i;
  for(i=0;i<count;++i)
  {
    dbg(" arg", i);
    int arg = va_arg(args, int);
    dbg(": ", arg);
  }
  putc('\n', stdout);
  va_end(args);
}

int main(void)
{
  //one_arg(__COUNTER__);
  //two_arg(__COUNTER__, __COUNTER__);
  //three_arg(__COUNTER__, __COUNTER__, __COUNTER__);
  //four_arg(__COUNTER__, __COUNTER__, __COUNTER__, __COUNTER__);
  //five_arg(__COUNTER__, __COUNTER__, __COUNTER__, __COUNTER__, __COUNTER__);
  //six_arg(__COUNTER__, __COUNTER__, __COUNTER__, __COUNTER__, __COUNTER__, __COUNTER__);
  var_arg(3, __COUNTER__, __COUNTER__, __COUNTER__);
  return 0;
}
