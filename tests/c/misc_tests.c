int test(int a)
{
    return a+(-1);
}

int test2(int a, int b)
{
  return a+b;
}

int test3(int a, int b)
{
  return a*b;
}

unsigned char test4(unsigned char a, unsigned char b)
{
  return a*b;
}

unsigned int test5(unsigned int a)
{
  return a*-1234;
}

int and(int a, int b) { return a & b; }
int or(int a, int b) { return a | b; }
int xor(int a, int b) { return a ^ b; }
int neg(int a) { return -a; }
int not(int a) { return ~a; }
int sl(int a, int b) { return a << b; }
unsigned int sr(unsigned int a, unsigned int b) { return a >> b; }
int asr(int a, int b) { return a >> b; }


/*unsigned long long lumul(unsigned long a, unsigned long b)
{
  return (unsigned long long)a * (unsigned long long)b;
}

long long lmul(long a, long b)
{
  return (long long)a * (long long)b;
}
*/

int test_eq(int a, int b) { if (a==b) return a; else return b; }
int test_ne(int a, int b) { if (a!=b) return a; else return b; }
int test_lt(int a, int b) { if (a<b) return a; else return b; }
int test_le(int a, int b) { if (a<=b) return a; else return b; }
int test_gt(int a, int b) { if (a>b) return a; else return b; }
int test_ge(int a, int b) { if (a>=b) return a; else return b; }

unsigned int test_ult(unsigned int a, unsigned int b) { if (a<b) return a; else return b; }
unsigned int test_ule(unsigned int a, unsigned int b) { if (a<=b) return a; else return b; }
unsigned int test_ugt(unsigned int a, unsigned int b) { if (a>b) return a; else return b; }
unsigned int test_uge(unsigned int a, unsigned int b) { if (a>=b) return a; else return b; }

int test_eqz(int a) { if (a==0) return a; else return 0; }
int test_nez(int a) { if (a!=0) return a; else return 0; }
int test_ltz(int a) { if (a<0) return a; else return 0; }
int test_lez(int a) { if (a<=0) return a; else return 0; }
int test_gtz(int a) { if (a>0) return a; else return 0; }
int test_gez(int a) { if (a>=0) return a; else return 0; }

unsigned int test_ultz(unsigned int a) { if (a<0) return a; else return 0; }
unsigned int test_ulez(unsigned int a) { if (a<=0) return a; else return 0; }
unsigned int test_ugtz(unsigned int a) { if (a>0) return a; else return 0; }
unsigned int test_ugez(unsigned int a) { if (a>=0) return a; else return 0; }

int test_qm(int a, int b) { return a > b; }

void test_void(void) { int a, b, c; a=1; b=2; c = a+b; }

int test_call(int a, int b) { return test_qm(a,b); }

extern void yyy(void);
extern void abort(void);

long int xxx(long int a)
{
    yyy();
  if (a)
    ;
  return a;
}


#define DT long long
#define DTL 63
inline int doo(DT a, DT b, DT *c) __attribute__((always_inline));

inline int doo(DT a, DT b, DT *c)
{
  *c = a+b;
  switch ((a >> DTL) + (b >> DTL)) {
    case 0:
      return 0;
    case 1:
      return !(*c >> DTL);
    case 2:
      return 1;
  }
}

#define DWtype long long
DWtype
__absvDI2 (DWtype a)
{
  const DWtype v = 0 - (a < 0);
  DWtype w;

  int x = __builtin_add_overflow (a, v, &w);
  int y= doo(a,v,&w);
  //if (x);
  //if(__builtin_add_overflow (a, v, &w))
  //  abort();
  

  return v ^ w;
}
