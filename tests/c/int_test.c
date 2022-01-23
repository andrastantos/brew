#include <stdio.h>

extern int __clzdi2 (unsigned long long);

#define CLZ_TEST(x) printf("clz(%llx %x %x) = %x \n", (unsigned long long)(x), (unsigned long long)(x), __clzdi2(x));
#define OP_TEST_ULL(x, y, op) { \
  unsigned long long xx = x; \
  unsigned long long yy = y; \
  printf("OP_TEST_ULL %llx %s %llx = %llx\n",xx,#op,yy,xx op yy); \
}
#define MOD_TEST_ULL(x, y) { \
  unsigned long long xx = x; \
  unsigned long long yy = y; \
  printf("MOD_TEST_ULL %x %x %% %x %x = %x %x\n",xx,yy,xx%yy); \
}
#define MOD_TEST_UL(x, y) { \
  unsigned long xx = x; \
  unsigned long yy = y; \
  printf("MOD_TEST_UL %x %% %x = %x\n",xx,yy,xx%yy); \
}

#define SH_TEST_ULL(x, y) { \
  unsigned long long xx = x; \
  unsigned long yy = y; \
  printf("SH_TEST_ULL %x %x >> %x = %x %x\n",xx,yy,xx >> yy); \
  printf("            %x %x << %x = %x %x\n",xx,yy,xx << yy); \
}

#define MARKER asm volatile ( "nop\nnop\nnop\nnop\n" );
void main(void)
{
  //int a = 0xdeadbeef;
  //int b = 33;
  //unsigned int c = 0xdeadbeef;

  unsigned long long d = 0xdeadbeeff001b001LL;
  //printf("%x %x >> %x = %x %x\n",d,4,d >> 4);
  //printf("%x %x << %x = %x %x\n",d,4,d << 4);
  //printf("%llx >> %x = %llx\n",d,4,d >> 4);

  //printf("%x %% %x = %x\n",a,b,a%b);
  //printf("%x %% %x = %x\n",c,b,c%b);
  //printf("%x / %x = %x\n",a,b,a/b);
  //printf("%x / %x = %x\n",c,b,c/b);
  //CLZ_TEST(0ULL);
  //CLZ_TEST(1ULL);
  //CLZ_TEST(31ULL);
  //CLZ_TEST(32ULL);
  //CLZ_TEST(255ULL);
  //CLZ_TEST(256ULL);
  //CLZ_TEST(1ULL << 31);
  //CLZ_TEST(1ULL << 63);
  //printf("clz(%x) = %x \n", d, __clzdi2(d));
  //printf("clz(%x) = %x \n", 0xe, __clzdi2(0xe));
  //printf("clz(%x) = %x \n", 0xb00, __clzdi2(0xb00));
  //printf("%x %x %% %x = %x %x\n",d,b,d%b);
  //MOD_TEST(5ULL,3ULL);
  //MOD_TEST_ULL(5ULL,3ULL);
  //MOD_TEST_UL(5ULL,3ULL);

  // operations tested and passed: >> << & | ^
  // operations tested and failed: % +
  //SH_TEST_ULL(0x012345678ULL,4);
  //OP_TEST_ULL(0xfedcba9876543210ULL,0x3333333377777777ULL,+);
  MARKER;
  d = d + 45;
  MARKER;
  //a = b + 45;
  //MARKER;
  OP_TEST_ULL(0xdeadbeefULL,0xd00db00bULL,+);
  OP_TEST_ULL(5, 3, %);
  printf("Int printf test: %d %u %i\n", 32, 45, -1);
  //MARKER;
  //printf("repeat with feeling\n");
  //for(a=0;a<64;++a) {MOD_TEST(5ULL << a,3);}
}
