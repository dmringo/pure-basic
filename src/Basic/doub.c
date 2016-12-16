#include <stdio.h>


typedef union
{
  double d;
  long l;
} Doub;


#define makeOpFun(funName, OP)                                          \
  double funName(double a, double b ){ return ((long) a) OP ((long) b); } 

#define makeShFun(funName, OP)                          \
  double funName(double a, int b) { return OP(a, b); }

static inline long shr(long a, int b) { return a >> b; }
static inline long shl(long a, int b) { return a << b; }
static inline long rol(long trg, int shift)
{
  __asm__("rolq %%cl, %[trg]" : [trg] "+r" (trg): "c" (shift));
  return trg;
}
static inline long ror(long trg, int shift)
{
  __asm__("rorq %%cl, %[trg]" : [trg] "+r" (trg): "c" (shift));
  return trg;
}
static inline long cmp(long trg, int ignored) { return ~trg; }


makeOpFun(dblAnd, &)
makeOpFun(dblOr,  |)
makeOpFun(dblXor, ^)

makeShFun(dblShr, shr)
makeShFun(dblShl, shl)
makeShFun(dblRol, rol)
makeShFun(dblRor, ror)
makeShFun(dblCmp, cmp)


void printBits(long b)
{
  int i = 63;
  for(; i >= 0; i-- ) putchar(1 & (b >> i) ? '1' : '0');
  putchar('\n');
}

#ifdef TESTING_DOUB_C

int main(void)
{
  /* tests go here */
}

#endif
