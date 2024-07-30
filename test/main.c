#include <stdio.h>
#include <stdlib.h>

#if 0 <= TEST_NO && TEST_NO <= 74
enum { max_args = 12 };

long f(long, long, long, long, long, long,
       long, long, long, long, long, long);

int main(int argc, char ** argv) {
  long seed = (argc > 1 ? atol(argv[1]) : 12345 + TEST_NO);
  unsigned short rg[3] = { (seed >>  0) & 0xffff,
                           (seed >> 16) & 0xffff,
                           (seed >> 32) & 0xffff };
  long a[max_args];
  for (long i = 0; i < max_args; i++) {
    a[i] = nrand48(rg);
  }
  long y = f(a[0], a[1], a[2], a[3], a[4], a[5],
             a[6], a[7], a[8], a[9], a[10], a[11]);
  printf("%ld\n", y);
  return 0;
}
#endif
