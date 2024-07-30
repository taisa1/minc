#if TEST_NO == 0
long f() { return 123; }
#endif
#if TEST_NO == 1
long f() { return -10; }
#endif
#if TEST_NO == 2
long f() { return 1 + 2; }
#endif
#if TEST_NO == 3
long f(long x) { return x; }
#endif
#if TEST_NO == 4
long f(long x) { return x + 5; }
#endif
#if TEST_NO == 5
long f(long x) { return -x; }
#endif
#if TEST_NO == 6
long f(long x) { return !x; }
#endif
#if TEST_NO == 7
long f(long x, long y) { return x + y; }
#endif
#if TEST_NO == 8
long f(long x, long y) { return x * y; }
#endif
#if TEST_NO == 9
long f(long x, long y) { return x / y; }
#endif
#if TEST_NO == 10
long f(long x, long y) { return x % y; }
#endif
#if TEST_NO == 11
long f(long x, long y) { return x == y; }
#endif
#if TEST_NO == 12
long f(long x, long y) { return x != y; }
#endif
#if TEST_NO == 13
long f(long x, long y) { return x < y; }
#endif
#if TEST_NO == 14
long f(long x, long y) { return x > y; }
#endif
#if TEST_NO == 15
long f(long x, long y) { return x <= y; }
#endif
#if TEST_NO == 16
long f(long x, long y) { return x >= y; }
#endif
#if TEST_NO == 17
long f(long x, long y, long z) { return x + y + z; }
#endif
#if TEST_NO == 18
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a0;
}
#endif
#if TEST_NO == 19
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a1;
}
#endif
#if TEST_NO == 20
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a2;
}
#endif
#if TEST_NO == 21
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a3;
}
#endif
#if TEST_NO == 22
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a4;
}
#endif
#if TEST_NO == 23
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a5;
}
#endif
#if TEST_NO == 24
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a6;
}
#endif
#if TEST_NO == 25
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a7;
}
#endif
#if TEST_NO == 26
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a8;
}
#endif
#if TEST_NO == 27
long f(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a9;
}
#endif
#if TEST_NO == 28
long f(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a10;
}
#endif
#if TEST_NO == 29
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a11;
}
#endif
#if TEST_NO == 30
long f(long x, long y) {
  return x * ! y;
}
#endif
#if TEST_NO == 31
long f(long x) {
  return x / 2 / 3;
}
#endif
#if TEST_NO == 32
long f(long x, long y, long z) {
  return x - y - z;
}
#endif
#if TEST_NO == 33
long f(long a, long x, long b, long y) {
  return a * x + b * y;
}
#endif
#if TEST_NO == 34
long f(long a, long b, long c) {
  return a < b < c;
}
#endif
#if TEST_NO == 35
long f(long a, long b, long c) {
  return a == b == c;
}
#endif
#if TEST_NO == 36
long f(long a, long b, long c) {
  return a == b < c;
}
#endif
#if TEST_NO == 37
long f(long a0, long a1, long a2,
       long a3, long a4, long a5,
       long a6, long a7, long a8) {
  return a0 + a1 * a2 == a3 + a4 * a5 < a6 + a7 * a8;
}
#endif
#if TEST_NO == 38
long f(long a0, long a1, long a2,
         long a3, long a4, long a5,
         long a6, long a7) {
  return a0 = a1 = a2 + a3 * a4 < a5 + a6 * a7;
}
#endif
#if TEST_NO == 39
long f(long x) {
  long y;
  y = x + 2;
  return y * y;
}
#endif
#if TEST_NO == 40
long f(long x, long y, long z) {
  if (x) {
    return y + z;
  } else {
    return y * z;
  }
}
#endif
#if TEST_NO == 41
long f(long x, long y, long z) {
  long p;
  p = 100;
  if (x > 0)
    if (y > 0)
      p = 200;
    else
      p = 300;
  return p;
}
#endif
#if TEST_NO == 42
long sum2(long n) {
  long i;
  long s;
  i = 0;
  s = 0;
  while (i < n * n) {
    s = s + i;
    i = i + 1;
  }
  return s;
}
long f(long n) {
  return sum2(n % 10000);
}
#endif
#if TEST_NO == 43
long f(long n) {
  long p;
  p = 2;
  while (p * p <= n) {
    if (n % p == 0) return 0;
    p = p + 1;
  }
  return 1;
}
#endif
#if TEST_NO == 44
long f01() { return -10; }
long f(long n) {
  return f01() + 1;
}
#endif
#if TEST_NO == 45
long f03(long x) { return x; }
long f(long n) {
  return f03(n) + 1;
}
#endif
#if TEST_NO == 46
long f07(long x, long y) { return x + y; }
long f(long x, long y) {
  return f07(x, y) + 1;
}
#endif
#if TEST_NO == 47
long f18(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return a0;
}
long f() {
  return f18(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 48
long f19(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a1;
}
long f() {
  return f19(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 49
long f20(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a2;
}
long f() {
  return f20(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 50
long f21(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a3;
}
long f() {
  return f21(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 51
long f22(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a4;
}
long f() {
  return f22(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 52
long f23(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a5;
}
long f() {
  return f23(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 53
long f24(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a6;
}
long f() {
  return f24(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 54
long f25(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a7;
}
long f() {
  return f25(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 55
long f26(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a8;
}
long f() {
  return f26(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 56
long f27(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a9;
}
long f() {
  return f27(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 57
long f28(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a10;
}
long f() {
  return f28(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 58
long f29(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a11;
}
long f() {
  return f29(100, 200, 300, 400, 500, 600,
             700, 800, 900, 1000, 1100, 1200) + 10;
}
#endif
#if TEST_NO == 59
long f18(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a0;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 60
long f19(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a1;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return f19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 61
long f20(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a2;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 62
long f21(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a3;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 63
long f22(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a4;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 64
long f23(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a5;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f23(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 65
long f24(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a6;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f24(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 66
long f25(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a7;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f25(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 67
long f26(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a8;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f26(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 68
long f27(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a9;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f27(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 69
long f28(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a10;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f28(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 70
long f29(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a11;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f29(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
#endif
#if TEST_NO == 71
long f37(long a0, long a1, long a2,
         long a3, long a4, long a5,
         long a6, long a7, long a8) {
  return a0 + a1 * a2 == a3 + a4 * a5 < a6 + a7 * a8;
}
long f(long a0, long a1, long a2,
       long a3, long a4, long a5,
       long a6, long a7, long a8) {
  return f37(a1, a2, a3, a4, a5, a6, a7, a8, a0) + 1;
}

#endif
#if TEST_NO == 72
long f37(long a0, long a1, long a2,
         long a3, long a4, long a5,
         long a6, long a7, long a8) {
  return a0 + a1 * a2 == a3 + a4 * a5 < a6 + a7 * a8;
}
long f(long a0, long a1, long a2,
       long a3, long a4, long a5,
       long a6, long a7, long a8) {
  long p;
  long q;
  p = f37(a1, a2, a3, a4, a5, a6, a7, a8, a0);
  q = f37(a2, a3, a4, a5, a6, a7, a8, a0, a1);
  return p + q;
}
#endif
#if TEST_NO == 73
long fact(long n) {
  if (n <= 0) {
    return 1;
  } else {
    long x;
    x = fact(n - 1);
    return n * x;
  }
}
long f(long n) {
  long m;
  m = 10 + n % 990;
  return fact(m);
}
#endif
#if TEST_NO == 74
long fib(long n) {
  if (n < 2) {
    return 1;
  } else {
    long x;
    long y;
    x = fib(n - 1);
    y = fib(n - 2);
    return x + y;
  }
}

long f(long n) {
  long m;
  long y;
  m = 10 + n % 20;
  y = fib(m);
  return y;
}
#endif

