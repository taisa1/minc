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
