long f(long n) {
  long p;
  p = 2;
  while (p * p <= n) {
    if (n % p == 0) return 0;
    p = p + 1;
  }
  return 1;
}
