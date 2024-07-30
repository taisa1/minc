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
