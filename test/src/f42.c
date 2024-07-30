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
