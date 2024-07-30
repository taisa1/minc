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
