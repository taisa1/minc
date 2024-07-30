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
