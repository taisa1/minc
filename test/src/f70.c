long f29(long a0, long a1, long a2, long a3, long a4, long a5,
         long a6, long a7, long a8, long a9, long a10, long a11) {
  return a11;
}
long f(long a0, long a1, long a2, long a3, long a4, long a5,
       long a6, long a7, long a8, long a9, long a10, long a11) {
  return f29(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a0) + 10;
}
