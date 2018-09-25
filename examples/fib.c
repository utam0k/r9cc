int fibdp[100];

int fib(int n) {
  if (n == 0 || n == 1) {
    return n;
  } else if (fibdp[n] != 0) {
    return fibdp[n];
  } else {
    fibdp[n] = fib(n-2) + fib(n-1);
    return fibdp[n];
  }
}

int main() {
  for (int i = 0; i < 100; i++) 
    fibdp[i] = 0;
  int ans = fib(46);
  printf("%d\n", ans);
  return 0;
}
