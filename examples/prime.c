int prime(int n) {
  if (n % 2 == 0) return 0;
  int k = 3;
  while (k * k <= n) {
    if (n % k == 0)
      return 0;
    k = k + 2;
  }
  return 1;
}

int main() {
  int i = 2;
  while (i < 10000) {
    if (prime(i))
      printf("prime!: %d\n", i);
    i++;
  }
  return 0;
}
