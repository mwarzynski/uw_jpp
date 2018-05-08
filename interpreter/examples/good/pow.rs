
fn pow(k:int, n:int = 2) -> int {
  i:int = 0;
  res:int = k;
  for i = 1, i < n, i++ {
    res = res * k;
  }
  return k;
}

fn main() -> int {
  i:int;
  for i = 0, i < 11, i++ {
    print(pow(i));
  }
  return 0;
}
