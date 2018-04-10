
fn pow(k:i32, n:i32 = 2) -> i32 {
  i:i32 = 0;
  res:i32 = k;
  for i = 1, i < n, i++ {
    res = res * k;
  }
  return k;
}

fn main() -> i32 {
  i:i32;
  for i = 0, i < 11, i++ {
    print(pow(i));
  }
  return 0;
}
