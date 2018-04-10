
// yay, functions may return value
fn main() -> i32 {
  n:i32 = 0;

  i:i32;
  for i = 0, i < 100, i++ {
    { // local scope
      i:i32;
      for i = 100, i < 105, i++ {
        n -= 50;
      }
    }
    if i > 100 {
      print("Something went wrong.");
    }

    n += 1000;
  }

  print(n); // n = 75000
  return 0;
}
