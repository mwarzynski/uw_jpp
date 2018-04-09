
fn fib(n:i64 = 5, a:i64 = 1, b:i64 = 1) : i64 {
  if 0 < n {
    return fib(n-1, b, a+b);
  }
  return a+b;
}

fn fib_better(n:i32 = 5) : i32 {
  a: i32 = 1;
  b: i32 = 1;
  c: i32;

  for i:i32 = 0, i < n, i++ {
    if a + b < 0 {
      // operacje przerywajace petle while - break i continue
      break; // overflow
    }

    c = a + b;
    a = b;
    b = c;
  }

  return b;
}

fn main() : {
  print(fib_better(5));
}

fn magic(a,b : i64) : i64 {
  return a;
}

