fn sum(i:int, j:int) -> {
  k:int;
  r:int = 0;
  for k = 0, k < 10, k++ {
    if i / k == 0 {
      continue;
    }
    r += (i + j) * k;
  }
  return r;
}

fn main() -> int {
  print(sum(10, 100)); 
}

