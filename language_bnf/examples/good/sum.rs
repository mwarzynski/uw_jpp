fn sum(i:i32, j:i32) : {
  k:i32;
  r:i32 = 0;
  for k = 0, k < 10, k++ {
    if i / k == 0 {
      continue;
    }
    r += (i + j) * k;
  }
  return r;
}

fn main() : i32 {
  print(sum(10, 100)); 
}

