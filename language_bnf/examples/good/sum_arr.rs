main() : i32 {
  // tablice indeksowane int lub cos a la listy
  values: [i32*100] = [ 1, .. ];

  i:i32;
  r:i32 = 0;
  for i = 0, i < 100, i++ {
    r += values[i];
  }

  // r = 100
  print(r);

  return 0;
}
