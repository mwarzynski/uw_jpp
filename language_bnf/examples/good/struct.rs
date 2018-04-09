struct Rectangle {
  x: i32,
  y: i32
}

rectangle_area(r: Rectangle) : i32 {
  return r.x * r.y;
}

main() : i32 {
  r: Rectangle;
  r.x = 10;
  r.y = 20;

  print(rectangle_area(r));

  return 0;
}
