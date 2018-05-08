struct Rectangle {
  x: int,
  y: int
}

fn rectangle_new(x,y: int) -> Rectangle {
  r: Rectangle;
  r.x = x;
  r.y = y;
}

fn main() -> int {
  fn rectangle_area(r: Rectangle) -> int {
    return r.x * r.y;
  }

  r = rectangle_new(10, 20);
  print(rectangle_area(r));

  return 0;
}
