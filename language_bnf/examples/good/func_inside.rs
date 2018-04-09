struct Rectangle {
  x: i32,
  y: i32
}

fn rectangle_new(x,y: i32) : Rectangle {
  r: Rectangle;
  r.x = x;
  r.y = y;
}

fn main() : i32 {
  fn rectangle_area(r: Rectangle) : i32 {
    return r.x * r.y;
  }

  r = rectangle_new(10, 20);
  print(rectangle_area(r));

  return 0;
}
