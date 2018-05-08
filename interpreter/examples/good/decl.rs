fn main() -> int {
  // string 
  err:str = "Unable to print your number.";
  str_number:str = "123";

  // at least two types
  is_number:bool = true;
  number:int;
 
  // str -> int
  number = parse_int(str_number);

  i:int = 0;
  i = (i / 100000) * 10000 + (-10) - (-10);
  is_number = i > 0;

  while i < 50 {
    number += i;
    i++;
  }

  // if .. else 
  if is_number {
    print(number);
  } else {
    print(err);
  }

  return 0;
}
