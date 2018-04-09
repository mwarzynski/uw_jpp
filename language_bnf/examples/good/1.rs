fn main() : i32 {
  // typ string, literaly napisowe, 
  err:str = "Unable to print your number.";
  str_number:str = "123";

  // Co najmniej dwa typy wartości w wyrażeniach: int i bool     
  is_number:bool = true;
  number:i32;
 
  // str -> i32
  number = parse_i32(str_number);

  // Arytmetyka, porównania
  i:i32 = 0;
  i = (i / 100000) * 10000 + (-10) - (-10);
  is_number = i > 0;

  // while
  while i < 50 {
    // wyrażenia z efektami ubocznymi (+=, ++)
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
