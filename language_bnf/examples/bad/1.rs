
// array as an argument does not work
nope1(i: [i32]) : bool {
  return false;
}

// array as a return value does not work
nope2() : [i32*2] {
  return [1,2];
}

nope3() : bool {
  if false {
    // goto, really?
    goto main; // nope, it's not the kernel space
  } else {
    return false; 
  }
}

nope4() : {
  text:str = "yay";
  if text == "y" {
    return false;
  } elif text == yay { // no elif
    return false;
  }
  return true;
}

nope5() : {
  i:i32 = 1;
  j:i32 = 2;

  i *= j; // nope
  i /= j; // nope
  // there is only support for += and -=
}

nope6() : {
  // should fail before execution
  i:i32 = "lol";
}

nope7() : {
  del:i32 = 0;
  val:i32 = 1029301238;

  // jawnie obsluzone dynamiczne bledy wykonania,
  // np. dzielenie przez zero

  // interpreter will detect runtime
  // error and as a result program ends
  // with appropiate text sent to stderr
  // like: 'you idiot: division by zero'
  return val / del;
}

nope8() : {
  // cannot initialize array with more
  // than one value
  i: [i32*10] = [1, 2, .. ];
}

nope9() : {
  // continue/break should be only used
  // inside for/while
  continue;
  break;

  // but, return anywhere inside the function is okay
}

nope10() : {
  j:i32 = 1;
  {
    // local scope
    i:i32 = 2;
  }

  // i was declared outside current scope ...
  print(j*i);
  // ... therefore using i here will produce runtime error
}

