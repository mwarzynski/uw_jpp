
fn sum(a:i32 = 1, n:i32 = 5) : i32 {
    if n < 0 {
        return a;
    }
    return a + sum(a, n-1);
}

fn main() : {
    print(sum(10));
}
