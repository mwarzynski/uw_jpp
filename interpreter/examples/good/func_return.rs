fn do_hello() -> fn () {
    fn hello() {
        print("Hello, World!");
    }
    return hello;
}

fn main() -> {
    h = fn do_hello();
    h();
}
