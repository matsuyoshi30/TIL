fn another_fn() {
    println!("another function");
}

fn main() {
    let x = plus_one(5);
    println!("The value of x is: {}", x);

    another_fn();
}

fn plus_one(x: i32) -> i32 {
    x + 1
}
