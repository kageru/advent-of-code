fn addi(first: i32, second: i32) -> i32 {
    return first + second;
}

fn main() {
    println!("Hello, world!");
    let a: fn(i32, i32) -> i32 = addi;
    println!("{}", a(1, 2));
}
