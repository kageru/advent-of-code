#[macro_use]
extern crate scan_fmt;

#[derive(Debug, PartialEq, Eq)]
struct Moon {
    x: i32,
    y: i32,
    z: i32,
    x_vel: i32,
    y_vel: i32,
    z_vel: i32,
}

fn main() {
    println!("Hello, world!");
}

fn gcd(mut x: u32, mut y: u32) -> u32 {
    let mut remainder;
    while y != 0 {
        remainder = x % y;
        x = y;
        y = remainder;
    }
    x
}

fn lcm(x: u32, y: u32) -> u32 {
    x * y / gcd(x, y)
}

#[rustfmt::skip]
fn parse(line: &str) -> Moon {
    let (x, y, z) = scan_fmt!(line, "<x={}, y={}, z={}>", i32, i32, i32).unwrap();
    Moon { x, y, z, x_vel: 0, y_vel: 0, z_vel: 0, }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gcd() {
        assert_eq!(gcd(20, 10), 10);
        assert_eq!(gcd(20, 15), 5);
        assert_eq!(gcd(15, 20), 5);
    }

    #[test]
    fn test_lcm() {
        assert_eq!(lcm(20, 10), 20);
        assert_eq!(lcm(3, 7), 21);
        assert_eq!(lcm(7, 3), 21);
        assert_eq!(lcm(lcm(7, 3), 5), 105);
    }

    #[test]
    #[rustfmt::skip]
    fn test_parse() {
        assert_eq!(parse("<x=-14, y=9, z=-4>"), Moon { x: -14, y: 9, z: -4, x_vel: 0, y_vel: 0, z_vel: 0 });
        assert_ne!(parse("<x=-14, y=9, z=-4>"), Moon { x: 14, y: 9, z: -4, x_vel: 0, y_vel: 0, z_vel: 0 });
    }
}
