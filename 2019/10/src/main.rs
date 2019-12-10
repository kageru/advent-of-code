fn main() {
    println!("Hello, world!");
}

fn calculate_angle(x_offset: i32, y_offset: i32) -> f32 {
    return ((y_offset as f32).atan2(x_offset as f32).to_degrees() * 1000.0).round();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_angle_calculation() {
        assert_eq!(calculate_angle(1, 1), 45_000.0);
        assert_eq!(calculate_angle(1, 0), 0.0);
        assert_eq!(calculate_angle(0, -1), -90_000.0);
        assert_eq!(calculate_angle(2, 1), 26_565.0);
        assert_eq!(calculate_angle(1, 1), calculate_angle(3, 3));
        assert_eq!(calculate_angle(-2, -2), -135_000.0);
        assert_eq!(calculate_angle(-2, 2), 135_000.0);
        assert_ne!(calculate_angle(1, 1), calculate_angle(3, -3));
    }
}
