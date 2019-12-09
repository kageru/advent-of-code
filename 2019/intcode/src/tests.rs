use super::*;

#[allow(dead_code)]
fn parse_test_input(raw: &str) -> Vec<i64> {
    raw.split(",").map(|x| x.parse().unwrap()).collect()
}

#[test]
fn test_find_max() {
    assert_eq!(
        find_max(
            0..5,
            &parse_test_input("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
        ),
        Some(43210)
    );
    assert_eq!(
        find_max(
            0..5,
            &parse_test_input(
                "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
            )
        ),
        Some(54321)
    );
    assert_eq!(
        find_max(
            0..5,
            &parse_test_input("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
        ),
        Some(65210)
    );
}

#[test]
fn test_find_max_with_loops() {
    assert_eq!(
        find_max(
            5..10,
            &parse_test_input("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
        ),
        Some(139629729)
    );
    /*
    assert_eq!(
        find_max(
            5..10,
            &parse_test_input("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
        ),
        Some(18216)
    );
    */
}

#[test]
fn test_position_less_than() {
    assert_eq!(
        run_for_input(
            &parse_test_input("3,9,8,9,10,9,4,9,99,-1,8"),
            &mut 0,
            vec![8]
        ),
        1
    );
    assert_eq!(
        run_for_input(
            &parse_test_input("3,9,8,9,10,9,4,9,99,-1,8"),
            &mut 0,
            vec![7]
        ),
        0
    );
}

#[test]
fn test_position_equals() {
    assert_eq!(
        run_for_input(
            &parse_test_input("3,9,7,9,10,9,4,9,99,-1,8"),
            &mut 0,
            vec![8]
        ),
        0
    );
    assert_eq!(
        run_for_input(
            &parse_test_input("3,9,7,9,10,9,4,9,99,-1,8"),
            &mut 0,
            vec![7]
        ),
        1
    );
}

#[test]
fn test_immediate_less_than() {
    assert_eq!(
        run_for_input(
            &parse_test_input("3,3,1107,-1,8,3,4,3,99"),
            &mut 0,
            vec![8]
        ),
        0
    );
    assert_eq!(
        run_for_input(
            &parse_test_input("3,3,1107,-1,8,3,4,3,99"),
            &mut 0,
            vec![7]
        ),
        1
    );
}

#[test]
fn test_immediate_equals() {
    assert_eq!(
        run_for_input(
            &parse_test_input("3,3,1108,-1,8,3,4,3,99"),
            &mut 0,
            vec![8]
        ),
        1
    );
    assert_eq!(
        run_for_input(
            &parse_test_input("3,3,1108,-1,8,3,4,3,99"),
            &mut 0,
            vec![7]
        ),
        0
    );
}

#[test]
fn test_quine() {
    assert_eq!(
        run_for_input(
            &parse_test_input("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"),
            &mut 0,
            vec![0]
        ),
        99
    );
    //"109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
}

#[test]
fn test_large_output() {
    assert_eq!(
        run_for_input(
            &parse_test_input("1102,34915192,34915192,7,4,7,99,0"),
            &mut 0,
            vec![0]
        ),
        1219070632396864
    );
    assert_eq!(
        run_for_input(
            &parse_test_input("104,1125899906842624,99"),
            &mut 0,
            vec![0]
        ),
        1125899906842624
    );
}

#[test]
fn test_gattix() {
    assert_eq!(
        run_for_input(
            &parse_test_input("109,5,1,2,1,0,203,-4,2,0,1,20,204,15,99"),
            &mut 0,
            vec![2]
        ),
        12
    );
    assert_eq!(
        run_for_input(
            &parse_test_input("109,5,1,2,1,0,203,-4,2,0,1,20,204,15,99"),
            &mut 0,
            vec![7]
        ),
        42
    );
}
