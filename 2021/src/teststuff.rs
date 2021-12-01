#[macro_export]
macro_rules! bench {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste! {
            #[bench]
            fn [<$part _bench>](b: &mut test::Bencher) {
                    let raw = &read_input();
                let input = parse_input(&raw);
                b.iter(|| assert_eq!($part(black_box(&input)$(, $param)*), $expected));
            }
        }
    };
}

#[macro_export]
macro_rules! bench_input {
    ($fn:ident == $expected_len:expr) => {
        #[bench]
        fn bench_input_parsing(b: &mut test::Bencher) {
            let raw = read_input();
            b.iter(|| assert_eq!(parse_input(black_box(&raw)).$fn(), $expected_len));
        }
    };
}

#[macro_export]
macro_rules! test {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste! {
            #[test]
            fn [<$part _test>]() {
                let input = parse_input(TEST_INPUT);
                assert_eq!($part(&input$(, $param)*), $expected);
            }
        }
    };
}
