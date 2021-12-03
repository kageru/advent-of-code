#[macro_export]
macro_rules! bench {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[bench]
            fn [<$part _bench>](b: &mut test::Bencher) {
                let raw = &read_file(DAY);
                let input = parse_input(&raw);
                b.iter(|| assert_eq!($part(test::black_box(&input)$(, $param)*), $expected));
            }
        }
    };
}

#[macro_export]
macro_rules! bench_input {
    ($fn:expr => $expected:expr) => {
        #[bench]
        fn bench_input_parsing(b: &mut test::Bencher) {
            let raw = &read_file(DAY);
            b.iter(|| assert_eq!($fn(&parse_input(test::black_box(&raw))), $expected));
        }
    };
}

#[macro_export]
macro_rules! test {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<$part _test>]() {
                let input = parse_input(TEST_INPUT);
                assert_eq!($part(&input$(, $param)*), $expected);
            }
        }
    };
}
