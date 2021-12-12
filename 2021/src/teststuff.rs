#[macro_export]
macro_rules! bench {
    ($part: ident $(<$gen: literal>)? ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[bench]
            fn [<$part $($gen)? _bench>](b: &mut test::Bencher) {
                let raw = &read_file(DAY);
                let input = parse_input(&raw);
                b.iter(|| assert_eq!($part $(::<$gen>)? (test::black_box(&input)$(, $param)*), $expected));
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
    ($part: ident $(<$gen: literal>)?  ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<$part $($gen)? _test>]() {
                let input = parse_input(TEST_INPUT);
                assert_eq!($part $(::<$gen>)? (&input$(, $param)*), $expected);
            }
        }
    };
    (with $input: ident: $part: ident $(<$gen: literal>)?  ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<$part $($gen)? _$input:lower _test>]() {
                let input = parse_input([<TEST_INPUT $input>]);
                assert_eq!($part $(::<$gen>)? (&input$(, $param)*), $expected);
            }
        }
    };
}
