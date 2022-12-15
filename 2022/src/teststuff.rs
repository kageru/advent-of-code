#[macro_export]
macro_rules! boilerplate {
    (
        TEST_INPUT == $ti: literal,
        tests: {
            $($part: ident: { $($tpi: expr $(,$ati: expr)* => $to: expr),+$(,)? }),*$(,)?
        },
        $(unittests: {
            $($unittest: ident: { $($($utpi: expr),+ => $uto: expr),+$(,)? }),*$(,)?
        },)?
        bench1$(($bi1: literal))? == $b1: literal,
        bench2 == $b2: literal,
        bench_parse: $input_fn: expr => $it: expr$(,)?
    ) => {
    fn main() {
        let raw_input = read_file(DAY);
        let input = parse_input(&raw_input);
        println!("Part 1: {}", part1(&input$(,$bi1)?));
        println!("Part 2: {}", part2(&input));
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use aoc2022::*;

        const TEST_INPUT: &str = $ti;

        $($($(paste::paste! {
            #[test]
            fn [<$unittest _test_ $uto:lower>]() {
                assert_eq!($unittest($($utpi),+), $uto);
            }
        })+)*)?
        $($(paste::paste! {
            #[test]
            fn [<$part _test_ $to:lower>]() {
                let input = parse_input($tpi);
                assert_eq!($part(&input, $($ati),*), $to);
            }
        })+)*
        bench!(part1($($bi1)?) == $b1);
        bench!(part2() == $b2);
        #[bench]
        fn bench_input_parsing(b: &mut test::Bencher) {
            let raw = &read_file(DAY);
            b.iter(|| assert_eq!($input_fn(&parse_input(test::black_box(&raw))), $it));
        }
    }
    }
}

#[macro_export]
macro_rules! bench {
    ($part: ident($($bi: literal)?) == $expected:expr) => {
        paste::paste! {
            #[bench]
            fn [<$part _bench>](b: &mut test::Bencher) {
                let raw = &read_file(DAY);
                let input = parse_input(&raw);
                b.iter(|| assert_eq!($part(test::black_box(&input)$(, $bi)?), $expected));
            }
        }
    };
}
