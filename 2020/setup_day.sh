#!/bin/sh

today=$(date +%d)
mkdir "$today"
cd "$today"
cargo init --name "day$today"
echo 'Initialized cargo project'
cargo add itertools

# this assumes that your puzzle input is already in your clipboard
xsel -b > input
# add trailing newline if necessary
sed -i -e '$a\' input

echo 'fn read_input() -> String {
    std::fs::read_to_string("input").unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

}' >> src/main.rs
