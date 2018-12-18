fn addi(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] + second;
    return regs;
}

fn addr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] + regs[second];
    return regs;
}

fn muli(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] * second;
    return regs;
}

fn mulr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] * regs[second];
    return regs;
}

fn bori(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] | second;
    return regs;
}

fn borr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] | regs[second];
    return regs;
}

fn seti(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] =  first;
    return regs;
}

fn setr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first];
    return regs;
}

fn gtir(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = if first > regs[second] {1} else {0};
    return regs;
}

fn gtri(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = if regs[first] > second {1} else {0};
    return regs;
}

fn gtrr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = if regs[first] > regs[second] {1} else {0};
    return regs;
}

fn eqir(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = if first == regs[second] {1} else {0};
    return regs;
}

fn eqri(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = if regs[first] == second {1} else {0};
    return regs;
}

fn eqrr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = if regs[first] == regs[second] {1} else {0};
    return regs;
}

fn bani(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = regs[first] & second;
    return regs;
}

fn banr(first: usize, second: usize, address: usize, registers: &Vec<usize>) -> Vec<usize> {
    let mut regs = registers.clone();
    regs[address] = first & regs[second];
    return regs;
}

fn main() {
    let functions = [addi, addr, muli, mulr, seti, setr, bani, banr, bori, borr, gtir, gtri, gtrr, eqir, eqri, eqrr];
    println!("{}", functions.len());
    let input = vec![1, 2, 3, 4];
    for func in functions.iter() {
        println!("{}", func(1, 2, 3, &input)[3]);
    }
}
