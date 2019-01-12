pub fn addi(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("addi");
    let mut regs = registers.clone();
    regs[address] = regs[first] + second;
    return regs;
}

pub fn addr(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("addr");
    let mut regs = registers.clone();
    regs[address] = regs[first] + regs[second];
    return regs;
}

pub fn muli(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("muli");
    let mut regs = registers.clone();
    regs[address] = regs[first] * second;
    return regs;
}

pub fn mulr(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("rst:");
    let mut regs = registers.clone();
    regs[address] = regs[first] * regs[second];
    return regs;
}

pub fn bori(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("bori");
    let mut regs = registers.clone();
    regs[address] = regs[first] | second;
    return regs;
}

pub fn borr(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("borr");
    let mut regs = registers.clone();
    regs[address] = regs[first] | regs[second];
    return regs;
}

pub fn seti(first: usize, _second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("seti");
    let mut regs = registers.clone();
    regs[address] =  first;
    return regs;
}

pub fn setr(first: usize, _second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("setr");
    let mut regs = registers.clone();
    regs[address] = regs[first];
    return regs;
}

pub fn gtir(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("gtir");
    let mut regs = registers.clone();
    regs[address] = if first > regs[second] {1} else {0};
    return regs;
}

pub fn gtri(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("gtri");
    let mut regs = registers.clone();
    regs[address] = if regs[first] > second {1} else {0};
    return regs;
}

pub fn gtrr(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("gtrr");
    let mut regs = registers.clone();
    regs[address] = if regs[first] > regs[second] {1} else {0};
    return regs;
}

pub fn eqir(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("eqir");
    let mut regs = registers.clone();
    regs[address] = if first == regs[second] {1} else {0};
    return regs;
}

pub fn eqri(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("eqri");
    let mut regs = registers.clone();
    regs[address] = if regs[first] == second {1} else {0};
    return regs;
}

pub fn eqrr(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("eqrr");
    let mut regs = registers.clone();
    regs[address] = if regs[first] == regs[second] {1} else {0};
    return regs;
}

pub fn bani(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("bani");
    let mut regs = registers.clone();
    regs[address] = regs[first] & second;
    return regs;
}

pub fn banr(first: usize, second: usize, address: usize, registers: &[usize; 4]) -> [usize; 4] {
    println!("banr");
    let mut regs = registers.clone();
    regs[address] = first & regs[second];
    return regs;
}


