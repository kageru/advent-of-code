#![feature(drain_filter)]
#![feature(array_windows)]
#![feature(test)]
extern crate test;
use itertools::{iproduct, Itertools};
use std::{collections::HashSet, ops::Range};

type Parsed = Vec<(Cube, bool)>;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
struct Cube {
    x: Range<isize>,
    y: Range<isize>,
    z: Range<isize>,
}

impl Cube {
    fn contains(&self, other: &Cube) -> bool {
        self.x.start <= other.x.start
            && self.x.end >= other.x.end
            && self.y.start <= other.y.start
            && self.y.end >= other.y.end
            && self.z.start <= other.z.start
            && self.z.end >= other.z.end
    }

    fn intersects(&self, Cube { x: x2, y: y2, z: z2 }: &Cube) -> bool {
        let Cube { x: x1, y: y1, z: z1 } = self;
        !((x1.end < x2.start || x2.end < x1.start)
            && (y1.end < y2.start || y2.end < y1.start)
            && (z1.end < z2.start || z2.end < z1.start))
    }

    fn split(self, other: Cube) -> Vec<Cube> {
        if !self.intersects(&other) {
            return vec![self, other];
        }
        let Cube { x: x1, y: y1, z: z1 } = self;
        let Cube { x: x2, y: y2, z: z2 } = other;
        let mut xs = vec![x1.start, x1.end, x2.start, x2.end];
        xs.sort_unstable();
        let mut ys = vec![y1.start, y1.end, y2.start, y2.end];
        ys.sort_unstable();
        let mut zs = vec![z1.start, z1.end, z2.start, z2.end];
        zs.sort_unstable();
        xs.push(xs[xs.len() - 1] + 1);
        ys.push(ys[ys.len() - 1] + 1);
        zs.push(zs[zs.len() - 1] + 1);
        iproduct!(xs.array_windows::<2>(), ys.array_windows::<2>(), zs.array_windows::<2>())
            // .inspect(|x| println!("{x:?}"))
            .map(|(&[x1, x2], &[y1, y2], &[z1, z2])| Cube { x: x1..x2 - 1, y: y1..y2 - 1, z: z1..z2 - 1 })
            .filter(|Cube { x, y, z }| x.start <= x.end && y.start <= y.end && z.start <= z.end)
            .collect()
    }

    fn len(&self) -> usize {
        (self.x.len() + 1) * (self.y.len() + 1) * (self.z.len() + 1)
    }

    fn try_combine(self, Cube { x: x2, y: y2, z: z2 }: Cube) -> Option<Cube> {
        // println!("Trying to combine {self:?} with ({x2:?}, {y2:?}, {z2:?})");
        let Cube { x: x1, y: y1, z: z1 } = self;
        if x1 == x2 && y1 == y2 && z1 == z2 {
            return Some(Cube { x: x1, y: y1, z: z1 });
        }
        if x1 == x2 && y1 == y2 && (z1.end == z2.start || z2.end == z1.start) {
            return Some(Cube { x: x1, y: y1, z: z1.start.min(z2.start)..z1.end.max(z2.end) });
        }
        if x1 == x2 && z1 == z2 && (y1.end == y2.start || y2.end == y1.start) {
            return Some(Cube { x: x1, y: y1.start.min(y2.start)..y1.end.max(y2.end), z: z1 });
        }
        if z1 == z2 && y1 == y2 && (x1.end == x2.start || x2.end == x1.start) {
            return Some(Cube { x: x1.start.min(x2.start)..x1.end.max(x2.end), y: y1, z: z1 });
        }
        // println!("Did not combine");
        None
    }
}

#[allow(non_upper_case_globals)]
const on: bool = true;
#[allow(non_upper_case_globals)]
const off: bool = false;

macro_rules! parse {
    ($($b: ident x=$x: expr,y=$y: expr,z=$z: expr)*) => {
        [$( (Cube { x: $x, y: $y, z: $z, }, $b), )*].to_vec()
    };
}

fn part1(parsed: &Parsed) -> usize {
    let mut cubes: Vec<Cube> = vec![];
    for (new_cube, state) in parsed {
        if new_cube.x.start.abs() > 50 {
            break;
        }
        let new_intersections: HashSet<_> = cubes
            .drain_filter(|c| c.intersects(&new_cube))
            .flat_map(|cube| {
                let mut cube_copy = new_cube.clone();
                // println!("new: {new_cube:?}");
                /*
                if !state {
                    cube_copy.x.end += 1;
                    cube_copy.y.end += 1;
                    cube_copy.z.end += 1;
                }*/
                let mut cs = cube.clone().split(cube_copy);
                if *state {
                    cs.drain_filter(|c| !cube.contains(c) && !new_cube.contains(c));
                } else {
                    cs.retain(|c| cube.contains(c) && !new_cube.contains(c));
                    // cs.drain_filter(|c| !(cube.contains(c) && !new_cube.contains(c)))
                        // .for_each(|c| println!("Turning off {c:?} because it’s in {new_cube:?} or not in {cube:?}"));
                }
                cs
            })
            .collect();
        if new_intersections.is_empty() && *state {
            cubes.push(new_cube.clone());
        } else {
            for e in new_intersections {
                cubes.push(e);
            }
        }
        let con: usize = cubes
            .iter()
            // .inspect(|c| println!("{c:?}")) //
            .map(Cube::len)
            .sum();
        println!("{con}");
        /*
        let v = cubes
            .iter()
            .flat_map(|Cube { x, y, z }| iproduct!(x.start..=x.end, y.start..=y.end, z.start..=z.end))
            .sorted_unstable_by_key(|(x, y, z)| x * 10000 + y * 100 + z)
            .collect_vec();
        // println!("{v:?}");
        */
    }
    cubes.iter().map(Cube::len).sum()
    // cubes.into_iter().inspect(|c| println!("{c:?}")).flat_map(Cube::points).collect::<HashSet<_>>().len()
}

fn part2(parsed: &Parsed) -> usize {
    todo!()
}

fn main() {
    let input = parse_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    #[test]
    fn part1_test() {
        let input = parse! {
            on x=10..12,y=10..12,z=10..12
            on x=11..13,y=11..13,z=11..13
            off x=9..11,y=9..11,z=9..11
            on x=10..10,y=10..10,z=10..10
        };
        assert_eq!(part1(&input), 39);
    }

    #[test]
    fn contains_top_right_test() {
        let c = Cube { x: 9..11, y: 9..11, z: 9..11 };
        let tr = Cube { x: 11..11, y: 11..11, z: 11..11 };
        assert!(c.contains(&tr));
        assert!(c.intersects(&tr));
    }
}

fn parse_input() -> Parsed {
    parse! {
        on x=-13..37,y=-36..14,z=-3..45
        on x=-1..43,y=-15..32,z=-35..15
        on x=-7..44,y=-6..48,z=-13..38
        on x=-42..3,y=-42..10,z=-3..44
        on x=-34..13,y=-27..27,z=-31..23
        on x=-14..39,y=-34..10,z=-40..10
        on x=-12..34,y=-25..19,z=-32..15
        on x=-47..6,y=-29..20,z=-45..9
        on x=-37..16,y=-17..34,z=-25..29
        on x=-28..20,y=-19..34,z=-3..47
        off x=1..19,y=-30..-14,z=19..33
        on x=-34..19,y=-22..25,z=-17..35
        off x=-49..-32,y=-26..-13,z=19..38
        on x=-28..25,y=-46..2,z=-11..43
        off x=-30..-21,y=-44..-26,z=6..21
        on x=-20..31,y=-39..11,z=-23..28
        off x=33..49,y=29..47,z=8..25
        on x=-28..25,y=-26..20,z=-49..-1
        off x=-19..-5,y=3..18,z=-44..-27
        on x=-34..15,y=-29..25,z=-34..17
        on x=-77707..-66888,y=-15495..7977,z=21658..44356
        on x=16734..33844,y=-90735..-54900,z=2270..19264
        on x=37676..47017,y=-53726..-31603,z=-58485..-51594
        on x=-25500..-16563,y=-6468..2463,z=69232..89880
        on x=-35872..-12957,y=-84461..-60975,z=-20007..-9532
        on x=-89647..-67921,y=-39059..-13640,z=-42281..-6574
        on x=47838..82298,y=-43677..-35749,z=9027..31549
        on x=-9400..11474,y=-39127..-21505,z=57808..89970
        on x=61666..80803,y=10083..24758,z=37048..46860
        on x=26068..46741,y=-77898..-58561,z=-30238..62
        on x=-22689..-13065,y=-81029..-64591,z=-21055..3749
        on x=38030..60209,y=-34391..-15028,z=-55983..-51628
        on x=42982..62845,y=-54347..-47689,z=15630..45725
        on x=-18872..11038,y=-76016..-52462,z=-42387..-37005
        on x=-55695..-38898,y=36001..55982,z=-70788..-36425
        on x=-7131..26336,y=42833..63923,z=-64129..-45423
        on x=-60488..-40325,y=-47405..-31877,z=48181..58567
        on x=-13302..15801,y=73748..92138,z=-34147..-17494
        on x=31153..53512,y=41173..61390,z=38591..49656
        on x=-35709..-8667,y=-34876..536,z=-79524..-59100
        on x=45731..73067,y=50149..54110,z=-34129..-20399
        on x=2923..23025,y=53277..82004,z=30350..45240
        on x=-34895..-14927,y=27451..50766,z=-67502..-59363
        on x=-8366..10206,y=22907..37139,z=70976..78784
        on x=26000..31619,y=55905..81695,z=28039..45741
        on x=-27099..342,y=70224..85805,z=-6497..11011
        on x=-1601..9361,y=-40465..-9946,z=64120..76194
        on x=-29110..-17895,y=38967..50663,z=56746..78592
        on x=-77526..-50930,y=-60463..-42672,z=-16932..667
        on x=-19801..1121,y=-95369..-68521,z=-2863..7840
        on x=-56034..-34754,y=-4651..19561,z=61780..87310
        on x=50357..81377,y=-54480..-35226,z=-13214..22298
        on x=-37854..-29487,y=-24307..-9466,z=-70318..-65828
        on x=-87626..-68588,y=-1140..28963,z=23659..43617
        on x=10140..45670,y=19441..48501,z=52468..67612
        on x=-61161..-46395,y=50168..60300,z=-43642..-22240
        on x=-41791..-23146,y=59820..92015,z=-30219..-13117
        on x=51416..66401,y=21581..26160,z=36042..66562
        on x=43985..71278,y=-76853..-47487,z=10319..33057
        on x=16554..44446,y=-83467..-49432,z=27702..46302
        on x=-40078..-32697,y=-19712..13098,z=-79179..-70318
        on x=-72544..-48748,y=-24529..-10822,z=25757..62217
        on x=-49848..-23893,y=49735..70928,z=-17843..4166
        on x=46504..62954,y=-63451..-39706,z=-7060..12582
        on x=-18091..5639,y=-80367..-60849,z=20700..28381
        on x=-70298..-58521,y=-36254..-16840,z=39462..58760
        on x=-57895..-36812,y=-80262..-60063,z=17594..35827
        on x=66005..74563,y=-40697..-10634,z=-28571..-12651
        on x=-34740..-12979,y=14459..27693,z=72315..83610
        on x=-54526..-27860,y=20919..36223,z=53916..69317
        on x=5482..28204,y=-72328..-47181,z=36101..68864
        on x=-10361..18618,y=50680..80016,z=29881..52293
        on x=22013..53090,y=61184..77888,z=-690..15445
        on x=49878..80860,y=-45597..-24600,z=8773..42131
        on x=-21481..-2534,y=-52590..-35874,z=59304..71419
        on x=-27491..-2116,y=-57213..-37184,z=35989..58007
        on x=-27046..-11339,y=-93198..-69701,z=2894..23463
        on x=-49531..-33827,y=-71407..-54183,z=28280..48597
        on x=2360..31991,y=25158..56562,z=-80620..-60755
        on x=-26128..-998,y=-62930..-52635,z=35279..63815
        on x=48650..66941,y=-61055..-28617,z=29166..31637
        on x=69844..85996,y=-22985..-2275,z=21553..38166
        on x=2845..20505,y=49897..61909,z=53069..67906
        on x=-28415..-3203,y=57533..73503,z=21661..31210
        on x=8443..20885,y=34174..61857,z=57794..68245
        on x=12152..42225,y=17448..37654,z=50480..71531
        on x=57295..73077,y=-41599..-9861,z=-67808..-36322
        on x=-39269..-26154,y=-14375..14800,z=71243..78784
        on x=10757..31258,y=-62179..-38241,z=-79093..-48669
        on x=-30816..-13772,y=31105..43599,z=56286..80856
        on x=-81989..-50706,y=-30815..-14580,z=18775..44958
        on x=40071..59419,y=44624..61873,z=29016..50450
        on x=40251..59324,y=-67414..-49803,z=-36761..-11231
        on x=27417..46406,y=-66475..-47513,z=-25177..-11900
        on x=-65899..-38890,y=-12430..5965,z=57577..63876
        on x=-25267..-2876,y=-44000..-32310,z=-80593..-53949
        on x=-18326..-7876,y=43132..66145,z=-66613..-36605
        on x=-57018..-53229,y=36751..59783,z=-30079..-9617
        on x=-66290..-47772,y=-31894..-1002,z=33540..57723
        on x=-125..24382,y=-80848..-72343,z=-49693..-15529
        on x=-24015..-2898,y=-53625..-39125,z=55225..73628
        on x=51897..80923,y=-4054..3520,z=-45412..-36746
        on x=-30093..-1431,y=34246..47381,z=-75270..-59132
        on x=31766..51586,y=48710..67006,z=39768..49335
        on x=-20923..6424,y=1580..14928,z=-89071..-61335
        on x=-78388..-68973,y=2726..15396,z=31839..51487
        on x=-17101..1916,y=-31904..-18696,z=62499..86196
        on x=60981..74827,y=-33558..-6318,z=17172..47217
        on x=-3097..3774,y=74123..95012,z=-15851..19259
        on x=-29901..-26536,y=-5670..9081,z=62581..77681
        on x=-23676..-6641,y=-87054..-60450,z=6151..27241
        on x=-75546..-60657,y=-46787..-18586,z=6164..28899
        on x=-62447..-45293,y=-55081..-38324,z=-45488..-36816
        on x=-35053..-11420,y=-78994..-73796,z=-27546..-2495
        on x=14791..35832,y=8044..17682,z=-75881..-70257
        on x=37114..54311,y=-62857..-40402,z=42866..62581
        on x=-78891..-57261,y=-19933..2380,z=-46848..-34329
        on x=-40885..-31987,y=56185..85808,z=-12714..14124
        on x=-22584..-1227,y=62080..69853,z=31132..43774
        on x=21369..37041,y=-29949..1877,z=-84346..-70161
        on x=-32994..-11854,y=-23931..-13655,z=68474..75824
        on x=28950..44900,y=37331..65913,z=-53762..-21514
        on x=14251..32416,y=-59808..-39089,z=-69466..-54177
        on x=-1001..11349,y=-91855..-70109,z=-20491..-5599
        on x=-55376..-22523,y=26566..45387,z=53177..76643
        on x=5773..21346,y=-24068..-6968,z=63947..88583
        on x=-61909..-45698,y=56099..78327,z=-6552..20928
        on x=-91524..-56986,y=-1853..11970,z=13238..33401
        on x=65923..82086,y=-3320..28990,z=2252..31777
        on x=-49874..-21418,y=3494..29323,z=-69436..-64723
        on x=73184..89417,y=-42741..-7654,z=2716..14770
        on x=-19543..5754,y=-45523..-35685,z=-69929..-58853
        on x=20033..44221,y=-84229..-57579,z=-8712..5719
        on x=-91462..-67462,y=4054..26833,z=-39415..-13802
        on x=27732..50763,y=-58005..-35512,z=-66011..-34072
        on x=27133..51350,y=45426..71070,z=-8192..-4652
        on x=32748..56378,y=-70321..-50194,z=26633..50320
        on x=-79635..-59790,y=-1252..13376,z=19282..37497
        on x=63307..93679,y=7073..20800,z=1161..28560
        on x=-36145..-14630,y=-37537..-21452,z=56070..76078
        on x=30686..50529,y=-67131..-36989,z=35265..66047
        on x=-15752..-11751,y=59850..88935,z=18731..42395
        on x=38283..60807,y=51015..57433,z=-40995..-25599
        on x=-28800..-5978,y=73498..82844,z=-33104..-12502
        on x=38063..67439,y=-35709..-20195,z=-53965..-32238
        on x=-68345..-50452,y=8456..26579,z=30921..48065
        on x=-32241..-19903,y=41696..65189,z=51000..65538
        on x=36262..42472,y=-64295..-31497,z=-60784..-48215
        on x=-19510..-110,y=31699..54917,z=-67629..-53476
        on x=-62256..-40043,y=-35060..-34579,z=-70111..-50680
        on x=-39707..-23353,y=-19810..6512,z=-87108..-53181
        on x=-82176..-51976,y=-50348..-40880,z=-33910..-14246
        on x=-83924..-70902,y=-19175..2985,z=-15384..15455
        on x=49316..72497,y=425..6060,z=52206..70825
        on x=-83699..-57057,y=-27150..-4795,z=22540..29041
        on x=-3101..12001,y=68638..91778,z=-40657..-29719
        on x=2252..24442,y=34848..55848,z=-75998..-48307
        on x=-5069..16361,y=-20661..-2439,z=65914..86332
        on x=-57402..-38222,y=-69446..-40489,z=-54314..-29224
        on x=-13242..408,y=68201..84940,z=-45871..-38004
        on x=56749..63913,y=-8638..3672,z=47720..62702
        on x=54011..76285,y=-62270..-39823,z=-22993..-14872
        on x=58382..77062,y=-20697..-7882,z=46625..52172
        on x=-47219..-11653,y=26546..45116,z=-73892..-62298
        on x=52120..73926,y=32675..54357,z=-45806..-32181
        on x=-11456..-3541,y=-68839..-41789,z=45637..76793
        on x=-45992..-25163,y=70590..94451,z=-3611..27080
        on x=-77343..-54249,y=25725..46466,z=-7443..1553
        on x=53624..60939,y=16322..19384,z=-60689..-54993
        on x=-62023..-51180,y=-11378..-249,z=-60785..-36599
        on x=51752..62523,y=-55025..-48761,z=-35067..-13224
        on x=-79436..-62124,y=17736..25062,z=-33415..-15642
        on x=58309..68890,y=-53920..-35770,z=-5858..12657
        on x=-70319..-60470,y=13686..43339,z=-44612..-11192
        on x=-14271..3865,y=-5940..16892,z=-89272..-77708
        on x=56603..74277,y=-30330..-17500,z=-65513..-34252
        on x=70395..81599,y=-8116..-4807,z=-47231..-23373
        on x=26409..53159,y=37900..58813,z=-58362..-44114
        on x=-6833..22656,y=49013..66204,z=-64722..-42138
        on x=-79939..-47600,y=-54134..-42160,z=7100..29266
        on x=-12387..1516,y=-93270..-73717,z=21246..31778
        on x=-68856..-43032,y=-13298..19069,z=45157..61355
        on x=-52372..-35450,y=7419..16683,z=-86081..-55547
        on x=24953..45687,y=45007..63906,z=14094..39220
        on x=-55616..-27396,y=62300..72738,z=-2200..9190
        on x=50053..83913,y=-11119..-1995,z=-60574..-21845
        on x=-34214..-11133,y=-11996..-4356,z=72157..82626
        on x=-81300..-54391,y=-17887..-200,z=31179..50660
        on x=-16117..18870,y=61218..84345,z=33172..58266
        on x=-94022..-70920,y=422..21148,z=-40110..-4601
        on x=7997..32024,y=-93325..-76394,z=-1522..5982
        on x=-9500..16520,y=23350..46041,z=-88268..-58078
        on x=-23747..-4542,y=-58091..-41118,z=62263..73989
        on x=-17732..-2905,y=-95543..-64883,z=-1683..25695
        on x=75498..83226,y=-6258..14464,z=2112..14733
        on x=20265..41561,y=11946..41840,z=50936..79878
        on x=53047..82535,y=-14468..1393,z=-43142..-21193
        on x=27130..52544,y=57145..81711,z=-35213..-9752
        on x=-45338..-15941,y=-19607..110,z=67448..89256
        on x=-59996..-26746,y=2539..21005,z=53468..67082
        on x=3402..28703,y=-93591..-66413,z=8528..10542
        on x=-49409..-35285,y=32697..52202,z=48515..71687
        on x=49025..53870,y=-9075..16495,z=-66082..-46321
        on x=30601..51564,y=-69996..-47533,z=-25031..2209
        on x=5635..35015,y=53724..79653,z=-26973..-16059
        on x=55318..86926,y=-23333..-9355,z=7047..44226
        on x=-30289..-12285,y=-68389..-57981,z=-48161..-40552
        on x=58882..92805,y=23648..37563,z=5964..22967
        on x=34496..42738,y=-72380..-48483,z=-34740..-955
        on x=20317..32221,y=20101..38130,z=-86821..-68574
        on x=-34120..-25879,y=-55894..-44842,z=-55903..-40976
        on x=-45645..-28331,y=-74951..-59701,z=30291..41140
        on x=-18717..-645,y=-49850..-25005,z=-85064..-56705
        on x=-66069..-62114,y=-7151..16958,z=31325..61285
        on x=-8768..2875,y=70141..79960,z=-16283..162
        on x=48671..75660,y=34126..57253,z=25955..40686
        on x=1592..40495,y=17266..42218,z=53722..79079
        on x=-19745..6994,y=-93790..-75591,z=-28647..9949
        on x=-33893..-12969,y=-90507..-60780,z=-42644..-10587
        on x=60910..79183,y=-49087..-26806,z=-209..8359
        on x=-35209..-832,y=18733..37433,z=-77554..-53413
        off x=-24707..-6441,y=-41376..-10856,z=55331..90949
        off x=6474..32653,y=-14454..17574,z=-88453..-71268
        on x=-13493..-6403,y=47314..66556,z=-54874..-29147
        on x=-9291..12608,y=53741..85000,z=-46306..-17017
        off x=13983..32183,y=38353..59846,z=44630..60489
        on x=-20250..12042,y=6499..35623,z=-81728..-71834
        off x=-12034..26843,y=-13773..3,z=-84021..-75465
        on x=-26797..-16552,y=-49960..-32197,z=-65821..-61668
        on x=8368..23515,y=21417..44347,z=55621..84595
        off x=49540..69051,y=31705..39693,z=-42453..-14061
        on x=-69780..-50337,y=-64895..-31351,z=-1140..22240
        on x=70874..82029,y=-4927..29616,z=-17097..7597
        off x=-35974..-21133,y=-17861..70,z=66698..84031
        off x=-47342..-11961,y=-672..21189,z=-74692..-65282
        off x=24383..49319,y=48619..83833,z=15987..31130
        off x=4559..23214,y=61581..86675,z=11178..41759
        on x=61861..73539,y=-60405..-42686,z=5953..23802
        off x=-59929..-28960,y=-56097..-28974,z=-52538..-46415
        off x=-72615..-55995,y=-36832..-13278,z=19227..40885
        on x=-12406..8284,y=-92734..-74910,z=-35203..-11978
        off x=11249..35967,y=28322..57776,z=56548..84816
        off x=44731..62241,y=35624..44662,z=-66511..-34520
        off x=-87104..-74134,y=-13992..12953,z=-3294..10178
        on x=-28985..-3937,y=-56788..-26115,z=63095..78456
        off x=22993..41149,y=-72025..-38641,z=53155..65281
        off x=-70263..-64986,y=10277..24698,z=17534..54469
        on x=33137..54068,y=49394..60447,z=14589..36485
        off x=1761..5646,y=-30207..-23218,z=67948..86068
        off x=23532..51240,y=-68091..-30165,z=-66152..-47659
        on x=63492..78630,y=26451..49621,z=-16485..2633
        on x=61364..70239,y=-44857..-25262,z=-8792..15341
        off x=-44240..-24659,y=40477..65486,z=46273..50810
        on x=-82196..-65068,y=22741..26991,z=21666..26831
        on x=19512..39779,y=51290..71942,z=33521..52593
        off x=52818..66473,y=38215..68935,z=11423..29773
        off x=19475..48035,y=50117..69010,z=-49063..-24987
        on x=-14143..9865,y=-10817..8431,z=65536..80561
        off x=-72733..-48163,y=-25291..-9724,z=-66757..-36479
        off x=-43106..-20567,y=58626..65043,z=30453..56787
        off x=13154..16214,y=5656..13752,z=66016..80418
        off x=-11408..18735,y=39272..59415,z=60254..73108
        on x=-77078..-61965,y=-17101..-10110,z=44955..62934
        off x=40661..68779,y=45899..59382,z=-21792..-15201
        off x=53315..78287,y=-31686..-15404,z=-41990..-23080
        off x=-18699..12677,y=-34848..-11433,z=63732..95545
        off x=-24502..-17077,y=-91563..-71124,z=15320..22882
        on x=24300..44953,y=-8117..20940,z=-85817..-64192
        off x=-57159..-39199,y=-29219..-23341,z=-68301..-54847
        on x=34394..67095,y=-53551..-28864,z=43253..61926
        off x=-50069..-30721,y=-14112..20450,z=-84644..-54725
        on x=23629..36100,y=59625..71637,z=-42808..-14597
        on x=-48311..-40298,y=-52473..-36850,z=-55879..-29005
        on x=-12671..11981,y=-46219..-25335,z=68262..79543
        on x=-4192..11450,y=-33767..-22561,z=62929..78589
        on x=-72223..-46005,y=-407..27380,z=-64061..-33742
        off x=41077..57062,y=-58884..-45405,z=19829..48890
        off x=49487..69913,y=17640..41283,z=44735..66488
        off x=-73528..-49377,y=-49007..-39363,z=9948..35961
        off x=-71643..-56910,y=30988..61528,z=1455..7445
        off x=-37842..-11285,y=64140..77407,z=-1880..26818
        on x=-73578..-43035,y=9872..28002,z=44466..65647
        off x=326..24287,y=68067..99200,z=-19343..11282
        on x=55367..74607,y=22023..39362,z=-34371..-18588
        off x=58603..79981,y=-22871..1116,z=22611..39894
        off x=47930..73685,y=-6214..-2685,z=-60846..-31022
        off x=22820..41562,y=-16265..-4928,z=58506..83229
        off x=-88133..-62301,y=15247..30618,z=-10265..10510
        off x=1253..22476,y=-60567..-30741,z=51569..81597
        off x=63494..75187,y=-55856..-27033,z=-16423..-4091
        off x=52204..70316,y=21707..45143,z=24511..43927
        off x=27774..33872,y=30457..60549,z=52881..74012
        off x=-67751..-48677,y=6535..14109,z=41406..63051
        on x=62817..79049,y=4793..27715,z=-40593..-20781
        on x=21141..51290,y=-74209..-55507,z=15792..30070
        on x=-32225..-15877,y=21103..45553,z=-66267..-46475
        off x=-53085..-35827,y=-36046..-19282,z=55940..62461
        off x=17599..44204,y=13384..25615,z=-76042..-64772
        on x=-23704..-3540,y=-20375..10502,z=59465..97525
        off x=-82359..-68971,y=12420..30737,z=-38435..-11957
        off x=-40061..-9787,y=-1989..10412,z=57295..74944
        on x=-76144..-61483,y=-58373..-37868,z=-43080..-14295
        on x=7696..27949,y=-72779..-56254,z=-54768..-32949
        off x=39003..59670,y=-62963..-47662,z=1974..16893
        off x=29968..35020,y=-22383..-11225,z=61966..80305
        on x=50004..63242,y=-20016..6273,z=44941..69658
        off x=-59279..-55566,y=-61178..-54387,z=-29065..-8057
        off x=-61413..-26740,y=60749..79809,z=-21764..16450
        off x=32196..66439,y=-60264..-26865,z=26941..60013
        on x=-58520..-43376,y=37472..61676,z=23422..43293
        off x=-56396..-25541,y=-68416..-55661,z=-19690..-9207
        on x=18618..44778,y=-85629..-64714,z=-8305..17838
        on x=-61504..-52282,y=-57912..-39990,z=-14780..6801
        on x=11156..42237,y=11867..43976,z=-85896..-64810
        on x=-55007..-28867,y=52919..63309,z=-40553..-23411
        off x=59765..80865,y=35650..56453,z=-27643..-24090
        off x=43182..81228,y=-34129..-7000,z=40063..52672
        on x=-13505..20448,y=64022..91445,z=26743..48275
        on x=27815..43279,y=-70617..-52124,z=24964..51972
        off x=35675..49869,y=-62404..-56365,z=18269..35287
        off x=35300..54594,y=35804..62612,z=-52851..-29341
        off x=-71395..-53018,y=-19495..-4921,z=24089..52405
        on x=-33332..-3755,y=-59570..-40365,z=56013..63939
        on x=42324..60577,y=-49964..-34186,z=-52816..-27743
        off x=48768..67360,y=-61320..-28892,z=-37831..-21312
        on x=16338..40996,y=-53751..-18105,z=48116..68706
        off x=-27168..-9712,y=-84533..-68775,z=4825..41138
        off x=-60047..-36975,y=37870..61975,z=32191..46708
        on x=26376..40244,y=-65020..-54338,z=43732..56693
        off x=-91738..-68157,y=-24748..6980,z=-39819..-19451
        on x=-5317..14596,y=-21035..-10563,z=71696..82823
        off x=49847..64417,y=23298..37336,z=21615..52831
        off x=-52432..-40255,y=-26836..-5279,z=-70876..-60890
        off x=-20102..9857,y=-1721..20913,z=78025..98271
        off x=-4990..6648,y=-21613..4090,z=-86165..-59847
        on x=43038..56116,y=-63896..-49438,z=-26881..-4325
        on x=-4380..2751,y=55174..85405,z=-46983..-21937
        on x=70338..80320,y=-37100..-1120,z=-25918..-14953
        on x=-54364..-27158,y=23057..50821,z=50502..69300
        on x=1520..12529,y=-78320..-56572,z=38211..50552
        off x=49738..66773,y=30561..59431,z=2951..29462
        off x=16271..28523,y=-45270..-31343,z=63465..75526
        on x=71234..85618,y=-41395..-7960,z=-4130..3364
        on x=-69317..-40653,y=-67757..-43558,z=6142..24036
        on x=25022..40901,y=-60207..-43245,z=29925..58586
        off x=-81117..-47868,y=-43751..-16831,z=-35356..-29663
        on x=-1180..17290,y=67627..89970,z=11528..29483
        on x=-61937..-38595,y=-63471..-51265,z=-41226..-7643
        off x=-79920..-67593,y=23081..43335,z=-27955..-4111
        on x=52546..75163,y=49315..56898,z=-14675..7154
        off x=3726..34564,y=-66091..-47518,z=-66664..-37455
        on x=34949..60501,y=-28771..-11144,z=51352..61485
        on x=-12954..14301,y=-93475..-65638,z=-10004..11614
        on x=11713..39311,y=-13965..-4062,z=-81763..-73474
        on x=51282..59807,y=-57951..-35734,z=5994..36901
        off x=-7411..26099,y=32784..55370,z=51744..64477
        off x=28307..41980,y=-60121..-44354,z=-62749..-36419
        off x=67213..84298,y=-29046..-11454,z=21296..44275
        off x=-63074..-36397,y=-44584..-33108,z=-62259..-41864
        on x=-52919..-32901,y=-76109..-58400,z=-18640..-10098
        off x=22269..46465,y=-75997..-51073,z=-46540..-31244
        on x=-13358..1813,y=-48320..-19885,z=65575..90081
        on x=57248..73703,y=50245..59021,z=-23752..-325
        off x=-61356..-40218,y=-63746..-48236,z=-4162..13109
        on x=-86721..-52575,y=18886..39871,z=-27525..-18940
        off x=-32958..-9613,y=54585..89567,z=-25434..-6238
        on x=-34933..-16868,y=35195..53598,z=53014..59010
        on x=-50111..-41812,y=53666..72265,z=-43276..-17297
        off x=-48541..-31980,y=-29629..-9928,z=-72149..-56249
        off x=8404..34890,y=-91946..-64517,z=-36821..-5171
        on x=-61476..-46150,y=34461..64440,z=-52672..-31719
        on x=17053..48276,y=8196..30921,z=-90002..-67999
        off x=30567..51548,y=48492..49629,z=-60064..-40577
        off x=-11485..16121,y=49496..85403,z=-58551..-28464
        off x=-71575..-66788,y=-28618..-14850,z=25881..53946
        on x=-82870..-69951,y=-31733..-21150,z=2448..31565
        off x=21102..31260,y=28479..55875,z=53439..84269
        on x=-92343..-73224,y=-5440..30589,z=-11302..13705
        off x=57846..67138,y=-51876..-19075,z=25743..40789
        on x=-42146..-14334,y=-91427..-56913,z=11466..17841
        on x=-8337..5046,y=-97914..-59588,z=-17297..-3854
        on x=72959..77792,y=-7695..23596,z=16641..36941
        off x=48708..70778,y=-52975..-24049,z=38893..49884
        off x=-48001..-30155,y=-34640..-8100,z=-68127..-58824
        on x=-23589..-642,y=-55673..-20006,z=53073..77106
        on x=-33552..-8377,y=53425..81818,z=-47589..-31657
        on x=-4435..19578,y=50903..85239,z=-52340..-31908
        off x=-65297..-43991,y=45255..59229,z=-39945..-7463
        off x=-27752..-7352,y=56826..62480,z=-59434..-52524
        on x=-52395..-36890,y=-60404..-53667,z=-52971..-34646
        on x=-17036..-10608,y=22073..34956,z=-87647..-59521
        on x=-20406..2585,y=-75526..-51689,z=-53856..-28503
        off x=57297..67915,y=40135..59210,z=-10318..20975
        on x=54413..78003,y=2451..10064,z=24601..41429
        off x=-30323..-21158,y=35905..68915,z=-71573..-39806
        off x=-24500..-12434,y=-76402..-60478,z=-18069..-8858
        off x=60931..77777,y=-41037..-25321,z=-7530..26700
        off x=37607..47278,y=-15018..6458,z=67782..87778
        off x=-32488..-5083,y=26413..40281,z=60169..76864
        off x=7746..18062,y=45927..81504,z=29094..62804
        off x=-60955..-54035,y=33191..54217,z=25181..37101
        on x=52094..81032,y=-29343..-25525,z=-50030..-21747
        off x=57932..85467,y=-4500..2029,z=-46429..-9720
        on x=36386..64660,y=-73512..-44017,z=16903..32394
        on x=-1492..33205,y=-82073..-69752,z=5149..23012
        off x=-6314..2597,y=-82434..-52899,z=-54568..-42166
        off x=47307..75857,y=-39178..-30672,z=23379..47503
        on x=64489..84771,y=-20277..-8288,z=-10334..20346
        off x=-40498..-13375,y=-21960..-1763,z=-89263..-65000
        on x=19554..31662,y=55672..61731,z=-60358..-37905
        on x=-39219..-7291,y=35227..63484,z=-72685..-36545
        on x=43613..70012,y=-33475..-18062,z=33764..41733
        on x=19674..39635,y=-73170..-53816,z=-59139..-34916
        on x=-2303..14767,y=69085..89874,z=-44748..-25607
        off x=-41923..-21656,y=-11475..12712,z=-92655..-63472
        off x=-65609..-28677,y=49338..76417,z=-17143..20767
        on x=-66708..-54445,y=-8844..3726,z=40665..69409
        on x=57741..80306,y=-53751..-27969,z=-2005..11767
        off x=-36833..-8071,y=11487..45001,z=-74496..-55088
        on x=-33089..-16969,y=-41928..-15082,z=-82121..-64107
    }
}
