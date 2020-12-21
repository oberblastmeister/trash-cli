use std::ops::Range;
use std::str::FromStr;
use std::{cmp, vec};

use eyre::{bail, eyre, Context, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RestoreIndex {
    /// includes both start and end
    Range(Range<usize>),
    Point(usize),
}

impl FromStr for RestoreIndex {
    type Err = eyre::Report;

    /// converts from a one based index including the end to 0 based index excluding the end
    fn from_str(s: &str) -> Result<RestoreIndex> {
        if s.is_empty() {
            bail!("Could not parse empty string into RestoreIndex");
        }
        let mut split = s.split('-');
        let start = split.next().expect("BUG: must have at least one");
        let start = start
            .parse::<usize>()
            .wrap_err_with(|| format!("Failed to parse `{}` before - into a number", start))?
            .checked_sub(1) // convert to 0 based index
            .ok_or_else(|| eyre!("Index must be 1 or greater"))?;

        let end = match split.next() {
            Some(end) => end
                .parse::<usize>()
                .wrap_err_with(|| format!("Failed to parse `{}` after - into a number", end))?,
            None => {
                return Ok(RestoreIndex::Point(start));
            }
        };

        if start > end {
            bail!(
                "The range is not well formed, {} is greater than {}",
                start,
                end
            );
        }
        Ok(RestoreIndex::Range(start..end))
    }
}

#[derive(Debug, Clone)]
pub struct RestoreIndexMultiple(Vec<RestoreIndex>);

impl FromStr for RestoreIndexMultiple {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            bail!("Could not parse empty string into restore indexes")
        }
        let mut res = Vec::new();
        for s in s.split_whitespace() {
            let restore_index = s.parse::<RestoreIndex>()?;
            if res.is_empty() {
                res.push(restore_index)
            } else {
                if res
                    .iter()
                    .any(|existing| existing.is_overlapping(&restore_index))
                {
                    bail!("Overlapping range found: {:?}", restore_index);
                }
                res.push(restore_index)
            }
        }
        Ok(RestoreIndexMultiple(res))
    }
}

impl IntoIterator for RestoreIndexMultiple {
    type Item = RestoreIndex;
    type IntoIter = vec::IntoIter<RestoreIndex>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

trait Overlap {
    fn is_overlapping(&self, other: &Self) -> bool;
}

impl Overlap for usize {
    fn is_overlapping(&self, other: &usize) -> bool {
        self == other
    }
}

impl Overlap for Range<usize> {
    fn is_overlapping(&self, other: &Self) -> bool {
        cmp::max(self.start, other.start) <= cmp::min(self.end, other.end)
    }
}

impl Overlap for RestoreIndex {
    fn is_overlapping(&self, other: &Self) -> bool {
        match (self, other) {
            (RestoreIndex::Point(p), RestoreIndex::Range(range)) => range.contains(p),
            (RestoreIndex::Range(range), RestoreIndex::Point(p)) => range.contains(p),
            (RestoreIndex::Range(range1), RestoreIndex::Range(range2)) => {
                range1.is_overlapping(range2)
            }
            (RestoreIndex::Point(p1), RestoreIndex::Point(p2)) => p1.is_overlapping(p2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::ops::Range;
    use RestoreIndex::*;

    #[test]
    fn subtract_overflow() {
        let _ = "0".parse::<RestoreIndex>();
    }

    #[test]
    #[should_panic]
    fn subtract_overflow_error() {
        let _ = "0".parse::<RestoreIndex>().unwrap();
    }

    prop_compose! {
        fn well_formed_range_points(start: usize)(x in start.., y in start..) -> (usize, usize) {
            (cmp::min(x, y), cmp::max(x, y))
        }
    }

    proptest! {
        #[test]
        fn doesnt_crash(s in "\\PC*") {
            let _ = s.parse::<RestoreIndex>();
        }
    }

    proptest! {
        #[test]
        fn parses_index_correctly_range((x, y) in well_formed_range_points(1)) {
            let s = format!("{}-{}", x, y);
            let parsed_range = match s.parse::<RestoreIndex>().unwrap() {
                RestoreIndex::Range(parsed_range) => parsed_range,
                _ => panic!("It was supposed to parse into a range"),
            };
            prop_assert_eq!(x - 1..y, parsed_range);
        }
    }

    proptest! {
        #[test]
        fn parses_index_correctly_point(x in 1usize..) {
            let s = x.to_string();
            let parsed_point = match s.parse::<RestoreIndex>().unwrap() {
                RestoreIndex::Point(point) => point,
                _ => panic!("It was supposed to parse into a point"),
            };
            prop_assert_eq!(x - 1, parsed_point);
        }
    }

    proptest! {
        #[test]
        fn overlapping_points_are_equal(x in any::<usize>(), y in any::<usize>()) {
            let xp = RestoreIndex::Point(x);
            let yp = RestoreIndex::Point(y);

            prop_assert_eq!(xp.is_overlapping(&yp), x == y);
        }
    }

    prop_compose! {
        fn arb_range()(start in any::<usize>(), end in any::<usize>()) -> Range<usize> {
            start..end
        }
    }

    proptest! {
        #[test]
        fn overlapping_range_and_point(p in any::<usize>(), range in arb_range()) {
            let point = RestoreIndex::Point(p);
            let restore_range = RestoreIndex::Range(range.clone());

            prop_assert_eq!(restore_range.is_overlapping(&point), range.contains(&p));
            prop_assert_eq!(point.is_overlapping(&restore_range), range.contains(&p));
        }
    }

    proptest! {
        #[test]
        fn two_overlapping_ranges(r1 in arb_range(), r2 in arb_range()) {
            let restore_range1 = RestoreIndex::Range(r1.clone());
            let restore_range2 = RestoreIndex::Range(r2.clone());

            prop_assert_eq!(restore_range1.is_overlapping(&restore_range2), r1.is_overlapping(&r2));
            prop_assert_eq!(restore_range2.is_overlapping(&restore_range1), r1.is_overlapping(&r2));
        }
    }

    proptest! {
        #[test]
        fn not_well_formed_ranges(x in any::<usize>(), y in any::<usize>()) {
            let s = format!("{}-{}", cmp::max(x, y), cmp::min(x, y));
            let res = s.parse::<RestoreIndex>();
            if res.is_ok() {
                panic!("Should be error");
            }
        }
    }

    fn test_parse_restore_index(s: &str, actual: RestoreIndex) {
        assert_eq!(
            s.parse::<RestoreIndex>()
                .expect(&format!("Failed to parse str `{}` into a restore index", s)),
            actual
        );
    }

    #[test]
    fn from_str() {
        test_parse_restore_index("123-1234", Range(122..1234));
    }

    #[test]
    fn point_from_str_test() {
        test_parse_restore_index("5", Point(4));
    }

    #[test]
    fn another_point_from_str_multiple_char_test() {
        test_parse_restore_index("12347", Point(12346));
    }

    #[should_panic]
    #[test]
    fn missing_end_test() {
        "2340958-".parse::<RestoreIndex>().unwrap();
    }

    #[should_panic]
    #[test]
    fn missing_beginning_test() {
        "-123434".parse::<RestoreIndex>().unwrap();
    }

    #[should_panic]
    #[test]
    fn too_many_dashes_test() {
        "123---1234".parse::<RestoreIndex>().unwrap();
    }

    #[should_panic]
    #[test]
    fn not_a_number_test() {
        "hello".parse::<RestoreIndex>().unwrap();
    }

    #[should_panic]
    #[test]
    fn parse_nothing_test() {
        "".parse::<RestoreIndex>().unwrap();
    }

    #[test]
    fn is_overlapping_same_range_test() {
        assert!((1..1).is_overlapping(&(1..1)));
    }

    #[test]
    fn is_overlapping2_range_test() {
        assert!((1..10).is_overlapping(&(1..4)));
    }

    #[test]
    fn is_not_overlapping_range_test() {
        assert!(!(1..4).is_overlapping(&(10..1234)));
    }

    #[test]
    fn is_overlapping_test() {
        assert!(Range(1..3).is_overlapping(&Range(1..3)));
    }

    #[test]
    fn is_overlapping2_test() {
        assert!(Range(1..9).is_overlapping(&Range(3..6)));
    }

    #[test]
    fn is_not_overlapping_test() {
        assert!(!Range(1..3).is_overlapping(&Range(5..10)));
    }

    #[test]
    fn is_overlapping_same_test() {
        assert!(Range(1..1).is_overlapping(&Range(1..1)));
    }

    #[test]
    fn is_overlapping_different_test() {
        assert!(Point(5).is_overlapping(&(Range(1..15))));
    }

    #[test]
    fn is_overlapping_different2_test() {
        assert!(Range(3..8).is_overlapping(&Point(4)));
    }

    #[test]
    fn is_overlapping_points_test() {
        assert!(Point(4).is_overlapping(&Point(4)));
    }

    #[test]
    fn get_multiple_test() {
        assert_eq!(
            "4 40 3 9-12".parse::<RestoreIndexMultiple>().unwrap().0,
            vec![Point(3), Point(39), Point(2), Range(8..12),]
        );
    }

    #[should_panic]
    #[test]
    fn get_multiple_overlapping_test() {
        "4 30 5-13 7-8 9".parse::<RestoreIndexMultiple>().unwrap();
    }

    #[should_panic]
    #[test]
    fn get_multiple_none() {
        "".parse::<RestoreIndexMultiple>().unwrap();
    }
}
