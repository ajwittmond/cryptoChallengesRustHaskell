
pub mod xor{
    use std::iter::FromIterator;
    pub fn xor_vec<I,J>(a: & I, b: & I) -> J
    where I: IntoIterator<Item = u8>,
          I::IntoIter : std::clone::Clone,
          I: std::clone::Clone,
          J: FromIterator<u8>{
        a.clone().into_iter().zip(b.clone().into_iter().cycle()).map(|(x,y)| x ^ y ).collect()
    }
}
