#[macro_use]
extern crate lazy_static;
extern crate num;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
pub mod utils {
    use std::iter::FromIterator;
    use std::iter::Iterator;
    use std::collections::HashMap;
    use std::ops::Shr;
    use std::ops::Shl;
    use std::ops::BitOr;
    use std::mem::size_of;


    fn to_ascii(word: &Vec<u8>) -> Vec<char> {
        word.clone().into_iter().map(|x| x as char).collect()
    }
    fn from_ascii(character: &Vec<char>) -> Vec<u8> {
        character.clone().into_iter().map(|x| x as u8).collect()
    }
    struct Encoding {
        to: HashMap<u8,char>,
        from: HashMap<char,u8>
    }
    fn encoding_from_list(pair: Vec<(u8,char)>) -> Encoding {
        Encoding {
            to: HashMap::from_iter(pair.clone().into_iter()),
            from: pair.clone().into_iter().map(|p| (p.1,p.0)).collect()
        } 
    }
    lazy_static! { static ref HEX: Encoding = encoding_from_list(
                            (0..15).zip((48u8..58).chain(97..103).map(|x| x as char))
                            .collect());
    }
    lazy_static! { static ref B64: Encoding = encoding_from_list(
                            (0..63).zip((0x41u8..0x5B)
                            .chain(0x61..0x7B).chain(0x30..0x3A)
                            .chain(0x2B..=0x2B).chain(0x2F..=0x2F).map(|x| x as char))
                            .collect());
    }
    fn char_to_integral<T>(encoding: Encoding, character: &char) -> T 
    where T: From<u8> 
    {
        T::from(encoding.from[character])
    }

    fn get_byte<T,S>(shiftable: T,byte: S) -> u8 
    where u8: From<T>,
          T: Shr<S, Output = T> 
    {
       u8::from(shiftable >> byte)
    }

    fn to_bytes<T>(val: T) -> Vec<u8> 
    where u8: From<T>,
          T: Shr<u32, Output = T>,
          T: Copy 
    {
        (0..size_of::<T>() as u32).map(|i| get_byte(val, i)).collect()
    }
    fn from_bytes<T>(bytes: &Vec<u8>) -> T
    where T: From<u8>,
          T: Shl<u8, Output = T>,
          T: Copy,
          T: BitOr<T, Output = T>,
          T: num::Zero
    {
        let mut v = bytes.clone();
        v.reverse();
        (0u8..).step_by(8).zip(v)
        .map(|x| (T::from(x.1) << x.0))
        .fold(num::Zero::zero(), |acc, x| acc | x)
    }
    fn get_num_of_bits(word:u32) -> u32
    {
        (word as f32).log2().ceil() as u32
    }
    fn encode_bits<T>(e: Encoding, value: T) -> Vec<char> 
    where T: Into<u8>,
          T: Into<u32>,
          T: Shl<u32, Output = T>,
          T: Shr<u32, Output = T>,
          T: Copy
    {
        let num_bits_in_endcoding = get_num_of_bits(value.into()) as f32;
        let mut out : Vec<char> = Vec::new();
        let length_of_value = size_of::<T>() as f32;
        for i in (0..((length_of_value/num_bits_in_endcoding).ceil() as u32)).step_by(num_bits_in_endcoding as usize)
        {
            let mut bits_out = value << i;
            bits_out = bits_out >> (size_of::<T>() as u32) - (num_bits_in_endcoding as u32);
            out.push(e.to[&bits_out.into()]);
        }
        out
    }


}
