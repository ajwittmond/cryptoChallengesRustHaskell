#[macro_use]
extern crate lazy_static;
extern crate num;


/// This module contains functions for encoding a decoding strings.
/// It works primarily with Vecs of u8s but contains functions for decoding
/// Vecs of chars to vecs of u8s.
/// This implementation is naive and makes lots of unnecessary allocations
/// TODO improve this by using iterators
pub mod byte_utils {
    use std::iter::FromIterator;
    use std::iter::Iterator;
    use std::collections::HashMap;
    use std::ops::Shr;
    use std::ops::Shl;
    use std::ops::{Mul, BitOr, BitAnd};
    use std::mem::size_of;
    use std::{cmp::max, convert::TryInto};


    /// This struct represents a way of encoding binary data as text.
    /// For example, it might represent base64 or hex.
    /// Two hashmaps are included for rapid encoding and decoding
    pub struct Encoding {
        to: HashMap<u8,char>,
        from: HashMap<char,u8>
    }

    /// Consumes a list of key value pairs to generate an encoding
    pub fn encoding_from_list(pair: Vec<(u8,char)>) -> Encoding {
        Encoding {
            to: HashMap::from_iter(pair.clone().into_iter()),
            from: pair.clone().into_iter().map(|p| (p.1,p.0)).collect()
        }
    }

    lazy_static! { pub static ref HEX: Encoding = encoding_from_list(
                            (0..=15).zip(('0'..='9').chain('a'..='f'))
                            .collect());
    }

    lazy_static! { pub static ref B64: Encoding = encoding_from_list(
                            (0..=63).zip(('A'..='Z')
                            .chain('a'..='z').chain('0'..='9').chain(vec!['+','/']))
                            .collect());
    }

    /// Tests if a char is a possible character value in the encoding.
    /// For example: in_encoding(HEX,';') == False
    pub fn in_encoding(encoding: &Encoding, character: &char) -> bool
    {
        encoding.from.keys().any(|c| c == character)
    }

    /// Gives the integral value of a character using a encoding in a polymophic way.
    /// So we can do
    /// let x: u32 = char_to_integral(HEX, 'a');
    /// and
    /// let y : u16 = char_to_integral(HEX, 'b');
    pub fn char_to_integral<T>(encoding: &Encoding, character: &char) -> T
    where T: From<u8>
    {
        T::from(encoding.from[character])
    }

    ///gets the byte of an integral value at a given position
    ///get_byte(512,1)
    pub fn get_byte<T,S>(shiftable: T,byte: S) -> u8
    where T: Shr<S, Output = T> + Copy + TryInto<u8> + BitAnd<Output = T> + From<u8>,
          S: Mul<Output = S>,
          S: From<u8>
    {
       match ((shiftable >> byte * S::from(8)) & T::from(255)).try_into() {
        Ok(a) => a,
        _ => unreachable!(),
       }
    }

    struct ToBytes<T> {
        start: usize,
        end:usize,
        value: T
    }

    impl<T: Copy> ToBytes<T>{
        fn new(val: &T) -> ToBytes<T> {
            ToBytes{start: 0 , end: size_of::<T>(), value: *val}
        }
    }

    impl<T> Iterator for ToBytes<T>
    where T : TryInto<u8> + Shr<u32, Output = T> + Copy + BitAnd<Output = T> + From<u8>{
        type Item = u8;

        fn next(self : &mut ToBytes<T>) -> Option<u8>{
            if self.start < self.end{
                self.start += 1;
                Some(get_byte(self.value,(self.start -1).try_into().unwrap()))
            }else{
                None
            }
        }
    }

    impl<T> DoubleEndedIterator for ToBytes<T>
    where T : TryInto<u8> + Shr<u32, Output = T> + Copy + BitAnd<Output = T> + From<u8>{

        fn next_back(self : &mut ToBytes<T>) -> Option<u8>{
            if self.start < self.end{
                self.end -= 1;
                Some(get_byte(self.value,(self.end).try_into().unwrap()))
            }else{
                None
            }
        }

    }

    /// Transform an integral value into a vector of its constituent bytes
    pub fn to_bytes<T>(val: & T) -> impl DoubleEndedIterator<Item = u8>
    where T : TryInto<u8> + Shr<u32, Output = T> + Copy + BitAnd<Output = T> + From<u8>{
        ToBytes::new(val)
    }

    ///Takes list of bytes and returns them as a single number in a polymorphic way.
    ///from_bytes(vec![1,0]) = 512
    pub fn from_bytes<I,T>(bytes: I) -> T
    where T: From<u8>,
          T: Shl<u8, Output = T>,
          T: Copy,
          T: BitOr<T, Output = T>,
          T: num::Zero,
          I: Iterator<Item = u8>
    {
        let block_size : u8 = match size_of::<T>().try_into(){
            Ok(x) => x,
            _  => panic!("invalid block size")
        };
        (0u8..).step_by(8).zip(bytes)
            .map(|x| (T::from(x.1) << 8 * (block_size - 1) - x.0))
            .fold(num::Zero::zero(), |acc, x| acc | x)
    }

    ///returns the number of bits needed to store the passed value
    pub fn get_num_of_bits(word:u32) -> u32
    {
        (word as f32).log2().ceil() as u32
    }

    ///Converts some integral value to its representation in the passed encoding
    fn encode_block<T>(e: & 'static Encoding, block: T) -> impl Iterator<Item = char>
    where T: TryInto<u8>,
          T: Into<u32>,
          T: Shl<u32, Output = T>,
          T: Shr<u32, Output = T>,
          T: Copy,
          <T as std::convert::TryInto<u8>>::Error : std::fmt::Debug
    {
        let num_bits_in_encoding = get_num_of_bits(e.from.len() as u32) ;
        let bits_in_block = ( size_of::<T>()  * 8 ) as u32;
        (0..bits_in_block)
            .step_by(num_bits_in_encoding as usize)
            .zip(std::iter::repeat(block)).map(move |(i,block)|{
                let mut bits_out = block <<   i;
                bits_out = bits_out >> bits_in_block - (num_bits_in_encoding as u32);
                e.to[&bits_out.try_into().expect("truncation error that should be impossible")]
        })
    }

    ///Takes a string and an encoding a puts the resulting bits into the desired integral value
    fn decode_block<T>(encoding: &Encoding, encoded: &Vec<char>) -> T
    where T: From<u8>,
          T: Shl<u32, Output = T>,
          T: Shr<u32, Output = T>,
          T: Copy,
          T: BitOr<T, Output = T>,
          T: num::Zero
    {
        let num_bits_in_encoding = get_num_of_bits(encoding.from.len() as u32);
        let zeroes = (num_bits_in_encoding * encoded.len() as u32) % 8;
        let mut bytes = encoded.iter().map(|c| encoding.from[&c]).rev();
        let mut out: T = num::Zero::zero();
        let mut position = 0;
        for b in bytes
        {
            let x = T::from(b) << position;
            position = position + num_bits_in_encoding;
            out = out | x;
        }
        out >> zeroes
    }

    /// Converts a hex encoded string to bytes
    pub fn hex_to_bytes(s: &Vec<char>) -> Vec<u8>
    {
        if s.len() % 2 != 0 {
            panic!("not a valid hex string")
        }
        s.chunks(2).map(|ls| decode_block(&HEX,&(ls.to_vec()))).collect()
    }

    /// Converts a base64 encoded string to bytes
    pub fn base64_to_bytes(s: &Vec<char>) -> Vec<u8>
    {
        let filtered : Vec<char> = s.iter().copied().filter(|x| *x != '=').collect();
        filtered.chunks(4).flat_map(|ls|
            {
                let l = ls.len();
                let drops = match l
                {
                    4 => 1,
                    3 => 1,
                    2 => 2,
                    1 => 1,
                    _ => panic!("Nani!")
                };
                let block: u32 = decode_block(&B64,&(ls.to_vec()));
                to_bytes(&block).rev().skip(drops)
            }).collect()
    }

    /// converts a string of bytes to base64
    pub fn bytes_to_base64(b: &Vec<u8>) -> impl Iterator<Item = char> + '_
    {
        //Number of bytes given
        let size: usize = b.len() * 4 / 3;

        // = chars added to the end of base64
        let tail = if size % 4 != 0 { 4 - (size % 4) } else {0};

        //Break bytes into chunks of three, i.e. one block, pad with zeros, and convert to u32
        //Recall that each b64 is 6 bits, and each byte is 8 bits. lcm(6,8) = 24
        b.chunks(3)
         .map(|chungus| chungus
                .into_iter()
                .copied()
                .chain(std::iter::repeat(0))
                .take(4)
        ).flat_map(
             |chungus| encode_block(&B64,from_bytes::<_,u32>(chungus)).take(4)
         ).chain(
             std::iter::repeat('=').take(tail)
         )
    }

    /// converts a string of bytes to a hex encoded string
    pub fn bytes_to_hex(b: &Vec<u8>) -> Vec<char>
    {
        b.iter().copied().flat_map(|byte| encode_block(&HEX,byte)).collect()
    }

    //Xors each byte. If lists are different lengths, it returns a list the length of the shortest list
    pub fn block_xor(bytes1: &Vec<u8>, bytes2: &Vec<u8>) -> Vec<u8>
    {
        bytes1.iter().zip(bytes2.iter()).map(|(b1,b2)| b1 ^ b2).collect()
    }

    /// returns the number of positive bits
    fn pop_count(n: u8) -> u8
    {
        let mut p: u8 = 0;
        for i in 0..8
        {
            p += (n >> i) & 1;
        }
        p
    }

    /// calculates the hamming distance between two bytestrings
    /// truncating the longer string
    pub fn hamming_distance(bytes1: &[u8], bytes2: &[u8]) -> usize
    {
        bytes1.iter().zip(bytes2.iter())
            .fold(0, |acc, (b1,b2)| acc + pop_count(b1^b2)) as usize
    }

    /// checks if character is a valid base64 character
    pub fn is_base64(character: &char) -> bool
    {
        in_encoding(&B64, character)
    }

}

#[cfg(test)]
mod tests {
    use super::byte_utils::*;


    const test_string : &str = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.";

    const base64_result : &str = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=";

    const hex_result : &str = "4d616e2069732064697374696e677569736865642c206e6f74206f6e6c792062792068697320726561736f6e2c2062757420627920746869732073696e67756c61722070617373696f6e2066726f6d206f7468657220616e696d616c732c2077686963682069732061206c757374206f6620746865206d696e642c20746861742062792061207065727365766572616e6365206f662064656c6967687420696e2074686520636f6e74696e75656420616e6420696e6465666174696761626c652067656e65726174696f6e206f66206b6e6f776c656467652c2065786365656473207468652073686f727420766568656d656e6365206f6620616e79206361726e616c20706c6561737572652e";

    const hex : &str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";

    const base64 : &str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

    
    #[test]
    fn hex_to_base_64(){
        let bytes = hex_to_bytes(&hex.chars().collect());
        assert!(2*bytes.len() == hex.len());
        assert_eq!(*base64,bytes_to_base64(&bytes).into_iter().collect::<String>()[..]);
    }


}
